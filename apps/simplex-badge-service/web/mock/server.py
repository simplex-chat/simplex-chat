"""Stands in for the Haskell service AND for Stripe and BTCPay, so the whole
browser flow can be driven without any of them. A test fixture: no signatures,
no persistence, no money, and not a specification of the real service.

Standard library only. Threaded, because the wait endpoint holds a connection.

Environment:
  MOCK_HOLD_SECONDS         how long GET /api/invoice/:id?wait= holds (default 30)
  STRIPE_PUBLISHABLE_KEY    substituted into the
                            served index.html. Public by design, but still not
                            committed: unset, the page has NO card form and
                            choosing Card reaches "The card form did not load",
                            so drive a purchase with btc or xmr and settle it
                            with POST /control/settle/<invoiceId> below.
                            Set it to a `pk_test_...` key to drive the real
                            Stripe path; a secret key here is refused at start.
"""
import json, os, re, secrets, sys, threading
from datetime import datetime, timedelta, timezone
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import urlparse, parse_qs

ROOT = Path(__file__).resolve().parent.parent
HOLD_SECONDS = float(os.environ.get("MOCK_HOLD_SECONDS", "30"))
STRIPE_PUBLISHABLE_KEY = os.environ.get("STRIPE_PUBLISHABLE_KEY", "").strip()
KEY_META = re.compile(r'(<meta id="stripe-publishable-key"[^>]*content=")[^"]*(")')

CATALOG = {
    "price_supporter": {"badgeType": "supporter", "monthPrice": 700},
    "price_legend": {"badgeType": "legend", "monthPrice": 7000},
}
OFFERS = {
    "offer_3m": {"months": 3, "free": 1},
    "offer_12m": {"months": 12, "discount": 50},
    "offer_3m_s": {"months": 3, "free": 1},
    "offer_12m_s": {"months": 12, "discount": 50},
}
MIME = {".html": "text/html", ".css": "text/css", ".js": "text/javascript",
        ".svg": "image/svg+xml", ".json": "application/json", ".webmanifest": "application/manifest+json"}
# BTCPay's default speed policy asks for one confirmation.
REQUIRED_CONFIRMATIONS = 1
ADDRESSES = {"btc": "bc1qexampleaddress0k3jq2wvcgmqz", "xmr": "48HqK2XmVexampleAddress9fRtWc"}

LOCK = threading.Lock()
INVOICES = {}
HASHES = {}
EVENTS = {}


def now_iso():
    return datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def total(price_id, offer_id):
    price = CATALOG.get(price_id)
    if price is None:
        return None
    if not offer_id:
        return {"months": 1, "amount": price["monthPrice"]}
    offer = OFFERS.get(offer_id)
    if offer is None:
        return None
    gross = price["monthPrice"] * offer["months"]
    if "free" in offer:
        amount = price["monthPrice"] * (offer["months"] - offer["free"])
    else:
        amount = (gross * (100 - offer["discount"])) // 100
    return {"months": offer["months"], "amount": amount}


def payment_mark(inv):
    """The figure the page shows and the verdict it shows it under, which together decide
    which screen it is on."""
    return (inv.get("cryptoAmountPaid") or "", inv.get("paidInFull") is True)


def swap_event(invoice_id):
    """Called under LOCK; returns the event to fire once it is released. A fresh Event for
    whoever parks next, so a waiter that read the pre-change status but calls wait() after we
    fire this one still catches its own (already-set) event instead of racing a clear() on a
    shared one and missing the wake."""
    old = EVENTS.get(invoice_id)
    EVENTS[invoice_id] = threading.Event()
    return old


def public_view(inv):
    """What the browser may see. Note what is absent: no code, no code hash — the service
    never has the code."""
    view = {
        "status": inv["status"], "badgeType": inv["badgeType"], "months": inv["months"],
        "amount": inv["amount"], "currency": inv["currency"],
        "expiresAt": inv["expiresAt"],
    }
    for k in ("amountPaid", "cryptoAmountPaid", "cryptoAmountDue", "settledAt"):
        if inv.get(k) is not None:
            view[k] = inv[k]
    if inv.get("paidInFull") is not None:
        view["paidInFull"] = inv["paidInFull"]
    if inv["method"] == "card":
        view["clientSecret"] = inv["clientSecret"]
    else:
        view["address"] = inv["address"]
        view["cryptoAmount"] = inv["cryptoAmount"]
        view["cryptoCurrency"] = inv["method"]
        view["requiredConfirmations"] = REQUIRED_CONFIRMATIONS
    return view


def with_publishable_key(html):
    """The publishable key is compiled into the page. Here it comes
    from the environment, so nothing that could be a real key is ever written
    back into public/index.html. Unset leaves the committed empty value, with
    which the page offers no card form at all."""
    if not STRIPE_PUBLISHABLE_KEY:
        return html
    text, found = KEY_META.subn(lambda m: m.group(1) + STRIPE_PUBLISHABLE_KEY + m.group(2), html.decode())
    if found != 1:
        raise RuntimeError("mock: the shell has no stripe-publishable-key meta element to fill")
    return text.encode()


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, *args):
        pass

    def _send(self, status, payload, ctype="application/json"):
        body = payload if isinstance(payload, bytes) else json.dumps(payload).encode()
        try:
            self.send_response(status)
            self.send_header("content-type", ctype)
            self.send_header("content-length", str(len(body)))
            self.send_header("cache-control", "no-store")
            self.end_headers()
            self.wfile.write(body)
        except (BrokenPipeError, ConnectionResetError):
            # A client that drops during a long poll leaves the invoice untouched and the page
            # reissues on its next load, so there is nothing to retry.
            self.close_connection = True

    def _read_json(self):
        length = int(self.headers.get("content-length") or 0)
        try:
            return json.loads(self.rfile.read(length) or b"{}")
        except Exception:
            return None

    def do_POST(self):
        path = urlparse(self.path).path

        if path.startswith("/control/"):
            parts = path.strip("/").split("/")
            if len(parts) != 3:
                return self._send(400, {"error": "bad_request"})
            _, action, invoice_id = parts
            with LOCK:
                inv = INVOICES.get(invoice_id)
                if inv is None:
                    return self._send(404, {"error": "not_found"})
                if action == "settle":
                    inv["status"] = "paid"
                    inv["amountPaid"] = inv["amount"]
                    inv["settledAt"] = now_iso()
                    inv["paidInFull"] = True
                    if inv["method"] != "card":
                        inv["cryptoAmountPaid"] = inv["cryptoAmount"]
                        inv["cryptoAmountDue"] = "0.000"
                elif action == "expire":
                    inv["status"] = "expired"
                elif action == "confirming":
                    inv["amountPaid"] = inv["amount"]
                    inv["paidInFull"] = True
                    if inv["method"] != "card":
                        inv["cryptoAmountPaid"] = inv["cryptoAmount"]
                        inv["cryptoAmountDue"] = "0.000"
                elif action == "verdict":
                    # Monero reports the payment as paid in full while its figures are still zero.
                    inv["paidInFull"] = True
                elif action == "partial":
                    inv["amountPaid"] = inv["amount"] // 2
                    inv["paidInFull"] = False
                    if inv["method"] != "card":
                        inv["cryptoAmountPaid"] = "0.734"
                        inv["cryptoAmountDue"] = "0.752"
                else:
                    return self._send(400, {"error": "bad_request"})
                status = inv["status"]
                old_event = swap_event(invoice_id)
            if old_event is not None:
                old_event.set()
            return self._send(200, {"ok": True, "status": status})

        if path.startswith("/api/invoice/") and path.endswith("/cancel"):
            invoice_id = path[len("/api/invoice/"):-len("/cancel")]
            with LOCK:
                inv = INVOICES.get(invoice_id)
                if inv is None:
                    return self._send(404, {"error": "not_found"})
                if inv["status"] != "open":
                    return self._send(409, {"error": "not_open"})
                if payment_mark(inv) != ("", False) or inv.get("amountPaid"):
                    # Cancelling a funded invoice would strand what the buyer already sent.
                    return self._send(409, {"error": "funded"})
                inv["status"] = "expired"
                payload = {"invoiceId": invoice_id, **public_view(inv)}
                old_event = swap_event(invoice_id)
            if old_event is not None:
                old_event.set()
            return self._send(200, payload)

        if path == "/api/invoice":
            body = self._read_json()
            if not body or not isinstance(body.get("codeHash"), str) or not body["codeHash"]:
                return self._send(400, {"error": "bad_request"})
            if body.get("method") not in ("card", "btc", "xmr"):
                return self._send(400, {"error": "bad_request"})
            with LOCK:
                if body["codeHash"] in HASHES:
                    return self._send(409, {"error": "code_conflict"})
                t = total(body.get("priceId"), body.get("offerId"))
                if t is None:
                    return self._send(400, {"error": "catalog_changed"})
                invoice_id = secrets.token_urlsafe(16)
                is_card = body["method"] == "card"
                inv = {
                    "invoiceId": invoice_id, "method": body["method"], "status": "open",
                    "badgeType": CATALOG[body["priceId"]]["badgeType"], "months": t["months"],
                    "amount": t["amount"], "currency": "usd",
                    "expiresAt": (datetime.now(timezone.utc) + timedelta(hours=1))
                        .replace(microsecond=0).isoformat().replace("+00:00", "Z"),
                    "clientSecret": f"cs_test_{secrets.token_hex(12)}" if is_card else None,
                    "address": None if is_card else ADDRESSES[body["method"]],
                    "cryptoAmount": None if is_card else "1.482",
                }
                INVOICES[invoice_id] = inv
                HASHES[body["codeHash"]] = invoice_id
                EVENTS[invoice_id] = threading.Event()
                payload = {"invoiceId": invoice_id, **public_view(inv)}
            return self._send(200, payload)

        return self._send(405, {"error": "bad_request"})

    def do_GET(self):
        parsed = urlparse(self.path)
        # An empty `seenPaid=` means the page rendered no figure, which differs from the parameter
        # being absent, so blank values are kept.
        path, query = parsed.path, parse_qs(parsed.query, keep_blank_values=True)

        if path.startswith("/api/invoice/"):
            invoice_id = path[len("/api/invoice/"):]
            with LOCK:
                inv = INVOICES.get(invoice_id)
                if inv is None:
                    return self._send(404, {"error": "not_found"})
                current = inv["status"]
                held = payment_mark(inv)
                event = EVENTS.get(invoice_id)
            wait = (query.get("wait") or [None])[0]
            # A payment recorded before this request arrived cannot fire the event this request
            # would wait on, so holding then would strand the buyer on the payment screen.
            seen = ((query.get("seenPaid") or [""])[0], (query.get("seenFull") or ["0"])[0] == "1")
            unseen = "seenPaid" in query and seen != held
            if wait is not None and wait == current and not unseen and event is not None:
                # The event is never cleared; settle, expire and partial replace it with a fresh
                # one under the lock, so a set event always means a change happened after this
                # reference was taken.
                event.wait(timeout=HOLD_SECONDS)
            with LOCK:
                inv = INVOICES[invoice_id]
                payload = {"invoiceId": invoice_id, **public_view(inv)}
            return self._send(200, payload)

        rel = "index.html" if path == "/" else path.lstrip("/")
        for base in ("public", "dist"):
            base_resolved = (ROOT / base).resolve()
            candidate = (base_resolved / rel).resolve()
            try:
                candidate.relative_to(base_resolved)
            except ValueError:
                # The path escaped this base directory, so move to the next one.
                continue
            if candidate.is_file():
                ctype = MIME.get(candidate.suffix, "application/octet-stream")
                body = candidate.read_bytes()
                if candidate.name == "index.html":
                    body = with_publishable_key(body)
                return self._send(200, body, ctype)
        return self._send(404, {"error": "not_found"})


class Server(ThreadingHTTPServer):
    daemon_threads = True

    def handle_error(self, request, client_address):
        # socketserver's default prints a traceback for a dropped client, which is routine under
        # a long poll, so suppress it while letting anything else surface.
        if isinstance(sys.exc_info()[1], (BrokenPipeError, ConnectionResetError)):
            return
        super().handle_error(request, client_address)


def main():
    port = 8099
    if "--port" in sys.argv:
        port = int(sys.argv[sys.argv.index("--port") + 1])
    if STRIPE_PUBLISHABLE_KEY and not STRIPE_PUBLISHABLE_KEY.startswith("pk_"):
        # A secret or restricted key (sk_, rk_) would be written into the page for anyone to read,
        # so only a publishable key (pk_) is allowed.
        sys.exit("mock: STRIPE_PUBLISHABLE_KEY must be a publishable key (pk_...)")
    server = Server(("127.0.0.1", port), Handler)
    print(f"mock badge service on http://localhost:{port}", flush=True)
    print("stripe: " + (f"publishable key {STRIPE_PUBLISHABLE_KEY[:11]}… — the real card form"
                        if STRIPE_PUBLISHABLE_KEY
                        else "no STRIPE_PUBLISHABLE_KEY — no card form; pay with btc or xmr"),
          flush=True)
    server.serve_forever()


if __name__ == "__main__":
    main()
