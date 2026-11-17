import fcntl, json, os, pty, re, select, signal, struct, sys, termios, time

args = sys.argv[1:]
timeout, out = float(args[0]), args[1]
split = args.index("--")
commands, cli = args[2:split], args[split + 1:]
ANSI = re.compile(rb"\x1b\[[0-9;?]*[A-Za-z]")
CODE = re.compile(rb"Code: (SN(\d+)(?:-[0-9A-Z]+)+)")
REFUSED = re.compile(rb"bad chat command|chat error|[Ee]rror:")

pid, fd = pty.fork()
if pid == 0:
    os.execvp(cli[0], cli)
fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 50, 200, 0, 0))
seen = b""

def fail(message):
    os.kill(pid, signal.SIGTERM)
    sys.exit(message)

def expect(pattern):
    global seen
    deadline = time.monotonic() + timeout
    while True:
        text = ANSI.sub(b"", seen)
        if (refused := REFUSED.search(text)):
            fail(f"refused: {text[refused.start():].splitlines()[0].decode(errors='replace')}")
        if (found := pattern.search(text)):
            seen = text[found.end():]
            return found
        left = deadline - time.monotonic()
        if left <= 0 or not select.select([fd], [], [], left)[0]:
            fail(f"no {pattern.pattern!r} within {timeout} s")
        try:
            chunk = os.read(fd, 65536)
        except OSError:
            chunk = b""
        if not chunk:
            fail(f"the console ended before {pattern.pattern!r}")
        sys.stdout.buffer.write(chunk)
        seen += chunk

def type_line(line):
    for typed in range(1, len(line) + 1):
        os.write(fd, line[typed - 1].encode())
        expect(re.compile(b"> " + re.escape(line[:typed].encode())))
    os.write(fd, b"\r")

expect(re.compile(rb"Current user: .*\n(?:.*\n)*?.*>  "))
codes = []
for command in commands:
    type_line(f"//issue {command}")
    found = expect(CODE)
    years = re.search(r"years (\d+)", command)
    codes.append({"code": found[1].decode(), "minLength": int(found[2]), "years": int(years[1]) if years else 2})
type_line("/quit")
os.waitpid(pid, 0)
with open(out, "w") as f:
    json.dump(codes, f)
