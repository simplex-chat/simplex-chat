# The World's Most Secure Messaging

**Ultimate security**: SimpleX network uses the most secure end-to-end encryption, with continuous post-quantum key exchange to protect all messages and metadata.

**Unique privacy**: SimpleX network has no user profile IDs, not even random numbers or keys. It provides better privacy of your contacts, protecting who you talk with from network servers.

**No spam**: nobody can contact you unless you share 1-time link or long-term address.

**Data ownership**: only your device stores your profiles, contacts and messages. You can securely move your data to another device. Servers store encrypted messages only while your device is offline.

**Secure decentralization**: you control which servers to connect to. For security 4 different servers are used in each chat — they can't observe which IP addresses talk to each other.

#### How to connect to others

- Tap new chat button in the corner, then create 1-time link.
- Share the link with your contact via any other messenger or email - it is secure.
- Ask your contact to use the link in the app - click it after the app is installed or paste into the search field in the app.

## Perché SimpleX è unico

### Hai una privacy completa

SimpleX protegge la privacy del tuo profilo, contatti e metadati, nascondendoli ai server della rete SimpleX e ad eventuali osservatori.

A differenza di qualsiasi altra rete di messaggistica esistente, SimpleX non ha identificatori assegnati agli utenti — **nemmeno numeri casuali**.

### Sei protetto da spam e abusi

Poiché non hai un identificatore o un indirizzo fisso sulla rete SimpleX, nessuno può contattarti a meno che tu non condivida un indirizzo utente una tantum o temporaneo, come codice un QR o un link.

### Sei tu a controllare i tuoi dati

SimpleX conserva tutti i dati utente sui dispositivi client in un **formato trasferibile di database crittografato** — può essere trasferito su un altro dispositivo.

I messaggi crittografati end-to-end vengono conservati temporaneamente sui server di inoltro SimpleX fino alla ricezione, quindi vengono eliminati definitivamente.

### Possiedi la rete SimpleX

La rete SimpleX è completamente decentralizzata e indipendente da qualsiasi criptovaluta o altra rete, ad eccezione di internet.

Puoi **usare SimpleX con i tuoi server personali** o con i server forniti da noi — e connetterti comunque a qualsiasi utente.

# Piena privacy della tua identità, profilo, contatti e metadati

A differenza di altre reti di messaggistica, SimpleX non ha **alcun identificatore assegnato agli utenti**. Non si basa su numeri di telefono, indirizzi basati su domini (come email o XMPP), nomi utente, chiavi pubbliche o persino numeri casuali per identificare i suoi utenti — gli operatori dei server non sanno quante persone usano i loro server.

Per recapitare i messaggi, SimpleX usa [indirizzi anonimi a coppie](https://csrc.nist.gov/glossary/term/Pairwise_Pseudonymous_Identifier) di code di messaggi unidirezionali, separate per i messaggi ricevuti e inviati, di solito tramite server diversi.

Questo design protegge la privacy di chi stai comunicando, nascondendola ai server della rete SimpleX e a qualsiasi osservatore. Per nascondere il tuo indirizzo IP ai server, puoi **connetterti ai server SimpleX tramite Tor**.

# La migliore protezione da spam e abusi

Poiché non hai alcun identificatore sulla rete SimpleX, nessuno può contattarti a meno che tu non condivida un indirizzo utente una tantum o temporaneo, come un codice QR o un link.

Anche l'indirizzo utente opzionale, che può essere usato per inviare richieste di contatto spam, è possibile modificarlo o eliminarlo completamente senza perdere alcuna connessione.

# Proprietà, controllo e sicurezza dei tuoi dati

SimpleX Chat conserva tutti i dati utente solo sui dispositivi client usando un **formato trasferibile di database crittografato** che può essere esportato e trasferito su qualsiasi dispositivo supportato.

I messaggi crittografati end-to-end vengono conservati temporaneamente sui server di inoltro SimpleX fino alla ricezione, quindi vengono eliminati definitivamente.

A differenza dei server di reti federate (email, XMPP o Matrix), i server SimpleX non conservano gli account utente, ma trasmettono solo i messaggi, proteggendo la privacy di entrambe le parti.

Non ci sono identificatori o testi cifrati in comune tra il traffico del server inviato e quello ricevuto — se qualcuno lo osserva, non può determinare facilmente chi comunica con chi, anche se il TLS è compromesso.

# Completamente decentralizzata — gli utenti possiedono la rete SimpleX

Puoi **usare SimpleX con i tuoi server personali** e continuare a comunicare con le persone che usano i server preconfigurati nelle app.

La rete di SimpleX usa un [protocollo aperto](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md) e fornisce un [SDK per creare chat bot](https://github.com/simplex-chat/simplex-chat/tree/stable/packages/simplex-chat-client/typescript), consentendo l'implementazione di servizi con cui gli utenti possono interagire tramite le app SimpleX Chat — siamo impazienti di vedere quali servizi SimpleX creerai.

Se stai pensando di sviluppare per la rete SimpleX, ad esempio, il chat bot per gli utenti dell'app SimpleX o l'integrazione della libreria SimpleX Chat nelle tue app mobili, [contattaci](https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D) per qualsiasi consiglio e supporto.

## Caratteristiche

Messaggi crittografati E2E con markdown e modifica

Immagini, video e file
crittografati E2E

Gruppi decentralizzati crittografati E2E — solo gli utenti sanno che esistono

Messaggi vocali crittografati E2E

Conversazioni segrete a tempo

Chiamate audio e video
crittografate E2E

Archiviazione dell'app crittografata e trasferibile — sposta il profilo su un altro dispositivo

Modalità incognito —
unica su SimpleX Chat

## Cosa rende SimpleX privato

### Identificatori temporanei anonimi a coppie

SimpleX usa indirizzi e credenziali temporanei anonimi a coppie, per ogni contatto o membro del gruppo.

Ciò consente ai messaggi di venire recapitati senza identificatori del profilo utente, garantendo una migliore privacy dei metadati rispetto alle alternative.

### Scambio di chiavi fuori banda

Molte reti di comunicazione sono vulnerabili agli attacchi MITM da parte di server o fornitori di rete.

Per evitarlo, le app SimpleX passano chiavi monouso fuori banda, quando condividi un indirizzo come link o codice QR.

### 2 livelli di crittografia end-to-end

Protocollo Double-ratchet —
Messaggistica OTR con Perfect Forward Secrecy e recupero da intrusione.

Cryptobox NaCL in ogni coda per evitare correlazioni di traffico tra code di messaggi se il TLS è compromesso.

### Verifica dell'integrità dei messaggi

Per garantire l'integrità, i messaggi sono numerati in sequenza e includono l'hash del messaggio precedente.

Se un messaggio viene aggiunto, rimosso o modificato, il destinatario verrà avvisato.

### Livello aggiuntivo di crittografia lato server

Livello aggiuntivo di crittografia lato server per il recapito al destinatario, per impedire la correlazione tra il traffico del server ricevuto e inviato se il TLS è compromesso.

### Mescolamento dei messaggi per ridurre le correlazioni

I server SimpleX fungono da nodi mix a bassa latenza — i messaggi in entrata e in uscita hanno un ordine diverso.

### Trasporto TLS autenticato sicuro

Usato solo TLS 1.2/1.3 con algoritmi avanzati per connessioni client-server.

Impronta del server e associazione dei canali evitano attacchi MITM e replay.

Ripresa della connessione disattivata per evitare attacchi alla sessione.

### Accesso via Tor opzionale

Per proteggere il tuo indirizzo IP puoi accedere ai server tramite Tor o un'altra rete di trasporto sovrapposta.

Per usare SimpleX tramite Tor, installa [l'app Orbot](https://guardianproject.info/apps/org.torproject.android/) e attiva il proxy SOCKS5 (o VPN [su iOS](https://apps.apple.com/us/app/orbot/id1609461599?platform=iphone)).

### Code di messaggi unidirezionali

Ogni coda di messaggi passa i messaggi in una direzione, con i diversi indirizzi di invio e ricezione.

Riduce i vettori di attacco, rispetto ai broker di messaggi tradizionali, e i metadati disponibili.

### Diversi strati di riempimento dei contenuti

SimpleX usa il riempimento dei contenuti per ogni livello di crittografia per frustrare gli attacchi alle dimensioni dei messaggi.

Fa sì che i messaggi di dimensioni diverse appaiano uguali ai server e a chi osserva la rete.

## Rete di SimpleX

SimpleX Chat offre la migliore privacy combinando i vantaggi del P2P e delle reti federate.

### A differenza delle reti P2P

tutti i messaggi vengono inviati tramite i server, garantendo una migliore privacy dei metadati e una consegna asincrona dei messaggi affidabile, evitando molti .

# Confronto con protocolli di messaggistica P2P

I protocolli e le app di messaggistica [P2P](https://it.wikipedia.org/wiki/Peer-to-peer) hanno diversi problemi che li rendono meno affidabili di SimpleX, più complessi da analizzare e vulnerabili a diversi tipi di attacco.

1. Le reti P2P si basano su alcune varianti di [DHT](https://it.wikipedia.org/wiki/Tabella_di_hash_distribuita) per instradare i messaggi. I progetti DHT devono bilanciare la garanzia di consegna e la latenza. SimpleX ha sia una migliore garanzia di consegna che una latenza minore rispetto al P2P, perché il messaggio può essere passato in modo ridondante attraverso diversi server in parallelo, usando i server scelti dal destinatario. Nelle reti P2P il messaggio viene passato attraverso i nodi *O(log N)* in sequenza, usando nodi scelti dall'algoritmo.
2. Il design di SimpleX, a differenza della maggior parte delle reti P2P, non ha identificatori utente globali di alcun tipo, nemmeno temporanei, e usa solo identificatori temporanei a coppie, garantendo una maggiore protezione dell'anonimato e dei metadati.
3. Il P2P non risolve il problema dell'[attacco MITM](https://it.wikipedia.org/wiki/Attacco_man_in_the_middle) e la maggior parte delle implementazioni esistenti non usa messaggi fuori banda per lo scambio iniziale di chiavi. SimpleX usa messaggi fuori banda o, in alcuni casi, connessioni sicure e attendibili preesistenti per lo scambio iniziale di chiavi.
4. Le implementazioni P2P possono essere bloccate da alcuni fornitori di internet (come [BitTorrent](https://it.wikipedia.org/wiki/BitTorrent)). SimpleX è indipendente dal trasporto — può funzionare su protocolli web standard, es. WebSocket.
5. Tutte le reti P2P conosciute possono essere vulnerabili all'[attacco di Sybil](https://it.wikipedia.org/wiki/Attacco_di_Sybil), perché ogni nodo è rilevabile e la rete opera come un tutt'uno. Le misure note per mitigarlo richiedono un componente centralizzato o [proof-of-work](https://it.wikipedia.org/wiki/Proof-of-work) costosi. La rete di SimpleX non ha la possibilità di scoprire i server, è frammentata e opera come più sottoreti isolate, rendendo impossibili gli attacchi a livello di rete.
6. Le reti P2P possono essere vulnerabili all'[attacco DRDoS](https://www.usenix.org/conference/woot15/workshop-program/presentation/p2p-file-sharing-hell-exploiting-bittorrent), quando i client possono ritrasmettere e amplificare il traffico, con conseguente "denial of service" a livello di rete. I client SimpleX si limitano a inoltrare il traffico da connessioni note e non possono essere usati da un aggressore per amplificare il traffico nell'intera rete.

---

### A differenza delle reti federate

i server di inoltro SimpleX NON conservano i profili utente, i contatti e i messaggi consegnati, NON si connettono tra loro e NON esiste una directory dei server.

---

### Nella rete di SimpleX

i server forniscono code unidirezionali per connettere gli utenti, ma non hanno visibilità del grafo delle connessioni di rete — solo gli utenti.

## SimpleX spiegato

1. Cosa fanno gli utenti

2. Come funziona

3. Cosa vedono i server

1. Cosa fanno gli utenti

Puoi creare contatti e gruppi, ed avere conversazioni bidirezionali, come in qualsiasi altro messenger.

Come può funzionare con code unidirezionali e senza identificatori utente?

2. Come funziona

Per ogni connessione usi due code di messaggi distinte per inviare e ricevere i messaggi attraverso server diversi.

I server passano i messaggi solo in una direzione, senza avere il quadro completo delle conversazioni dell'utente o delle connessioni.

3. Cosa vedono i server

I server hanno credenziali anonime separate per ogni coda e non sanno a quali utenti appartengano.

Gli utenti possono aumentare ulteriormente la privacy dei metadati usando Tor per accedere ai server, evitando correlazioni per indirizzo IP.

## Confronto con altri protocolli

|  |  | Signal, grandi piattaforme | XMPP, Matrix | Protocolli P2P |
| --- | --- | --- | --- | --- |
| Richiede un'identità globale | No - privato | Sì [1] | Sì [2] | Sì [3] |
| Possibilità di MITM | No - sicuro [4] | Sì [5] | Sì | Sì |
| Dipendenza dai DNS | No - resistente | Sì | Sì | No |
| Rete singola o centralizzata | No - decentralizzato | Sì | No - federato [6] | Sì [7] |
| Componente centrale o altro attacco a livello di rete | No - resistente | Sì | Sì [2] | Sì [8] |

---

1. Solitamente si basa su un numero di telefono, in alcuni casi su nomi utente
2. Indirizzi basati su DNS
3. Chiave pubblica o altro ID univoco globale
4. I relay di SimpleX non possono compromettere la crittografia e2e. Verifica il codice di sicurezza per mitigare gli attacchi sul canale fuori banda
5. Se i server dell'operatore sono compromessi. Verifica il codice di sicurezza in Signal e alcune altre app per mitigarlo
6. Non protegge la privacy dei metadati degli utenti
7. Sebbene i P2P siano distribuiti, non sono federati — operano come un'unica rete
8. Le reti P2P hanno un'autorità centrale o l'intera rete può essere compromessa — [vedi qui](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/SIMPLEX.md#comparison-with-p2p-messaging-protocols)

## Confronto della sicurezza di crittografia end-to-end in diverse app di messaggistica

|  | Session | Briar | Element | Cwtch | Signal | SimpleX |
| --- | --- | --- | --- | --- | --- | --- |
| Riempimento dei messaggi | ✗ | ✔︎[1] | ✗ | ✔︎ | ✔︎[1] | ✔︎ |
| Ripudio (negabilità) | ✗ | ✗ | ✗ | ✔︎[2] | ✔︎[3] | ✔︎ |
| Forward secrecy | ✗ | ✔︎ | ✔︎ | ✔︎ | ✔︎ | ✔︎ |
| Sicurezza post-compromissione | ✗ | ✗ | ✗ | ✔︎ | ✗[4] | ✔︎ |
| Scambio di chiavi a 2 fattori | ✔︎ | ✔︎[5] | ✔︎[5] | ✔︎ | ✔︎[5] | ✔︎ |
| Crittografia ibrida quantistica | ✗ | ✗ | ✗ | ✗ | ✔︎[6] | ✔︎ |

---

1. Briar riempie i messaggi ad una dimensione arrotondata fino a 1024 byte, Signal a 160 byte
2. Il ripudio non include la connessione client-server.
3. Sembra che l'uso di firme crittografiche comprometta il ripudio (negabilità), ma occorrono chiarimenti.
4. L'implementazione multi-dispositivo compromette la sicurezza post-compromissione di Double Ratchet — [vedi qui](https://eprint.iacr.org/2021/626.pdf).
5. Lo scambio di chiavi a 2 fattori è facoltativo tramite la verifica del codice di sicurezza.
6. L'accordo sulle chiavi post-quantistico è “scarno” — protegge solo alcuni dei passaggi del ratchet.
