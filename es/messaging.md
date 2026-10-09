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

## Por qué SimpleX es único

### La privacidad es completa

SimpleX protege la privacidad de tu perfil, contactos y metadatos, ocultándolos de los servidores SimpleX y de cualquier observador.

A diferencia de cualquier otra red de mensajería existente, SimpleX no tiene identificadores asignados a los usuarios, **ni siquiera números aleatorios**.

### Estás protegido contra el spam y el abuso

Al no tener identificadores o dirección permanente en la red SimpleX, nadie puede ponerse en contacto contigo salvo que compartas una dirección de usuario de un solo uso o una dirección temporal en forma de enlace o código QR.

### Tu gestionas tus datos

SimpleX almacena todos los datos de usuario únicamente en los dispositivos cliente usando un **formato cifrado y portable de la base de datos**, la cual puede ser transferida a otro dispositivo.

Los mensajes cifrados de extremo a extremo (E2E) se mantienen temporalmente en los servidores SimpleX hasta que se entregan al destinatario, y después se borran definitivamente.

### La red SimpleX te pertenece

La red SimpleX está completamente descentralizada y es independiente de cualquier criptodivisa o de cualquier red salvo Internet.

Puedes **usar SimpleX con tus propios servidores** o con los servidores proporcionados por nosotros para contactar con cualquier usuario.

# Privacidad total de tu identidad, perfil, contactos y metadatos

A diferencia de otras redes de mensajería, SimpleX **no tiene identificadores asignados a los usuarios**. No depende de números de teléfono, direcciones basadas en dominios (como es el caso del email o XMPP), nombres de usuario, claves públicas o incluso números aleatorios para identificar a sus usuarios. Los operadores de servidores SimpleX desconocen cuántas personas usan sus servidores.

Para la entrega de mensajes, SimpleX usa [direcciones anónimas por pares](https://csrc.nist.gov/glossary/term/Pairwise_Pseudonymous_Identifier) de colas de mensajes unidireccionales separadas para mensajes salientes y entrantes a través de servidores diferentes.

Este diseño protege la privacidad de la persona que se comunica contigo ocultándole a los servidores de la red SimpleX y de cualquier observador. Para ocultar tu dirección IP a los servidores puedes **conectarte a los servidores SimpleX mediante Tor**.

# La mejor protección contra el spam y el abuso

Al no tener identificadores en la red SimpleX, nadie puede ponerse en contacto contigo salvo que compartas una dirección de usuario de un solo uso o una dirección temporal en forma de enlace o código QR.

Incluso con la dirección de usuario opcional, aunque pueda usarse para enviar solicitudes de contacto spam, puedes cambiarla o eliminarla por completo sin perder ninguna de tus conexiones.

# Titularidad, control y seguridad de tus datos

SimpleX Chat almacena todos los datos de usuario únicamente en los dispositivos cliente usando un **formato cifrado y portable de la base de datos**, la cual puede ser exportada y transferida a cualquier dispositivo compatible.

Los mensajes cifrados de extremo a extremo (E2E) se mantienen temporalmente en los servidores SimpleX hasta que se entregan al destinatario, y después se borran definitivamente.

A diferencia de los servidores de redes federadas (correo electrónico, XMPP o Matrix), los servidores SimpleX no almacenan cuentas de usuario, sólo retransmiten mensajes, protegiendo así la privacidad de ambas partes.

No hay identificadores ni texto cifrado en común entre el tráfico de servidor enviado y el recibido. Si alguien te está monitorizando, difícilmente podría determinar quién se comunica con quién, incluso si el protocolo TLS se ve comprometido.

# Totalmente descentralizado, los usuarios son dueños de la red SimpleX

Puedes **usar SimpleX con tus propios servidores** y aún así comunicarte con personas que usen los servidores preconfigurados de la aplicación.

La red SimpleX usa un [protocolo abierto](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md) y proporciona el [SDK para crear chatbots](https://github.com/simplex-chat/simplex-chat/tree/stable/packages/simplex-chat-client/typescript), permitiendo implementar servicios con los que los usuarios interactúen con las aplicaciones SimpleX Chat — esperamos ver con interés los servicios SimpleX que creará usted.

Si estás considerando desarrollar para la red SimpleX, por ejemplo un chatbot para los usuarios de SimpleX o la integración de las librerías SimpleX Chat en tus aplicaciones móviles, por favor [ponte en contacto](https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D) para consejos y soporte.

## Características

Mensajes cifrados E2E
con sintáxis markdown y edición

Cifrado E2E
de imágenes, vídeos y archivos

Grupos descentralizados cifrados E2E, sólo los usuarios saben de su existencia

Mensajes de voz cifrados E2E

Mensajes temporales

Llamadas y videollamadas
con cifrado E2E

Almacenamiento portable y cifrado, podrás transferir tu perfil a otro dispositivo

Modo Incógnito
exclusivo de SimpleX Chat

## Qué hace que SimpleX sea privado

### Identificadores por pares temporales y anónimos

SimpleX usa direcciones y credenciales temporales y anónimas por pares para cada contacto o miembro del grupo.

Permite la entrega de mensajes sin identificadores del perfil de usuario y proporciona mayor privacidad de metadatos que las alternativas.

### Intercambio de claves fuera de banda

Muchas redes de comunicaciónes son vulnerables a ataques MITM por parte de los servidores o los proveedores de red.

Para evitarlo las aplicaciones SimpleX pasan claves de un solo uso fuera de banda cuando compartes un enlace de dirección o un código QR.

### Doble capa de cifrado de extremo a extremo

Protocolo de '[Double Ratchet](https://en.wikipedia.org/wiki/Double_Ratchet_Algorithm)' —
Mensajería [OTR](https://es.wikipedia.org/wiki/Off_the_record_messaging) con [secreto perfecto hacia adelante](https://es.wikipedia.org/wiki/Perfect_forward_secrecy) y recuperación de intrusión.

Y si el protocolo TLS se ve comprometido, NaCL cryptobox en cada cola de mensajes para prevenir la correlación de tráfico entre las colas.

### Verificación de la integridad del mensaje

Para garantizar la integridad los mensajes se numeran secuencialmente e incluyen el hash del mensaje anterior.

Si se añade, elimina o modifica algún mensaje, el destinatario es avisado.

### Capa de cifrado adicional en el servidor

Capa de cifrado adicional desde el servidor al destinatario, para prevenir la correlación entre el tráficos de entrada y salida del servidor si el protocolo TLS se ve comprometido.

### Mezcla de mensajes para prevenir la correlación

Los servidores SimpleX actúan como nodos de mezcla de baja latencia, los mensajes entrantes y salientes siguen un orden diferente.

### Transporte TLS seguro y autenticado

Para las conexiones cliente servidor se usan exclusivamente el protocolo TLS 1.2/1.3 con algoritmos robustos.

La huella digital del servidor y la vinculación de canales evitan los ataques de respuesta y MITM.

La reanudación de la conexión está deshabilitada para evitar ataques de sesión.

### Acceso opcional a través de Tor

Para proteger tu dirección IP, puedes acceder a los servidores a través Tor u otra red superpuesta de transporte.

Para usar SimpleX a través de Tor, instala la aplicación [Orbot](https://guardianproject.info/apps/org.torproject.android/) y activa el proxy SOCKS5 (o VPN [en iOS](https://apps.apple.com/us/app/orbot/id1609461599?platform=iphone)).

### Colas de mensajes unidireccionales

Cada cola de mensajes transmite los mensajes en un sol sentido. entre las direcciónes de envío y recepción.

Esto reduce los vectores de ataque y los metadatos disponibles en comparación con los brokers de mensajes tradicionales.

### Múltiples capas de contenido acolchado

SimpleX utiliza relleno de contenido en cada capa de cifrado para frustrar los ataques al tamaño de los mensajes.

Esto hace que mensajes de distintos tamaños parezcan iguales desde el punto de vista de servidores y observadores de red.

## Red SimpleX

SimpleX Chat proporciona la mejor privacidad al combinar las ventajas de P2P y las redes federadas.

### Distinta de las redes P2P

Todos los mensajes se envían a través de los servidores, proporcionando mejor privacidad para los metadatos y entrega asíncrona fiable, evitando al mismo tiempo muchos de los .

# Comparativa con protocolos de mensajería P2P

Los protocolos y aplicaciones de mensajería [P2P](https://en.wikipedia.org/wiki/Peer-to-peer) presentan varios problemas que los hacen menos fiables que SimpleX, más complejos de analizar y más vulnerables a ciertos tipos de ataques.

1. Para enrutar mensajes, las redes P2P se basan en alguna variante de [DHT](https://es.wikipedia.org/wiki/Tabla_de_hash_distribuida). Los diseños DHT tienen que equilibrar la garantía de entrega y latencia. SimpleX ofrece mayor garantía de entrega y menor latencia que P2P ya que el mensaje puede transmitirse en paralelo y de forma redundante a través de varios servidores elegidos por el destinatario. En las redes P2P el mensaje se transmite secuencialmente por *O(log N)* nodos, usando nodos elegidos por el algoritmo.
2. Por diseño SimpleX, a diferencia de la mayoría de las redes P2P, no tiene identificadores globales de usuario de ningún tipo, ni siquiera temporales, y sólo usa identificadores temporales por pares, lo que proporciona un mejor anonimato y protección de los metadatos.
3. P2P no resuelve el problema del [ataque MITM](https://es.wikipedia.org/wiki/Ataque_de_intermediario) , y la mayoría de implementaciones existentes no usan mensajes fuera de banda para el intercambio de claves inicial. SimpleX usa mensajes fuera de banda, o en algunos casos conexiones seguras y de confianza preexistentes para el intercambio inicial de claves.
4. Algunos proveedores de Internet pueden bloquear las aplicaciones P2P (como [BitTorrent](https://en.wikipedia.org/wiki/BitTorrent)). SimpleX es independiente del transporte. Puede trabajar con protocolos web estándar, por ejemplo WebSockets.
5. Todas las redes P2P conocidas pueden ser vulnerables al ataque [Sybil](https://es.wikipedia.org/wiki/Ataque_Sybil) porque cada nodo es susceptible de ser descubierto y la red funciona como una unidad. Las medidas de mitigación conocidas requieren un componente centralizado o bien costosas [pruebas de trabajo](https://en.wikipedia.org/wiki/Proof_of_work). La red SimpleX no tiene la capacidad para descubrir los servidores, está fragmentada y funciona como múltiples subredes aisladas haciendo imposibles los ataques a toda la red.
6. Las redes P2P pueden ser vulnerables al [ataque DRDoS](https://www.usenix.org/conference/woot15/workshop-program/presentation/p2p-file-sharing-hell-exploiting-bittorrent), en el que los clientes retransmiten y amplifican el tráfico provocando el bloqueo del servicio en la red. Los clientes SimpleX sólo transmiten el tráfico desde conexiones conocidas y no pueden ser usados por un atacante para amplificar el tráfico en toda la red.

---

### Distinta de las redes federadas

Los servidores de SimpleX NO almacenan perfiles de usuario, contactos, o mensajes entregados. NO contactan entre sí y NO existe un directorio de servidores.

---

### Red Simplex

los servidores proporcionan colas unidireccionales para conectar a los usuarios pero no ven la gráfica de conexiónes red, sólo los usuarios pueden verla.

## SimpleX explicado

1. Experiencia del usuario

2. Cómo funciona la red

3. Qué ven los servidores

1. Experiencia del usuario

Puedes crear contactos y grupos, y mantener conversaciones bidireccionales igual que en cualquier aplicación de mensajería.

¿Cómo puede funcionar con colas unidireccionales y sin identificadores de usuario?

2. Cómo funciona la red

Por cada conexión se usan dos colas de mensajes separadas, para que el envío y la recepción se realicen a través de servidores diferentes.

Los servidores sólo transmiten en un sentido para no disponer de la conversación completa o las conexiones del usuario.

3. Qué ven los servidores

Para cada cola los servidores disponen de credenciales separadas y anónimas, por lo que desconocen a qué usuarios pertenecen.

El usuario puede mejorar aún más la privacidad de sus metadatos, haciendo uso de la red Tor para acceder a los servidores, evitando así la correlación por dirección IP.

## Comparación con otros protocolos

|  |  | Signal, grandes plataformas | XMPP, Matrix | Protocolos P2P |
| --- | --- | --- | --- | --- |
| Requiere de identidad global | No - privado | Sí [1] | Sí [2] | Sí [3] |
| Posibilidad de MITM | No - seguro [4] | Sí [5] | Sí | Sí |
| Dependencia del DNS | No - resiliente | Sí | Sí | No |
| Red única o centralizada | No - descentralizado | Sí | No - federado [6] | Sí [7] |
| Componente central u otro ataque en toda la red | No - resiliente | Sí | Sí [2] | Sí [8] |

---

1. Generalmente basada en un número de teléfono, y en algunos casos en nombres de usuario
2. Direcciones basadas en DNS
3. Clave pública o algun otro ID único a nivel global
4. Los servidores no pueden comprometer el cifrado E2E. Para evitar posibles ataques, verifica el código de seguridad mediante un canal alternativo
5. Si los servidores del operador se ven comprometidos. Verifica el código de seguridad en Signal y otras aplicaciónes para mitigarlo
6. No protege la privacidad de los metadatos del usuario
7. A pesar de que las redes P2P son distribuidas, no son federadas. Funcionan como una única red
8. Las redes P2P, o bien disponen de una autoridad central, o bien la red completa podría verse comprometida — [ver aquí](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/SIMPLEX.md#comparison-with-p2p-messaging-protocols)

## Comparativa del cifrado de extremo a extremo en los distintos mensajeros

|  | Session | Briar | Element | Cwtch | Signal | SimpleX |
| --- | --- | --- | --- | --- | --- | --- |
| Relleno de mensajes | ✗ | ✔︎[1] | ✗ | ✔︎ | ✔︎[1] | ✔︎ |
| Repudio (negabilidad) | ✗ | ✗ | ✗ | ✔︎[2] | ✔︎[3] | ✔︎ |
| Secreto hacia adelante | ✗ | ✔︎ | ✔︎ | ✔︎ | ✔︎ | ✔︎ |
| Seguridad tras ser comprometido | ✗ | ✗ | ✗ | ✔︎ | ✗[4] | ✔︎ |
| Intercambio de claves de dobre factor | ✔︎ | ✔︎[5] | ✔︎[5] | ✔︎ | ✔︎[5] | ✔︎ |
| Cifrado híbrido postcuántico | ✗ | ✗ | ✗ | ✗ | ✔︎[6] | ✔︎ |

---

1. Briar rellena los mensajes hasta el tamaño redondeado a 1024 bytes, Signal a 160 bytes
2. El repudio no incluye la conexión cliente‑servidor.
3. Parece que el uso de firmas criptográficas compromete el repudio (negabilidad) pero es necesario aclararlo.
4. La implementación multi‑dispositivo compromete la seguridad posterior a ser comprometida del Double Ratchet — [ver aquí](https://eprint.iacr.org/2021/626.pdf).
5. El intercambio de clave de doble factor es opcional mediante la verificación del código de seguridad.
6. El acuerdo de claves postcuántico es "parcial", solo protege determinados pasos del ratchet.
