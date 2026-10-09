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

## Por que o SimpleX é único

### Você tem privacidade total

O SimpleX protege a privacidade do seu perfil, contatos e metadados, ocultando-os dos servidores da rede SimpleX e de quaisquer observadores.

Diferente de qualquer outra rede de mensagens existente, o SimpleX não tem identificadores atribuídos aos usuários — **nem mesmo números aleatórios**.

### Você está protegido contra spam e abusos

Como você não tem um identificador ou endereço fixo na rede SimpleX, ninguém pode entrar em contato com você, a menos que você compartilhe um endereço de usuário único ou temporário, como um QR code ou um link.

### Você controla seus dados

O SimpleX armazena todos os dados do usuário nos dispositivos clientes em um **formato de banco de dados criptografado portátil** — que pode ser transferido para outro dispositivo.

As mensagens criptografadas de ponta-a-ponta são mantidas temporariamente nos servidores de retransmissão SimpleX até serem recebidas e, em seguida, são excluídas permanentemente.

### Sua própria rede SimpleX

A rede SimpleX é totalmente descentralizada e independente de qualquer criptomoeda ou de qualquer outra rede, exceto a Internet.

Você pode **usar o SimpleX com seus próprios servidores** ou com os servidores fornecidos por nós — e ainda assim se conectar a qualquer usuário.

# Privacidade total de sua identidade, perfil, contatos e metadados

Ao contrário de outras redes de mensagens, o SimpleX **não tem identificadores atribuídos aos usuários**. Ele não depende de números de telefone, endereços baseados em domínio (como email ou XMPP), nomes de usuário, chaves públicas ou mesmo números aleatórios para identificar seus usuários — Os operadores dos servidores não sabem quantas pessoas usam os servidores SimpleX.

Para entregar mensagens, o SimpleX usa [endereços anônimos em pares](https://csrc.nist.gov/glossary/term/Pairwise_Pseudonymous_Identifier) de filas de mensagens unidirecionais, separadas para mensagens recebidas e enviadas, geralmente por meio de servidores diferentes.

Esse design protege a privacidade com quem você está se comunicando, ocultando-a dos servidores da rede SimpleX e de quaisquer observadores. Para ocultar seu endereço IP dos servidores, você pode **se conectar aos servidores do SimpleX via Tor**.

# Você está protegido contra spam e abusos

Como você não tem um identificador na rede SimpleX, ninguém pode entrar em contato com você, a menos que compartilhe um endereço de usuário único ou temporário, como um QR code ou um link.

Mesmo com o endereço de usuário opcional, embora ele possa ser usado para enviar solicitações de contato de spam, você pode alterá-lo ou excluí-lo completamente sem perder nenhuma das suas conexões.

# Propriedade, controle e segurança dos seus dados

O SimpleX Chat armazena todos os dados do usuário somente em dispositivos clientes usando um **formato de banco de dados criptografado portátil** que pode ser exportado e transferido para qualquer dispositivo compatível.

As mensagens criptografadas de ponta-a-ponta são mantidas temporariamente nos servidores de retransmissão SimpleX até serem recebidas e, em seguida, são excluídas permanentemente.

Diferente dos servidores de redes federadas (email, XMPP ou Matrix), os servidores SimpleX não armazenam contas de usuários, apenas retransmitem mensagens, protegendo a privacidade de ambas as partes.

Não há identificadores ou texto cifrado em comum entre o tráfego de servidor enviado e recebido — se alguém estiver observando, não poderá determinar facilmente quem se comunica com quem, mesmo que o TLS esteja comprometido.

# Totalmente descentralizado — os usuários são proprietários da rede SimpleX

Você pode **usar o SimpleX com seus próprios servidores** e ainda se comunicar com pessoas que usam os servidores pré-configurados fornecidos por nós.

A rede SimpleX usa um [protocolo aberto](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md) e fornece um [SDK para criar bots de chat](https://github.com/simplex-chat/simplex-chat/tree/stable/packages/simplex-chat-client/typescript), permitindo a implementação de serviços com os quais os usuários podem interagir por meio dos aplicativos SimpleX Chat — estamos' realmente ansiosos para ver quais serviços SimpleX você pode criar.

Se estiver pensando em desenvolver para a rede SimpleX, por exemplo, o bot de chat para os usuários do aplicativo SimpleX ou a integração da biblioteca SimpleX Chat em seus aplicativos móveis, [entre em contato](https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D) para qualquer orientação e suporte.

## Recursos

Mensagens criptografadas de ponta-a-ponta com markdown e edição

Criptografia de ponta-a-ponta
imagens, vídeos e arquivos

Criptografia de ponta-a-ponta descentralizada de grupos — somente os usuários sabem que eles existem

Mensagens de voz criptografadas de ponta-a-ponta

Mensagens que desaparecem

Chamadas de áudio e vídeo
criptografadas de ponta-a-ponta

Armazenamento do aplicativo criptografado portátil — mova o perfil para outro dispositivo

Modo anônimo —
único do SimpleX Chat

## O que torna o SimpleX privado

### Identificadores temporários anônimos em pares

O SimpleX usa endereços anônimos temporários em pares e credenciais para cada contato de usuário ou membro de grupo.

Ele permite entregar mensagens sem identificadores de perfil, proporcionando melhor privacidade de metadados do que as alternativas.

### Troca de chaves fora da rede

Muitas plataformas de comunicação são vulneráveis a ataques MITM por servidores ou provedores de rede.

Para evitar isso, os aplicativos SimpleX passam chaves de uso único fora da banda, quando você compartilha um endereço como um link ou um QR code.

### 2 camadas de criptografia ponta-a-ponta

Protocolo de dupla catraca —
mensagens OTR com Sigilo de Encaminhamento Perfeito (Perfect Forward Secrecy) e recuperação de invasão.

Caixa de criptografia NaCL em cada envio para evitar a correlação de tráfego entre os envios de mensagens se o TLS for comprometido.

### Verificação de integridade da mensagem

Para garantir a integridade, as mensagens são numeradas sequencialmente e incluem o hash da mensagem anterior.

Se alguma mensagem for adicionada, removida ou alterada, o destinatário será alertado.

### Camada adicional de criptografia do servidor

Camada adicional de criptografia de servidor para entrega ao destinatário, para evitar a correlação entre o tráfego de servidor recebido e enviado se o TLS for comprometido.

### Mistura de mensagens para reduzir a correlação

Os servidores SimpleX atuam como nós de mistura de baixa latência — as mensagens de entrada e saída têm ordem diferente.

### Transporte TLS seguro e autenticado

Somente o TLS 1.2/1.3 com algoritmos fortes é usado para conexões cliente-servidor.

A impressão digital do servidor e a vinculação de canais evitam ataques MITM e de repetição.

A retomada da conexão é desativada para evitar ataques à sessão.

### Acesso opcional via Tor

Para proteger seu endereço IP, você pode acessar os servidores por meio do Tor ou de alguma outra rede de sobreposição de transporte.

Para usar o SimpleX via Tor, instale o aplicativo [Orbot](https://guardianproject.info/apps/org.torproject.android/) e ative o proxy SOCKS5 (ou VPN [no iOS](https://apps.apple.com/us/app/orbot/id1609461599?platform=iphone)).

### Envios de mensagens unidirecionais

Cada fila de mensagens transmite mensagens em uma direção, com diferentes endereços de envio e recebimento.

Isso reduz os vetores de ataque, em comparação com os corretores de mensagens tradicionais, e os metadados disponíveis.

### Múltiplas camadas de preenchimento de conteúdos

O SimpleX utiliza preenchimento de conteúdo (padding) em cada camada de criptografia para dificultar ataques baseados no tamanho da mensagem.

Isso faz com que mensagens de tamanhos diferentes tenham a mesma aparência para os servidores e observadores de rede.

## Rede SimpleX

O SimpleX Chat oferece a melhor privacidade ao combinar as vantagens das redes P2P e federadas.

### Diferente das redes P2P

Todas as mensagens são enviadas por meio dos servidores, o que proporciona melhor privacidade de metadados e entrega de mensagens assíncronas confiáveis, além de evitar muitos dos .

# Comparação com protocolos de mensagens P2P

Os protocolos e aplicativos de mensagens [P2P](https://pt.wikipedia.org/wiki/Peer-to-peer) têm vários problemas que os tornam menos confiáveis do que o SimpleX, mais complexos de analisar e vulneráveis a vários tipos de ataque.

1. As redes P2P dependem de alguma variante de [DHT](https://pt.wikipedia.org/wiki/Distributed_hash_table) para rotear mensagens. Os projetos de DHT precisam equilibrar a garantia de entrega e a latência. O SimpleX tem melhor garantia de entrega e menor latência do que o P2P, porque a mensagem pode ser passada de forma redundante por vários servidores em paralelo, usando os servidores escolhidos pelo destinatário. Nas redes P2P, a mensagem é passada por nós *O(log N)* sequencialmente, usando nós escolhidos pelo algoritmo.
2. O design do SimpleX, ao contrário da maioria das redes P2P, não tem identificadores de usuário globais de qualquer tipo, mesmo temporários, e usa apenas identificadores temporários em pares, proporcionando melhor anonimato e proteção de metadados.
3. O P2P não resolve o problema do [ataque MITM](https://pt.wikipedia.org/wiki/Ataque_man-in-the-middle), e a maioria das implementações existentes não usa mensagens fora de banda para a troca de chaves inicial. O SimpleX usa mensagens fora de banda ou, em alguns casos, conexões pré-existentes seguras e confiáveis para a troca de chaves inicial.
4. As implementações de P2P podem ser bloqueadas por alguns provedores de Internet (como o [BitTorrent](https://pt.wikipedia.org/wiki/BitTorrent)). O SimpleX é independente de transporte, podendo funcionar com protocolos padrão da Web, por exemplo, WebSockets.
5. Todas as redes P2P conhecidas podem ser vulneráveis ao [ataque Sybil](https://pt.wikipedia.org/wiki/Ataque_Sybil), pois cada nó pode ser descoberto e a rede opera como um todo. As medidas conhecidas para combatê-lo exigem um componente centralizado ou uma [prova de trabalho](https://pt.wikipedia.org/wiki/Prova_de_trabalho) cara. A rede SimpleX não tem capacidade de descoberta de servidor, é fragmentada e opera como várias sub-redes isoladas, impossibilitando ataques em toda a rede.
6. As redes P2P podem ser vulneráveis a [ataques DRDoS](https://www.usenix.org/conference/woot15/workshop-program/presentation/p2p-file-sharing-hell-exploiting-bittorrent), quando os clientes podem retransmitir e amplificar o tráfego, resultando em uma negação de serviço em toda a rede. Os clientes SimpleX apenas retransmitem o tráfego de uma conexão conhecida e não podem ser usados por um invasor para amplificar o tráfego em toda a rede.

---

### Diferente das redes federadas

Os servidores de retransmissão SimpleX NÃO armazenam perfis de usuário, contatos e mensagens entregues, NÃO se conectam uns aos outros e NÃO há diretório de servidores.

---

### Rede SimpleX

Os servidores fornecem filas unidirecionais para conectar os usuários, mas não têm visibilidade do gráfico de conexão de rede — somente os usuários têm.

## Explicação do SimpleX

1. O que os usuários experimentam

2. Como funciona

3. O que os servidores veem

1. O que os usuários experimentam

Você pode criar contatos e grupos e ter conversas bidirecionais, como em qualquer outro mensageiro.

Como ele pode funcionar com filas unidirecionais e sem identificadores de perfil de usuário?

2. Como funciona

Para cada conexão, são usadas duas filas de mensagens separadas para enviar e receber mensagens por meio de servidores diferentes.

Os servidores só passam mensagens em uma direção, sem ter a imagem completa da conversa ou conexões do usuário.

3. O que os servidores veem

Os servidores têm credenciais anônimas separadas para cada envio e não sabem a que usuários elas pertencem.

Os usuários podem melhorar ainda mais a privacidade de metadados usando o Tor para acessar os servidores, impedindo a correlação pelo endereço de IP.

## Comparação com outros protocolos

|  |  | Signal, grandes plataformas | Matrix, XMPP | Protocolos P2P |
| --- | --- | --- | --- | --- |
| Requer uma identidade global | Não - privado | Sim [1] | Sim [2] | Sim [3] |
| Possibilidade de MITM | Não - seguro [4] | Sim [5] | Sim | Sim |
| Dependência do DNS | Não - resiliente | Sim | Sim | Não |
| Rede única ou centralizada | Não - descentralizado | Sim | Não - federado [6] | Sim [7] |
| Componente central ou outro ataque em toda a rede | Não - resiliente | Sim | Sim [2] | Sim [8] |

---

1. Geralmente com base em um número de telefone, em alguns casos em nomes de usuário
2. Endereços baseados no DNS
3. Chave pública ou alguma outra ID globalmente exclusiva
4. Os relays SimpleX não podem comprometer a criptografia e2e. Verifique o código de segurança para mitigar ataques em canais fora de banda
5. Se os servidores da operadora forem comprometidos. Verifique o código de segurança no Signal e outros aplicativos para mitigá-lo
6. Não protege a privacidade da metadados dos usuários
7. Embora os P2P sejam distribuídos, eles não são federados &mdash - operam como uma única rede
8. As redes P2P têm uma autoridade central ou toda a rede pode ser comprometida — [veja aqui](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/SIMPLEX.md#comparison-with-p2p-messaging-protocols)

## Comparação da segurança da criptografia de ponta a ponta em diferentes mensageiros

|  | Session | Briar | Element | Cwtch | Signal | SimpleX |
| --- | --- | --- | --- | --- | --- | --- |
| Preenchimento de mensagem | ✗ | ✔︎[1] | ✗ | ✔︎ | ✔︎[1] | ✔︎ |
| Repúdio (negação plausível) | ✗ | ✗ | ✗ | ✔︎[2] | ✔︎[3] | ✔︎ |
| Sigilo de Encaminhamento | ✗ | ✔︎ | ✔︎ | ✔︎ | ✔︎ | ✔︎ |
| Segurança pós-comprometimento | ✗ | ✗ | ✗ | ✔︎ | ✗[4] | ✔︎ |
| Troca de chaves de dois fatores | ✔︎ | ✔︎[5] | ✔︎[5] | ✔︎ | ✔︎[5] | ✔︎ |
| Criptografia híbrida pós-quântica | ✗ | ✗ | ✗ | ✗ | ✔︎[6] | ✔︎ |

---

1. O Briar preenche as mensagens até que o tamanho seja arredondado para 1024 bytes, enquanto o Signal as preenche para 160 bytes
2. O repúdio não inclui a conexão cliente-servidor.
3. Parece que o uso de assinaturas criptográficas compromete o repúdio (denegabilidade), mas isso precisa ser esclarecido.
4. A implementação de múltiplos dispositivos compromete a segurança pós-comprometimento do Double Ratchet — [veja aqui](https://eprint.iacr.org/2021/626.pdf).
5. A troca de chaves de dois fatores é opcional por meio da verificação do código de segurança.
6. O acordo de chaves pós-quântico é 'esparso' — ele protege apenas algumas das etapas da catraca (ratchet).
