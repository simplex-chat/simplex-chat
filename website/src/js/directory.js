const directoryDataURL = 'http://localhost:8080/directory-data/';

async function renderDirectory() {
  const listing = await fetchJSON(directoryDataURL + 'listing.json')
  displayEntries(listing.entries)
}

async function fetchJSON(url) {
  try {
    const response = await fetch(url)
    if (!response.ok) throw new Error(`HTTP error! Status: ${response.status}`)
    return await response.json()
  } catch (error) {
    console.error('Error fetching JSON:', error)
  }
}

function displayEntries(entries) {
  const directory = document.getElementById('directory');
  directory.innerHTML = '';

  for (let entry of entries) {
    const { entryType, displayName, groupLink, shortDescr, welcomeMessage, imageFile } = entry;
    const entryDiv = document.createElement('div');
    entryDiv.className = 'entry';

    const textContainer = document.createElement('div');
    textContainer.className = 'text-container';

    const nameElement = document.createElement('h2');
    nameElement.textContent = displayName;
    textContainer.appendChild(nameElement);

    if (shortDescr) {
      const descrElement = document.createElement('p');
      descrElement.innerHTML = renderMarkdown(shortDescr);
      textContainer.appendChild(descrElement);
    }

    const messageElement = document.createElement('p');
    messageElement.innerHTML = renderMarkdown(welcomeMessage);
    textContainer.appendChild(messageElement);

    if (entryType.type == 'group') {
      const memberCount = entryType.summary?.currentMembers
      if (typeof memberCount == 'number') {
        const memberCountElement = document.createElement('p');
        memberCountElement.innerText = `${memberCount} members`;
        memberCountElement.classList = ['text-sm'];
        textContainer.appendChild(memberCountElement);
      }
    }

    const imgElement = document.createElement('a');
    imgSource =
      imageFile
        ? directoryDataURL + imageFile
        : "/img/group.svg";
    imgElement.innerHTML = `<img src="${imgSource}" alt="${displayName}">`;
    imgElement.href = groupLink.connShortLink ?? groupLink.connFullLink;
    // TODO use simplex.chat site when appropriate
    if (!isCurrentSite(imgElement.href)) imgElement.target = "_blank";
    imgElement.title = `Join ${displayName}`;
    entryDiv.appendChild(imgElement);

    entryDiv.appendChild(textContainer);
    directory.appendChild(entryDiv);
  }

  for (let el of document.querySelectorAll('.secret')) {
    el.addEventListener('click', () => el.classList.toggle('visible'));
  }
}

renderDirectory()

function escapeHtml(text) {
  return text
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#039;");
}

function getSimplexLinkDescr(linkType) {
  switch (linkType) {
    case 'contact': return 'SimpleX contact address';
    case 'invitation': return 'SimpleX one-time invitation';
    case 'group': return 'SimpleX group link';
    case 'channel': return 'SimpleX channel link';
    case 'relay': return 'SimpleX relay link';
    default: return 'SimpleX link';
  }
}

function viaHost(smpHosts) {
  const first = smpHosts[0] ?? '?';
  return `via ${first}`;
}

function isCurrentSite(uri) {
  return uri.startsWith("https://simplex.chat") || uri.startsWith("https://www.simplex.chat")
}

function targetBlank(uri) {
  return isCurrentSite(uri) ? '' : ' target="_blank"'
}

function renderMarkdown(fts) {
  let html = '';
  for (const ft of fts) {
    const { format, text } = ft;
    if (!format) {
      html += escapeHtml(text);
      continue;
    }
    try {
      switch (format.type) {
        case 'bold':
          html += `<strong>${escapeHtml(text)}</strong>`;
          break;
        case 'italic':
          html += `<em>${escapeHtml(text)}</em>`;
          break;
        case 'strikeThrough':
          html += `<s>${escapeHtml(text)}</s>`;
          break;
        case 'snippet':
          html += `<span style="font-family: monospace;">${escapeHtml(text)}</span>`;
          break;
        case 'secret':
          html += `<span class="secret">${escapeHtml(text)}</span>`;
          break;
        case 'colored':
          html += `<span style="color: ${format.color};">${escapeHtml(text)}</span>`;
          break;
        case 'uri':
          let href = text.startsWith('http://') || text.startsWith('https://') || text.startsWith('simplex:/') ? text : 'https://' + text;
          html += `<a href="${href}"${targetBlank(href)}>${escapeHtml(text)}</a>`;
          break;
        case 'hyperLink': {
          const { showText, linkUri } = format;
          html += `<a href="${linkUri}"${targetBlank(linkUri)}>${escapeHtml(showText ?? linkUri)}</a>`;
          break;
        }
        case 'simplexLink': {
          const { showText, linkType, simplexUri, smpHosts } = format;
          const linkText = showText ? escapeHtml(showText) : getSimplexLinkDescr(linkType);
          // TODO replace simplexUri on desktop with the link on server domain or in simplex.chat domain
          html += `<a href="${simplexUri}">${linkText} <em>(${viaHost(smpHosts)})</em></a>`;
          break;
        }
        case 'command':
          html += `<span style="font-family: monospace;">${escapeHtml(text)}</span>`;
          break;
        case 'mention':
          html += `<strong>${escapeHtml(text)}</strong>`;
          break;
        case 'email':
          html += `<a href="mailto:${text}">${escapeHtml(text)}</a>`;
          break;
        case 'phone':
          html += `<a href="tel:${text}">${escapeHtml(text)}</a>`;
          break;
        case 'unknown':
          html += escapeHtml(text);
          break;
        default:
          html += escapeHtml(text);
      }
    } catch {
      html += escapeHtml(text);
    }
  }
  return html;
}
