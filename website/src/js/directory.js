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

  entries.forEach(entry => {
    const { displayName, welcomeMessage, shortDescr, imageFile } = entry;
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

    const imgElement = document.createElement('img');
    imgElement.src =
      imageFile
        ? directoryDataURL + imageFile
        : "/img/group.svg"
    imgElement.alt = `${displayName} image`;
    entryDiv.appendChild(imgElement);

    entryDiv.appendChild(textContainer);
    directory.appendChild(entryDiv);
  });
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

function renderMarkdown(fts) {
  let html = '';
  for (const ft of fts) {
    const { format, text } = ft;
    if (!format) {
      html += escapeHtml(text);
      continue;
    }
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
        const color = format.color;
        html += `<span style="color: ${color};">${escapeHtml(text)}</span>`;
        break;
      case 'uri':
        let href = text.startsWith('http://') || text.startsWith('https://') || text.startsWith('simplex:/') ? text : 'https://' + text;
        html += `<a href="${escapeHtml(href)}">${escapeHtml(text)}</a>`;
        break;
      case 'hyperLink': {
        const { showText, linkUri } = format;
        html += `<a href="${escapeHtml(linkUri)}">${escapeHtml(showText ?? linkUri)}</a>`;
        break;
      }
      case 'simplexLink': {
        const { showText, linkType, simplexUri, smpHosts } = format;
        const desc = getSimplexLinkDescr(linkType);
        // TODO replace simplexUri on desktop with the link on server domain or in simplex.chat domain
        html += `<a href="${escapeHtml(simplexUri)}">${escapeHtml(showText ?? desc) + ` (${viaHost(smpHosts)})`}</a>`;
        break;
      }
      case 'command':
        html += `<span style="font-family: monospace;">${escapeHtml(text)}</span>`;
        break;
      case 'mention':
        html += `<strong>${escapeHtml(text)}</strong>`;
        break;
      case 'email':
        html += `<a href="mailto:${escapeHtml(text)}">${escapeHtml(text)}</a>`;
        break;
      case 'phone':
        html += `<a href="tel:${escapeHtml(text)}">${escapeHtml(text)}</a>`;
        break;
      case 'unknown':
        html += escapeHtml(text);
        break;
      default:
        html += escapeHtml(text);
    }
  }
  return html;
}
