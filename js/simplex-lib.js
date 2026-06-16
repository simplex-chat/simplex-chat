










const isMobile = {
  Android: () => navigator.userAgent.match(/Android/i),
  iOS: () => navigator.userAgent.match(/iPhone|iPad|iPod/i),
  any: () => navigator.userAgent.match(/Android|iPhone|iPad|iPod/i)
};

function escapeHtml(text) {
  return text
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#039;")
    .replace(/\n/g, "<br>");
}

function escapeAttr(text) {
  return text
    .replace(/&/g, "&amp;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#039;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;");
}

const SAFE_URI_SCHEME = /^(https?:|simplex:|mailto:|tel:)/i;

function safeHref(uri) {
  if (SAFE_URI_SCHEME.test(uri)) return escapeAttr(uri);
  return escapeAttr(`javascript:void(alert('Potentially malicious link blocked:\\n'+${JSON.stringify(uri)}))`);
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
        case 'small':
          html += `<span class="small-text">${escapeHtml(text)}</span>`;
          break;
        case 'colored':
          html += `<span class="${(format.color || '').replace(/[^a-zA-Z0-9_-]/g, '')}">${escapeHtml(text)}</span>`;
          break;
        case 'uri': {
          let href = text.startsWith('http://') || text.startsWith('https://') || text.startsWith('simplex:/') ? text : 'https://' + text;
          html += `<a href="${safeHref(href)}"${targetBlank(href)}>${escapeHtml(text)}</a>`;
          break;
        }
        case 'hyperLink': {
          const { showText, linkUri } = format;
          html += `<a href="${safeHref(linkUri)}"${targetBlank(linkUri)}>${escapeHtml(showText ?? linkUri)}</a>`;
          break;
        }
        case 'simplexLink': {
          const { showText, linkType, simplexUri, smpHosts } = format;
          const linkText = showText ? escapeHtml(showText) : getSimplexLinkDescr(linkType);
          html += `<a href="${safeHref(platformSimplexUri(simplexUri))}" target="_blank">${linkText} <em>(${escapeHtml(viaHost(smpHosts))})</em></a>`;
          break;
        }
        case 'command':
          html += `<span style="font-family: monospace;">${escapeHtml(text)}</span>`;
          break;
        case 'mention':
          html += `<strong>${escapeHtml(text)}</strong>`;
          break;
        case 'email':
          html += `<a href="mailto:${escapeAttr(text)}">${escapeHtml(text)}</a>`;
          break;
        case 'phone':
          html += `<a href="tel:${escapeAttr(text)}">${escapeHtml(text)}</a>`;
          break;
        case 'unknown':
          html += escapeHtml(text);
          break;
        default:
          html += escapeHtml(text);
      }
    } catch(e) {
      console.log(e);
      html += escapeHtml(text);
    }
  }
  return html;
}

const simplexAddressRegexp = /^simplex:\/([a-z]+)#(.+)/i;

const simplexShortLinkTypes = ["a", "c", "g", "i", "r"];

function platformSimplexUri(uri) {
  if (isMobile.any()) return uri;
  const res = uri.match(simplexAddressRegexp);
  if (!res || !Array.isArray(res) || res.length < 3) return uri;
  const linkType = res[1];
  const fragment = res[2];
  if (simplexShortLinkTypes.includes(linkType)) {
    const queryIndex = fragment.indexOf('?');
    if (queryIndex === -1) return uri;
    const hashPart = fragment.substring(0, queryIndex);
    const queryStr = fragment.substring(queryIndex + 1);
    const params = new URLSearchParams(queryStr);
    const host = params.get('h');
    if (!host) return uri;
    params.delete('h');
    let newFragment = hashPart;
    const remainingParams = params.toString();
    if (remainingParams) newFragment += '?' + remainingParams;
    return `https://${host}:/${linkType}#${newFragment}`;
  } else {
    return `https://simplex.chat/${linkType}#${fragment}`;
  }
}
