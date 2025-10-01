(function() {
if (!document.location.pathname.startsWith('/directory')) return;

let allEntries = [];

let filteredEntries = [];

let currentSortMode = '';

let currentSearch = '';

let currentPage = 1;

async function initDirectory() {
  const listing = await fetchJSON(simplexDirectoryDataURL + 'listing.json')
  const liveBtn = document.querySelector('#top-pagination .live');
  const newBtn = document.querySelector('#top-pagination .new');
  const topBtn = document.querySelector('#top-pagination .top');
  const searchInput = document.getElementById('search');
  allEntries = listing.entries
  renderEntries('top', bySortPriority, topBtn)
  searchInput.addEventListener('input', (e) => renderEntries('top', bySortPriority, topBtn, e.target.value.trim()));
  liveBtn.addEventListener('click', () => renderEntries('live', byActiveAtDesc, liveBtn));
  newBtn.addEventListener('click', () => renderEntries('new', byCreatedAtDesc, newBtn));
  topBtn.addEventListener('click', () => renderEntries('top', bySortPriority, topBtn));

  function renderEntries(mode, comparator, btn, search = '') {
    if (currentSortMode === mode && search == currentSearch) return;
    currentSortMode = mode;
    if (location.hash) location.hash = '';
    liveBtn.classList.remove('active');
    newBtn.classList.remove('active');
    topBtn.classList.remove('active');
    if (search == '') {
      currentSearch = '';
      currentPage = 1;
      searchInput.value = '';
      btn.classList.add('active');
    } else {
      currentSearch = search;
    }
    filteredEntries = filterEntries(mode, search ?? '').sort(comparator);
    renderDirectoryPage();
  }
}

function renderDirectoryPage() {
  const currentEntries = addPagination(filteredEntries);
  displayEntries(currentEntries);
}

function filterEntries(mode, s) {
  if (s === '' && mode == 'top') return allEntries.slice();
  const query = s.toLowerCase();
  return allEntries.filter(entry =>
    ( mode === 'top'
      || (mode === 'new' && entry.createdAt)
      || (mode === 'live' && entry.activeAt)
    ) &&
    ( query === ''
      || (entry.displayName || '').toLowerCase().includes(query)
      || includesQuery(entry.shortDescr, query)
      || includesQuery(entry.welcomeMessage, query)
    )
  );
}

function includesQuery(field, query) {
  return field
    && Array.isArray(field)
    && field.some(ft => {
        switch (ft.format?.type) {
          case 'uri': return uriIncludesQuery(ft.text, query);
          case 'hyperLink': return textIncludesQuery(ft.format.showText, query) || uriIncludesQuery(ft.format.linkUri, query);
          case 'simplexLink': return textIncludesQuery(ft.format.showText, query);
          default: return textIncludesQuery(ft.text, query);
        }
      });
}

function textIncludesQuery(text, query) {
  return text ? text.toLowerCase().includes(query) : false
}

function uriIncludesQuery(uri, query) {
  if (!uri) return false;
  uri = uri.toLowerCase();
  return !uri.includes('simplex') && uri.includes(query);
}

async function fetchJSON(url) {
  try {
    const response = await fetch(url)
    if (!response.ok) throw new Error(`HTTP status: ${response.status}`)
    return await response.json()
  } catch (e) {
    console.error(e)
  }
}

function bySortPriority(entry1, entry2) {
  return entrySortPriority(entry2) - entrySortPriority(entry1);
}

function byActiveAtDesc(entry1, entry2) {
  return (roundedTs(entry2.activeAt) - roundedTs(entry1.activeAt)) * 10
    + Math.sign(bySortPriority(entry1, entry2));
}

function byCreatedAtDesc(entry1, entry2) {
  return (roundedTs(entry2.createdAt) - roundedTs(entry1.createdAt)) * 10
    + Math.sign(bySortPriority(entry1, entry2));
}

function roundedTs(s) {
  try {
    return new Date(s).valueOf();
  } catch {
    return 0;
  }
}

function entrySortPriority(entry) {
  return entry.displayName === simplexUsersGroup
    ? Number.MAX_VALUE
    : entryMemberCount(entry)
}

function entryMemberCount(entry) {
  return entry.entryType.type == 'group'
    ? (entry.entryType.summary?.currentMembers ?? 0)
    : 0
}

const now = new Date();
const nowVal = now.valueOf();
const today = new Date(now);
today.setHours(0, 0, 0, 0);
const todayVal = today.valueOf();
const todayYear = today.getFullYear();

const dateFormatter = Intl?.DateTimeFormat?.(undefined, {month: '2-digit', day: '2-digit'});
const dateYearFormatter = Intl?.DateTimeFormat?.(undefined, {year: 'numeric', month: '2-digit', day: '2-digit'});

function showDate(d) {
  return dateFormatter && d.getFullYear() == todayYear
    ? dateFormatter.format(d)
    : dateYearFormatter?.format(d) ?? d.toLocaleDateString();
}

function showCreatedOn(s) {
  const d = new Date(s)
  d.setHours(0, 0, 0, 0);
  return 'Created' + (d.valueOf() === todayVal ? ' today' : ' on ' + showDate(d));
}

function showActiveOn(s) {
  const d = new Date(s)
  const ago = nowVal - d.valueOf();
  if (ago <= 1200000) return 'Active now'; // 20 minutes
  if (ago <= 10800000) return 'Active recently'; // 3 hours
  d.setHours(0, 0, 0, 0);
  return 'Active' + (d.valueOf() === todayVal ? ' today' : ' on ' + showDate(d));
}

function displayEntries(entries) {
  const directory = document.getElementById('directory');
  directory.innerHTML = '';

  for (let entry of entries) {
    try {
      const { entryType, displayName, groupLink, shortDescr, welcomeMessage, imageFile } = entry;
      const entryDiv = document.createElement('div');
      entryDiv.className = 'entry w-full flex flex-col items-start md:flex-row rounded-[4px] overflow-hidden shadow-[0px_20px_30px_rgba(0,0,0,0.12)] dark:shadow-none bg-white dark:bg-[#11182F] mb-8';

      const textContainer = document.createElement('div');
      textContainer.className = 'text-container';

      const nameElement = document.createElement('h2');
      nameElement.textContent = displayName;
      nameElement.className = 'text-grey-black dark:text-white !text-lg md:!text-xl font-bold';
      textContainer.appendChild(nameElement);

      const welcomeMessageHTML = welcomeMessage ? renderMarkdown(welcomeMessage) : undefined;
      const shortDescrHTML = shortDescr ? renderMarkdown(shortDescr) : undefined;
      if (shortDescrHTML && welcomeMessageHTML?.includes(shortDescrHTML) !== true) {
        const descrElement = document.createElement('p');
        descrElement.innerHTML = renderMarkdown(shortDescr);
        textContainer.appendChild(descrElement);
      }

      if (welcomeMessageHTML) {
        const messageElement = document.createElement('p');
        messageElement.innerHTML = welcomeMessageHTML;
        textContainer.appendChild(messageElement);

        const readMore = document.createElement('p');
        readMore.textContent = 'Read more';
        readMore.className = 'read-more';
        readMore.style.display = 'none';
        textContainer.appendChild(readMore);

        setTimeout(() => {
          const computedStyle = window.getComputedStyle(messageElement);
          const lineHeight = parseFloat(computedStyle.lineHeight);
          const maxLines = 5;
          const maxHeight = maxLines * lineHeight
          const maxHeightPx = `${maxHeight}px`;
          messageElement.style.maxHeight = maxHeightPx;
          messageElement.style.overflow = 'hidden';

          if (messageElement.scrollHeight > maxHeight + 4) {
            readMore.style.display = 'block';
            readMore.addEventListener('click', () => {
              if (messageElement.style.maxHeight === maxHeightPx) {
                messageElement.style.maxHeight = 'none';
                readMore.className = 'read-less';
                readMore.innerHTML = '&#9650;';
              } else {
                messageElement.style.maxHeight = maxHeightPx;
                readMore.className = 'read-more';
                readMore.textContent = 'Read more';
              }
            });
          }
        }, 0);
      }

      const entryTimestamp = currentSortMode === 'new' && entry.createdAt
        ? showCreatedOn(entry.createdAt)
        : entry.activeAt
        ? showActiveOn(entry.activeAt)
        : '';
      if (entryTimestamp) {
        timestampElement = document.createElement('p');
        timestampElement.textContent = entryTimestamp;
        timestampElement.className = 'text-sm';
        textContainer.appendChild(timestampElement);
      }

      const memberCount = entryMemberCount(entry);
      if (typeof memberCount == 'number' && memberCount > 0) {
        const memberCountElement = document.createElement('p');
        memberCountElement.textContent = `${memberCount} members`;
        memberCountElement.className = 'text-sm';
        textContainer.appendChild(memberCountElement);
      }

      const imgLinkElement = document.createElement('a');
      const groupLinkUri = groupLink.connShortLink ?? groupLink.connFullLink
      try {
        imgLinkElement.href = platformSimplexUri(groupLinkUri);
      } catch(e) {
        console.log(e);
        imgLinkElement.href = groupLinkUri;
      }
      imgLinkElement.target = "_blank";
      imgLinkElement.title = `Join ${displayName}`;

      const imgElement = document.createElement('img');
      imgElement.src = imageFile ? simplexDirectoryDataURL + imageFile : '/img/group.svg';
      imgElement.alt = displayName;
      imgElement.addEventListener('error', () => imgElement.src = '/img/group.svg');
      imgLinkElement.appendChild(imgElement);
      entryDiv.appendChild(imgLinkElement);

      entryDiv.appendChild(textContainer);
      directory.appendChild(entryDiv);
    } catch (e) {
      console.log(e);
    }
  }

  for (let el of document.querySelectorAll('.secret')) {
    el.addEventListener('click', () => el.classList.toggle('visible'));
  }

  directory.style.height = '';
}

function goToPage(p) {
  currentPage = p;
  renderDirectoryPage();
}

function addPagination(entries) {
  const entriesPerPage = 10;
  const totalPages = Math.ceil(entries.length / entriesPerPage);
  if (currentPage < 1) currentPage = 1;
  if (currentPage > totalPages) currentPage = totalPages;

  const startIndex = (currentPage - 1) * entriesPerPage;
  const endIndex = Math.min(startIndex + entriesPerPage, entries.length);
  const currentEntries = entries.slice(startIndex, endIndex);

  // addPaginationElements('top-pagination')
  addPaginationElements('bottom-pagination')
  return currentEntries;

  function addPaginationElements(paginationId) {
    const pagination = document.getElementById(paginationId);
    if (!pagination) {
      return currentEntries;
    }
    pagination.innerHTML = '';

    try {
      let startPage, endPage;
      const pageButtonCount = 8
      if (totalPages <= pageButtonCount) {
        startPage = 1;
        endPage = totalPages;
      } else {
        startPage = Math.max(1, currentPage - 4);
        endPage = Math.min(totalPages, startPage + pageButtonCount - 1);
        if (endPage - startPage + 1 < pageButtonCount) {
          startPage = Math.max(1, endPage - pageButtonCount + 1);
        }
      }

      // if (currentPage > 1 && startPage > 1) {
      //   const firstBtn = document.createElement('button');
      //   firstBtn.textContent = 'First';
      //   firstBtn.classList.add('text-btn');
      //   firstBtn.addEventListener('click', () => goToPage(1));
      //   pagination.appendChild(firstBtn);
      // }

      if (currentPage > 1) {
        const prevBtn = document.createElement('button');
        prevBtn.textContent = 'Prev';
        prevBtn.classList.add('text-btn');
        prevBtn.addEventListener('click', () => goToPage(currentPage - 1));
        pagination.appendChild(prevBtn);
      }

      for (let p = startPage; p <= endPage; p++) {
        const pageBtn = document.createElement('button');
        pageBtn.textContent = p.toString();
        if (p === currentPage) {
          pageBtn.classList.add('active');
        } else if (p === currentPage - 1 || p === currentPage + 1 || (currentPage === 1 && p === 3) || (currentPage === totalPages && p === totalPages - 2)) {
          pageBtn.classList.add('neighbor');
        }
        pageBtn.addEventListener('click', () => goToPage(p));
        pagination.appendChild(pageBtn);
      }

      if (currentPage < totalPages) {
        const nextBtn = document.createElement('button');
        nextBtn.textContent = 'Next';
        nextBtn.classList.add('text-btn');
        nextBtn.addEventListener('click', () => goToPage(currentPage + 1));
        pagination.appendChild(nextBtn);
      }

      // if (endPage < totalPages) {
      //   const lastBtn = document.createElement('button');
      //   lastBtn.textContent = 'Last';
      //   lastBtn.classList.add('text-btn');
      //   lastBtn.addEventListener('click', () => goToPage(totalPages));
      //   pagination.appendChild(lastBtn);
      // }

    } catch (e) {
      console.log(e);
    }
  }
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initDirectory);
} else {
  initDirectory();
}

function escapeHtml(text) {
  return text
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#039;")
    .replace(/\n/g, "<br>");
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
          html += `<span class="${format.color}">${escapeHtml(text)}</span>`;
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
          html += `<a href="${platformSimplexUri(simplexUri)}" target="_blank">${linkText} <em>(${viaHost(smpHosts)})</em></a>`;
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
    } catch(e) {
      console.log(e);
      html += escapeHtml(text);
    }
  }
  return html;
}
})();

const simplexDirectoryDataURL = 'https://directory.simplex.chat/data/';

// const simplexDirectoryDataURL = 'http://localhost:8080/directory-data/';

const simplexUsersGroup = 'SimpleX users group';

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
