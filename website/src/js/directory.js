const directoryDataURL = 'https://directory.simplex.chat/data/';

// const directoryDataURL = 'http://localhost:8080/directory-data/';

let allEntries = [];

let sortedEntries = [];

let filteredEntries = [];

let currentSortMode = '';

async function initDirectory() {
  console.log('initDirectory')
  const listing = await fetchJSON(directoryDataURL + 'listing.json')
  const topBtn = document.querySelector('#top-pagination .top');
  const newBtn = document.querySelector('#top-pagination .new');
  const searchInput = document.getElementById('search');
  allEntries = listing.entries
  renderSortedEntries('new', byCreatedAtDesc, newBtn)
  window.addEventListener('hashchange', renderDirectoryPage);
  searchInput.addEventListener('input', (e) => renderFilteredEntries(e.target.value));

  newBtn.addEventListener('click', () => renderSortedEntries('new', byCreatedAtDesc, newBtn));
  topBtn.addEventListener('click', () => renderSortedEntries('top', byMemberCountDesc, topBtn));

  function renderSortedEntries(mode, comparator, btn) {
    if (currentSortMode === mode) return;
    currentSortMode = mode;
    newBtn.classList.remove('active');
    topBtn.classList.remove('active');
    btn.classList.add('active');
    sortedEntries = allEntries.slice().sort(comparator);
    renderFilteredEntries(searchInput.value);
  }
}

function renderDirectoryPage() {
  const currentEntries = addPagination(filteredEntries);
  displayEntries(currentEntries);
}

function renderFilteredEntries(s) {
  const query = s.toLowerCase().trim();
  if (query === '') {
    filteredEntries = sortedEntries.slice();
  } else {
    filteredEntries = sortedEntries.filter(entry =>
      (entry.displayName || '').toLowerCase().includes(query)
        || includesQuery(entry.shortDescr, query)
        || includesQuery(entry.welcomeMessage, query)
    );
  }
  renderDirectoryPage();
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
  text ? text.toLowerCase().includes(query) : false
}

function uriIncludesQuery(uri, query) {
  if (!uri) return false;
  uri = uri.toLowerCase();
  return !uri.includes('simplex') && uri.includes(query);
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

function byMemberCountDesc(entry1, entry2) {
  return entryMemberCount(entry2) - entryMemberCount(entry1)
}

function byCreatedAtDesc(entry1, entry2) {
  return entry2.createdAt?.localeCompare(entry1.createdAt ?? '') ?? 0
}

function entryMemberCount(entry) {
  return entry.entryType.type == 'group'
    ? (entry.entryType.summary?.currentMembers ?? 0)
    : 0
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

      const memberCount = entryMemberCount(entry);
      if (typeof memberCount == 'number' && memberCount > 0) {
        const memberCountElement = document.createElement('p');
        memberCountElement.innerText = `${memberCount} members`;
        memberCountElement.classList = ['text-sm'];
        textContainer.appendChild(memberCountElement);
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
  location.hash = p.toString();
}

function addPagination(entries) {
  const entriesPerPage = 10;
  const totalPages = Math.ceil(entries.length / entriesPerPage);
  let currentPage = parseInt(location.hash.slice(1)) || 1;
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
        } else if (p === currentPage - 1 || p === currentPage + 1) {
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
