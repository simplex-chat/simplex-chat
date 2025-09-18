const directoryDataURL = 'http://localhost:8080/directory-data/';

async function renderDirectory() {
  const listing = await fetchJSON(directoryDataURL + 'listing.json')
  console.log(listing)
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
      descrElement.textContent = shortDescr;
      textContainer.appendChild(descrElement);
    }

    const messageElement = document.createElement('p');
    messageElement.textContent = welcomeMessage;
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
