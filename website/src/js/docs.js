document.addEventListener("DOMContentLoaded", function () {
    const imgs = document.querySelectorAll('p img')
    imgs.forEach(img => {
        console.log(img.height)
        img.style.height = `${img.getAttribute('height')}px`
    })

    const allParagraphs = document.querySelectorAll("p")
    allParagraphs.forEach((paragraph) => {
        if (paragraph.querySelector("img")) {
            paragraph.style.display = "flex"
        }
    })

    const docMain = document.querySelector('#doc main')
    const menuBtn = document.querySelector('#doc main button.menu')
    docMain.addEventListener('click', () => {
        docMain.classList.remove("overlay")
        document.body.classList.remove('lock-scroll')
    })

    menuBtn.addEventListener('click', (e) => {
        docMain.classList.add("overlay")
        document.body.classList.add('lock-scroll')
        e.stopPropagation()
    })

    const headerWithIds = document.querySelectorAll('h1[id], h2[id]')
    const hashList = document.querySelector('.hash-list')

    headerWithIds.forEach(header => {
        const id = header.getAttribute('id')

        const listItem = document.createElement('li')
        const anchor = document.createElement('a')
        anchor.href = `#${id}`
        anchor.textContent = header.textContent
        listItem.appendChild(anchor)
        hashList.appendChild(listItem)

        const hashLink = document.createElement('a')
        hashLink.href = `#${id}`
        hashLink.textContent = '#'
        hashLink.classList.add('hash-link')
        header.appendChild(hashLink)
    })

    function scrollToIdWithTopOffset(id, topOffset) {
        const element = document.getElementById(id)
        if (element) {
            const elementRect = element.getBoundingClientRect()
            const absoluteElementTop = elementRect.top + window.scrollY
            const scrollToPosition = absoluteElementTop - topOffset
            window.scrollTo({ top: scrollToPosition, behavior: 'smooth' })
        }
    }

    function scrollToHashWithTopOffset() {
        const topOffset = window.innerWidth <= 1024 ? 104 : 66 // Adjust the top offset value as needed
        const hash = window.location.hash.substring(1)

        if (hash) {
            setTimeout(() => {
                scrollToIdWithTopOffset(hash, topOffset)
            }, 0)
        }
    }

    document.addEventListener('click', (event) => {
        const target = event.target
        if (target.tagName.toLowerCase() === 'a' && target.getAttribute('href').startsWith('#')) {
            event.preventDefault()
            const id = target.getAttribute('href').substring(1)

            const topOffset = window.innerWidth <= 1024 ? 104 : 66 // Adjust the top offset value as needed
            history.pushState(null, null, '#' + id)

            scrollToIdWithTopOffset(id, topOffset)
        }
    })

    window.addEventListener('load', () => {
        scrollToHashWithTopOffset()
    })

    const backToTop = document.getElementById('back-to-top')
    backToTop.addEventListener('click', function (event) {
        event.preventDefault()
        window.scrollTo({ top: 0, behavior: 'smooth' })
    })
})