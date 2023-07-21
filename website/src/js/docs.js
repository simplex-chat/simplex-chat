document.addEventListener("DOMContentLoaded", function () {
    if (window.location.pathname.endsWith('cli.html')) {
        const cliHeader = document.querySelector('h1')
        const demoSection = document.createElement('section')
        demoSection.id = 'demo'
        demoSection.innerHTML = `
            <div class="all-users">
                <div class="user alice">
                    <h3>@alice</h3>
                    <div class="terminal">
                        <div class="display"></div>
                        <div class="input"></div>
                        <input type="text" />
                    </div>
                </div>
                <div class="user bob">
                    <h3>@bob</h3>
                    <div class="terminal">
                        <div class="display"></div>
                        <div class="input"></div>
                        <input type="text" />
                    </div>
                </div>
                <div class="user tom">
                    <h3>@tom</h3>
                    <div class="terminal">
                        <div class="display"></div>
                        <div class="input"></div>
                        <input type="text" />
                    </div>
                </div>
                <button class="run-demo">Run demo</button>
                <button class="run-faster">Faster!</button>
                <button class="try-it">Try it!</button>
            </div>
        `
        cliHeader.parentNode.insertBefore(demoSection, cliHeader.nextSibling)

        const demoScript = document.createElement('script')
        demoScript.src = '/js/demo.js'
        document.body.appendChild(demoScript)

        const demoStyles = document.createElement('link')
        demoStyles.rel = 'stylesheet'
        demoStyles.href = '/css/demo.css'
        document.head.appendChild(demoStyles)
    }

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