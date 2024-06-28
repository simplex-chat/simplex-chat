const uniqueSwiperContainer = document.querySelector('.unique-swiper')
if (uniqueSwiperContainer) {
const uniqueSwiper = new Swiper('.unique-swiper', {
    slidesPerView: 1,
    spaceBetween: 80,
    allowTouchMove: true,
    breakpoints: {
        1400: {
            allowTouchMove: false,
        },
        1200: {
            allowTouchMove: true,
        },
        768: {
            allowTouchMove: true,
        }
    },
    direction: 'horizontal',
    pagination: {
        el: '.simplex-unique-swiper-pagination',
        clickable: true,
    },
    navigation: {
        nextEl: '.unique-swiper-button-next',
        prevEl: '.unique-swiper-button-prev',
    },
});
}

const isMobile = {
    Android: () => navigator.userAgent.match(/Android/i),
    iOS: () => navigator.userAgent.match(/iPhone|iPad|iPod/i)
};

const privateSwiperContainer = document.querySelector('.private-swiper')
if (privateSwiperContainer) {
const privateSwiper = new Swiper('.private-swiper', {
    slidesPerView: 1,
    slidesPerGroup: 1,
    spaceBetween: 20,
    allowTouchMove: true,
    scrollbar: {
        el: '.swiper-scrollbar',
        draggable: true,
        snapOnRelease: true,
        dragSize: 100,
    },
    direction: 'horizontal',
    navigation: {
        nextEl: '.private-swiper-button-next',
        prevEl: '.private-swiper-button-prev',
    },
    breakpoints: {
        1400: {
            slidesPerView: 4,
            slidesPerGroup: 4,
            spaceBetween: 20,
            allowTouchMove: false,
        },
        1200: {
            slidesPerView: 3,
            slidesPerGroup: 3,
            spaceBetween: 20,
            allowTouchMove: true,
        },
        768: {
            slidesPerView: 2,
            slidesPerGroup: 2,
            spaceBetween: 20,
            allowTouchMove: true,
        }
    },
    on: {
        slideChange: function () {
            const privateSwiperGlossaryTerms = document.querySelectorAll('.private-swiper .glossary-term');
            privateSwiperGlossaryTerms.forEach(function (glossaryTerm) {
                var tooltipId = glossaryTerm.getAttribute('data-glossary');
                var tooltip = document.getElementById(tooltipId);
                tooltip.style.visibility = 'hidden';
                tooltip.style.opacity = '0';
                const privateSwiper = glossaryTerm.closest('.private-swiper')
                if (privateSwiper) glossaryTerm.closest('.card').classList.remove('hovered');
            })
        }
    }
});
}

const simplexExplainedSwiperContainer = document.querySelector('.simplex-explained-swiper')
if (simplexExplainedSwiperContainer){
const simplexExplainedSwiper = new Swiper(".simplex-explained-swiper", {
    slidesPerView: 1,
    spaceBetween: 80,
    allowTouchMove: true,
    breakpoints: {
        1400: {
            allowTouchMove: false,
        },
        1200: {
            allowTouchMove: true,
        },
        768: {
            allowTouchMove: true,
        }
    },
    direction: "horizontal",
    pagination: {
        el: ".simplex-explained-swiper-pagination",
        clickable: true
    },
    on: {
        slideChange: function () {
            const explainedSwiperGlossaryTerms = document.querySelectorAll('.simplex-explained-swiper .glossary-term');
            explainedSwiperGlossaryTerms.forEach(function (glossaryTerm) {
                var tooltipId = glossaryTerm.getAttribute('data-glossary');
                var tooltip = document.getElementById(tooltipId);
                tooltip.style.visibility = 'hidden';
                tooltip.style.opacity = '0';
            })
        }
    }
});
}

function closeOverlay(e) {
    e.target.closest('.overlay').classList.remove('flex');
    e.target.closest('.overlay').classList.add('hidden');
    document.body.classList.remove('lock-scroll');
    history.replaceState(null, null, ' ');
}

window.addEventListener('click', clickHandler)

if (isMobile.iOS) {
    for (const btn of document.getElementsByClassName("close-overlay-btn")) {
        btn.addEventListener("touchend", (e) => setTimeout(() => closeOverlay(e), 100))
    }
}

function clickHandler(e) {
    if (e.target.closest('.card')) {
        e.target.closest('.card').classList.toggle('card-active');
        e.target.closest('.card').classList.toggle('no-hover');
    }

    // ---------------For Overlay--------------------
    else if (e.target.closest('.close-overlay-btn')) {
        closeOverlay(e);
    }
    else if (e.target.closest('.overlay-card')) {
        return;
    }
    else if (e.target.closest('.overlay')) {
        closeOverlay(e);
    }
    else if (e.target.closest('.open-overlay-btn')) {
        let id = e.target.closest('.open-overlay-btn').dataset.showOverlay;
        window.location.hash = id;
    }
    // -----------------------------------------------
    // ---------- For Contact & Invitation Page tabs
    else if (e.target.closest('.contact-tab-btn')) {
        e.target.closest('.contact-tab').classList.toggle('active')
    }
}

window.addEventListener('load', () => {
    const googlePlayBtn = document.querySelector('.google-play-btn');
    const appleStoreBtn = document.querySelector('.apple-store-btn');
    const fDroidBtn = document.querySelector('.f-droid-btn');
    if (!googlePlayBtn || !appleStoreBtn || !fDroidBtn) return;


    if (isMobile.Android()) {
        googlePlayBtn.classList.remove('hidden');
        fDroidBtn.classList.remove('hidden');
    }
    else if (isMobile.iOS()) {
        appleStoreBtn.classList.remove('hidden');
    }
    else {
        appleStoreBtn.classList.remove('hidden');
        googlePlayBtn.classList.remove('hidden');
        fDroidBtn.classList.remove('hidden');
    }
})

function openOverlay() {
    let hash = window.location.hash
    if (hash) {
        const id = hash.split('#')[1];
        const el = document.getElementById(id)
        if (el.classList.contains('overlay')) {
            const scrollTo = el.getAttribute('data-scroll-to')
            if (scrollTo) {
                const scrollToEl = document.getElementById(scrollTo)
                if (scrollToEl) scrollToEl.scrollIntoView(true)
            }

            const currentOpenedGlossaryOverlay = document.querySelector('.glossary-overlay.flex')
            if (currentOpenedGlossaryOverlay) {
                currentOpenedGlossaryOverlay.classList.remove('flex')
                currentOpenedGlossaryOverlay.classList.add('hidden')
            }

            el.classList.remove('hidden')
            el.classList.add('flex')
            document.body.classList.add('lock-scroll')
        }
    }
}

function updatePointerEventsInPrivateSwiperCards() {
    var privateSwiperCards = document.querySelectorAll('.private-swiper .card')

    privateSwiperCards.forEach(function (card) {
        var cardContent = card.querySelector('.card-content')

        function updatePointerEvents() {
            var cardContentGlossaryTerms = cardContent.querySelectorAll('.glossary-term')
            cardContentGlossaryTerms.forEach(function (glossaryTerm) {
                if (cardContent.offsetHeight >= 270) {
                    glossaryTerm.style.pointerEvents = 'all'
                } else {
                    glossaryTerm.style.pointerEvents = 'none'
                }
            })
        }
        updatePointerEvents()

        cardContent.addEventListener('click', updatePointerEvents)
        cardContent.addEventListener('mousemove', updatePointerEvents)
    })
}

function updateTooltipPosition(glossaryTerm, tooltip) {
    var glossaryTermOffset = glossaryTerm.getBoundingClientRect()
    var tooltipOffset = tooltip.getBoundingClientRect()

    if (glossaryTermOffset.top >= tooltipOffset.height) {
        tooltip.style.top = glossaryTermOffset.top - tooltipOffset.height + 'px'
    } else {
        tooltip.style.top = glossaryTermOffset.bottom + 'px'
    }

    var leftPosition = glossaryTermOffset.left + glossaryTerm.offsetWidth / 2 - tooltip.offsetWidth / 2
    if (leftPosition < 0) {
        tooltip.style.left = '0px'
    } else if (leftPosition + tooltip.offsetWidth > window.innerWidth) {
        tooltip.style.left = window.innerWidth - tooltip.offsetWidth + 'px'
    } else {
        tooltip.style.left = leftPosition + 'px'
    }
}

function setupTooltip(glossaryTerm) {
    var tooltipId = glossaryTerm.getAttribute('data-glossary')
    var tooltip = document.getElementById(tooltipId)

    function showTooltip() {
        tooltip.style.visibility = 'visible'
        tooltip.style.opacity = '1'
        updateTooltipPosition(glossaryTerm, tooltip)
        const privateSwiper = glossaryTerm.closest('.private-swiper')
        if (privateSwiper) glossaryTerm.closest('.card').classList.add('hovered')
    }

    function hideTooltip() {
        tooltip.style.visibility = 'hidden'
        tooltip.style.opacity = '0'
        const privateSwiper = glossaryTerm.closest('.private-swiper')
        if (privateSwiper) glossaryTerm.closest('.card').classList.remove('hovered')
        if (!glossaryTerm.matches(':hover') && !tooltip.matches(':hover')) {
            glossaryTerm.classList.remove('active-term')
            tooltip.removeEventListener('mouseover', showTooltip)
            tooltip.removeEventListener('mouseout', hideTooltip)
        }
    }

    let click = 0
    glossaryTerm.addEventListener('mouseover', () => {
        glossaryTerm.classList.add('active-term')
        showTooltip()
        tooltip.addEventListener('mouseover', showTooltip)
        tooltip.addEventListener('mouseout', hideTooltip)
    })
    glossaryTerm.addEventListener('mouseout', function (event) {
        click = 0
        hideTooltip()
    })
    glossaryTerm.addEventListener('click', function (event) {
        event.stopPropagation()
        if (click == 1) {
            hideTooltip()
            click = 0
        }
        else {
            showTooltip()
            click = 1
        }
    })
}

window.addEventListener('load', () => {
    openOverlay()
    updatePointerEventsInPrivateSwiperCards()

    window.addEventListener('scroll', function () {
        let activeTerm = document.querySelector('.active-term')
        if (activeTerm) {
            var tooltipId = activeTerm.getAttribute('data-glossary')
            var tooltip = document.getElementById(tooltipId)
            updateTooltipPosition(activeTerm, tooltip)
        }
    })

    const glossaryTerms = document.querySelectorAll('.glossary-term')
    glossaryTerms.forEach(setupTooltip)
})
window.addEventListener('hashchange', openOverlay)
