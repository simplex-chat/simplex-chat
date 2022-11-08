const uniqueSwiper = new Swiper('.unique-swiper', {
    slidesPerView: 1,
    spaceBetween: 80,
    // autoplay: {
    //   delay: 3000,
    //   disableOnInteraction: false,
    // },
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

const isMobile = {
    Android: () => navigator.userAgent.match(/Android/i),
    iOS: () => navigator.userAgent.match(/iPhone|iPad|iPod/i)
};

const privateSwiper = new Swiper('.private-swiper', {
    slidesPerView: 1,
    spaceBetween: 20,
    // autoplay: {
    //     delay: 3000,
    // },

    direction: 'horizontal',
    scrollbar: {
        el: ".swiper-scrollbar",
        dragSize: 100,
    },

    navigation: {
        nextEl: '.private-swiper-button-next',
        prevEl: '.private-swiper-button-prev',
    },

    breakpoints: {
        1280: {
            slidesPerView: 4,
            spaceBetween: 20,
        },
        1024: {
            slidesPerView: 3,
            spaceBetween: 20,
        },
        768: {
            slidesPerView: 2,
            spaceBetween: 20,
        }
    },
});

const simplexExplainedSwiper = new Swiper(".simplex-explained-swiper", {
    slidesPerView: 1,
    spaceBetween: 80,
    // autoplay: {
    //   delay: 3000,
    //   disableOnInteraction: false,
    // },
    direction: "horizontal",
    pagination: {
        el: ".simplex-explained-swiper-pagination",
        clickable: true
    }
});

function closeOverlay (e) {
    e.target.closest('.overlay').classList.remove('flex');
    e.target.closest('.overlay').classList.add('hidden');
    document.body.classList.remove('lock-scroll');
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
        let overlay = document.getElementById(id);
        overlay.classList.remove('hidden');
        overlay.classList.add('flex');
        document.body.classList.toggle('lock-scroll');
    }
    // -----------------------------------------------
    // ---------- For Contact & Invitation Page tabs
    else if(e.target.closest('.contact-tab-btn')){
        e.target.closest('.contact-tab').classList.toggle('active')
    }
}

window.addEventListener('load', () => {
    const googlePlayBtn = document.querySelector('.google-play-btn');
    const appleStoreBtn = document.querySelector('.apple-store-btn');
    const fDroidBtn = document.querySelector('.f-droid-btn');
    if(!googlePlayBtn || !appleStoreBtn || !fDroidBtn) return;


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
