// switch theme
var navBar = document.querySelector("nav");
var landingPageHeader = document.querySelector('.landing-page-header');

const sunIcon = document.querySelector('.sun');
const moonIcon = document.querySelector('.moon');

const userTheme = localStorage.getItem('theme');
const systemTheme = window.matchMedia('(prefers-color-scheme: dark)').matches;

const iconToggle = () => {
    sunIcon.classList.toggle('hidden');
    moonIcon.classList.toggle('hidden');
}

const themeCheck = () => {
    if(userTheme === 'dark' || (!userTheme && systemTheme)){
        document.documentElement.classList.add('dark');
        moonIcon.classList.add('hidden');
    }
    else{
        sunIcon.classList.add('hidden');
    }
}

const themeSwitch = () => {
    if(document.documentElement.classList.contains('dark')){
        document.documentElement.classList.remove('dark');
        localStorage.setItem('theme','light');
        iconToggle();
    }
    else{
        document.documentElement.classList.add('dark');
        localStorage.setItem('theme','dark');
        iconToggle();
    }
}

const nav = document.querySelector('header nav');
window.addEventListener('click',(e)=>{
    if(e.target.closest('.nav-link')){
        if(e.target.closest('.nav-link').classList.contains('active')){
            e.target.closest('.nav-link').classList.remove('active');
        }
        else{
            document.querySelectorAll('.nav-link').forEach(el => el.classList.remove('active'))
            e.target.closest('.nav-link').classList.add('active');
        }
    }
    else if(e.target.closest('.nav-toggle-btn')){
        document.body.classList.toggle('lock-scroll');
        if(nav.classList.contains('open')){
            nav.classList.remove('open');
            document.getElementById('hamburger').classList.remove('hidden');
            document.getElementById('cross').classList.add('hidden');
        }
        else{
            nav.classList.add('open');
            document.getElementById('hamburger').classList.add('hidden');
            document.getElementById('cross').classList.remove('hidden');
        }
    }
    else if(e.target.closest('.theme-switch-btn')){
        themeSwitch();
    }
})

themeCheck();

const changeHeaderBg = ()=>{
    const header = document.querySelector('header')
    const scrollValue = window.scrollY
    if(scrollValue > 5){
        header.classList.add('bg-primary-bg-light');
        header.classList.add('dark:bg-primary-bg-dark');
    }
    else{
        header.classList.remove('bg-primary-bg-light');
        header.classList.remove('dark:bg-primary-bg-dark');
    }
}

window.addEventListener('scroll',changeHeaderBg);



window.addEventListener('load', function() {
    window.addEventListener('click',e=>{
        if(e.target.closest('.open-card-btn') || e.target.closest('.close-card-btn')){
            e.target.closest('.simplex-unique-card').classList.toggle('active-card');
            e.target.closest('.hide-show-btn').querySelector('.open-card-btn').classList.toggle('hidden');
            e.target.closest('.hide-show-btn').querySelector('.open-card-btn').classList.toggle('flex');
            e.target.closest('.hide-show-btn').querySelector('.close-card-btn').classList.toggle('hidden');
            e.target.closest('.hide-show-btn').querySelector('.close-card-btn').classList.toggle('flex');
        }
    })
});

window.addEventListener("load", function () {
    const simplexExplainedSwiper__bullets = document.querySelectorAll(".simplex-explained-swiper .swiper-pagination > span");
    const simplexExplainedSwiper__tabs = document.querySelectorAll("#simplex-explained .tabs .tab-button");

    const observer = new MutationObserver((mutations) => {
        mutations.forEach((mutation) => {
            if (mutation.type === "attributes" && mutation.attributeName === "class" && mutation.target.classList.contains("swiper-pagination")) {
                simplexExplainedSwiper__bullets.forEach((el,index) => {
                    if (el.classList.contains("swiper-pagination-bullet-active")) {
                        simplexExplainedSwiper__tabs[index].classList.add("active");
                    } else {
                        simplexExplainedSwiper__tabs[index].classList.remove("active");
                    }
                });
            }
        });
    }); 

    const targetNode = document.querySelector(".simplex-explained-swiper .swiper-pagination");
    observer.observe(targetNode, { attributes: true });

    window.addEventListener('click',e=>{
        if(e.target.closest("#simplex-explained .tabs .tab-button")){
            const index = e.target.closest("#simplex-explained .tabs .tab-button").dataset.btnIndex;
            simplexExplainedSwiper__bullets[index].click();
        }
    })
});


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
    document.body.classList.toggle('lock-scroll');
}

window.addEventListener('click', clickHandler)
window.addEventListener('touchstart', clickHandler)

function clickHandler(e) {
    console.log(e)
    e.stopPropagation()
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

const isMobile = {
    Android: () => navigator.userAgent.match(/Android/i),
    iOS: () => navigator.userAgent.match(/iPhone|iPad|iPod/i)
};

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
