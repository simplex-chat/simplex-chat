// console.log(window.location.href)
// if(window.location.href.includes('comparison')){
//     console.log('Comparison');
//     document.querySelector('.comparison-nav-btn').classList.add('nav-button-active');
// }
// else if(window.location.href.includes('blog')){
//     document.querySelector('.blog-nav-btn').classList.add('nav-button-active');
// }
// const btnMobile = document.getElementById('btn-mobile');
// const hamburger = document.getElementById('hamburger');
// const cross = document.getElementById('cross');

// function toggleMenu(event) {
//   if (event.type === 'touchstart') event.preventDefault();
//   const nav = document.querySelector('nav');
//   nav.classList.toggle('active');
//   hamburger.classList.toggle('hidden');
//   cross.classList.toggle('hidden');

//   const active = nav.classList.contains('active');
//   if (active) {

//   } else {

//   }
// }

// btnMobile.addEventListener('click', toggleMenu);
// btnMobile.addEventListener('touchstart', toggleMenu);

const swiper = new Swiper('.unique-swiper', {
    slidesPerView: 1,
    spaceBetween: 80,
    // autoplay: {
    //   delay: 3000,
    //   disableOnInteraction: false,
    // },
    direction: 'horizontal',
    pagination: {
        el: '.swiper-pagination',
        clickable: true,
    },
    navigation: {
        nextEl: '.swiper-button-next',
        prevEl: '.swiper-button-prev',
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
        nextEl: '.swiper-button-next',
        prevEl: '.swiper-button-prev',
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

const closeOverlay = (e) => {
    e.target.closest('.overlay').classList.remove('flex');
    e.target.closest('.overlay').classList.add('hidden');
    document.body.classList.toggle('lock-scroll');
}

window.addEventListener('click', (e) => {
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
})