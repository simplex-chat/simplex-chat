function trackNavColor() {
    const footer = document.querySelector('.footer.page');
    const mobileHeader = document.getElementById('mobile-header');
    if (!footer || !('IntersectionObserver' in window)) return;

    const observer = new IntersectionObserver((entries) => {
        const atFooter = entries.some((e) => e.isIntersecting && e.intersectionRatio >= 0.02);
        document.body.classList.toggle('change-nav-color', !atFooter);
        if (mobileHeader) {
            mobileHeader.classList.toggle('footer', atFooter);
            mobileHeader.classList.toggle('main', !atFooter);
        }
    }, { threshold: [0, 0.02, 1] });

    observer.observe(footer);
}

trackNavColor();
