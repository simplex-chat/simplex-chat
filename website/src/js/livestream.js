function showLocalTime() {
    const utc = document.querySelector('.event-utc');
    const local = document.querySelector('.event-local');
    if (!utc || !local) return;

    const start = new Date(utc.dateTime);
    if (isNaN(start.getTime())) return;

    const format = new Intl.DateTimeFormat('en-US', {
        weekday: 'long',
        month: 'long',
        day: 'numeric',
        hour: 'numeric',
        minute: '2-digit',
        timeZoneName: 'short'
    });

    local.textContent = 'Your time: ' + format.format(start);
    local.removeAttribute('hidden');
}

function setupRegisterOverlay() {
    const overlay = document.getElementById('register');
    const openBtn = document.querySelector('.register-btn');
    if (!overlay || !openBtn) return;

    const email = overlay.querySelector('input[type="email"]');
    const form = overlay.querySelector('form');

    function openOverlay(e) {
        e.preventDefault();
        overlay.classList.add('open');
        document.documentElement.classList.add('lock-scroll');
        email.focus();
    }

    function closeOverlay() {
        overlay.classList.remove('open');
        document.documentElement.classList.remove('lock-scroll');
    }

    openBtn.addEventListener('click', openOverlay);
    form.addEventListener('submit', closeOverlay);
    overlay.addEventListener('click', (e) => {
        if (e.target === overlay || e.target.closest('.close-register')) closeOverlay();
    });
    document.addEventListener('keydown', (e) => {
        if (e.key === 'Escape' && overlay.classList.contains('open')) closeOverlay();
    });
}

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

showLocalTime();
setupRegisterOverlay();
trackNavColor();
