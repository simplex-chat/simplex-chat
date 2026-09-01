function showLocalTime() {
    const utc = document.querySelector('.event-utc');
    if (!utc) return;

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

    utc.textContent = format.format(start);
}

function startCountdown() {
    const utc = document.querySelector('.event-utc');
    const join = document.querySelector('.event-join');
    const count = join && join.querySelector('.join-count');
    if (!utc || !count) return;

    const start = new Date(utc.dateTime);
    if (isNaN(start.getTime())) return;

    const pad = (n) => String(n).padStart(2, '0');

    function tick() {
        const left = Math.floor((start.getTime() - Date.now()) / 1000);
        if (left <= 0) {
            count.textContent = 'now';
            join.classList.remove('counting');
        } else {
            const days = Math.floor(left / 86400);
            const dayPart = days ? days + (days === 1 ? ' day ' : ' days ') : '';
            count.textContent = 'in ' + dayPart
                + pad(Math.floor((left % 86400) / 3600)) + ' hrs '
                + pad(Math.floor((left % 3600) / 60)) + ' min '
                + pad(left % 60) + ' sec';
            join.classList.add('counting');
        }
        count.removeAttribute('hidden');
    }

    tick();
    setInterval(tick, 1000);
}

function setSignupSource() {
    const field = document.querySelector('input[name="SOURCE"]');
    if (!field) return;

    const inHash = new URLSearchParams(location.hash.replace(/^#\??/, '')).get('utm_source');
    const source = inHash ?? new URLSearchParams(location.search).get('utm_source');
    if (source && /^[\w.-]{1,40}$/.test(source)) field.value = source;
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
startCountdown();
setSignupSource();
setupRegisterOverlay();
trackNavColor();
