var analytics = {
    domain: 'simplex.chat',
    endpoint: 'https://api.simplex.chat/api/event',
    logging: false,
    outboundLinks: true,
    transformRequest: function (event) {
        if (window !== window.top || location.hostname.endsWith('.onion')) return null;
        if (location.pathname.startsWith('/directory')) event.u = event.u.split('#')[0];
        if (event.p && event.p.url) event.p.url = event.p.url.split('#')[0];
        return event;
    }
};

function trackLink(e) {
    var link = e.target.closest('a[href]');
    if (!link || link.host !== location.host || link.pathname === location.pathname) return;
    if (e.type === 'click' || e.button === 1) track('Link', { props: { to: link.pathname } });
}

document.addEventListener('click', trackLink);
document.addEventListener('auxclick', trackLink);
