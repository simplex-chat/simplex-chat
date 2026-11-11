var pageEvents = {
    domain: 'simplex.chat',
    endpoint: 'https://api.simplex.chat/api/event',
    logging: false,
    outboundLinks: true,
    transformRequest: function (event) {
        if (window !== window.top || location.hostname.endsWith('.onion')) return null;
        if (location.pathname.startsWith('/directory')) event.u = event.u.split('#')[0];
        // remove hash from outbound links
        if (event.p && event.p.url) event.p.url = event.p.url.split('#')[0];
        return event;
    }
};
