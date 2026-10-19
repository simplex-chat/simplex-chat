(function () {
    var VALID = /^[\w.-]{1,40}$/;
    var KEYS = ['utm_source', 'utm_campaign'];

    function fromUrl(name) {
        var hash = new URLSearchParams(location.hash.replace(/^#\??/, '')).get(name);
        var query = new URLSearchParams(location.search).get(name);
        var value = hash != null ? hash : query;
        return value && VALID.test(value) ? value : null;
    }

    function remember(name, value) {
        try {
            if (value) sessionStorage.setItem(name, value);
            return value || sessionStorage.getItem(name);
        } catch (e) {
            return value;
        }
    }

    var utm = {};
    KEYS.forEach(function (name) {
        var value = remember(name, fromUrl(name));
        if (value && VALID.test(value)) utm[name] = value;
    });

    if (!utm.utm_source && !utm.utm_campaign) return;

    function fill(name, value) {
        if (!value) return;
        document.querySelectorAll('input[name="' + name + '"]').forEach(function (input) {
            input.value = value;
        });
    }

    function withUtm(href) {
        var url = new URL(href, location.href);
        KEYS.forEach(function (name) {
            if (utm[name]) url.searchParams.set(name, utm[name]);
        });
        return url.toString();
    }

    function apply() {
        fill('SOURCE', utm.utm_source);
        fill('CAMPAIGN', utm.utm_campaign);

        if (/^\/blog(\/|$)/.test(location.pathname)) return;

        document.querySelectorAll('a[href^="https://wefunder.com/"]').forEach(function (link) {
            link.href = withUtm(link.getAttribute('href'));
        });
    }

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', apply);
    } else {
        apply();
    }
})();
