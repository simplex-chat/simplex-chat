(function () {
    var THANKYOU = '/subscribe/thankyou';
    var TIMEOUT = 12000;

    function init() {
        var forms = document.querySelectorAll('form[action*="list-manage.com"]');
        if (!forms.length) return;

        var frame = document.createElement('iframe');
        frame.name = 'mc-target';
        frame.title = 'Subscription result';
        frame.className = 'mc-frame';
        (document.querySelector('.mc-frame-host') || document.body).appendChild(frame);

        var pending = false;
        var timer = null;

        function done(ok) {
            if (!pending) return;
            pending = false;
            clearTimeout(timer);
            document.dispatchEvent(new CustomEvent('mc:result', { detail: { ok: ok } }));
        }

        frame.addEventListener('load', function () {
            if (!pending) return;
            var ok = false;
            try {
                ok = frame.contentWindow.location.pathname.indexOf(THANKYOU) === 0;
            } catch (e) {
                ok = false;
            }
            done(ok);
        });

        forms.forEach(function (form) {
            form.target = 'mc-target';
            form.addEventListener('submit', function () {
                pending = true;
                clearTimeout(timer);
                timer = setTimeout(function () { done(false); }, TIMEOUT);
            });
        });
    }

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', init);
    } else {
        init();
    }
})();
