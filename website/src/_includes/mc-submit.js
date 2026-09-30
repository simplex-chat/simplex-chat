(function () {
    var LANDINGS = [
        { path: '/subscribe/thankyou', status: 'subscribed' },
        { path: '/subscribe/confirmed', status: 'confirmed' }
    ];
    var MESSAGES = {
        'mc:subscribed': 'subscribed',
        'mc:confirmed': 'confirmed',
        'mc:failed': 'failed'
    };
    var TIMEOUT = 12000;
    var REDIRECT_GRACE = 5000;

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

        function done(status) {
            if (!pending) return;
            pending = false;
            clearTimeout(timer);
            document.dispatchEvent(new CustomEvent('mc:result', {
                detail: { ok: status !== 'failed', status: status }
            }));
        }

        function landedStatus() {
            var path;
            try {
                path = frame.contentWindow.location.pathname;
            } catch (e) {
                return null;
            }
            for (var i = 0; i < LANDINGS.length; i++) {
                if (path.indexOf(LANDINGS[i].path) === 0) return LANDINGS[i].status;
            }
            return null;
        }

        window.addEventListener('message', function (e) {
            if (!pending || e.source !== frame.contentWindow) return;
            var status = MESSAGES[e.data];
            if (status) done(status);
        });

        frame.addEventListener('load', function () {
            if (!pending) return;
            var status = landedStatus();
            if (status) {
                done(status);
                return;
            }
            clearTimeout(timer);
            timer = setTimeout(function () { done('failed'); }, REDIRECT_GRACE);
        });

        forms.forEach(function (form) {
            form.target = 'mc-target';
            form.addEventListener('submit', function () {
                pending = true;
                clearTimeout(timer);
                timer = setTimeout(function () { done('failed'); }, TIMEOUT);
            });
        });
    }

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', init);
    } else {
        init();
    }
})();
