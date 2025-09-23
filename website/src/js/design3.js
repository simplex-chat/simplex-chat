const isMobile = {
    Android: () => navigator.userAgent.match(/Android/i),
    iOS: () => navigator.userAgent.match(/iPhone|iPad|iPod/i)
};

document.addEventListener('DOMContentLoaded', () => {
    const googlePlayBtn = document.querySelector('.google-play-btn');
    const appleStoreBtn = document.querySelector('.apple-store-btn');
    const fDroidBtn = document.querySelector('.f-droid-btn');
    const testflightBtn = document.querySelector('.testflight-btn');
    const androidBtn = document.querySelector('.android-btn');

    if (!googlePlayBtn || !appleStoreBtn || !fDroidBtn || !testflightBtn || !androidBtn) return;


    if (isMobile.Android()) {
        googlePlayBtn.classList.remove('hidden');
        fDroidBtn.classList.remove('hidden');
        androidBtn.classList.remove('hidden');
    }
    else if (isMobile.iOS()) {
        appleStoreBtn.classList.remove('hidden');
        testflightBtn.classList.remove('hidden');
    }
    else {
        appleStoreBtn.classList.remove('hidden');
        googlePlayBtn.classList.remove('hidden');
        fDroidBtn.classList.remove('hidden');
        testflightBtn.classList.remove('hidden');
        androidBtn.classList.remove('hidden');
    }
});