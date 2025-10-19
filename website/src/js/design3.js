const isMobile = {
    Android: () => navigator.userAgent.match(/Android/i),
    iOS: () => navigator.userAgent.match(/iPhone|iPad|iPod/i),
    any: () => navigator.userAgent.match(/Android|iPhone|iPad|iPod/i)
};

(function() {
document.addEventListener('DOMContentLoaded', () => {
    const googlePlayBtn = document.querySelector('.google-play-btn');
    const appleStoreBtn = document.querySelector('.apple-store-btn');
    const fDroidBtn = document.querySelector('.f-droid-btn');
    const testflightBtn = document.querySelector('.testflight-btn');
    const androidBtn = document.querySelector('.android-btn');
    const desktopAppBtn = document.querySelector('.desktop-app-btn');

    if (!googlePlayBtn || !appleStoreBtn || !fDroidBtn || !testflightBtn || !androidBtn || !desktopAppBtn) return;


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
        desktopAppBtn.classList.remove('hidden');
        // fDroidBtn.classList.remove('hidden');
        // testflightBtn.classList.remove('hidden');
        // androidBtn.classList.remove('hidden');
    }

    showPromotedGroups();
});

async function showPromotedGroups() {
    welcome();
    const listing = await fetchJSON(simplexDirectoryDataURL + 'promoted.json');
    let [entries, imgPath] =
            Array.isArray(listing?.entries) && listing.entries.length > 0
            ? [listing.entries, simplexDirectoryDataURL]
            : [fallbackEntries(), '/img/groups/'];
    // Uncomment to log fallback entries
    // entries.forEach(e => {
    //     delete e.activeAt;
    //     delete e.createdAt;
    //     delete e.entryType;
    //     delete e.groupLink.connFullLink;
    //     delete e.shortDescr;
    //     delete e.welcomeMessage;
    // });
    // console.log(entries);
    const links = document.querySelectorAll('.group-images a.group-image');
    entries = shuffleEntries(entries, links.length);

    for (let i = 0; i < links.length; i++) {
        const link = links[i]
        const img = link.querySelector('img');
        const {displayName, imageFile, groupLink} = entries[i % entries.length];
        img.src = imageFile ? imgPath + imageFile : '/img/group.svg';
        img.addEventListener('error', () => img.src = '/img/group.svg');
        link.title = displayName;
        const groupLinkUri = groupLink.connShortLink ?? groupLink.connFullLink
        try {
          link.href = platformSimplexUri(groupLinkUri);
        } catch(e) {
          console.log(e);
          link.href = groupLinkUri;
        }
    }

    function shuffleEntries(entries, count) {
        let a = entries.filter(e => e.displayName != simplexUsersGroup)
        shuffle();
        let result = a;
        while (result.length < count) {
            shuffle();
            result = result.concat(a);
        }
        return result;

        function shuffle() {
            for (let i = a.length - 1; i > 0; i--) {
                const j = Math.floor(Math.random() * (i + 1));
                [a[i], a[j]] = [a[j], a[i]];
            }
        }
    }

    async function fetchJSON(url) {
        try {
            const response = await fetch(url)
            if (!response.ok) throw new Error(`HTTP status: ${response.status}`)
            return await response.json()
        } catch (e) {
            console.error(e)
        }
    }

    function welcome() {
        console.log('%c%s', 'font-family: monospace; white-space: pre;',
`Welcome to                  __   __
 ___ ___ __  __ ___ _    ___\\ \\ / / ___ _  _   _ _____
/ __|_ _|  \\/  | _ \\ |  | __ \\ V / / __| || | /_\\_   _|
\\__ \\| || |\\/| |  _/ |__| _| / . \\| (__| __ |/ _ \\| |
|___/___|_|  |_|_| |____|___/_/ \\_\\\\___|_||_/_/ \\_\\_|

SimpleX directory: https://simplex.chat/directory
Ask SimpleX team: https://smp6.simplex.im/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw
GitHub: https://github.com/simplex-chat/simplex-chat
Reddit: https://www.reddit.com/r/SimpleXChat
X/Twitter: https://x.com/SimpleXChat

Docs
----
Whitepaper: https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md
Bots API: https://github.com/simplex-chat/simplex-chat/tree/stable/bots
TypeScript library: https://github.com/simplex-chat/simplex-chat/tree/stable/packages/simplex-chat-client/typescript
Terminal CLI: https://github.com/simplex-chat/simplex-chat/blob/stable/docs/CLI.md
Hosting SMP servers: https://simplex.chat/docs/server.html

Downloads
---------
Apps: https://simplex.chat/downloads
Servers: https://github.com/simplex-chat/simplexmq/releases

Project
-------
About & Contact us: https://simplex.chat/about
Privacy policy: https://simplex.chat/privacy
Join team: https://simplex.chat/jobs
Donations: https://github.com/simplex-chat/simplex-chat#please-support-us-with-your-donations
`
        );
    }

    function fallbackEntries() {
        console.log('Error: using hardcoded listing as fallback');
        return [
            {
                displayName: "Bitcoin&LightningNetwork",
                groupLink: {
                    connShortLink: "https://smp4.simplex.im/g#-xXBhQRrvRB1ffhxcPpB44Im1_ci4BMIdCHwj8m8IHo"
                },
                imageFile: "images/F4hPy5IGO6G9QUH6-nM_-A.jpg"
            },
            {
                displayName: "Freedom.Tech",
                groupLink: {
                    connShortLink: "https://smp4.simplex.im/g#r5z3uzHp8_pL3ZPyuBCJWmvzQxMnc0Tj3QMLTEnyw6c"
                },
                imageFile: "images/HylzOLARvIhUR1wiq0fnJA.png"
            },
            {
                displayName: "Spirituality",
                groupLink: {
                    connShortLink: "https://smp5.simplex.im/g#OZ8ml_2dj5AutxnNIrHy0CPn1QdnSkQ0oh_84nAv5io"
                },
                imageFile: "images/eb2RINRsdEBI0a06ghzqVg.jpg"
            },
            {
                displayName: "SovereignStack",
                groupLink: {
                    connShortLink: "https://smp5.simplex.im/g#u2D3BdOb3nt9wMR_qoweAINLcEZaU60Xjwfpf74Dq2I"
                },
                imageFile: "images/YLLz-47eBnAoIH1PKYJ75g.jpg"
            },
            {
                displayName: "Monero",
                groupLink: {
                    connShortLink: "https://smp6.simplex.im/g#FlIy4-q4TzZDI9fK2aw3lQTUvnmaoiLCyeKCoP27kGU"
                },
                imageFile: "images/TwaN96DcV2OCMfUo6oJ4LQ.png"
            },
            {
                displayName: "Start9 - Sovereign Computing",
                groupLink: {
                    connShortLink: "https://smp4.simplex.im/g#JArWigpS6OB0gYE2U94pDSzPQyejOdmqe98ohBNoW2Q"
                },
                imageFile: "images/478ec86_izoJb95VXKWEhg.jpg"
            },
            {
                displayName: "Meshtastic",
                groupLink: {
                    connShortLink: "https://smp5.simplex.im/g#Ub1c3ByH5vkhXMMsRdG0fBhSik_qPuEZHcx8AQ2f2Tw"
                },
                imageFile: "images/PPTLdveOyb9Wsg3bm6Y_IQ.png"
            },
            {
                displayName: "GrapheneOS (unofficial)",
                groupLink: {
                    connShortLink: "https://smp5.simplex.im/g#6OTo6kP4ccV4lPOOHekZfOajdxGkxC1_DkAR39_cU4U"
                },
                imageFile: "images/zIotMF8Zoe85k956B48N9g.jpg"
            },
            {
                displayName: "BasicSwap",
                groupLink: {
                    connShortLink: "https://smp5.simplex.im/g#yXMQy5Si6sD5YAdCB4DlUM0kvlKYcUfSiIvOA-RXI_U"
                },
                imageFile: "images/AXalaTZ4HsgGtEkaGFbclg.png"
            },
            {
                displayName: "Linux",
                groupLink: {
                    connShortLink: "https://smp5.simplex.im/g#gqXY-Fxwral34c4bsHlYKZ5QuB6ptVSRMwhWgLKHz54"
                },
                imageFile: "images/gqyRO21CwqK3huZ2zbkOqQ.jpg"
            },
            {
                displayName: "ModernSurvival",
                groupLink: {
                    connShortLink: "https://smp5.simplex.im/g#e4_r2E20FSWl97XGsHyXoPOkW5B6SmjlWQ1ngVW6Umc"
                },
                imageFile: "images/5QzoN8PNFD2dkButszPu5g.png"
            },
            {
                displayName: "Qubes OS",
                groupLink: {
                    connShortLink: "https://smp5.simplex.im/g#COegA1s1ppZG4hRbcpuWwx_QWB4ScouQcIDWwXx64SY"
                },
                imageFile: "images/Wsdcf731ufbEOYpHc2rqrg.jpg"
            },
            {
                displayName: "RoboSats",
                groupLink: {
                    connShortLink: "https://smp4.simplex.im/g#PNRhbupXbsSr5SpkjqP8IjkI6ACPCr2WOxAqSAW4jr0"
                },
                imageFile: "images/uZU6Cn1przsjVJ-DBm1-eA.jpg"
            },
            {
                displayName: "Guardian Project (Unofficial)",
                groupLink: {
                    connShortLink: "https://smp5.simplex.im/g#7zpeTUzhVIwEbZpvo9SUGz1LE35jKfmF_AHYx5YLsxQ"
                },
                imageFile: "images/NvmOInofh4RSB2fHdN0zQA.jpg"
            },
            {
                displayName: "Private Messaging Apps 2",
                groupLink: {
                    connShortLink: "https://smp6.simplex.im/g#aPhAePNB7Nn-W4kUBNBpZELXttysG-yAM8ZiU2XoB10"
                },
                imageFile: "images/9aT7qRY_JbSJsW4qDaIHEg.jpg"
            },
            {
                displayName: "Cake Wallet - Official",
                groupLink: {
                    connShortLink: "https://smp5.simplex.im/g#D46rZgLF1eLDLkOyqYnaifRuZcYjlEMgkujKV2buH6k"
                },
                imageFile: "images/YhyznG68PNewCNjoD_ceOg.png"
            },
            {
                displayName: "CoMaps (EN)",
                groupLink: {
                    connShortLink: "https://smp4.simplex.im/g#mBdIDCJotrN7pimTmXoaAPybC7CmCaaAFxQcwultCvo"
                },
                imageFile: "images/BD6FXuHO-eKOnYCAzkRfmA.jpg"
            },
            {
                displayName: "SimpleX users group",
                groupLink: {
                    connShortLink: "https://smp4.simplex.im/g#hr4lvFeBmndWMKTwqiodPz3VBo_6UmdGWocXd1SupsM"
                },
                imageFile: "images/CX-1MPD3r3a7NYBvW7de6g.jpg"
            },
            {
                displayName: "NBTV Community",
                groupLink: {
                    connShortLink: "https://smp6.simplex.im/g#RX598AUwyQBG6bqa4TOnEnUg7xONdrA-_e0CmNGxEBI"
                },
                imageFile: "images/n3VLCEBWhqU9rVtlEAKUhQ.jpg"
            },
            {
                displayName: "Lossless Audio Community",
                groupLink: {
                    connShortLink: "https://smp6.simplex.im/g#D4P5ENAzT-JoVlFNvRC7geHkkMUuQ9IuhQQC15OFE-o"
                },
                imageFile: "images/B3xza7zUzYi3dHiRsg6Afg.jpg"
            },
            {
                displayName: "RetoSwap – Official SimpleXChat Group",
                groupLink: {
                    connShortLink: "https://smp5.simplex.im/g#-_h6fBWisca6RKhteZtVuXol1a49vFH1Jo-n74fnRK0"
                },
                imageFile: "images/ESomaJp7MlFlThqcoj3Ycg.png"
            },
            {
                displayName: "UW Support 💬",
                groupLink: {
                    connShortLink: "https://smp5.simplex.im/g#6KPZcRjE6KDNQ1VcS1a2wd9LRuJy1zgdvldaE5bhg5c"
                },
                imageFile: "images/t1RmI4AhKgelVoWeSBTqUA.jpg"
            }
        ]
    }
}
})();
