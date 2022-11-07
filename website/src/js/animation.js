window.addEventListener("load", function () {
  const imgPath = "/img/hero"
  const fastImages = mkFastImages(0, 20);
  const slowImages = mkSlowImages([
    ["0005", 3],
    ["0008", 1],
    ["0009", 2],
    ["0011", 3],
    ["0014", 3],
    ["0017", 1],
    ["0018", 2],
    ["0020", 2],
    ["0022", 2],
    ["0024", 3],
    ["0027", 1],
    ["0028", 2],
    ["0030", 2],
    ["0032", 3],
    ["0035", 1],
    ["0036", 2],
    ["0038", 1],
    ["0039", 1]
  ])
  
  let complete = false;
  run();
  window.onload = run;

  function mkFastImages(from, to) {
    const seq = [];
    for (let n = from; n <= to; n++) {
      let img = "" + n;
      img = "0".repeat(4 - img.length) + img;
      seq.push([`${imgPath}/light/10fps/${img}.webp`, `${imgPath}/dark/10fps/${img}.webp`, 1]);
    }
    return seq;
  }

  function mkSlowImages(images) {
    return images.map(([img, n]) => [`${imgPath}/light/2fps/${img}.webp`, `${imgPath}/dark/2fps/${img}.webp`, n])
  }

  // function setupScroll (imgBackground) {
  //   const totalHeight = this.document.body.scrollHeight - this.window.innerHeight;
  //   this.window.onscroll = () => {
  //     let progressHeight = (window.pageYOffset / totalHeight) * 100;
  //     if (progressHeight > 10) {
  //       imgBackground.style.position = "absolute";
  //       imgBackground.style.top = "100%";
  //     } else {
  //       imgBackground.style.position = "fixed";
  //       imgBackground.style.top = "auto";
  //     }
  //   };
  // }

  async function run() {
    const lightImageEl = document.getElementById("hero-phone-light");
    const darkImageEl = document.getElementById("hero-phone-dark");
    const preloadEl = document.getElementById("hero-phone-preload");
    const imgBackground = document.getElementsByClassName("hero-phone-background")[0]
    const videoEl = this.document.getElementById("hero-phone-video")
    if (complete || !lightImageEl || !darkImageEl || !preloadEl || !imgBackground || !videoEl) return;
    complete = true;
    // setupScroll(imgBackground)
    await preload(fastImages.concat(slowImages));
    await animate(fastImages, 100);
    await delay(500)
    await animate(slowImages, 500);
    // todo - this should happen on scroll, not when animation ends
    lightImageEl.style.animationName = "big";
    darkImageEl.style.animationName = "big";
    videoEl.style.animationName = "bigvideo";
    if (window.innerWidth > 1279) {
      videoEl.style.display = "block";
    }

    async function preload(images) {
      let imgEls = [];
      let resolved = false;
      return new Promise((resolve) => {
        setTimeout(() => {
          if (!resolved) {
            resolved = true;
            resolve();
          }
        }, 2000);
        for (const [lightImg, darkImg] of images) {
          preloadImg(lightImg)
          preloadImg(darkImg)
        }

        function preloadImg(img) {
          const el = document.createElement("img");
          el.src = img;
          imgEls.push(el);
          preloadEl.appendChild(el);
          const loaded = () => {
            imgEls = imgEls.filter((e) => e !== el);
            if (imgEls.length === 0 && !resolved) {
              resolved = true;
              resolve();
            }
          };
          el.addEventListener("load", loaded);
          el.addEventListener("error", loaded);
        }
      });
    }

    async function animate(images, ms) {
      let i = 0;
      for (const [lightImg, darkImg, n] of images) {
        lightImageEl.src = lightImg;
        darkImageEl.src = darkImg;
        i++;
        if (ms == 500 && i > 16) {
          // TODO this is hack, also this should happen on scroll, not during the animation
          lightImageEl.style.animationName = "small";
          darkImageEl.style.animationName = "small";
        }
        await delay(ms * n);
      }
    }

    async function delay(ms) {
      return new Promise((resolve) => setTimeout(resolve, ms));
    }
  }
});