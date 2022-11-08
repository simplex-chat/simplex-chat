window.addEventListener("load", function () {
  // const slowImages = mkSequence("./slowImages2/Lightphone1_", 8, 39, ".png")
  console.log(this.window.innerWidth);
  // if (window.innerWidth <= 770) {
  //   this.document.getElementsByClassName(
  //     "flex items-center gap-10"
  //   )[0].style.display = "none";
  //   this.document.getElementsByClassName("hero-phone-background")[0].className =
  //     "tablet-hero-phone-background";
  //   this.document.getElementById("hero-phone").src =
  //     "../slowImages1/Lightphone1_0015.png";
  //   this.document.getElementById("hero-phone").className = "tablet-hero-phone";
  // } else
  if (window.innerWidth > 770) {
    const fastImages = mkSequence("/img/new/fastImages/Lightphone1_", 0, 30, ".png");
    const slowImages = mkSequence("/img/new/slowImages1/Lightphone1_", 8, 47, ".png");

    let complete = false;
    run();
    window.onload = run;

    function mkSequence(prefix, from, to, ext) {
      const seq = [];
      for (let n = from; n <= to; n++) {
        let suffix = "" + n;
        suffix = "0".repeat(4 - suffix.length) + suffix;
        seq.push(prefix + suffix + ext);
      }
      return seq;
    }

    let totalHeight = this.document.body.scrollHeight - this.window.innerHeight;
    this.window.onscroll = function () {
      let progressHeight = (window.pageYOffset / totalHeight) * 100;
      if (progressHeight > 11) {
        document.getElementsByClassName(
          "hero-phone-background"
        )[0].style.position = "absolute";
        document.getElementsByClassName("hero-phone-background")[0].style.top =
          "100%";
      } else {
        document.getElementsByClassName(
          "hero-phone-background"
        )[0].style.position = "fixed";
        document.getElementsByClassName("hero-phone-background")[0].style.top =
          "auto";
      }
    };

    async function run() {
      const imageEl = document.getElementById("hero-phone");
      const preloadEl = document.getElementById("hero-phone-preload");
      if (complete || !imageEl || !preloadEl) return;
      complete = true;
      await preload(fastImages.concat(slowImages));
      await animate(fastImages, 100);
      await animate(slowImages, 500);

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
          for (const img of images) {
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
        for (const img of images) {
          imageEl.src = img;
          i++;
          if (ms == 500 && i > 30) {
            this.document.getElementById("hero-phone").style.animationName =
              "small";
          }
          if (ms == 500 && i > 38) {
            this.document.getElementById("hero-phone").style.animationName =
              "big";
            this.document.getElementById(
              "hero-phone-video"
            ).style.animationName = "bigvideo";
            this.document.getElementById("hero-phone-video").style.display =
              "block";
          }
          await delay(ms);
        }
      }

      async function delay(ms) {
        return new Promise((resolve) => setTimeout(resolve, ms));
      }
    }
  }
});