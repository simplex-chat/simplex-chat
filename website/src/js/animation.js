(function () {

  const fastImages = mkSequence("/img/new/fastImages/Lightphone1_", 0, 30, ".png")
  const slowImages = mkSequence("/img/new/slowImages1/Lightphone1_", 8, 39, ".png")
  // const slowImages = mkSequence("/img/new/slowImages2/Lightphone1_", 8, 39, ".png")


  let complete = false
  run()
  window.onload = run

  function mkSequence(prefix, from, to, ext) {
    const seq = []
    for (let n = from; n <= to; n++) {
      let suffix = '' + n
      suffix = '0'.repeat(4 - suffix.length) + suffix
      seq.push(prefix + suffix + ext)
    }

    console.log(seq)
    return seq
  }

  async function run() {
    const imageEl = document.getElementById("hero-phone");
    const preloadEl = document.getElementById("hero-phone-preload");
    if (complete || !imageEl || !preloadEl) return
    complete = true
    await preload(fastImages.concat(slowImages))
    await animate(fastImages, 100)
    await animate(slowImages, 200)

    async function preload(images) {
      let imgEls = []
      let resolved = false
      return new Promise((resolve) => {
        setTimeout(() => {
          if (!resolved) {
            resolved = true
            resolve()
          }
        }, 2000)
        for (const img of images) {
          const el = document.createElement("img");
          el.src = img
          imgEls.push(el)
          preloadEl.appendChild(el)
          const loaded = () => {
            imgEls = imgEls.filter((e) => e !== el)
            if (imgEls.length === 0 && !resolved) {
              resolved = true
              resolve()
            }
          }
          el.addEventListener('load', loaded)
          el.addEventListener('error', loaded)
        }
      })
    }

    async function animate(images, ms) {
      let i = 8;
      for (const img of images) {
        imageEl.src = img
        if (ms == 200 && i > 33) {
          this.document.getElementById("hero-phone").style.animationName =
            "small";
        }

        console.log(i)
        if (ms == 200 && i > 38) {
          this.document.getElementById("hero-phone").style.animationName =
            "big";
          this.document.getElementById(
            "hero-phone-video"
          ).style.animationName = "bigvideo";
        }

        if (ms == 200 && i == 39){  
          // this.document.getElementById("hero-phone").classList.remove("block")
          // this.document.getElementById("hero-phone").classList.add("hidden")

          this.document.getElementById("hero-phone-video").classList.remove("hidden");
          this.document.getElementById("hero-phone-video").classList.add("block");
        }

        i++;

        await delay(ms)
      }
    }

    async function delay(ms) {
      return new Promise((resolve) => setTimeout(resolve, ms))
    }
  }
})();
