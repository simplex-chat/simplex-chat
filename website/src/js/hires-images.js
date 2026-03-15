// Progressive image loading: LQIP -> full resolution
// CSS loads low-quality placeholders. This script loads full-res and swaps them in.
(function () {
  var isMobile = window.innerWidth <= 959
  var isDark = document.documentElement.classList.contains('dark')

  // Section selectors -> full-res image paths [light, dark]
  // Desktop and mobile have different images for sections (not cover)
  var sections = [
    {
      sel: 'section.cover',
      light: '/img/design_3/cover-light.webp',
      dark: '/img/design_3/cover.webp'
    },
    {
      sel: '.page-2',
      light: '/img/design_3/section-2-' + (isMobile ? 'mobile' : 'desktop') + '-light.webp',
      dark: '/img/design_3/section-2-' + (isMobile ? 'mobile' : 'desktop') + '.webp'
    },
    {
      sel: '.page-3',
      light: '/img/design_3/section-3-' + (isMobile ? 'mobile' : 'desktop') + '-light.webp',
      dark: '/img/design_3/section-3-' + (isMobile ? 'mobile' : 'desktop') + '.webp'
    },
    {
      sel: '.page-4',
      light: '/img/design_3/section-4-' + (isMobile ? 'mobile' : 'desktop') + '-light.webp',
      dark: '/img/design_3/section-4-' + (isMobile ? 'mobile' : 'desktop') + '.webp'
    },
    {
      sel: '.page-5',
      light: '/img/design_3/section-5-' + (isMobile ? 'mobile' : 'desktop') + '-light.webp',
      dark: '/img/design_3/section-5-' + (isMobile ? 'mobile' : 'desktop') + '.webp'
    }
  ]

  // Hash fragment -> section index (cover is always 0)
  var hashToIndex = {
    '': 0,
    'messaging': 1,
    'nextweb': 2,
    'token': 3,
    'roadmap': 4,
    'directory': 4 // page-6 has no background, load page-5
  }

  // Track which images have been loaded
  var loaded = {}

  function loadImage(index) {
    var sec = sections[index]
    if (!sec) return
    var url = isDark ? sec.dark : sec.light
    if (loaded[url]) return
    loaded[url] = true

    var img = new Image()
    img.onload = function () {
      var el = document.querySelector(sec.sel)
      if (el) el.style.backgroundImage = 'url("' + url + '")'
    }
    img.src = url
  }

  function loadAll() {
    for (var i = 0; i < sections.length; i++) loadImage(i)
  }

  // Determine which section to load first based on hash fragment
  var hash = window.location.hash.replace('#', '')
  var firstIndex = hashToIndex[hash] !== undefined ? hashToIndex[hash] : 0

  // Load the current section immediately, then the rest
  loadImage(firstIndex)
  // Also load cover if we jumped to a fragment
  if (firstIndex !== 0) loadImage(0)

  // Load remaining sections after DOM is ready
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', loadAll)
  } else {
    loadAll()
  }

  // Handle dark/light mode switch: reload images for new mode
  var observer = new MutationObserver(function (mutations) {
    for (var i = 0; i < mutations.length; i++) {
      if (mutations[i].attributeName === 'class') {
        var nowDark = document.documentElement.classList.contains('dark')
        if (nowDark !== isDark) {
          isDark = nowDark
          // Reload all sections with new mode
          for (var j = 0; j < sections.length; j++) {
            var sec = sections[j]
            var url = isDark ? sec.dark : sec.light
            if (loaded[url]) {
              // Already loaded, just swap
              var el = document.querySelector(sec.sel)
              if (el) el.style.backgroundImage = 'url("' + url + '")'
            } else {
              loadImage(j)
            }
          }
        }
        break
      }
    }
  })
  observer.observe(document.documentElement, { attributes: true })
})()
