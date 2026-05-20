document.addEventListener("DOMContentLoaded", function () {
  var ITEMS_PER_PAGE = 20
  var allItems = Array.from(document.querySelectorAll("#links-list .link-item"))
  var filteredItems = allItems.slice()
  var currentPage = 1

  var langSelect = document.getElementById("filter-language")
  var pills = Array.from(document.querySelectorAll("#pill-filters .filter-chip"))
  var pagination = document.getElementById("links-pagination")
  var countEl = document.getElementById("links-count")

  var activePill = ""

  var MEDIA_PILLS = { "Video": "video", "Audio": "audio" }

  function matchesPillFn(pill) {
    if (!pill) return function () { return true }
    var media = MEDIA_PILLS[pill]
    if (media) return function (el) { return el.getAttribute("data-media") === media }
    return function (el) { return el.getAttribute("data-pill") === pill }
  }

  function matchesPill(el) {
    return matchesPillFn(activePill)(el)
  }

  function matchesLang(el) {
    var lang = langSelect.value
    return !lang || el.getAttribute("data-lang") === lang
  }

  function getFiltered() {
    return allItems.filter(function (el) {
      return matchesPill(el) && matchesLang(el)
    })
  }

  // Cross-filter: update language options based on active pill
  function updateLanguageOptions() {
    var available = {}
    allItems.forEach(function (el) {
      if (matchesPill(el)) {
        var lang = el.getAttribute("data-lang")
        if (lang) available[lang] = true
      }
    })
    var options = langSelect.options
    for (var i = 1; i < options.length; i++) {
      var hasItems = available[options[i].value]
      options[i].disabled = !hasItems
      options[i].style.display = hasItems ? "" : "none"
    }
    if (langSelect.value && !available[langSelect.value]) {
      langSelect.value = ""
    }
  }

  // Cross-filter: update pill availability based on active language
  function updatePillAvailability() {
    pills.forEach(function (pill) {
      var pillValue = pill.getAttribute("data-pill")
      if (!pillValue) return
      var matcher = matchesPillFn(pillValue)
      var hasItems = allItems.some(function (el) {
        return matcher(el) && matchesLang(el)
      })
      pill.disabled = !hasItems
      pill.style.opacity = hasItems ? "" : "0.3"
      pill.style.pointerEvents = hasItems ? "" : "none"
    })
  }

  function applyFilters() {
    filteredItems = getFiltered()
    currentPage = 1
    updateLanguageOptions()
    updatePillAvailability()
    render()
    updateHash()
  }

  function render() {
    var totalPages = Math.max(1, Math.ceil(filteredItems.length / ITEMS_PER_PAGE))
    if (currentPage > totalPages) currentPage = totalPages
    var start = (currentPage - 1) * ITEMS_PER_PAGE
    var end = start + ITEMS_PER_PAGE

    for (var i = 0; i < allItems.length; i++) allItems[i].style.display = "none"
    for (var i = 0; i < filteredItems.length; i++) {
      filteredItems[i].style.display = (i >= start && i < end) ? "" : "none"
    }

    countEl.textContent = filteredItems.length + " links" +
      (filteredItems.length !== allItems.length ? " (filtered from " + allItems.length + ")" : "")

    renderPagination(totalPages)
  }

  function renderPagination(totalPages) {
    pagination.innerHTML = ""
    if (totalPages <= 1) return

    var prevBtn = document.createElement("button")
    prevBtn.textContent = "Prev"
    prevBtn.className = "text-btn"
    prevBtn.disabled = currentPage <= 1
    prevBtn.addEventListener("click", function () { goToPage(currentPage - 1) })
    pagination.appendChild(prevBtn)

    var pages = getPaginationRange(currentPage, totalPages)
    for (var i = 0; i < pages.length; i++) {
      if (pages[i] === "...") {
        var span = document.createElement("span")
        span.textContent = "…"
        span.style.padding = "8px 4px"
        pagination.appendChild(span)
      } else {
        var btn = document.createElement("button")
        btn.textContent = String(pages[i])
        if (pages[i] === currentPage) btn.className = "active"
        if (Math.abs(pages[i] - currentPage) === 1) btn.classList.add("neighbor")
        ;(function (p) {
          btn.addEventListener("click", function () { goToPage(p) })
        })(pages[i])
        pagination.appendChild(btn)
      }
    }

    var nextBtn = document.createElement("button")
    nextBtn.textContent = "Next"
    nextBtn.className = "text-btn"
    nextBtn.disabled = currentPage >= totalPages
    nextBtn.addEventListener("click", function () { goToPage(currentPage + 1) })
    pagination.appendChild(nextBtn)
  }

  function goToPage(page) {
    currentPage = page
    render()
    updateHash()
    window.scrollTo({ top: document.getElementById("links-page").offsetTop - 80, behavior: "smooth" })
  }

  function getPaginationRange(current, total) {
    if (total <= 7) {
      var r = []
      for (var i = 1; i <= total; i++) r.push(i)
      return r
    }
    var pages = [1]
    if (current > 3) pages.push("...")
    var start = Math.max(2, current - 1)
    var end = Math.min(total - 1, current + 1)
    for (var i = start; i <= end; i++) pages.push(i)
    if (current < total - 2) pages.push("...")
    pages.push(total)
    return pages
  }

  // Hash management
  function getHashParams() {
    var hash = window.location.hash.slice(1)
    if (!hash) return {}
    var params = {}
    hash.split("&").forEach(function (part) {
      var kv = part.split("=")
      if (kv.length === 2) params[kv[0]] = decodeURIComponent(kv[1])
    })
    return params
  }

  function setHash(params) {
    var parts = []
    if (params.link) parts.push("link=" + encodeURIComponent(params.link))
    else if (params.page && params.page > 1) parts.push("page=" + params.page)
    if (params.lang) parts.push("lang=" + encodeURIComponent(params.lang))
    if (params.pill) parts.push("pill=" + encodeURIComponent(params.pill))
    var hash = parts.join("&")
    history.replaceState(null, "", hash ? "#" + hash : window.location.pathname)
  }

  function updateHash() {
    setHash({
      page: currentPage,
      lang: langSelect.value,
      pill: activePill
    })
  }

  function readHashAndApply() {
    var params = getHashParams()

    // Restore filters from hash
    if (params.lang) langSelect.value = params.lang
    if (params.pill) {
      activePill = params.pill
    }
    pills.forEach(function (p) {
      p.classList.toggle("active", p.getAttribute("data-pill") === activePill)
    })

    filteredItems = getFiltered()
    updateLanguageOptions()
    updatePillAvailability()

    if (params.link) {
      var target = document.getElementById(params.link)
      if (target) {
        var idx = filteredItems.indexOf(target)
        if (idx === -1) {
          // Item filtered out - clear filters to show it
          langSelect.value = ""
          activePill = ""
          pills.forEach(function (p) { p.classList.toggle("active", !p.getAttribute("data-pill")) })
          filteredItems = getFiltered()
          updateLanguageOptions()
          updatePillAvailability()
          idx = filteredItems.indexOf(target)
        }
        if (idx >= 0) {
          currentPage = Math.floor(idx / ITEMS_PER_PAGE) + 1
          render()
          setTimeout(function () {
            target.scrollIntoView({ behavior: "smooth", block: "start" })
            target.classList.add("highlighted")
            setTimeout(function () { target.classList.remove("highlighted") }, 3000)
          }, 100)
          return
        }
      }
    }

    if (params.page) currentPage = parseInt(params.page) || 1
    render()
  }

  // Share anchor click
  document.getElementById("links-list").addEventListener("click", function (e) {
    var anchor = e.target.closest(".share-anchor")
    if (!anchor) return
    e.preventDefault()
    var id = anchor.getAttribute("href").split("=")[1]
    setHash({ link: id, lang: langSelect.value, pill: activePill })
    if (navigator.clipboard) navigator.clipboard.writeText(window.location.href)
  })

  // Language filter
  langSelect.addEventListener("change", applyFilters)

  // Pill clicks
  pills.forEach(function (pill) {
    pill.addEventListener("click", function () {
      var value = pill.getAttribute("data-pill")
      activePill = (activePill === value) ? "" : value
      if (!activePill) {
        pills.forEach(function (p) { p.classList.remove("active") })
        pills[0].classList.add("active")
      } else {
        pills.forEach(function (p) { p.classList.remove("active") })
        pill.classList.add("active")
      }
      applyFilters()
    })
  })

  window.addEventListener("hashchange", readHashAndApply)
  readHashAndApply()
})
