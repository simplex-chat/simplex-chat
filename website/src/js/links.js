document.addEventListener("DOMContentLoaded", function () {
  var ITEMS_PER_PAGE = 10
  var allItems = Array.from(document.querySelectorAll("#links-list .link-item"))
  var filteredItems = allItems.slice()
  var currentPage = 1

  var langSelect = document.getElementById("filter-language")
  var pills = Array.from(document.querySelectorAll("#links-filters .filter-chip"))
  var pagination = document.getElementById("links-pagination")

  var activeFilter = ""     // "" | "media" | "cat"
  var activeValue = ""      // "video" | "audio" | "review" | etc.

  function matchesActivePill(el) {
    if (!activeFilter) return true
    if (activeFilter === "media") return el.getAttribute("data-media") === activeValue
    if (activeFilter === "cat") return el.getAttribute("data-category") === activeValue
    return true
  }

  function matchesLang(el) {
    var lang = langSelect.value
    return !lang || el.getAttribute("data-lang") === lang
  }

  function getFiltered() {
    return allItems.filter(function (el) {
      return matchesActivePill(el) && matchesLang(el)
    })
  }

  function updateLanguageOptions() {
    var available = {}
    allItems.forEach(function (el) {
      if (matchesActivePill(el)) {
        var lang = el.getAttribute("data-lang")
        if (lang) available[lang] = true
      }
    })
    var options = langSelect.options
    for (var i = 1; i < options.length; i++) {
      var has = !!available[options[i].value]
      options[i].disabled = !has
      options[i].style.display = has ? "" : "none"
    }
    if (langSelect.value && !available[langSelect.value]) langSelect.value = ""
  }

  function updatePillAvailability() {
    pills.forEach(function (pill) {
      var filter = pill.getAttribute("data-filter")
      if (filter === null || filter === "") return
      var value = pill.getAttribute("data-value")
      var has = allItems.some(function (el) {
        if (!matchesLang(el)) return false
        if (filter === "media") return el.getAttribute("data-media") === value
        if (filter === "cat") return el.getAttribute("data-category") === value
        return false
      })
      pill.style.opacity = has ? "" : "0.3"
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
        ;(function (p) { btn.addEventListener("click", function () { goToPage(p) }) })(pages[i])
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
    var s = Math.max(2, current - 1), e = Math.min(total - 1, current + 1)
    for (var i = s; i <= e; i++) pages.push(i)
    if (current < total - 2) pages.push("...")
    pages.push(total)
    return pages
  }

  // Hash
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
    if (params.media) parts.push("media=" + encodeURIComponent(params.media))
    if (params.cat) parts.push("cat=" + encodeURIComponent(params.cat))
    var hash = parts.join("&")
    history.replaceState(null, "", hash ? "#" + hash : window.location.pathname)
  }

  function updateHash() {
    var params = { page: currentPage, lang: langSelect.value }
    if (activeFilter === "media") params.media = activeValue
    if (activeFilter === "cat") params.cat = activeValue
    setHash(params)
  }

  function setActivePill(filter, value) {
    activeFilter = filter
    activeValue = value
    pills.forEach(function (p) {
      var pf = p.getAttribute("data-filter")
      var pv = p.getAttribute("data-value")
      p.classList.toggle("active", pf === filter && (pv || "") === (value || ""))
    })
  }

  function readHashAndApply() {
    var params = getHashParams()

    if (params.lang) langSelect.value = params.lang
    if (params.media) setActivePill("media", params.media)
    else if (params.cat) setActivePill("cat", params.cat)
    else setActivePill("", "")

    filteredItems = getFiltered()
    updateLanguageOptions()
    updatePillAvailability()

    if (params.link) {
      var target = document.getElementById(params.link)
      if (target) {
        var idx = filteredItems.indexOf(target)
        if (idx === -1) {
          setActivePill("", "")
          langSelect.value = ""
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

  // Share anchor
  document.getElementById("links-list").addEventListener("click", function (e) {
    var anchor = e.target.closest(".share-anchor")
    if (!anchor) return
    e.preventDefault()
    var id = anchor.getAttribute("href").split("=")[1]
    var params = { link: id, lang: langSelect.value }
    if (activeFilter === "media") params.media = activeValue
    if (activeFilter === "cat") params.cat = activeValue
    setHash(params)
    if (navigator.clipboard) navigator.clipboard.writeText(window.location.href)
  })

  langSelect.addEventListener("change", applyFilters)

  pills.forEach(function (pill) {
    pill.addEventListener("click", function () {
      var filter = pill.getAttribute("data-filter")
      var value = pill.getAttribute("data-value") || ""
      if (activeFilter === filter && activeValue === value) {
        setActivePill("", "")
      } else {
        // If pill is greyed out (no items with current language), reset language first
        if (pill.style.opacity === "0.3") {
          langSelect.value = ""
        }
        setActivePill(filter, value)
      }
      applyFilters()
    })
  })

  window.addEventListener("hashchange", readHashAndApply)
  readHashAndApply()
})
