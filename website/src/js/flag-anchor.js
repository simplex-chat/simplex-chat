document.addEventListener("DOMContentLoaded", () => {
    const flagAnchors = document.getElementsByClassName("flag-anchor")
    Array.from(flagAnchors).forEach(flagAnchor => {
        flagAnchor.addEventListener("click", (e) => {
            e.preventDefault()
            document.location = flagAnchor.href + location.hash
        })
    })
})