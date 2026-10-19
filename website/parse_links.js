const fs = require("fs")
const slugify = require("slugify")

function parseLinks(linksFilePath) {
  const content = fs.readFileSync(linksFilePath, "utf8")
  const lines = content.split("\n")
  const entries = []

  // First pass: split into raw entry blocks at ## boundaries
  const blocks = []
  let current = null
  for (const line of lines) {
    if (line.startsWith("## ")) {
      if (current) blocks.push(current)
      current = { title: line.slice(3).trim(), lines: [] }
    } else if (current) {
      current.lines.push(line)
    }
  }
  if (current) blocks.push(current)

  // Second pass: parse each block
  for (const block of blocks) {
    // Collect non-empty lines in order
    const parts = block.lines.map(l => l.trim()).filter(l => l)

    let originalTitle = ""
    let publisher = ""
    let category = ""
    let featured = false
    let preview = ""
    let image = ""
    let language = ""
    let date = ""
    let estimated = false
    let url = ""

    let idx = 0

    // Optional: original title in parentheses
    if (idx < parts.length && parts[idx].startsWith("(") && parts[idx].endsWith(")")) {
      originalTitle = parts[idx].slice(1, -1)
      idx++
    }

    // Publisher: first line that's not a metadata prefix and not "Featured"
    if (idx < parts.length && !isMetadata(parts[idx]) && parts[idx] !== "Featured") {
      publisher = parts[idx]
      idx++
    }

    // Category: next non-metadata, non-Featured line
    if (idx < parts.length && !isMetadata(parts[idx]) && parts[idx] !== "Featured") {
      category = parts[idx]
      idx++
    }

    // Optional: Featured
    if (idx < parts.length && parts[idx] === "Featured") {
      featured = true
      idx++
    }

    // Preview: collect lines until we hit a metadata line
    const previewLines = []
    while (idx < parts.length && !isMetadata(parts[idx])) {
      previewLines.push(parts[idx])
      idx++
    }
    preview = previewLines.join(" ")

    // Metadata lines: Image, Language, Date, URL
    while (idx < parts.length) {
      const line = parts[idx]
      if (line.startsWith("Image: ")) {
        image = line.slice(7)
      } else if (line.startsWith("Language: ")) {
        language = line.slice(10)
      } else if (line.startsWith("Date: ")) {
        const rawDate = line.slice(6)
        if (rawDate.includes("(estimated)")) {
          estimated = true
          date = rawDate.replace("(estimated)", "").trim()
        } else {
          date = rawDate
        }
      } else if (line.startsWith("http")) {
        url = line
      }
      idx++
    }

    if (!block.title || !url) continue

    let contentCategory = category
    let explicitMedia = ""
    if (category.includes(", ")) {
      const parts = category.split(", ")
      contentCategory = parts[0].trim()
      explicitMedia = parts[1].trim().toLowerCase()
    }

    entries.push({
      id: slugify(block.title, { lower: true, strict: true }).slice(0, 80),
      title: block.title,
      originalTitle,
      publisher,
      category: contentCategory,
      featured,
      preview,
      image,
      language,
      date,
      dateSort: normalizeDateForSort(date),
      estimated,
      url,
      mediaType: explicitMedia || deriveMediaType(category),
    })
  }

  // Deduplicate IDs by appending language suffix where needed
  const idCounts = {}
  for (const entry of entries) {
    idCounts[entry.id] = (idCounts[entry.id] || 0) + 1
  }
  for (const entry of entries) {
    if (idCounts[entry.id] > 1 && entry.language) {
      entry.id = entry.id.slice(0, 70) + "-" + slugify(entry.language, { lower: true, strict: true })
    }
  }
  // Final pass: if still duplicates, append index
  const seen = {}
  for (const entry of entries) {
    if (seen[entry.id]) {
      entry.id = entry.id + "-" + (seen[entry.id]++)
    } else {
      seen[entry.id] = 1
    }
  }

  entries.sort((a, b) => b.dateSort.localeCompare(a.dateSort))
  return entries
}

function isMetadata(line) {
  return line.startsWith("Image: ") ||
    line.startsWith("Language: ") ||
    line.startsWith("Date: ") ||
    line.startsWith("http")
}

function deriveMediaType(category) {
  const lower = category.toLowerCase()
  if (lower.includes("video") || lower.includes("livestream") || lower.includes("conference talk")) return "video"
  if (lower.includes("podcast") || lower.includes("audio")) return "audio"
  return "text"
}


function normalizeDateForSort(dateStr) {
  if (!dateStr) return "1970-01-01"

  // Full date: "Apr 29, 2026" or "Dec 2, 2022"
  const fullDate = new Date(dateStr)
  if (!isNaN(fullDate.getTime())) {
    return fullDate.toISOString().slice(0, 10)
  }

  // Month + year: "May 2026"
  const monthYear = new Date(dateStr + " 1")
  if (!isNaN(monthYear.getTime())) {
    return monthYear.toISOString().slice(0, 10)
  }

  // Year only: "2024"
  const yearMatch = dateStr.match(/(\d{4})/)
  if (yearMatch) {
    return yearMatch[1] + "-01-01"
  }

  return "1970-01-01"
}

module.exports = parseLinks
