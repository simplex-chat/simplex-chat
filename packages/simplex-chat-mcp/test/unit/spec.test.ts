import {readFileSync, readdirSync} from "fs"
import {join} from "path"

const root = join(__dirname, "../..")
const read = (file: string): string => readFileSync(join(root, file), "utf8")
const filesIn = (folder: string, extension: string): string[] => readdirSync(join(root, folder)).filter(file => file.endsWith(extension)).map(file => (folder === "." ? file : `${folder}/${file}`))
const docs = [...filesIn(".", ".md"), ...filesIn("spec", ".md")]
const anchors = (doc: string): string[] => [...read(doc).matchAll(/<a id="([^"]+)"><\/a>/g)].map(([, id]) => id)

describe("spec", () => {
  it("links files without line numbers", () => {
    const lineRefs = docs.flatMap(doc => [...read(doc).matchAll(/\]\([^)\s]*#L\d+[^)]*\)|[\w.-]+\.(?:ts|js|cjs|mjs|json):\d+/g)].map(([ref]) => `${doc}: ${ref}`))
    expect(lineRefs).toEqual([])
  })

  it("defines each anchor once", () => {
    const repeated = docs.flatMap(doc => anchors(doc).filter((id, i, ids) => ids.indexOf(id) !== i).map(id => `${doc}#${id}`))
    expect(repeated).toEqual([])
  })

  it("places each spec comment above code and points it to an anchor", () => {
    const pointers = filesIn("src", ".ts").flatMap(file => {
      const lines = read(file).split("\n")
      return lines.flatMap((line, i) => {
        const target = /^[ \t]*\/\/ spec: (\S+)$/.exec(line)?.[1]
        return target === undefined ? [] : [{at: `${file}:${i + 1}`, target, next: (lines[i + 1] ?? "").trim()}]
      })
    })
    expect(pointers.length).toBeGreaterThan(0)
    const broken = pointers.filter(({target}) => {
      const [doc, id] = target.split("#")
      return !docs.includes(doc) || !anchors(doc).includes(id)
    })
    expect(broken.map(({at, target}) => `${at} ${target}`)).toEqual([])
    expect(pointers.filter(({next}) => next === "" || next.startsWith("//")).map(({at}) => at)).toEqual([])
  })
})
