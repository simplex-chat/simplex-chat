import {readFileSync, readdirSync} from "fs"
import {join} from "path"
import * as ts from "typescript"

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

  it("places each spec comment directly above a declaration and points it to an anchor", () => {
    const pointers = filesIn("src", ".ts").flatMap(file => {
      const text = read(file)
      const declared = declarationLines(ts.createSourceFile(file, text, ts.ScriptTarget.Latest, true))
      return text.split("\n").flatMap((line, i) => (/\/\/\s*spec\b/i.test(line) ? [{at: `${file}:${i + 1}`, target: /^[ \t]*\/\/ spec: (spec\/[\w-]+\.md#[a-z0-9-]+)$/.exec(line)?.[1], declaration: declared.has(i + 1)}] : []))
    })
    expect(pointers.length).toBeGreaterThan(0)
    expect(pointers.filter(({target}) => target === undefined).map(({at}) => at)).toEqual([])
    expect(pointers.filter(({declaration}) => !declaration).map(({at}) => at)).toEqual([])
    const broken = pointers.filter(({target}) => {
      const [doc, id] = (target ?? "").split("#")
      return !docs.includes(doc) || !anchors(doc).includes(id)
    })
    expect(broken.map(({at, target}) => `${at} ${target}`)).toEqual([])
  })
})

function declarationLines(source: ts.SourceFile): Set<number> {
  const lines = new Set<number>()
  const visit = (node: ts.Node): void => {
    const declaration = ts.isFunctionDeclaration(node) || ts.isClassDeclaration(node) || ts.isInterfaceDeclaration(node) || ts.isTypeAliasDeclaration(node) || ts.isEnumDeclaration(node) || ts.isMethodDeclaration(node) || ts.isGetAccessorDeclaration(node) || ts.isSetAccessorDeclaration(node) || ts.isConstructorDeclaration(node) || (ts.isVariableStatement(node) && ts.isSourceFile(node.parent))
    if (declaration) lines.add(source.getLineAndCharacterOfPosition(node.getStart(source)).line)
    ts.forEachChild(node, visit)
  }
  visit(source)
  return lines
}
