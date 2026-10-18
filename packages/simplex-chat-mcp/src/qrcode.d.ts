declare module "qrcode" {
  export function create(text: string, options: {errorCorrectionLevel?: "L" | "M" | "Q" | "H"}): {modules: {size: number; get(row: number, col: number): number}}
}
