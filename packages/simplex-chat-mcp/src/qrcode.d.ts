declare module "qrcode" {
  export function toString(text: string, options: {type: "terminal"; small?: boolean}): Promise<string>
}
