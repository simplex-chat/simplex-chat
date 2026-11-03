export type Reader<T> = (v: unknown) => T | undefined;

// An array passes `typeof v === "object"`, so it is rejected here.
export function asObject(v: unknown): Record<string, unknown> | undefined {
  return typeof v === "object" && v !== null && !Array.isArray(v) ? (v as Record<string, unknown>) : undefined;
}

export const text: Reader<string> = (v) => (typeof v === "string" ? v : undefined);

export const filledText: Reader<string> = (v) => (typeof v === "string" && v.length > 0 ? v : undefined);

export const nonNegativeInteger: Reader<number> = (v) =>
  typeof v === "number" && Number.isInteger(v) && v >= 0 ? v : undefined;

export const positiveInteger: Reader<number> = (v) =>
  typeof v === "number" && Number.isInteger(v) && v > 0 ? v : undefined;

export const flag: Reader<boolean> = (v) => (typeof v === "boolean" ? v : undefined);

export function oneOf<T extends string>(values: readonly T[]): Reader<T> {
  return (v) => values.find((member) => member === v);
}

/** A field with the wrong type is rejected rather than treated as "not sent", so a wrong-typed value cannot reach the screen. */
export function fieldsInto<T extends object>(body: Record<string, unknown>, out: Partial<T>) {
  return <K extends keyof T & string>(key: K, read: Reader<NonNullable<T[K]>>): boolean => {
    const raw = body[key];
    if (raw === undefined || raw === null) return true;
    const value = read(raw);
    if (value === undefined) return false;
    out[key] = value;
    return true;
  };
}
