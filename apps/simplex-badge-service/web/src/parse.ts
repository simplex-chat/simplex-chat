// Readers for data this page did not produce. Each answers the value or undefined, never a
// lie about its type; what undefined costs is the caller's to decide.

export type Reader<T> = (v: unknown) => T | undefined;

// The one assertion in these modules, and the only place it is safe. An array passes
// `typeof v === "object"` and is never a body or a record, so it is turned away here.
export function asObject(v: unknown): Record<string, unknown> | undefined {
  return typeof v === "object" && v !== null && !Array.isArray(v) ? (v as Record<string, unknown>) : undefined;
}

export const text: Reader<string> = (v) => (typeof v === "string" ? v : undefined);

export const filledText: Reader<string> = (v) => (typeof v === "string" && v.length > 0 ? v : undefined);

// Named on the same axis as `positiveInteger`, since the pair is read side by side: these are
// minor units and confirmation counts, so a fraction or a negative is a wrong answer rather than
// a small one, and a screen would render it as money or as "2.5 confirmations". NaN and both
// infinities go the same way: numbers to `typeof`, arithmetic to nobody.
export const nonNegativeInteger: Reader<number> = (v) =>
  typeof v === "number" && Number.isInteger(v) && v >= 0 ? v : undefined;

export const positiveInteger: Reader<number> = (v) =>
  typeof v === "number" && Number.isInteger(v) && v > 0 ? v : undefined;

export const flag: Reader<boolean> = (v) => (typeof v === "boolean" ? v : undefined);

/** `find` returns a member of the list, so the answer is the union with nothing asserted. */
export function oneOf<T extends string>(values: readonly T[]): Reader<T> {
  return (v) => values.find((member) => member === v);
}

/** Refuses the whole body if any field carries the wrong type; absent and null both mean
 * "not sent". Reading a wrong type as absent let a currency of `42` reach the screen. */
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
