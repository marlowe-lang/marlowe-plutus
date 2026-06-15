export type ISO8601 = string;

export type POSIXTime = bigint;

export const posixTimeToIso8601 = (posixTime: POSIXTime): ISO8601 =>
  new Date(Number(posixTime)).toISOString();

export const iso8601ToPosixTime = (iso8601: ISO8601): POSIXTime =>
  BigInt(new Date(iso8601).getTime());

export const MINUTES = 1000 * 60;

// FIXME: fp-ts (setTimeout not available without DOM lib)
// export const sleep = (seconds: number): Promise<void> =>
//   new Promise((resolve) => setTimeout(resolve, seconds * 1_000));

// export const waitForPredicatePromise = async (
//   predicate: () => Promise<boolean>,
//   seconds: number = 3
// ): Promise<void> => {
//   if (await predicate()) {
//     return;
//   }
//   await sleep(seconds);
//   await waitForPredicatePromise(predicate, seconds);
// };