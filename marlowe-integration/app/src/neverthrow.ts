import { err, ok, ResultAsync } from "neverthrow";

export const delay = (ms: number) =>
  new Promise((resolve) => setTimeout(resolve, ms));

export const waitForPromise = <T>(
  fn: () => Promise<T>,
  pred: (v: T) => boolean,
  { timeoutMs = 10_000, everyMs = 2_000 } = {},
): ResultAsync<T, null> => {
  const start = Date.now();
  const promise: Promise<T> = (async () => {
    while (true) {
      const v = await fn();
      if (pred(v)) return v;
      if (Date.now() - start > timeoutMs) throw null;
      await delay(everyMs);
    }
  })();
  return ResultAsync.fromPromise(promise, () => null);
}

export const waitForResultAsync = <T, E>(
  fn: () => ResultAsync<T, E>,
  pred: (v: T) => boolean,
  { timeoutMs = 10_000, everyMs = 2_000 } = {},
): ResultAsync<T, E | null> => {
  const start = Date.now();
  const promise = (async () => {
    while (true) {
      const result = await fn();
      result.match(
        (v) => {
          if (pred(v)) return ok(v);
        },
        (e: E) => {
          return err(e);
        }
      );
      if (Date.now() - start > timeoutMs) return err(null);
      await delay(everyMs);
    }
  })();
  return new ResultAsync(promise);
}

export const waitPatientlyForResultAsync = <T, E>(
  fn: () => ResultAsync<T, E>,
  pred: (v: T) => boolean,
  { timeoutMs = 10_000, everyMs = 2_000 } = {},
): ResultAsync<T, E | null> => {
  const start = Date.now();
  let timeoutError = null;
  const promise = (async () => {
    while (true) {
      const result = await fn().match(
        (v) => {
          if (pred(v)) return v;
          return null;
        },
        (e: E) => {
          timeoutError = e;
          return null;
        }
      );
      if(result !== null) return ok(result);
      if (Date.now() - start > timeoutMs) return err(timeoutError);
      await delay(everyMs);
    }
  })();
  return new ResultAsync(promise);
}


