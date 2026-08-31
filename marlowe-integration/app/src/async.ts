export const delay = (ms: number) =>
  new Promise((resolve) => setTimeout(resolve, ms));

export async function waitFor<T>(
  fn: () => Promise<T>,
  pred: (v: T) => boolean,
  { timeoutMs = 10_000, everyMs = 2_000, onTimeout = "timeout" } = {},
): Promise<T> {
  const start = Date.now();
  while (true) {
    const v = await fn();
    if (pred(v)) return v;
    if (Date.now() - start > timeoutMs) throw new Error(onTimeout);
    await delay(everyMs);
  }
}
