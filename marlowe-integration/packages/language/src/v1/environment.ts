export interface TimeInterval {
  from: bigint;
  to: bigint;
}

export interface Environment {
  timeInterval: TimeInterval;
}

export const mkEnvironment =
  (start: Date) =>
  (end: Date): Environment => ({
    timeInterval: { from: BigInt(start.getTime()), to: BigInt(end.getTime()) },
  });