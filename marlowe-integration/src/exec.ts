import { execSync } from "node:child_process";

export type Path = string;

export type CliArg = [string, string | number | boolean | undefined] | string;
export type CliArgs = CliArg[];

export function runCommand(command: string): string {
  return execSync(command).toString().trim();
}

export function runCommandJson<T>(command: string): T {
  const jsonStr = runCommand(command);
  try {
    return JSON.parse(jsonStr) as T;
  } catch (e) {
    throw new Error(
      `Failed to parse JSON from command: ${command}, Error: ${e}, JSON: ${jsonStr}`,
    );
  }
}

export function buildCliCommand(
  program: string,
  args: CliArgs
): string {
  const parts: string[] = [program];
  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    if (typeof arg === 'string') {
      parts.push(arg);
      continue;
    }
    const [key, value] = arg;
    if (value === undefined || value === null) continue;
    if (typeof value === 'boolean') {
      if (value) parts.push(key);
    } else if (typeof value === 'number') {
      parts.push(key, String(value));
    } else {
      parts.push(key, value);
    }
  }
  return parts.join(' ');
}

export function execCli(
  program: string,
  args: CliArgs
): string {
  const cmd = buildCliCommand(program, args);
  return runCommand(cmd);
}

export function execCliJson<T>(
  program: string,
  args: CliArgs
): T {
  const cmd = buildCliCommand(program, args);
  return runCommandJson<T>(cmd);
}