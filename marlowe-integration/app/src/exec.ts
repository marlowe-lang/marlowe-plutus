import { err, ok, type Result } from "neverthrow";
import { execSync } from "node:child_process";

export type Path = string;

export type CliArg = [string, string | number | boolean | undefined] | string;
export type CliArgs = CliArg[];

export type CommandError = {
  stderr: string;
  stdout: string;
  status: bigint;
  type: "CommandError";
};

export namespace CommandError {
  export const fromExecError = (e: any): Result<CommandError, any> => {
    if (e.status !== undefined || e.code !== undefined) {
      return ok({
        stderr: e.stderr?.toString() || '',
        stdout: e.stdout?.toString() || '',
        status: BigInt(e.status ?? e.code),
        type: "CommandError",
      });
    }
    return err(e);
  }
}

export function runCommand(command: string, debug: boolean = false): Result<string, CommandError> {
  if(debug) {
    console.debug(`Running command: ${command}`);
  }
  try {
    return ok(execSync(command).toString().trim());
  } catch (e: any) {
    return CommandError.fromExecError(e).match(
      cmdErr => err(cmdErr),
      origErr => { throw origErr }
    );
  }
}

// FIXME: We should replace internal casting with a proper codec.
// Till then we use this unsafe approach.
export function runCommandJson<T>(command: string): Result<T, CommandError | string> {
  return runCommand(command).andThen(jsonStr => {
    try {
      return ok(JSON.parse(jsonStr) as T);
    } catch (e) {
      return err(`Failed to parse JSON from command: ${command}, Error: ${e}, JSON: ${jsonStr}`);
    }
  });
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
  args: CliArgs,
  debug: boolean = false,
): Result<string, CommandError> {
  const cmd = buildCliCommand(program, args);
  return runCommand(cmd, debug);
}

export function execCliJson<T>(
  program: string,
  args: CliArgs
): Result<T, CommandError | string> {
  const cmd = buildCliCommand(program, args);
  return runCommandJson<T>(cmd);
}
