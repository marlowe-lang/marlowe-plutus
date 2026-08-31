import { err, ok, type Result } from "neverthrow";
import { execSync } from "node:child_process";
import { Json } from '@konduit/codec/json';
import type { JsonDeserialiser, JsonError } from "@konduit/codec/json/codecs";
import type { Tagged } from "type-fest";

export type Path = Tagged<string, "Path">;

export type CliArg = [string, string | number | boolean | undefined | bigint] | string;
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
export function runCommandJson(command: string, debug = false): Result<Json, CommandError | string> {
  return runCommand(command, debug).andThen(jsonStr => {
    if(debug) {
      console.debug(`Parsing cmd output: ${jsonStr}`);
    }
    return Json.fromString(jsonStr)
  });
}

export function runCommandJsonTyped<T>(
  command: string,
  deserialiser: JsonDeserialiser<T>,
  debug: boolean = false
): Result<T, CommandError | JsonError> {
  return runCommand(command, debug)
    .andThen((str) => {
      if(debug) {
        console.debug(`Parsing cmd output: ${str}`);
      }
      return Json.fromString(str);
    })
    .andThen(deserialiser);
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
    } else if (typeof value === 'bigint') {
      parts.push(key, value.toString());
    } else {
      // FIXME: paluh: this should use `execFileSync` with an argument
      // array instead of going through the shell. For now we shell-quote
      // strings that contain whitespace so complex JSON values survive
      // the round-trip.
      parts.push(key, shellQuote(String(value)));
    }
  }
  return parts.join(' ');
}

// Wrap @str@ in single quotes for the shell, escaping any embedded
// single quotes via the standard `'\''` idiom. Numbers and simple values
// can still be passed through unwrapped because `String(value)` only
// produces shell-safe output for those, but @shellQuote@ is always safe.
function shellQuote(str: string): string {
  return `'${str.replace(/'/g, `'\\''`)}'`;
}

export function execCli(
  program: string,
  args: CliArgs,
  debug: boolean = false,
): Result<string, CommandError> {
  const cmd = buildCliCommand(program, args);
  return runCommand(cmd, debug);
}

export function execCliJson(
  program: string,
  args: CliArgs
): Result<Json, CommandError | string> {
  const cmd = buildCliCommand(program, args);
  return runCommandJson(cmd);
}

export function execCliJsonTyped<T>(
  program: string,
  args: CliArgs,
  deserialiser: JsonDeserialiser<T>,
  debug: boolean = false,
): Result<T, CommandError | JsonError> {
  const cmd = buildCliCommand(program, args);
  return runCommandJsonTyped(cmd, deserialiser, debug);
}
