import { Result } from 'neverthrow';
import { type Assertion, expect } from 'vitest';

export const unwrapOk = <T, Err>(result: Result<T, Err>, msgTemplate?: string): T => {
  const mkMessage = (err: any) => {
    const tpl = msgTemplate || "Expected Ok result, got Err: ERROR_STR"
    return tpl.replace("ERROR_STR", String(err));
  };
  return result.match(
    (value) => value,
    (error) => { throw new Error(mkMessage(error)); }
  );
}

const unwrapErrWith = <T, E>(result: Result<T, E>, onErr: (error: E) => boolean): E => {
  return result.match(
    (value) => { throw new Error(`Expected Err result, got Ok: ${value}`); },
    (error) => {
      if (!onErr(error)) {
        throw new Error(`Error did not match expected condition: ${error}`);
      }
      return error;
    }
  );
}

export const unwrapErr = <T, E>(result: Result<T, E>): E => {
  return unwrapErrWith(result, (_error) => true);
}

export const expectOk = <T, E>(result: Result<T, E>, label?: string): Assertion<T> => {
  return result.match(
    (value) => expect(value),
    (error) => {
      if(label) throw new Error(`Expected Ok result, got Err (${label}): ${error}`)
      throw new Error(`Expected Ok result, got Err: ${error}`)
    }
  );
}

