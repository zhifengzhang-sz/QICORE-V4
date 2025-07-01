import { type Either, isLeft, isRight, left, right } from "fp-ts/Either";
import { pipe } from "fp-ts/function";
import type { QiError } from "./error.js";

export type Result<T> = Either<QiError, T>;

export const success = <T>(value: T): Result<T> => right(value);
export const failure = <T>(error: QiError): Result<T> => left(error);

export const isSuccess = <T>(result: Result<T>): result is Either<never, T> =>
	isRight(result);
export const isFailure = <T>(
	result: Result<T>,
): result is Either<QiError, never> => isLeft(result);

export const map =
	<T, U>(fn: (value: T) => U) =>
	(result: Result<T>): Result<U> => {
		return pipe(result, (r) =>
			isRight(r) ? right(fn(r.right)) : left(r.left),
		);
	};

export const flatMap =
	<T, U>(fn: (value: T) => Result<U>) =>
	(result: Result<T>): Result<U> => {
		return pipe(result, (r) => (isRight(r) ? fn(r.right) : left(r.left)));
	};

export const unwrap = <T>(result: Result<T>): T => {
	if (isLeft(result)) {
		throw new Error(result.left.toString());
	}
	return result.right;
};

export const unwrapOr =
	<T>(defaultValue: T) =>
	(result: Result<T>): T => {
		return isRight(result) ? result.right : defaultValue;
	};

export const match =
	<T, U>(onSuccess: (value: T) => U, onError: (error: QiError) => U) =>
	(result: Result<T>): U => {
		return isRight(result) ? onSuccess(result.right) : onError(result.left);
	};

export const orElse =
	<T>(alternativeFn: (error: QiError) => Result<T>) =>
	(result: Result<T>): Result<T> => {
		return isLeft(result) ? alternativeFn(result.left) : result;
	};

export const fromTryCatch = <T>(
	fn: () => T,
	onError: (error: unknown) => QiError,
): Result<T> => {
	try {
		return success(fn());
	} catch (error) {
		return failure(onError(error));
	}
};

export const fromAsyncTryCatch = async <T>(
	fn: () => Promise<T>,
	onError: (error: unknown) => QiError,
): Promise<Result<T>> => {
	try {
		const value = await fn();
		return success(value);
	} catch (error) {
		return failure(onError(error));
	}
};

// Additional utility functions for working with Results
export const all = <T>(results: Result<T>[]): Result<T[]> => {
	const values: T[] = [];

	for (const result of results) {
		if (isLeft(result)) {
			return left(result.left);
		}
		values.push(result.right);
	}

	return right(values);
};

export const sequence = <T>(results: Result<T>[]): Result<T[]> => all(results);

export const traverse =
	<T, U>(fn: (value: T) => Result<U>) =>
	(values: T[]): Result<U[]> => {
		return all(values.map(fn));
	};

// Tap function for side effects without changing the result
export const tap =
	<T>(fn: (value: T) => void) =>
	(result: Result<T>): Result<T> => {
		if (isRight(result)) {
			fn(result.right);
		}
		return result;
	};

export const tapError =
	<T>(fn: (error: QiError) => void) =>
	(result: Result<T>): Result<T> => {
		if (isLeft(result)) {
			fn(result.left);
		}
		return result;
	};

// Monadic bind operator alias
export const bind = flatMap;
export const chain = flatMap;
