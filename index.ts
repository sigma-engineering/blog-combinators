interface Ctx {
  text: string;
  index: number;
}

type Parser<T> = (ctx: Ctx) => Result<T>;

type Result<T> = Success<T> | Failure;

interface Success<T> {
  success: true;
  value: T;
  ctx: Ctx;
}

interface Failure {
  success: false;
  expected: string;
  ctx: Ctx;
}

function success<T>(ctx: Ctx, value: T): Success<T> {
  return { success: true, value, ctx };
}

function failure<T>(ctx: Ctx, expected: string): Failure {
  return { success: false, expected, ctx };
}

function str(match: string): Parser<string> {
  return ctx => {
    if (ctx.text.substring(ctx.index, ctx.index + match.length) === match) {
      return success({ ...ctx, index: ctx.index + match.length }, match);
    } else {
      return failure(ctx, match);
    }
  };
}

function regex(re: RegExp, expected: string): Parser<string> {
  return ctx => {
    re.lastIndex = ctx.index;
    const res = re.exec(ctx.text);
    if (res && res.index === ctx.index) {
      return success({ ...ctx, index: ctx.index + res[0].length }, res[0]);
    } else {
      return failure(ctx, expected);
    }
  };
}

function any<T>(parsers: Parser<T>[]): Parser<T> {
  return ctx => {
    // try each matcher in order, starting from the same point in the input
    let furthestRes: Result<T> | null = null;
    for (const parser of parsers) {
      const res = parser(ctx);
      if (res.success) return res;
      if (!furthestRes || furthestRes.ctx.index < res.ctx.index)
        furthestRes = res;
    }
    // or fail to match any parsers, returning the last failure.
    return furthestRes!;
  };
}

function optional<T>(parser: Parser<T>) {
  return any([parser, ctx => success(ctx, null)]);
}

// look for 0 or more of something, until we can't parse any more. note that this function never fails, it will instead succeed with an empty array.
function many<T>(parser: Parser<T>): Parser<T[]> {
  return ctx => {
    let values: T[] = [];
    let nextCtx = ctx;
    while (true) {
      const res = parser(nextCtx);
      console.log(res);
      if (!res.success) break;
      values.push(res.value);
      nextCtx = res.ctx;
    }
    return success(nextCtx, values);
  };
}

function sequence<T>(parsers: Parser<T>[]): Parser<T[]> {
  return ctx => {
    let values: T[] = [];
    let nextCtx = ctx;
    for (const parser of parsers) {
      const res = parser(nextCtx);
      if (!res.success) return res;
      values.push(res.value);
      nextCtx = res.ctx;
    }
    return success(nextCtx, values);
  };
}

// a convenience method that will map a Success to callback, to let us do common things like build AST nodes from input strings.
function map<A, B>(parser: Parser<A>, fn: (val: A) => B): Parser<B> {
  return ctx => {
    const res = parser(ctx);
    return res.success ? success(res.ctx, fn(res.value)) : res;
  };
}

// Begin language specific stuff

// Our AST nodes
type Expr = Call | number;

interface Call {
  target: string;
  args: Expr[];
}

function expr(ctx: Ctx): Result<Expr> {
  return any<Expr>([call, numberLiteral])(ctx);
}

const ident = regex(/[a-zA-Z][a-zA-Z0-9]*/g, "identifier");

const numberLiteral = map(
  regex(/[+\-]?[0-9]+(\.[0-9]*)?/g, "number"),
  parseFloat
);

const trailingArg = map(
  sequence<any>([str(","), expr]),
  ([_comma, argExpr]) => argExpr as Expr
);

const args = map(
  sequence<any>([expr, many(trailingArg)]),
  ([arg1, rest]) => [arg1, ...rest] as Expr[]
);

const call = map(
  sequence<any>([ident, str("("), args, str(")")]),
  ([fnName, _laren, argList, _rparen]) => ({
    target: fnName,
    args: argList || []
  })
);

function parse(text: string) {
  const res = expr({ text, index: 0 });
  if (res.success) return res.value;
  throw `Parse error, expected ${res.expected} at char ${res.ctx.index}`;
}

console.log(JSON.stringify(parse("Foo(Bar(2,3))"), null, 2));
