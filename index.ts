type Parser<T> = (ctx: Context) => Result<T>;

type Result<T> = Success<T> | Failure;

type Success<T> = Readonly<{
  success: true;
  value: T;
  ctx: Context;
}>;

type Failure = Readonly<{
  success: false;
  expected: string;
  ctx: Context;
}>;

type Context = Readonly<{
  text: string;
  index: number;
}>;

function success<T>(ctx: Context, value: T): Success<T> {
  return { success: true, value, ctx };
}

function failure<T>(ctx: Context, expected: string): Failure {
  return { success: false, expected, ctx };
}

// match an exact string or fail
function str(match: string): Parser<string> {
  return ctx => {
    const endIdx = ctx.index + match.length;
    if (ctx.text.substring(ctx.index, endIdx) === match) {
      return success({ ...ctx, index: endIdx }, match);
    } else {
      return failure(ctx, match);
    }
  };
}

// match a regexp or fail
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

// try each matcher in order, starting from the same point in the input. return the first one that succeeds.
// or return the failure that got furthest in the input string.
// which failure to return is a matter of taste, we prefer the furthest failure because.
// it tends be the most useful / complete error message.
function any<T>(parsers: Parser<T>[]): Parser<T> {
  return ctx => {
    let furthestRes: Result<T> | null = null;
    for (const parser of parsers) {
      const res = parser(ctx);
      if (res.success) return res;
      if (!furthestRes || furthestRes.ctx.index < res.ctx.index)
        furthestRes = res;
    }
    return furthestRes!;
  };
}

// match a parser, or succeed with null
function optional<T>(parser: Parser<T>): Parser<T | null> {
  return any([parser, ctx => success(ctx, null)]);
}

// look for 0 or more of something, until we can't parse any more. note that this function never fails, it will instead succeed with an empty array.
function many<T>(parser: Parser<T>): Parser<T[]> {
  return ctx => {
    let values: T[] = [];
    let nextCtx = ctx;
    while (true) {
      const res = parser(nextCtx);
      if (!res.success) break;
      values.push(res.value);
      nextCtx = res.ctx;
    }
    return success(nextCtx, values);
  };
}

// look for an exact sequence of parsers, or fail
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

// the two AST nodes for our tiny language
type Expr = Call | number;

interface Call {
  target: string;
  args: Expr[];
}

// our top level parsing function that takes care of creating a `Ctx`, and unboxing the final AST (or throwing)
function parse(text: string): Expr {
  const res = expr({ text, index: 0 });
  if (res.success) return res.value;
  throw `Parse error, expected ${res.expected} at char ${res.ctx.index}`;
}

// expr = call | numberLiteral
function expr(ctx: Context): Result<Expr> {
  return any<Expr>([call, numberLiteral])(ctx);
}

// our regexp to match identifiers
const ident = regex(/[a-zA-Z][a-zA-Z0-9]*/g, "identifier");

// a regexp parser to match a number string
const numberLiteral = map(
  regex(/[+\-]?[0-9]+(\.[0-9]*)?/g, "number"),
  // which we map to javascript's built in `parseFloat` method
  parseFloat
);

// trailingArg = ',' arg
const trailingArg = map(
  sequence<any>([str(","), expr]),
  // we map to this function that throws away the leading comma, returning only the argument expression
  ([_comma, argExpr]): Expr[] => argExpr
);

// args = expr ( trailingArg ) *
const args = map(
  sequence<any>([expr, many(trailingArg)]),
  // we combine the first argument and the trailing arguments into a single array
  ([arg1, rest]): Expr[] => [arg1, ...rest]
);

// call = ident "(" args ")"
const call = map(
  sequence<any>([ident, str("("), optional(args), str(")")]),
  // we throw away the lparen and rparen, and use the function name and arguments to build a Call AST node.
  ([fnName, _lparen, argList, _rparen]): Call => ({
    target: fnName,
    args: argList || []
  })
);

function example(code: string) {
  console.log(JSON.stringify(parse(code), null, 2));
}

example("1");
example("Foo()");
example("Foo(Bar())");
example("Foo(Bar(1,2,3))");
