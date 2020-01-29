# Writing a Parser Combinator from Scratch in TypeScript

There are countless ways to parse programming languages, and deciding which approach to take is a huge topic that thousands of articles have been written on. In this blog post, we'll explore the approach we take to parsing formulas in Sigma, and write a small parser for a simple language from scratch.

## Background

At Sigma, we have our own small language to let users write formulas in worksheets, which is very similar to what users are familiar with in Excel or Google Docs. We chose to write our own parser, so we could have full control over error handling and annotations. Our parser is recursive descent with backtracking, built using combinators. The code here isn't the Sigma parser, but a similar one written for a simpler grammar, to show the basic concepts. By the end of this article we'll have written a small parser capable of parsing a simple language that supports function calls and number literals, like `Foo(Bar(1,2,3))`.

This article assumes the reader has some previous experience with basic parsing concepts, which you can read more about at (list of helpful links). The full source code for this project is available at (link to github / gist).

## Let's start coding

Since we're trying to parse an input string and return an AST [Abstract Syntax Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree), that seems like a good place to start defining our types.

For each part of the grammar we're creating, we want to:

1. Take an input `Context` (containing the string of our code, and position to start parsing from)
2. On success: return a `Success` containing a `value` and a new `Context` (the new position in the input string after what we just parsed) to continue parsing from.
3. On failure, we return a `Failure` object with the position and reason for failure.

```
        Input Context -----> Parser -----> Success
                                \
                                 \
                                  -----> Failure

         /
(\__/)  /
(•ㅅ•) /
/ 　 づ
```

```ts
// every parsing function will have this signature
type Parser<T> = (ctx: Context) => Result<T>;

// to track progress through our input string.
// we should make this immutable, because we can.
type Context = Readonly<{
  text: string; // the full input string
  index: number; // our current position in it
}>;

// our result types
type Result<T> = Success<T> | Failure;

// on success we'll return a value of type T, and a new Ctx (position in the string) to continue parsing from
type Success<T> = Readonly<{
  success: true;
  value: T;
  ctx: Ctx;
}>;

// when we fail we want to know where and why
type Failure = Readonly<{
  success: false;
  expected: string;
  ctx: Ctx;
}>;

// some convenience methods to build `Result`s for us
function success<T>(ctx: Context, value: T): Success<T> {
  return { success: true, value, ctx };
}

function failure<T>(ctx: Context, expected: string): Failure {
  return { success: false, expected, ctx };
}
```

Whoa, that's a bunch of types upfront, so lets give a concrete examples of how they'd work:

```ts
function parseCow = ...; // implementation not important yet. it's looking for the word 'cow'
const ctx = { text: "cow says moo", index: 0 }; // initializing a Ctx to pass in
const result = parseCow(ctx);
// { success: true, value: 'cow', ctx: {text: "cow says moo", index: 3}}
//              ^            ^                                       ^
//              |            |                                       |
//            horray       our result                the new input position, after the word 'cow'
```

How would we implement `parseCow`?

```ts
function parseCow(ctx: Context): Parser<string> {
  const match = "cow";
  const endIdx = ctx.index + match.length;
  if (ctx.text.substring(ctx.index, endIdx) === match) {
    // returning a new ctx starting after the string we just consumed, along with the result of this method
    return success({ ...ctx, index: endIdx }, match);
  } else {
    return failure(ctx, match);
  }
}
```

Okay, but that's a lot of code just to parse the word 'cow'. Luckily javascript is a language that supports first class functions. By writing functions that return functions, we can create parsers in a declarative way. These are the "combinators" this article is about.

```ts
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

const parseCow = str("cow");
```

To take a detour and parse this (meaningless) `cow says moo` sentence, we could write something like this, which has some immediately obvious issues:

```ts
const cow = str("cow");
const says = str("says");
const moo = str("moo");
const space = str(" ");

const parseCowSentence = function(ctx) {
  const cowRes = cow(ctx);
  if (!cowRes.success) return cowRes;

  const spaceRes1 = space(cowRes.ctx); // must pass the previous result's ctx into the next parser
  if (!spaceRes1.success) return spaceRes1;

  const saysRes = says(spaceRes1.ctx); // and again. you can see this is getting tedious and is error prone
  if (!saysRes.success) return saysRes;

  const spaceRes2 = space(saysRes.ctx); // i'm getting really tired of typing this pattern
  if (!spaceRes2.success) return spaceRes2;

  const mooRes = moo(spaceRes2.ctx); // please no more
  if (!mooRes.success) return mooRes;

  return success(mooRes.ctx, [
    // phew, we made it.
    // i couldn't think of anything interesting to do with this grammar so let's just return the strings we parsed.
    cowRes.value,
    spaceRes1.value,
    saysRes.value,
    spaceRes2.value,
    mooRes.value
  ]);
};
```

Yikes! This is not fun. It's really easy to pass the wrong context forward to the next parser. Let's write a function that abstracts this away for us.

```ts
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
```

```
                              ______________________________________________
                             |                   Sequence:                  |
        Input Context -----> |  Parser1 ---> Parser2 ---> ... ---> ParserN  | -----> Success
                             |______________________________________________|
                                                   \
                                                    \
                                                     -----> Failure

         /
(\__/)  /
(•ㅅ•) /
/ 　 づ
```

Using this combinator, we can express the previous parser in a much more terse manner:

```ts
const parseCowSentence = sequence([cow, space, says, space, moo]);
```

To write our little language, we're going to need a handful more combinators that we'll compose together to create a parser that supports our grammar.

Here's once called `any` that takes an array of parsers, and tries them all until one succeeds.

```
                              ____________________
                             |        Any:       |
        Input Context -----> |                   | -----> Success
                             |  Parser1          |
                             |  or Parser2       |
                             |  ...              |
                             |  or ParserN       |
                             |___________________|
                                         \
                                          \
                                           -----> Failure

         /
(\__/)  /
(•ㅅ•) /
/ 　 づ
```

Here's once called `many` that take on parser, and gathers as many repeats

```
                              ____________________
                             |        Many:      |
        Input Context -----> |                   | -----> Success
                             |  Parser x N       |
                             |___________________|

                                           (cannot fail, only returns [] if Parser fails)


         /
(\__/)  /
(•ㅅ•) /
/ 　 づ
```

Here's all the combinators we'll need for our language. The full implementation of each function is included at the end of the article.

```ts

// match a regexp
function regex(re: RegExp, expected: string): Parser<string> {
  ...
}

// look for an exact sequence of parsers
function sequence<T>(parsers: Parser<T>[]): Parser<T[]> {
  ...
}

// try each matcher in order, starting from the same point in the input. return the first one that succeeds.
// or return the failure that got furthest in the input string.
// which failure to return is a matter of taste, we prefer the furthest failure because.
// it tends be the most useful / complete error message.
// any time you see several choices in a grammar, you'll use `any`
function any<T>(parsers: Parser<T>[]): Parser<T> {
  ...
}

// match a parser, or succeed with null if not found. cannot fail.
function optional<T>(parser: Parser<T>) {
  ...
}

// look for 0 or more of something, until we can't parse any more. note that this function never fails, it will instead succeed with an empty array.
function many<T>(parser: Parser<T>): Parser<T[]> {
  ...
}

// a convenience method that will map a Success to callback, to let us do common things like build AST nodes from input strings. Failures are passed through untouched.
function map<A, B>(parser: Parser<A>, fn: (val: A) => B): Parser<B> {
  ...
}
```

Now lets put our parser together. We're going to support a language that has function calls and number literals, like this:

```
Foo(Bar(1,2,3))
```

We want to parse this into a syntax tree that looks like this:

```ts
{
  "target": "Foo",
  "args": [{
    "target": "Bar",
    "args": [1, 2, 3]
  }]
}
```

We could describe this language in (loosely) [EBNF](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form) like this:

```
program = expr
expr = call | number
call = ident '(' [ argList ] ')'
argList = arg (',' arg) *
number = ... whatever number format we decide to support
```

```ts
// the two AST nodes for our tiny language
type Expr = Call | number;

interface Call {
  target: string;
  args: Expr[];
}

// our top level parsing function that takes care of creating a `Ctx`, and unboxing the final AST (or throwing)
function parse(text: string) {
  const res = expr({ text, index: 0 });
  if (res.success) return res.value;
  throw `Parse error, expected ${res.expected} at char ${res.ctx.index}`;
}

// expr = call | numberLiteral
function expr(ctx: Ctx): Result<Expr> {
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
  sequence([str(","), expr]),
  // we map to this function that throws away the leading comma, returning only the argument expression
  ([_comma, argExpr]) => argExpr as Expr
);

// args = expr ( trailingArg ) *
const args = map(
  sequence([expr, many(trailingArg)]),
  // we combine the first argument and the trailing arguments into a single array
  ([arg1, rest]) => [arg1, ...rest] as Expr[]
);

// call = ident "(" args ")"
const call = map(
  sequence([ident, str("("), args, str(")")]),
  // we throw away the lparen and rparen, and use the function name and arguments to build a Call AST node.
  ([fnName, _lparen, argList, _rparen]): Call => ({
    target: fnName,
    args: argList || []
  })
);
```

Finally, running this code:

```ts
parse("Foo(Bar(1,2,3))");
// {
//   "target": "Foo",
//   "args": [
//     {
//       "target": "Bar",
//       "args": [
//         1,
//         2,
//         3
//       ]
//     }
//   ]
// }
```

The result is an implementation of our grammar that is very terse (about 60 lines with comments and spacing), and is backed up by about 120 lines of types and supporting combinator code, with no external dependencies. And because we wrote it by hand, we can easily extend it to support arbitrary extra functionality like annotations, error recovery, etc.

## Final thoughts:

All parsing approaches have pros and cons, and so do combinators:

### Pros:

- simple contract
- expressive, declarative
- functional, immutable, composable
- actual language grammar implementation can be terse, maps closely to EBNF for the language
- doesn't rely on exception handling
- backtracking is trivially easy, doesn't require managing a stack

### Cons:

- introduces a layer of indirection / abstraction
- the type of the `sequence` combinator may be difficult to express strictly enough
- requires a language with first class functions

# Appendix

(more links)

Here is the full implementation of the res of the combinators used above.

```ts
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
      if (!res.success) break;
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
```
