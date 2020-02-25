# Writing a Parser Combinator from Scratch in TypeScript

Writing parsers can be challenging. The problem is simple to describe: convert an input string into a ([syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree)). The same language can be parsed by many different algorithms, and all have different tradeoffs with regards to speed, memory usage, readability, and maintainability. In this blog post, we'll explore the approach we take to parsing formulas in Sigma, using _parser combinators_. Parser combinators allow us to compose many simple functions together to define our entire grammar. The underlying algorithm is [Recursive Descent](https://en.wikipedia.org/wiki/Recursive_descent_parser) with backtracking, using techniques that are available to us in languages with first class functions like javascript.

This article assumes the reader has some previous experience with basic parsing concepts.

## Background

At Sigma, we have our own small language to let users write formulas in worksheets, which is very similar to what users are familiar with in Excel or Google Docs. We chose to write our own parser so we could have full control over error handling and annotations, which allow us to provide code completion even on malformed strings, like a formula that's only halfway written. Our parser uses recursive descent with backtracking, built using combinators. The code here isn't the Sigma parser, but a similar one written for a simpler grammar to show the basic concepts. In this article we'll walk through building a parser for a simple language that supports function calls and number literals, like `Foo(Bar(1,2,3))`.

## The Goal

Our high level goal is to write a function that takes an input string, and returns a syntax tree. Our language is going to look like this:

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

We could describe this language in (loosely) [EBNF](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form) like this:

```
program = expr
expr = call | number
call = ident '(' [ argList ] ')'
argList = arg (',' arg) *
number = ... whatever number format we decide to support
ident = ... idk, how about /[a-zA-Z][a-zA-Z0-9]+/
```

Our parser will use recursive descent, which means we're going to start from the highest level structure in the language (in this case, an expression), and branch out into the rest of the language definition based on the rules we've defined.

The leaves of this tree, (`number`, `ident`, `'('` and `')'`) are called [terminal symbols](https://en.wikipedia.org/wiki/Terminal_and_nonterminal_symbols), and are the only parts of the parser that directly consume characters from the input string.

The rest of the parser is `non-terminals`, which means they represent combinations of other `symbols`. (in parsing terminology, a `symbol` is just a part of the language. each line in the ENBF above represents a symbol)

Parsers commonly have a [lexer](https://en.wikipedia.org/wiki/Lexical_analysis) before the actual parser. In short, a lexer preprocesses an input string into a flat list of `tokens`. We can skip this process entirely because the code for our terminal symbols will consume the string directly.

## Some Types

We'll be writing this in a pure functional style, which means we want every function to return an immutable result, with no side effects. We want to traverse the input string from the top, all the way down the the leaf nodes (the `terminals`), and then return the parts of the tree from the bottom up.

Functional purity is desirable here because it makes it easier to reason about the behaviour of a single parser and its output, instead of wondering what other non-obvious things could be happening in the parsing process outside of the code you're looking at.

For each part of the grammar we're creating, we want to:

1. Take an input `Context` (the string containing our code, and the position we're currently at)
2. On success: return a `Success` containing a `value` and a new `Context` containing the next position in the string to continue parsing from.
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

By creating these parsers, and writing a variety of functions that _compose parsers together_, we will build our complete parser for this language.

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

// on success we'll return a value of type T, and a new Ctx
// (position in the string) to continue parsing from
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
// implementation not important yet. it's looking for the word 'cow'
function parseCow = ...;
// initializing a Ctx to pass in
const ctx = { text: "cow says moo", index: 0 };
const result = parseCow(ctx);
// { success: true, value: 'cow', ctx: {text: "cow says moo", index: 3}}
//              ^            ^                                       ^
//              |            |                                       |
//            hooray       our result                the new input position,
//                                                   after the word 'cow'
```

How would we implement `parseCow`?

```ts
function parseCow(ctx: Context): Parser<string> {
  const match = "cow";
  const endIdx = ctx.index + match.length;
  if (ctx.text.substring(ctx.index, endIdx) === match) {
    // returning a new ctx starting after the string
    // we just consumed, along with the result of this method
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

  // must pass the previous result's ctx into the next parser
  const spaceRes1 = space(cowRes.ctx);
  if (!spaceRes1.success) return spaceRes1;

  // and again. you can see this is getting tedious and is error prone
  const saysRes = says(spaceRes1.ctx);
  if (!saysRes.success) return saysRes;

  // i'm getting really tired of typing this pattern
  const spaceRes2 = space(saysRes.ctx);
  if (!spaceRes2.success) return spaceRes2;

  // please no more
  const mooRes = moo(spaceRes2.ctx);
  if (!mooRes.success) return mooRes;

  return success(mooRes.ctx, [
    // phew, we made it.
    // i couldn't think of anything interesting to do with this
    // grammar so let's just return the strings we parsed.
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
Input Context --> |  Parser1 ---> Parser2 ---> ... ---> ParserN  | ---> Success
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

Here's another one called `many` that take on parser, and gathers as many repeats

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

Below are all the combinators we'll need for our language. The full implementation of each function is included at the end of the article, but the understanding the intent of each should be enough to move forward.

```ts

// match a regexp
function regex(re: RegExp, expected: string): Parser<string> {
  ...
}

// look for an exact sequence of parsers
function sequence<T>(parsers: Parser<T>[]): Parser<T[]> {
  ...
}

// try each matcher in order, starting from the same point in the input.
// return the first one that succeeds. or return the failure that got furthest
// in the input string. which failure to return is a matter of taste, we prefer
// the furthest failure because. it tends be the most useful / complete error
// message. any time you see several choices in a grammar, you'll use `any`
function any<T>(parsers: Parser<T>[]): Parser<T> {
  ...
}

// match a parser, or succeed with null if not found. cannot fail.
function optional<T>(parser: Parser<T>): Parser<T | null> {
  ...
}

// look for 0 or more of something, until we can't parse any more. note that
// this function never fails, it will instead succeed with an empty array.
function many<T>(parser: Parser<T>): Parser<T[]> {
  ...
}

// a convenience method that will map a Success to callback, to let us do
// common things like build AST nodes from input strings.
// Failures are passed through untouched.
function map<A, B>(parser: Parser<A>, fn: (val: A) => B): Parser<B> {
  ...
}
```

# Implementing the Grammar

Here's our basic plan for the language again.

```
program = expr
expr = call | number
call = ident '(' [ argList ] ')'
argList = arg (',' arg) *
number = ... whatever number format we decide to support
ident = ... idk, how about /[a-zA-Z][a-zA-Z0-9]+/
```

Let's turn it into a working parser using the combinators at our disposal.

```ts
// the two AST nodes for our tiny language
type Expr = Call | number;

interface Call {
  target: string;
  args: Expr[];
}

// our top level parsing function that takes care of creating a `Ctx`,
// and unboxing the final AST (or throwing)
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
  // we map to this function that throws away the leading comma,
  // returning only the argument expression
  ([_comma, argExpr]): Expr[] => argExpr
);

// args = expr ( trailingArg ) *
const args = map(
  sequence<any>([expr, many(trailingArg)]),
  // we combine the first argument and the
  // trailing arguments into a single array
  ([arg1, rest]): Expr[] => [arg1, ...rest]
);

// call = ident "(" args ")"
const call = map(
  sequence<any>([ident, str("("), optional(args), str(")")]),
  // we throw away the lparen and rparen, and use the function name and
  // arguments to build a Call AST node.
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

- Simple contract
- Expressive, declarative
- Functional, immutable, composable
- Actual language grammar implementation can be terse, maps closely to EBNF for the language
- Doesn't rely on exception handling
- Backtracking is trivially easy, doesn't require managing a stack

### Cons:

- Introduces a layer of indirection / abstraction
- Requires a language with first class functions
- The type of the `sequence` combinator may be difficult to express strictly enough. It takes an array of parsers with mixed return types, and returns a list of results mapping directly to the output of those parsers, which is tricky to enforce in a type system. This can be overcome in typescript with function overloading, but it's outside the scope of this article, so I opted to use `any` and trust the ordering of parsers.

The full source code for this blog post, including all the combinators used, is available on [github](https://github.com/sigma-engineering/blog-combinators)
