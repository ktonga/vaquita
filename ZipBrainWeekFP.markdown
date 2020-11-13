# Functional Programming with Haskell

Agenda
------

- About me
- What is FP?
- Intro to Haskell
- Basic Tooling
- Basic Syntax
- Haskell in Action
- Property Testing
- Convince y'all of joining the cult

# About me

- Have been using FP very successfully for the past 10 years
- That means, when I started using it I stopped delivering *bugs*
- Started with Scala, which is kinda okay. Then switched to ♥️ **Haskell** ♥️
- People call Scala "A gateway drug to Haskell"
- Even the creator, Martin Odersky, called it [Haskellator](https://twitter.com/deanwampler/status/577624353044463616)

# What is not FP

It is not just passing lambdas around.

JavaScript, for instance, is considered by some as a FP'ish PL
because you can pretty much do whatever you want with functions;
put them in vars, pass them as argument and return them as result.
But that is not enough to call it FP.

# What is FP

One could say that FP is the opposite to **Imperative Programming**

```java
// Java (Imperative)
int total = 0;
for (int i = 1; i <= n; i++)
    total = total + i;
```

FP is programming using **Pure Functions**. They are the equivalent
in programming to **Mathematical Functions**.

```haskell
-- Haskell (Functional)
foldl (+) 0 [1..n]
```

# Pure Functions

- Mapping between the input and the result
- Same result for same input
- No side-effects

```java
// Java URL does name resolution while comparing two URLs :O
public boolean equals(Object obj)

// This performs blocking networking :facepalm:
if (url1.equals(url2)) ...
```

```haskell
-- Equivalent Haskell signature
equals :: URL -> URL -> Bool
```

# Benefits

- Easy to reason about, which makes code easier to read, modify and test
- Simplifies **Concurrency**
- Enables compiling optimizations
- Makes you a better programmer in whichever language you use


# Intro to Haskell

Haskell is a statically typed, lazy and purely functional programming language.

Haskell is a language specification called Haskell Report, its latest
revision is from 2010.
It has a reference compiler implementation called GHC (Glasgow Haskell Compiler)

Main Features
-------------

- Concise
- Types System from another world
- Recursive Functions
- Higher-order Functions
- Effect Tracking
- Polymorphic Functions
- Lazy Evaluation

# Basic Tooling

- `ghc`: compiler
- `ghci`: interactive interpreter
- `cabal`: package manager and build tool
- `hackage`: centralised package repository
- `hoogle`: super powerful docs searching
- `haskell-language-server`: LSP server to enable IDE-like features in text editors

# Basic Syntax

Slides are boring, let's jump into the REPL!


# Types and Classes

In Haskell everything is a value and has a type. A types is a collection
of related values.

- Basic built-in types
- List, Tuple and Function types
- Type inference
- Polymorphic types
- Basic classes


# Functions

- Function application
- Function partial application
- Operator sections
- Function composition
- Function definition
  - Conditional
  - Guards
  - `let/in`
  - `where`
  - Pattern matching
  - Recursion


# Effects

Pure functions are awesome and all... But I want to change the world!

`IO` to the rescue. `IO` is the way to let Haskell know you want to do
some nasty stuff, launch the missile as they call it.

A function returning `IO` is still pure, `IO` is just a type that
produces values, given the same input the `IO` value will be the
same and it won't perform any side effects until evaluated. The
evaluation is what can yield different results.

In Haskell multiple `IO` operations can be easily chained together
using a `do` block.

# Haskell in Action

> Talk is cheap. Show me the code.
>
> - Linus Torvalds

# Property Testing

Usually developers write example-based tests. It is comparing
the result against some expected value.

```haskell
reverse [1, 2, 3, 4, 5] == [5, 4, 3, 2, 1]
```

Property tests are based in generic properties of the code. It is
proving that all possible inputs satisfy said properties.

```haskell
anyList = generateRandomList
reverse (reverse anyList) == anyList
```


# Join the Cult

- Become the best version of yourself
- I've created the Slack channel `#fp-cult`, come join me
- You can find the repo here: https://github.com/ktonga/vaquita

