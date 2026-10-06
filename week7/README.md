# Exam preparation exercises

Like all exercises, the following exercises are part of the curriculum for
Advanced Programming in 2026, and questions at the exam may reference them. It
is expected that an attendee at the exam has familiarised themselves with them.
In contrast to previous exercises, these ones do not introduce new concepts, but
merely emphasize concepts previously seen. Naturally, they build on previous
exercises and assignments. There is no corresponding assignment to be handed,
and therefore they are much larger than previous weekly exercises.

## Introduction

The starting point for these exercises will be a slightly cut down variant of
the *AP Language* (APL) that was the topic of several of your assignments. The
most significant simplification is that printing is not supported. The
`localEnv` function is also implemented in a more direct way.

Your overall goal is to extend APL with various new features focussed on support
for concurrency. This is partitioned into several tasks. Some of the tasks have
dependencies on each other, which we will note explicitly. If you find yourself
struggling with a task, consider attempting another (non-dependent) task. You do
not need to solve (or even attempt) all tasks in order to pass, although a good
performance across all tasks is necessary for a top grade.

The full ambiguous grammar, including features to be introduced later in this
text, is shown below. The new language constructs you will implement are these:
tuples, projection, `&&`, and `||`. The semantics of these language constructs
will be discussed in the related tasks.

You are given a code handout with a complete AST definition and a partial
implementation of a parser and an evaluator. The code handout corresponds
roughly to the features developed during the course exercises. The evaluator
expresses evaluation through the free monad `EvalM`, for which you will write
three interpreters:

* `APL.InterpPure`: the *pure interpreter*, which simply executes the given
  program sequentially and straightforwardly. Most of this interpreter is
  already complete in the code handout, although you are asked to make some
  extensions.

* `APL.InterpSim`: the *simulated concurrent interpreter*, which simulates
  concurrency without using `IO`. You are asked to implement this interpreter in
  task D.

* `APL.InterpConcurrent`: the *concurrent interpreter*, which uses true
  `IO`-based concurrency, based on the SPC job scheduler.

You do not need to define new effect types, as these are already included in the
handout. The code handout also includes a complete definition of a slightly
simplified version of the SPC job scheduler (which you must not modify), as well
as some other skeleton files that you will extend as part of the exam tasks.

You should read the code handout carefully. It contains helpful comments. The
handout contains a small collection of tests, of which some will initially fail.
A comment connected to each test will mention after which task the test is
supposed to work. You are *strongly* advised to add more tests of your own.

When the semantics for a language construct states that something "is an
error", it means that you must report an appropriate runtime error if
that situation occurs (similar to division by zero). No specific error
message is required.

## Grammar

APL syntax in EBNF, including all extensions described later in the text. This
grammar is ambiguous and contains left recursion, which you will be asked to
address. Operator priority is specified in a table below. The new language
constructs are described in the corresponding tasks. Whitespace is permitted
between all terminals.

```
Atom ::= var
       | int
       | bool
       | "(" ")"
       | "(" Exp ")"
       | "(" Exp Exps ")"
       | "put" Atom Atom
       | "get" Atom
       | Atom "." int

Exps ::= "," Exp
       | "," Exp Exps

FExp ::= FExp FExp
       | Atom

LExp ::= "if" Exp "then" Exp "else" Exp
       | "\" var "->" Exp
       | "let" var "=" Exp "in" Exp
       | "loop" var "=" Exp "for" var "<" Exp "do" Exp
       | "loop" var "=" Exp "while" Exp "do" Exp
       | FExp

Exp ::= LExp
      | Exp "==" Exp
      | Exp "+" Exp
      | Exp "-" Exp
      | Exp "*" Exp
      | Exp "/" Exp
      | Exp "&&" Exp
      | Exp "||" Exp
```

### Operator precedence

| **Operators** | **Associativity** |
|---|---|
| Application | Left |
| `*` `/` | Left |
| `+` `-` | Left |
| `==` | Left |
| `&&` | Left |
| `||` | Left |

## Examples

Examples of APL parsing and evaluation follow. Consult these if you find the
semantics of some of the language constructs unclear, but consider skipping this
section until you reach the relevant tasks.

### Tuples

* Syntax: `let x = (1,2) in x.0`

* AST: `Let "x" (Tuple [CstInt 1,CstInt 2]) (Project (Var "x") 0)`

* Evaluation: `ValInt 1`

------------------------------------------------------------------------

* Syntax: `(1,2).1`

* AST: Project (Tuple [CstInt 1,CstInt 2]) 1`

* Evaluation: `ValInt 2`

### Concurrency operators

*Due to nondeterminism, expressions may produce different results in the pure
and simulated/concurrent interpreters. This is noted where relevant. The
concurrent interpreter may further produce results that differ from the
simulated interpreter, due to nondeterminism---only a single possible result is
listed below.*

* Syntax: `(1+2) && (3+4)`

* AST: `BothOf (Add (CstInt 1) (CstInt 2)) (Add (CstInt 3) (CstInt 4)) `

* Evaluation: `ValTuple [ValInt 3,ValInt 7]`

------------------------------------------------------------------------

* Syntax: `(1+2) || (3+4+5+6)`

* AST: `OneOf (Add (CstInt 1) (CstInt 2)) (Add (Add (Add (CstInt 3) (CstInt 4)) (CstInt 5)) (CstInt 6))`

* Evaluation (pure):  `ValInt 3`

* Evaluation (simulated): `ValInt 3`

### Concurrent `put`/`get`

*The evaluation results for the following examples apply *only* for the
simulated and concurrent interpretation functions.*

* Syntax: `get 0 && put 0 true`

* AST: `BothOf (KvGet (CstInt 0)) (KvPut (CstInt 0) (CstBool True))`

* Evaluation: `ValTuple [ValBool True,ValBool True]`

------------------------------------------------------------------------

* Syntax: `get 0 + 1 && put 0 2`

* AST: `BothOf (Add (KvGet (CstInt 0)) (CstInt 1)) (KvPut (CstInt 0) (CstInt 2))`

* Evaluation: `ValTuple [ValInt 3,ValInt 2]`

------------------------------------------------------------------------

* Syntax: `put (get 0) 1 && let x = put 0 2 in get 2`

* AST: `BothOf (KvPut (KvGet (CstInt 0)) (CstInt 1)) (Let "x" (KvPut (CstInt 0) (CstInt 2)) (KvGet (CstInt 2)))`

* Evaluation: `ValTuple [ValInt 1,ValInt 1]`

## Tasks

### Task A: Implement tuples

For this task you must implement *tuples* in APL. A tuple is a value that
comprises zero or more values. They are defined as follows in `APL.Monad`:

```Haskell
data Val
  = ...
  | ValTuple [Val]
```

Tuples are constructed by the rule

```
"(" ")"
```

corresponding to a tuple with no elements, and by the rule

```
"(" Exp "," Exps ")"
```

corresponding to a tuple with two or more elements. Single-element tuples are
thus not syntactically legal (just as in Haskell), although the AST
representation can express them. Note that the production `"(" Exp ")"` is a
parenthesized expression and not a single-element tuple.

The corresponding AST constructor is defined in `APL.AST` as follows:

```Haskell
data Exp
  = ...
  | Tuple [Exp]
```

For example, the input `(a, b)` corresponds to the following `Exp`:

```
Tuple [Var "a", Var "b"]
```

A tuple expression `(e1, ..., eN)` is evaluated by evaluating the components
from left to right, then constructing a `ValTuple` with the resulting values in
the same order as their corresponding expressions. If the evaluation of any
expression fails, the result of the evaluation of the tuple also fails and the
first error encountered should be reported.

The elements of a tuple `x` can be projected (accessed) with the syntax `x.i`,
where `i` is a literal integer, such as in `x.0`, which projects element 0 of
the tuple. For example, `(e1, ..., eN) == e1`. If `x` has *N* elements, then `i`
must be between *0* and *N - 1*; using an index outside of this range is an
error. The corresponding AST constructor is `Project`. It is an error to try to
project an element from a non-tuple.

##### Your task:

Implement parsing of tuple construction and projection in `APL.Parser`
and evaluation of tuples and projections in `APL.Eval`.

#### Solution

<details>
<summary>Open this to see the implementation</summary>

Tuples are parsed in `APL.Parser.pAtom0`, with some care (using `try`) to
disambiguate them from parenthesized expressions. That is the only subtle part
of this task.

Their evaluation is straightforward:

```Haskell
eval (Tuple es) =
  ValTuple <$> mapM eval es
```

Since `mapM` traverses the list left-to-right, this ensures the component
expressions are evaluated in the desired order.

</details>

### Task B: Implement `while` loops and stepping

For this task you must extend APL with a notion of `while` loops, as well as a
"step" effect that must be emitted for every iteration of *any* loop (including
`for` loops)

A `while`-loop expression

```
loop p = init while cond do body
```

is represented with the `WhileLoop` AST constructor and is evaluated as
follows:

1.  Evaluate expression `init` to a value *v*.

2.  Bind variable `p` to *v*.

3.  Evaluate expression `cond` to a boolean *c*. It is an error if *c* does not
    evaluate to a boolean. If *c* is false, the loop stops and returns the value
    of `p`. Otherwise, if *c* is true, evaluate expression `body` and bind `p`
    to the result.

Note that in a `while`-loop `p` is in scope when evaluating `cond`.

*Stepping* is an effect represented by the `StepOp` constructor of `EvalOp`. In
later tasks we will use it to interrupt computations that do not otherwise have
any effects. For `APL.InterpPure`, interpretation of `StepOp` is simply by
recursively executing its continuation. For now, you must modify `APL.Eval` to
use the `StepOp` effect (via `evalStep`) in the following cases: just before a
loop body is executed (for both `for`- and `while`-loops), and just before a
function value is applied.

##### Your task:

Implement parsing of `while`-loops in `APL.Parser` and evaluation in `APL.Eval`.
Treat `while` as keywords. Add uses of `evalStep` as stated above, including in
the existing case for `ForLoop`. Extend `APL.InterpPure` to handle the `StepOp`
effect.

### Task C: Implement `&&` and `||`

For this task you must implement operators intended for concurrent programming.

The expression `e1 && e2` denotes concurrent execution of two expressions,
returning the result of both. To evaluate the expression `e1 && e2` we evaluate
`e1` and `e2` in unspecified order, then return a pair of their results. If
either subexpression fails, then the overall expression also fails. If both
fail, either error may be reported. Using the pure interpreter, `e1 && e2` is
similar to `(e1,e2)` (except for evaluation order), but it will behave
differently when using the concurrent interpreters.

The expression `e1 || e2` denotes concurrent execution of two expressions,
returning the result of the one that finishes "first" (the meaning of which
depends on the interpreter). To evaluate the expression `e1 || e2` we evaluate
either `e1` or `e2` and return one of their results. The pure interpreter must
start by evaluating `e1`, returning its value if evaluation succeeds. If `e1`
fails, the pure interpreter should return the result of evaluating `e2`.

##### Your task:

Transform the grammar of APL to eliminate left recursion and ambiguity. Then
implement parsing of `&&` and `||` in `APL.Parser` and evaluation in `APL.Eval`.
Evaluation of `&&` is with the `BothOfOp` effect (`evalBothOf`), and evaluation
of `||` is with `OneOfOp` (`evalOneOf`).

Extend `APL.InterpPure` to handle the `BothOfOp` and `OneOfOp` effects. For
`OneOfOp`, execute the first computation and return its value. If execution of
the first computation results in an error, execute the second computation and
return its value. If the second computation also results in an error, then the
overall computation fails with either the first or second error message.

### Task D: Implement a simulated concurrent interpreter

For this task you must implement an interpretation function for `EvalM` that
simulates concurrent execution. The main feature will be that `e1 || e2` can be
implemented such that the expression that finishes *first* (i.e., in the
shortest number of steps) has its value returned. As a special case, it means
that `e1 || e2` can terminate even if one of `e1` or `e2` is an infinite loop
(but not both).

The key idea is writing a function `step` that evaluates a `EvalM` computation
up to (and including) the next `StepOp` effect if possible *but no further*. By
repeatedly "stepping", we can progress a computation arbitrarily. But
importantly, we can interleave steppings of different concurrent computations.

Further, we also refine how the `KvGetOp`/`KvPutOp` effects work in a concurrent
setting. When a `KvGetOp` effect requests the value of a key that does not exist
in the state, then that is not an error. Rather, execution is just stuck until
some other conconcurrent computation (if any) performs a `KvPutOp` with the
desired key. It does, however, remain an error in the pure interpreter.

##### Your task:

Finish the implementation of the simulated concurrent interpreter in
`APL.InterpSim`.

##### Hints:

Consider postponing the treatment of `KvGetOp`/`KvPutOp` until the basics work.

### Task E: Implement a key-value database

For this task you must implement a simple server for managing a concurrent
key-value database *KVDB*. Your implementation must be in the `KVDB` module and
implement the following API:

```haskell
-- | A reference to a KVDB instance that stores keys
-- of type 'k' and corresponding values of type 'v'.
data KVDB k v

-- | Start a new KVDB instance.
startKVDB :: (Ord k) => IO (KVDB k v)

-- | Retrieve the value corresponding to a given key.
-- If that key does not exist in the store, then this
-- function blocks until another thread writes the desired
-- key with 'kvPut', after which this function returns
-- the now available value.
kvGet :: KVDB k v -> k -> IO v

-- | Write a key-value mapping to the database.
-- Replaces any prior mapping of the key.
kvPut :: KVDB k v -> k -> v -> IO ()
```

Note that the API is polymorphic, as KVDB can store keys of any type that is an
instance of `Ord`, and values of any type.

##### Your task:

Implement the KVDB interface. You must make use of the `GenServer` module. It is
up to you to design the state and protocol used to implement KVDB. You must also
define appropriate tests in `KVDB_Tests`.

### Task F: Implement a concurrent interpreter

In this task you will implement a truly concurrent interpreter that potentially
uses multiple Haskell threads to execute the `EvalM` monad. In particular,
execution of the `BothOfOp` and `OneOfOp` involve actual IO-level concurrency.
You must implement this concurrency by enqueuing jobs in `SPC`, *not* by using
`forkIO` directly.

You must implement the same semantics for `KvGetOp`/`KvPutOp` as in Task D,
meaning that trying to retrieve a value for a key that does not exist causes the
thread to block until the key is available. Interpretation of `StepOp` is simply
by recursively executing its continuation.

##### Your task:

Finish the implementation of the concurrent interpreter in
`APL.InterpConcurrent`. For handling `BothOfOp` and `OneOfOp`, you must make use
of `SPC`. For handling `KvPutOp` and `KVGetOp` you must make use of KVDB.

##### Hints:

Since SPC jobs are executed purely for side effects and have no return value as
such, you will need to use `IORef`s to actually store the results of executing
tasks. As in Task D, consider postponing the treatment of `KvGetOp`/`KvPutOp`
until the basics work.

Your interpreter may crash with an error message "thread blocked indefinitely in
an MVar operation", signalled by the Haskell runtime system. This is not
necessarily an error in your implementation, but can be due to the APL program
you are testing being invalid.
