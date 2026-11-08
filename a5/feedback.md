# A5

## Code

- Your code seems to work as intended, and passes all of our tests.
- Avoid trailing whitespace to prevent unnecessary changes flagged by tools or editors and to reduce the risk of merge conflicts. This ensures smoother collaboration and maintains a cleaner, more professional code base.

## Report

### Introduction

- Good introduction about your decisions.

### Questions

#### Q1

- Good. It would be good if you could provide a specific expression directly.
- The "smarter" generator to avoid patterns, as you mentioned, is way harder.
- Another option is to add a time limit to the evaluation in `onlyCheckedErrors`.
- FYI, you can use `discardAfter` or `timeout` to handle infinite execution (timeouts).

#### Q2

- Good.
- "no type error" at least 20% is also hard. This is because `checkExp` reports a type error for every arithmetic, comparison, conditional and application node, so an expression has no type error only if it is built from `CstInt`, `CstBool`, `Var`, `Let`, `Lambda` and `TryCatch` alone.

#### Q3

- Good.
- Nice fix for negative integers in parser!
- It's also possible to just generate the positive number, because we did not restrict whether a negative number is valid or not; it depends on your own decision.

#### Q4

- Good.

#### Q5

- Good.
