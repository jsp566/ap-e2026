# A4

## Code

- Your code seems to work as intended, and passes all of our tests.
- Avoid trailing whitespace to prevent unnecessary changes flagged by tools or editors and to reduce the risk of merge conflicts. This ensures smoother collaboration and maintains a cleaner, more professional code base.

## Test

- Both positive and negative tests.
- Your tests also seem fairly organized. Very Good!

## Report

### Introduction

- The handout explicitly says,

    ```txt
    You are expected to comment on the interesting details of your implemen-
    tation.
    ```

  It means that you should find 1 or 2 important details to discuss.

### Questions

#### Q1

- Good. Clear explanations.

#### Q2

- Good.
- Also, you may think about: "Does `Transaction` still provide any side effects that may change something even if the return is `()`?"

#### Q3

- The problem is what the Functor instance does with that definition. It requires:

  ```haskell
  fmap f (TryCatchOp x y) = TryCatchOp (f x) (f y)
  ```

  That `Functor` instance is fine by its own, but it breaks our intuition for what monadic sequencing means, because now

  ```haskell
  TryCatchOp x y >>= f = TryCatchOp (x >>= f) (y >>= f)
  ```

  So when you do something "after" a `TryCatchOp`, you do it in both cases, rather than after the code in the `TryCatchOp` has "finished".

  This is perfectly law-abiding, but means that code like this does not do what one might intuit:

  ```haskell
  do
    x `catch` y
    z
  ```

  Specifically, if `z` fails, then `y` executes.
