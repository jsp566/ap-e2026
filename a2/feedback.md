# Code

Your code for task 1 and 2 works, but could be better:
You make the `State` part of `Env`, and while you seemingly make it work,
this is not the intended way of doing it and ends up complicating your code.
The main complication is that lambda functions capture the *local* environment,
while the `State` is meant to be global, and so you have to use the `const'` function to compensate.
A simpler solution is to let the `Env` only be for local variables,
and then have `State` as its own parameter to the `EvalM` constructor.
This also makes it more clear that `Env` is (locally) read-only while the state is mutable.

In task 3, your monad implementation for `CheckM` is incorrect:
You use `undefined` in the implementation of `>>=`,
and in general you should never use `undefined` unless you are 100% sure that a code path is not reachable, which is not the case here.
For example, the following snippet crashes:
```
runCheck $ pure "foo" >>= \m -> CheckM (\_ -> Just m)
```
Additionally, your monad instance breaks the monad laws!
The *Right identity* law says `m >>= return == m`,
but running `runCheck $ Just "foo" >>= return` we get `Nothing`.

It is actually not possible in general to write a lawful monad instance
which uses `Maybe` to propagate success rather than failure,
so you are better off using `Either Error a` in `CheckM`.
(Another warning sign that you were off-track is that you never use the `a` type variable in the constructor of `CheckM`).

# Tests

Both positive and negative tests (where applicable). Very good.

# Report.

In your definition of `CheckM` you have a comment `TODO - give this a proper definition`,
which is alright, but you should mention it in your report as a possible concern.
It sort of contradicts you writing that you are "confident that it is functional".

## Question 1

Your reasoning is correct, but the most relevant part is that for
`n` calls of `Print` the complexity is quadratic.
And your solution would indeed work, and make `n` calls to print have linear complexity.

## Question 2

Correct.

## Question 3

Very good!
