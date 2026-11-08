# Code

Your code seems to work as intended, and passes all of our tests.

Stylistically:
- You can remove the TODO comments from the handout once they have been
  implemented.
- As `evalForLoop` is only used in the `ForLoop` case of `eval`
  you could have made it a local function via a `where` clause.
  This would
   1) make it syntactically close to where it is used, and
   2) make it so you don't have to pass `env`, `p` and `i`
      as would already be in the outer scope and don't change between loop iterations.

# Tests

Both positive and negative tests (where applicable). Very good.

- Checking for a specific error message in your tests tailors them too closely
  to your code, which can lead to maintenance issues when messages change and
  makes your tests less flexible. You can use a function that just checks
  that the program returned an error (also defined in `Data.Either`).:
  ```
  isLeft :: Either a b -> Bool
  isLeft (Left _) = True
  isLeft _        = False
  ```
  (though for some tests matching the exact error message makes sense).

# Report.

Very good.

You are correct that the handout does not specify what should happen
if `p` and `i` are the same, and you chose a sane way of resolving it.

## Question 1

Correct.

Note that this is also ambiguous in the handout as it
does not describe which order you should evaluate the `Apply` expression.

## Question 2

Very good!

## Question 3

Correct.

## Question 4

a. Correct.

b. Correct.

## Question 5

Correct.

## Question 6

Correct.

For a way to avoid this see difference lists:
<https://wiki.haskell.org/Difference_list>

## Question 7

Fine, but the first property is not easily tested.

By "be able to run" you would then need a parsing function,
and it would be better to describe the property in terms of that, i.e.:
stating the property as `eval . parseExp . printExp == eval`.
This also gives you a directly *testable* property.

## Question 8

Correct, very good :)
