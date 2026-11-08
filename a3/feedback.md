# A3

# Code

Your code seems to work as intended, and passes most of our tests.
But it does have at least one minor problem:

- Symbols, such as `=` in let and `\` in lambda, does not have to be followed by whitespace.
  As such, you should not use `lKeyword` to parse those symbols.

# Tests

- Both positive and negative tests.      - Good
- Your tests also seem fairly organized. - Very Good!

# Report.

Good report.

## Question 1

Good.

## Question 2

Correct.
However you could imagine if we first did the tokenizing up-front before any parsing it could be a problem.

## Question 3

Correct.

## Question 4

Very good.
This is however a pretty trivial case, but your parser is written
so that the order of the choice does not matter for any interesting (i.e. non-`pure`) cases.

## Question 5

Correct.
If you change the `lexeme` function you will also have to remember
to consume any potential comments at the very start of the file
in `parseAPL`.