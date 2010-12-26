Nonterminals Value Int Number Minus Frac.

Terminals false null true string digit digit19 zero minus decimal_point

Rootsymbol Value.

Endsymbol '$end'.

Value -> false.
Value -> null.
Value -> true.
Value -> string.
Value -> Number.

Number -> Minus Int Frac.

Minus -> '$empty'.
Minus -> minus.

Int -> zero.
Int -> digit19 Digits.

Digits -> digit.
Digits -> digit Digits.

Frac -> '$empty'.
Frac -> decimal_point Digits.