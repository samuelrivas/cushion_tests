Nonterminals Value Int Number Minus Frac Exp Sign.

Terminals false null true string digit digit19 zero minus plus decimal_point exp

Rootsymbol Value.

Endsymbol '$end'.

Value -> false.
Value -> null.
Value -> true.
Value -> string.
Value -> Number.

Number -> Minus Int Frac Exp.

Minus -> '$empty'.
Minus -> minus.

Int -> zero.
Int -> digit19 Digits.

Digits -> digit.
Digits -> digit Digits.

Frac -> '$empty'.
Frac -> decimal_point Digits.

Exp -> '$empty'.
Exp -> exp Sign Digits.

Sign -> '$empty'.
Sign -> minus.
Sign -> plus.