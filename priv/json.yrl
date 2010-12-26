Nonterminals Value Int Number Minus Frac Exp Sign Chars Char.

Terminals false null true digit digit19 zero minus plus decimal_point exp quotation_mark unescaped escape escaped.

Rootsymbol Value.

Endsymbol '$end'.

Value -> false.
Value -> null.
Value -> true.
Value -> String.
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

String -> quotation_mark Chars quotation_mark.
Chars -> '$empty'.
Chars -> Char Chars.

Char -> unescaped.
Char -> escape escaped.