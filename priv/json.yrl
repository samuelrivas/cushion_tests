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

%% For exponent I couldn't find a way to restrict to result to exponents smaller
%% than 309 (which would fail when converted to float) so I just restricted the
%% grammar to two digit exponents
Exp -> '$empty'.
Exp -> exp Sign digit.
Exp -> exp Sign digit digit.

Sign -> '$empty'.
Sign -> minus.
Sign -> plus.

String -> quotation_mark Chars quotation_mark.
Chars -> '$empty'.
Chars -> Char Chars.

Char -> unescaped.
Char -> escape escaped.
