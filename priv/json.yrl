Nonterminals Value Values Int Number Minus Frac Exp Sign Chars Char Object Field Fields Array

Terminals false null true digit digit19 zero minus plus decimal_point exp quotation_mark unescaped escape escaped left_curl right_curl colon comma left_bracket right_bracket

Rootsymbol Value.

Endsymbol '$end'.

Value -> false : nil.
Value -> null : nil.
Value -> true : nil.
Value -> String : nil.
Value -> Number : nil.
Value -> Object : nil.
Value -> Array : nil.

Number -> Minus Int Frac Exp : nil.

Minus -> '$empty' : nil.
Minus -> minus : nil.

Int -> zero : nil.
Int -> digit19 : nil.
Int -> digit19 Digits : nil.

Digits -> digit : nil.
Digits -> digit Digits : nil.

Frac -> '$empty' : nil.
Frac -> decimal_point Digits : nil.

%% For exponent I couldn't find a way to restrict to result to exponents smaller
%% than 309 (which would fail when converted to float) so I just restricted the
%% grammar to two digit exponents
Exp -> '$empty' : nil.
Exp -> exp Sign digit : nil.
Exp -> exp Sign digit digit : nil.

Sign -> '$empty' : nil.
Sign -> minus : nil.
Sign -> plus : nil.

String -> quotation_mark Chars quotation_mark : nil.
Chars -> '$empty' : nil.
Chars -> Char Chars : nil.

Char -> unescaped : nil.
Char -> escape escaped : nil.

Object -> left_curl right_curl : nil.
Object -> left_curl Fields right_curl : nil.

Fields -> Field : nil.
Fields -> Field comma Fields : nil.

Field -> String colon Value : nil.

Array -> left_bracket right_bracket: nil.
Array -> left_bracket Values right_bracket: nil.

Values -> Value : nil.
Values -> Value comma Values : nil.
