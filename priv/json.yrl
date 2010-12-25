Nonterminals Value.

Terminals false null true string number.

Rootsymbol Value.

Endsymbol '$end'.

Value -> false : false.
Value -> null : null.
Value -> true : true.
Value -> string : list_to_binary('$1').
Value -> number: '$1'.

