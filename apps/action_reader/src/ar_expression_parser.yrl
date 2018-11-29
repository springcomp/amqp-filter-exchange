Nonterminals 
    statement
    statement_list
    property
    scope
    property_name
    constant
.

Terminals   approximate_number_constant boolean_constant decimal_constant integer_constant string_constant
            delimited_identifier regular_identifier quoted_identifier
            t_eq
            t_dot t_semicolon
            t_set
.

Rootsymbol statement_list.

statement_list -> statement: ['$1'].
statement_list -> statement_list statement : '$1' ++ ['$2'].

statement -> t_set property t_eq constant t_semicolon : { '$2', '$4' }.

property -> property_name: symbol_of('$1').
property -> scope t_dot property_name: symbol_of('$3').

scope -> regular_identifier: '$1'.

property_name -> regular_identifier:    '$1'.
property_name -> quoted_identifier:     '$1'.
property_name -> delimited_identifier:  '$1'.

constant -> integer_constant:               { integer_constant, symbol_of('$1') }.
constant -> decimal_constant:               { decimal_constant, symbol_of('$1') }.
constant -> approximate_number_constant:    { approximate_number_constant, symbol_of('$1') }.
constant -> boolean_constant:               { boolean_constant, symbol_of('$1') }.
constant -> string_constant:                { string_constant, symbol_of('$1') }.

Erlang code.

% Tokens produced by leex scanner are a tuple containing information
% about syntactic category, position in the input text and the actual
% terminal symbol found in the text:
% {Category, Position, Symbol}

symbol_of({ _, _, V }) -> V.