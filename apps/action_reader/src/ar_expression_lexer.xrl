Definitions.

WS = [\s\t\r\n]

REGULAR_IDENTIFIER = [a-zA-Z][_a-zA-Z0-9]*
QUOTED_IDENTIFIER = \"([^\"]|\"\")*\"
DELIMITED_IDENTIFIER = \[([^\[\]]|\[\[|\]\])+\]

INTEGER_CONSTANT = [0-9]+
DECIMAL_CONSTANT = [0-9]+\.[0-9]+
NUMBER_CONSTANT = [0-9]+(\.[0-9]+)?[eE][-+]?[0-9]+
BOOLEAN_CONSTANT = ([tT][rR][uU][eE])|([fF][aA][lL][sS][eE])
STRING_CONSTANT = '([^']|'')*'

SET = [sS][eE][tT]

EQ = =
DOT = \.
SEMICOLON = ;

Rules.

{WS} : skip_token.
{EQ} : t(t_eq, TokenLine, TokenChars).
{DOT} : t(t_dot, TokenLine, TokenChars).
{SEMICOLON} : t(t_semicolon, TokenLine, TokenChars).

{SET} : t(t_set, TokenLine, TokenChars).

{INTEGER_CONSTANT} : t(integer_constant, TokenLine, TokenChars).
{DECIMAL_CONSTANT} : t(decimal_constant, TokenLine, TokenChars).
{NUMBER_CONSTANT} : t(approximate_number_constant, TokenLine, TokenChars).
{BOOLEAN_CONSTANT} : t(boolean_constant, TokenLine, TokenChars).
{STRING_CONSTANT} : t(string_constant, TokenLine, unquote_string_constant(TokenChars)).

{REGULAR_IDENTIFIER} : t(regular_identifier, TokenLine, TokenChars).
{QUOTED_IDENTIFIER} : t(quoted_identifier, TokenLine, unquote_quoted_identifier(TokenChars)).
{DELIMITED_IDENTIFIER} : t(delimited_identifier, TokenLine, unquote_delimited_identifier(TokenChars)).

Erlang code.

t(TokenType, TokenLine, TokenChars) -> { token, {TokenType, TokenLine, TokenChars } }.

unquote_delimited_identifier(Identifier) ->
    Len = string:len(Identifier),
    Text = string:substr(Identifier, 2, Len - 2),
    re:replace(
        re:replace(Text, "\\[\\[", "[", [global, { return, list }]),  
        "\\]\\]", 
        "]", 
        [global, { return, list }])
.

unquote_quoted_identifier(Identifier) ->
    Len = string:len(Identifier),
    Text = string:substr(Identifier, 2, Len - 2),
    re:replace(Text, "\"\"", "\"", [global, { return, list }])
.

unquote_string_constant(String) ->
    Len = string:len(String),
    Text = string:substr(String, 2, Len - 2),
    re:replace(Text, "''", "'", [global, { return, list }])    
.