{application, 'action_reader', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['action_reader','ar_expression_lexer','ar_expression_parser']},
	{registered, []},
	{applications, [kernel,stdlib]},
	{env, []}
]}.