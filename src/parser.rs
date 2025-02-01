rs_parser_generator::rules!(
	//@mod parser;
	@debug;
	@graphviz;

	letter_lower: "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z";
	letter_upper: "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z";
	letter: letter_lower | letter_upper;

	digit_dec: "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
	number_dec: digit_dec+;
	number: number_dec;

	token: (letter | "_") (letter | digit_dec | "_")*;

	wsc: " " | "\t" | "\n" | "\r";
	ws: wsc*;
	wss: wsc+;

	semi: ";";
	colon: ":";
	comma: ",";

	op_assign: "=";
	op_add: "+";
	op_sub: "-";
	op_mul: "*";
	op_div: "/";
	op_mod: "%";

	op_bin_and: "&";
	op_bin_or: "|";

	br_ropen: "(";
	br_rclose: ")";
	br_topen: "<";
	br_tclose: ">";
	br_sopen: "[";
	br_sclose: "]";
	br_copen: "{";
	br_cclose: "}";

	kv_let: "let";
	kv_return: "return";

	token_type: token;

	params_decl: token ws colon ws token_type (ws comma ws token ws colon ws token_type)*;

	expr_block: br_copen ws ((stmt ws)*) ((expr ws)?) br_cclose;
	expr_function: op_bin_or ws ((params_decl ws)?) op_bin_or ws ((op_sub br_tclose ws token_type ws)?) (expr_block | expr);
	expr_basic: token | number | expr_function | expr_block;
	expr_mul: expr_basic (ws (op_mul | op_div | op_mod) ws expr_basic)?;
	expr_add: expr_mul (ws (op_add | op_sub) ws expr_mul)?;
	expr: expr_add;

	stmt_let: kv_let wss token ws op_assign ws expr ws semi;
	stmt_ret: kv_return ((wss expr)?) ws semi;
	stmt: stmt_let | stmt_ret;

	file: ws (stmt ws)*;
);
