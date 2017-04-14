% Reads in a Spark source file and outputs the parse tree.
%
% Executing the query 'working_directory(CWD, CWD)' will output the
% current working directory in the Prolog interpreter.

source_to_tree(Filename) :-
    read_file_to_codes(Filename, Codes, []),
    atom_codes(Atom, Codes),
    atom_chars(Atom, Characters),
    lexer(Tokens, Characters, []),
    program(Tree, Tokens, []),
    write(Tree),
    nl.

% ----------------------------------------------------------------------
%   LEXER
% ----------------------------------------------------------------------

% Reserved words.

lexer(Tokens) --> ['w', 'h', 'i', 'l', 'e'], lexer(R), {append(['while'], R, Tokens)}.
lexer(Tokens) --> ['f', 'o', 'r'], lexer(R), {append(['for'], R, Tokens)}.
lexer(Tokens) --> ['i', 'f'], lexer(R), {append(['if'], R, Tokens)}.
lexer(Tokens) --> ['e', 'l', 's', 'e'], lexer(R), {append(['else'], R, Tokens)}.

% Relational operators.

lexer(Tokens) --> ['<', '='], lexer(R), {append(['<='], R, Tokens)}.
lexer(Tokens) --> ['>', '='], lexer(R), {append(['>='], R, Tokens)}.
lexer(Tokens) --> ['=', '='], lexer(R), {append(['=='], R, Tokens)}.

% Whitespace characters.

lexer(Tokens) --> [' '], lexer(R), {Tokens = R}.
lexer(Tokens) --> ['\t'], lexer(R), {Tokens = R}.
lexer(Tokens) --> ['\n'], lexer(R), {Tokens = R}.

% Everything else.

lexer(Tokens) --> [C], lexer(R), {Tokens = [C | R]}.
lexer(Tokens) --> [], {Tokens = []}.

% ----------------------------------------------------------------------
%   PARSER
% ----------------------------------------------------------------------

% Digits and letters.

digit(d(0)) --> ['0'].
digit(d(1)) --> ['1'].
digit(d(2)) --> ['2'].
digit(d(3)) --> ['3'].
digit(d(4)) --> ['4'].
digit(d(5)) --> ['5'].
digit(d(6)) --> ['6'].
digit(d(7)) --> ['7'].
digit(d(8)) --> ['8'].
digit(d(9)) --> ['9'].

letter(l('a')) --> ['a'].
letter(l('b')) --> ['b'].
letter(l('c')) --> ['c'].
letter(l('d')) --> ['d'].
letter(l('e')) --> ['e'].
letter(l('f')) --> ['f'].
letter(l('g')) --> ['g'].
letter(l('h')) --> ['h'].
letter(l('i')) --> ['i'].
letter(l('j')) --> ['j'].
letter(l('k')) --> ['k'].
letter(l('l')) --> ['l'].
letter(l('m')) --> ['m'].
letter(l('n')) --> ['n'].
letter(l('o')) --> ['o'].
letter(l('p')) --> ['p'].
letter(l('q')) --> ['q'].
letter(l('r')) --> ['r'].
letter(l('s')) --> ['s'].
letter(l('t')) --> ['t'].
letter(l('u')) --> ['u'].
letter(l('v')) --> ['v'].
letter(l('w')) --> ['w'].
letter(l('x')) --> ['x'].
letter(l('y')) --> ['y'].
letter(l('z')) --> ['z'].

letter(l('A')) --> ['A'].
letter(l('B')) --> ['B'].
letter(l('C')) --> ['C'].
letter(l('D')) --> ['D'].
letter(l('E')) --> ['E'].
letter(l('F')) --> ['F'].
letter(l('G')) --> ['G'].
letter(l('H')) --> ['H'].
letter(l('I')) --> ['I'].
letter(l('J')) --> ['J'].
letter(l('K')) --> ['K'].
letter(l('L')) --> ['L'].
letter(l('M')) --> ['M'].
letter(l('N')) --> ['N'].
letter(l('O')) --> ['O'].
letter(l('P')) --> ['P'].
letter(l('Q')) --> ['Q'].
letter(l('R')) --> ['R'].
letter(l('S')) --> ['S'].
letter(l('T')) --> ['T'].
letter(l('U')) --> ['U'].
letter(l('V')) --> ['V'].
letter(l('W')) --> ['W'].
letter(l('X')) --> ['X'].
letter(l('Y')) --> ['Y'].
letter(l('Z')) --> ['Z'].

% Identifiers.

identifier(id(I, L)) --> letter(L), identifier(I).
identifier(id(L)) --> letter(L).

% Integers.

integer(int(D, I)) --> digit(D), integer(I).
integer(int(D)) --> digit(D).

% Expressions.

expression(I) --> identifier(I).
expression(I) --> integer(I).
expression(neg(I)) --> ['-'], integer(I).
expression(ep(I, E)) --> integer(I), ['+'], expression(E).
expression(em(I, E)) --> integer(I), ['*'], expression(E).
expression(es(I, E)) --> integer(I), ['-'], expression(E).
expression(ed(I, E)) --> integer(I), ['/'], expression(E).
expression(er(I, E)) --> integer(I), ['%'], expression(E).
expression(ep(I, E)) --> identifier(I), ['+'], expression(E).
expression(em(I, E)) --> identifier(I), ['*'], expression(E).
expression(es(I, E)) --> identifier(I), ['-'], expression(E).
expression(ed(I, E)) --> identifier(I), ['/'], expression(E).
expression(er(I, E)) --> identifier(I), ['%'], expression(E).
expression(ea(I, E)) --> identifier(I), ['='], expression(E).
expression(elt(I, E)) --> integer(I), ['<'], expression(E).
expression(egt(I, E)) --> integer(I), ['>'], expression(E).
expression(ele(I, E)) --> integer(I), ['<='], expression(E).
expression(ege(I, E)) --> integer(I), ['>='], expression(E).
expression(eeq(I, E)) --> integer(I), ['=='], expression(E).
expression(elt(I, E)) --> identifier(I), ['<'], expression(E).
expression(egt(I, E)) --> identifier(I), ['>'], expression(E).
expression(ele(I, E)) --> identifier(I), ['<='], expression(E).
expression(ege(I, E)) --> identifier(I), ['>='], expression(E).
expression(eeq(I, E)) --> identifier(I), ['=='], expression(E).

% All expressions terminated by a semicolon are statements.

statement(stmt(E)) --> expression(E), [';'].

% 'for' loop statement.

statement(for(Init, Cond, Inc, SL)) -->
    ['for'], ['('],
    expression(Init),
    [';'],
    expression(Cond),
    [';'],
    expression(Inc),
    [')'], ['{'],
    statement_list(SL),
    ['}'].

% 'while' loop statement.

statement(while(E, SL)) -->
    ['while'], ['('],
    expression(E),
    [')'], ['{'],
    statement_list(SL),
    ['}'].

% 'if' and 'else-if' conditional statements.

statement(if(Cond, SL)) -->
    ['if'], ['('],
    expression(Cond),
    [')'], ['{'],
    statement_list(SL),
    ['}'].

statement(if(Cond, SL1, SL2)) -->
    ['if'], ['('],
    expression(Cond),
    [')'], ['{'],
    statement_list(SL1),
    ['}'], ['else'], ['{'],
    statement_list(SL2).
    ['}'].

statement_list(sl(S)) --> statement(S).
statement_list(sl(S, SL)) --> statement(S), statement_list(SL).
program(program(SL)) --> statement_list(SL).

% ----------------------------------------------------------------------
%   INTERPRETER
% ----------------------------------------------------------------------

% Evaluate integers.

eval_expr(int(d(D)), D, 10).
eval_expr(int(d(D), I), R, X) :-
    eval_expr(I, R1, X1),
    X is X1 * 10,
    R is X1 * D + R1.

eval_expr(int(d(D)), D).
eval_expr(int(d(D), I), R) :-
    eval_expr(I, R1, X),
    R is X * D + R1.

% Evaluate unary minus (negative integers).

eval_expr(neg(I), R) :-
    eval_expr(I, R1),
    R is R1 * -1.

% Evaluate addition expressions.

eval_expr(ep(E1, E2), R) :-
    eval_expr(E1, R1),
    eval_expr(E2, R2),
    R is R1 + R2.

% Evaluate multiplication expressions.

eval_expr(em(E1, E2), R) :-
    eval_expr(E1, R1),
    eval_expr(E2, R2),
    R is R1 * R2.

% Evaluate subtraction expression.

eval_expr(es(E1, E2), R) :-
    eval_expr(E1, R1),
    eval_expr(E2, R2),
    R is R1 - R2.

% Evaluate division expressions.

eval_expr(ed(E1, E2), R) :-
    eval_expr(E1, R1),
    eval_expr(E2, R2),
    R is R1 / R2.

% Evaluate modulus expressions.

eval_expr(er(E1, E2), R) :-
    eval_expr(E1, R1),
    eval_expr(E2, R2),
    R is R1 mod R2.

% Evaluate the result of an expression.

eval_expr(Tokens, Result) :-
    expression(ParseTree, Tokens, []),
    eval_expr(ParseTree, Result).
