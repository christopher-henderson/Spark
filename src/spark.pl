:- discontiguous eval_expr/3.
:- discontiguous eval_expr/2.

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
    eval(Tree).

% ----------------------------------------------------------------------
%   LEXER
% ----------------------------------------------------------------------

% Reserved words.

whitespace --> [' '].
whitespace --> ['\t'].
whitespace --> ['\n'].

lexer(Tokens) --> ['w', 'h', 'i', 'l', 'e'], lexer(R), {append(['while'], R, Tokens)}.
lexer(Tokens) --> ['f', 'o', 'r'], lexer(R), {append(['for'], R, Tokens)}.
lexer(Tokens) --> ['i', 'f'], lexer(R), {append(['if'], R, Tokens)}.
lexer(Tokens) --> ['e', 'l', 's', 'e'], lexer(R), {append(['else'], R, Tokens)}.
lexer(Tokens) --> ['p', 'r', 'i', 'n', 't'], lexer(R), {append(['print'], R, Tokens)}.
lexer(Tokens) --> ['t', 'r', 'u', 'e'], lexer(R), {append(['true'], R, Tokens)}.
lexer(Tokens) --> ['f', 'a', 'l', 's', 'e'], lexer(R), {append(['false'], R, Tokens)}.
lexer(Tokens) --> ['a', 'n', 'd'], lexer(R), {append(['and'], R, Tokens)}.
lexer(Tokens) --> ['o', 'r'], lexer(R), {append(['or'], R, Tokens)}.
lexer(Tokens) --> ['n', 'o', 't'], lexer(R), {append(['not'], R, Tokens)}.
lexer(Tokens) --> ['x', 'o', 'r'], lexer(R), {append(['xor'], R, Tokens)}.

% Relational operators.
lexer(Tokens) --> ['<', '='], lexer(R), {append(['<='], R, Tokens)}.
lexer(Tokens) --> ['>', '='], lexer(R), {append(['>='], R, Tokens)}.
lexer(Tokens) --> ['=', '='], lexer(R), {append(['=='], R, Tokens)}.
lexer(Tokens) --> ['!', '='], lexer(R), {append(['!='], R, Tokens)}.

% Whitespace characters.
lexer(Tokens) --> whitespace, lexer(R), {Tokens = R}.

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

% Booleans
boolean(bool('true')) --> ['true'].
boolean(bool('false')) --> ['false'].

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

% Print to stdout.
print(print(E)) --> ['print'], expression(E).

% Identifiers.
identifier(id(L, I)) --> letter(L), identifier(I).
identifier(id(L)) --> letter(L).

% Integers.
integer(int(D, I)) --> digit(D), integer(I).
integer(int(D)) --> digit(D).

% Expressions.
expression(I) --> identifier(I).
expression(B) --> boolean_expression(B).
expression(I) --> integer_expression(I).
expression(P) --> print(P).
expression(ea(I, E)) --> identifier(I), ['='], expression(E).

integer_expression(I) --> integer(I).
integer_expression(I) --> identifier(I).
integer_expression(neg(I)) --> ['-'], integer(I).
integer_expression(neg(I)) --> ['-'], identifier(I).
integer_expression(ep(I, E)) --> integer(I), ['+'], integer_expression(E).
integer_expression(em(I, E)) --> integer(I), ['*'], integer_expression(E).
integer_expression(es(I, E)) --> integer(I), ['-'], integer_expression(E).
integer_expression(ed(I, E)) --> integer(I), ['/'], integer_expression(E).
integer_expression(er(I, E)) --> integer(I), ['%'], integer_expression(E).
integer_expression(ep(I, E)) --> identifier(I), ['+'], integer_expression(E).
integer_expression(em(I, E)) --> identifier(I), ['*'], integer_expression(E).
integer_expression(es(I, E)) --> identifier(I), ['-'], integer_expression(E).
integer_expression(ed(I, E)) --> identifier(I), ['/'], integer_expression(E).
integer_expression(er(I, E)) --> identifier(I), ['%'], integer_expression(E).


boolean_expression(B) --> boolean(B).
boolean_expression(B) --> identifier(B).
boolean_expression(elt(I, E)) --> integer(I), ['<'], integer_expression(E).
boolean_expression(egt(I, E)) --> integer(I), ['>'], integer_expression(E).
boolean_expression(ele(I, E)) --> integer(I), ['<='], integer_expression(E).
boolean_expression(ege(I, E)) --> integer(I), ['>='], integer_expression(E).
boolean_expression(eeq(I, E)) --> integer(I), ['=='], integer_expression(E).
boolean_expression(enq(I, E)) --> integer(I), ['!='], integer_expression(E).
boolean_expression(elt(I, E)) --> identifier(I), ['<'], integer_expression(E).
boolean_expression(egt(I, E)) --> identifier(I), ['>'], integer_expression(E).
boolean_expression(ele(I, E)) --> identifier(I), ['<='], integer_expression(E).
boolean_expression(ege(I, E)) --> identifier(I), ['>='], integer_expression(E).
boolean_expression(eeq(I, E)) --> identifier(I), ['=='], integer_expression(E).
boolean_expression(enq(I, E)) --> identifier(I), ['!='], integer_expression(E).

boolean_expression(eeq(B, E)) --> boolean(B), ['=='], boolean_expression(E).
boolean_expression(enq(B, E)) --> boolean(B), ['!='], boolean_expression(E).
boolean_expression(ebc(B, E)) --> boolean(B), ['and'], boolean_expression(E).
boolean_expression(ebd(B, E)) --> boolean(B), ['or'], boolean_expression(E).
boolean_expression(ebx(B, E)) --> boolean(B), ['xor'], boolean_expression(E).
boolean_expression(ebn(E)) --> ['not'], boolean_expression(E).

boolean_expression(eeq(B, E)) --> identifier(B), ['=='], boolean_expression(E).
boolean_expression(enq(B, E)) --> identifier(B), ['!='], boolean_expression(E).
boolean_expression(ebc(B, E)) --> identifier(B), ['and'], boolean_expression(E).
boolean_expression(ebd(B, E)) --> identifier(B), ['or'], boolean_expression(E).
boolean_expression(ebx(B, E)) --> identifier(B), ['xor'], boolean_expression(E).
boolean_expression(ebn(E)) --> ['not'], identifier(E).

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

%
%   Runner
%   These eval predicates are the top level drivers of the interpreter.
%

%  Evaluate a program given a statement list and an empty environment.
eval(program(SL)) :- eval(SL, []).

% Evaluate S in Environment and produce NewEnv, then excecute the
% statement list using NewEnv.
eval(sl(S, SL), Environment) :-
  eval(S, Environment, NewEnv),
  eval(SL, NewEnv).

% A statement that has only one statement has no meaningful effect on the
% environment.
eval(sl(S), Environment) :- eval(S, Environment, _).

% Evaluate a single statement in an environment.
eval(stmt(Expression), Environment, NewEnv) :-
  eval_expr(Expression, Environment, NewEnv).

% Evaluating an expression in isolation has no side effects.
eval(expression(E), _, _) :- eval_expr(E, _).

%
% Environment Definition
%

% Getter.
% Get from the environment.
env(Identifier, Value, [[Identifier, Value] | _]).
env(Identifier, Value, [_ | T]) :- env(Identifier, Value, T).

% Setter.
% If the name cannot be resolved in the given environment, then append it
% and its value.
env(Identifier, Value, [], NewEnv) :-
  NewEnv = [[Identifier, Value]].

% Setter.
% Symbol was found, so now update its value.
env(Identifier, Value, [[Identifier, _] | T], NewEnv) :-
  append([[Identifier, Value]], T, NewEnv).

% Setter
% Symbol not yet found. Attempt to find and update it which will return
% an updated tail environment that must be appended to the old head
% environment.
env(Identifier, Value, [H | T], NewEnv) :-
  env(Identifier, Value, T, InterimEnv),
  append([H], InterimEnv, NewEnv).

%
% Expression Evaluation Predicates
%

% Assignment
% Build the tree node for id into a single string and use it to update
% that symbol's value using the given right hand expression.
eval_expr(ea(I, E), Environment, NewEnv) :-
  build_symbol(I, Identifier),
  eval_expr(E, Environment, Value),
  env(Identifier, Value, Environment, NewEnv).

% Evaluate integers.
eval_expr(int(d(D)), _, D).
eval_expr(int(D, I), _, R) :-
  build_integer(int(D, I), R).

build_integer(int(d(D)), D).
build_integer(int(d(D), I), R) :-
  build_integer(I, I1),
  atom_length(I1, Length),
  R is D * 10 ** Length + I1.

% Evaluate unary minus (negative integers).
eval_expr(neg(I), Env, R) :-
    eval_expr(I, Env, R1),
    R is R1 * -1.

% Evaluate addition expressions.
eval_expr(ep(E1, E2), Env, R) :-
    eval_expr(E1, Env, R1),
    eval_expr(E2, Env, R2),
    R is R1 + R2.

% Evaluate multiplication expressions.
eval_expr(em(E1, E2), Env, R) :-
    eval_expr(E1, Env, R1),
    eval_expr(E2, Env, R2),
    R is R1 * R2.

% Evaluate subtraction expression.
eval_expr(es(E1, E2), Env, R) :-
    eval_expr(E1, Env, R1),
    eval_expr(E2, Env, R2),
    R is R1 - R2.

% Evaluate division expressions.
eval_expr(ed(E1, E2), Env, R) :-
    eval_expr(E1, Env, R1),
    eval_expr(E2, Env, R2),
    R is div(R1, R2). % We must use div as / can yield a real number.

% Evaluate modulus expressions.
eval_expr(er(E1, E2), Env, R) :-
    eval_expr(E1, Env, R1),
    eval_expr(E2, Env, R2),
    R is R1 mod R2.

% Evalute boolean expressions.
eval_expr(bool(B), _, bool(B)).

% Conjunction.
eval_expr(ebc(bool('true'), bool('true')), _, bool('true')).
eval_expr(ebc(bool('true'), bool('false')), _, bool('false')).
eval_expr(ebc(bool('false'), bool('true')), _, bool('false')).
eval_expr(ebc(bool('false'), bool('false')), _, bool('false')).
eval_expr(ebc(E1, E2), Env, B) :-
  eval_expr(E1, Env, R1),
  eval_expr(E2, Env, R2),
  eval_expr(ebc(R1, R2), Env, B).

% Disjunction.
eval_expr(ebd(bool('false'), bool('false')), _, bool('false')).
eval_expr(ebd(bool('false'), bool('true')), _, bool('true')).
eval_expr(ebd(bool('true'), bool('false')), _, bool('true')).
eval_expr(ebd(bool('true'), bool('true')), _, bool('true')).
eval_expr(ebd(E1, E2), Env, B) :-
  eval_expr(E1, Env, R1),
  eval_expr(E2, Env, R2),
  eval_expr(ebd(R1, R2), Env, B).

% Negation.
eval_expr(ebn(bool('true')), _, bool('false')).
eval_expr(ebn(bool('false')), _, bool('true')).
eval_expr(ebn(E), Env, B) :-
  eval_expr(E, Env, R),
  eval_expr(ebn(R), Env, B).

% Exclusive Disjunction.
eval_expr(ebx(bool('false'), bool('false')), _, bool('false')).
eval_expr(ebx(bool('false'), bool('true')), _, bool('true')).
eval_expr(ebx(bool('true'), bool('false')), _, bool('true')).
eval_expr(ebx(bool('true'), bool('true')), _, bool('false')).
eval_expr(ebx(E1, E2), Env, B) :-
  eval_expr(E1, Env, R1),
  eval_expr(E2, Env, R2),
  eval_expr(ebx(R1, R2), Env, B).

% Less than.
eval_expr(elt(I, E), Env, bool('true')) :-
  eval_expr(I, Env, R1),
  eval_expr(E, Env, R2),
  R1 < R2.
eval_expr(elt(I, E), Env, bool('false')) :-
  eval_expr(I, Env, R1),
  eval_expr(E, Env, R2),
  R1 >= R2.

% Less than or equal.
eval_expr(ele(I, E), Env, bool('true')) :-
  eval_expr(I, Env, R1),
  eval_expr(E, Env, R2),
  R1 =< R2.
eval_expr(ele(I, E), Env, bool('false')) :-
  eval_expr(I, Env, R1),
  eval_expr(E, Env, R2),
  R1 > R2.

% Greater than.
eval_expr(egt(I, E), Env, bool('true')) :-
  eval_expr(I, Env, R1),
  eval_expr(E, Env, R2),
  R1 > R2.
eval_expr(egt(I, E), Env, bool('false')) :-
  eval_expr(I, Env, R1),
  eval_expr(E, Env, R2),
  R1 =< R2.

% Greater than or equal.
eval_expr(ege(I, E), Env, bool('true')) :-
  eval_expr(I, Env, R1),
  eval_expr(E, Env, R2),
  R1 >= R2.
eval_expr(ege(I, E), Env, bool('false')) :-
  eval_expr(I, Env, R1),
  eval_expr(E, Env, R2),
  R1 < R2.

% Equal.
eval_expr(eeq(I, E), Env, bool('true')) :-
  eval_expr(I, Env, R1),
  eval_expr(E, Env, R2),
  R1 == R2.
eval_expr(eeq(I, E), Env, bool('false')) :-
  eval_expr(I, Env, R1),
  eval_expr(E, Env, R2),
  R1 \= R2.

% Not equal.
eval_expr(enq(I, E), Env, bool('true')) :-
  eval_expr(I, Env, R1),
  eval_expr(E, Env, R2),
  R1 \= R2.
eval_expr(enq(I, E), Env, bool('false')) :-
  eval_expr(I, Env, R1),
  eval_expr(E, Env, R2),
  R1 == R2.

% Evaluate an identifier to its value using the provided environment.
eval_expr(I, Env, Value) :-
  build_symbol(I, Identifier),
  env(Identifier, Value, Env).

% Evaluate an identifier to a single symbol
build_symbol(l(L), Character) :- Character = L.
build_symbol(id(l(L)), Character) :- Character = L.

build_symbol(id(L, I), R) :-
  build_symbol(L, Head),
  build_symbol(I, Tail),
  string_concat(Head, Tail, R).

% Print.
eval_expr(print(E), Env, Env) :-
  eval_expr(E, Env, R),
  write(R), nl.
