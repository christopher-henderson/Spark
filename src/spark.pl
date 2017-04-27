:- discontiguous eval_expr/3.
:- discontiguous eval_expr/2.
:- discontiguous eval/2.
:- discontiguous eval/3.

% Reads in a Spark source file and "executes" the program.
%
%                        +-------+                           +-------+
%   +---[Source file]--->| run/1 |---[List of characters]--->| lexer |---+
%                        +-------+                           +-------+   |
%                                                                        |
%   +-------------------------[List of tokens]---------------------------+
%   |
%   |     +--------+                     +-------------+
%   +---->| parser |----[Parse tree]---->| interpreter |----[Output]----->
%         +--------+                     +-------------+
%
% Executing the query '?- working_directory(CWD, CWD).' will output the
% current working directory in the Prolog interpreter.

run(Filename) :-
    % Read file into list of characters.
    read_file_to_codes(Filename, Codes, []),
    atom_codes(Atom, Codes),
    atom_chars(Atom, Characters),

    % Tokenize, parse, and evaluate.
    write("Lexing..."), nl,
    lexer(Tokens, Characters, []), !,
    write("Parsing..."), nl,
    parser(Tree, Tokens, []), !,
    % print_term(Tree, []),
    write("Interpreting..."), nl,
    eval(Tree), !.

% ----------------------------------------------------------------------
%   LEXER
% ----------------------------------------------------------------------

% Whitespace characters.
whitespace --> [' '].
whitespace --> ['\t'].
whitespace --> ['\n'].

% Comment delimiters.
comment_single --> ['/', '/'].
comment_multi_start --> ['/', '*'].
comment_multi_end --> ['*', '/'].

% The longest_match_alpha/3 predicate unifies with the next longest string of
% uppercase or lowercase letters in the list of remaining characters.
longest_match_alpha([H|T]) --> [H], longest_match_alpha_(T), {char_type(H, alpha)}.
longest_match_alpha_([H|T]) --> [H], longest_match_alpha_(T), {char_type(H, alpha)}.
longest_match_alpha_([]) --> [].

% The longest_match_int/3 predicate unifies with the next longest string of
% digits in the list of remaining characters.
longest_match_int([H|T]) --> [H], longest_match_int_(T), {char_type(H, digit)}.
longest_match_int_([H|T]) --> [H], longest_match_int_(T), {char_type(H, digit)}.
longest_match_int_([]) --> [].

% The longest_match_no_nl/3 predicate unifies with the next longest string of
% non-newline characters in the list of remaining characters.
longest_match_no_nl([H|T]) --> [H], longest_match_no_nl_(T), {H \== '\n'}.
longest_match_no_nl_([H|T]) --> [H], longest_match_no_nl_(T), {H \== '\n'}.
longest_match_no_nl_([]) --> [].

% The comment/3 predicate is used to consume a variable number of characters.
% This is useful for consuming all characters between multi-line comment delimiters.
comment --> [].
comment --> [_], comment.

% Comments.
lexer(Tokens) --> comment_single, longest_match_no_nl(_), lexer(R), {Tokens = R}.
lexer(Tokens) --> comment_multi_start, comment, comment_multi_end, lexer(R), {Tokens = R}.

% Identifiers and integer literals.
lexer(Tokens) --> longest_match_alpha(I), lexer(R), {atom_chars(IA, I), append([IA], R, Tokens)}.
lexer(Tokens) --> longest_match_int(I), lexer(R), {atom_chars(IA, I), append([IA], R, Tokens)}.

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

% Reserved words.
reserved_words([
    while,
    for,
    if,
    else,
    print,
    true,
    false,
    and,
    or,
    not,
    xor
]).

% Identifiers.
% All atom tokens that are composed entirely of lowercase and uppercase
% letters that are not reserved words are parsed as identifiers.
identifier(id(I)) -->
    [I],
    {
        atom_chars(I, C),
        all_letters(C),
        reserved_words(RW),
        \+ member(I, RW)
    }.
all_letters([H|T]) :- char_type(H, alpha), all_letters(T).
all_letters([]).

% Integers.
% All atom tokens that are composed entirely of digit characters are
% parsed as integers.
integer(int(I)) --> [I], {atom_chars(I, C), all_digits(C)}.
all_digits([H|T]) :- char_type(H, digit), all_digits(T).
all_digits([]).

% Booleans.
boolean(bool('true')) --> ['true'].
boolean(bool('false')) --> ['false'].

% Print to stdout.
print(print(E)) --> ['print'], expression(E).

% Expressions.
expression(I) --> identifier(I).
expression(B) --> boolean_expression(B).
expression(I) --> integer_expression(I).
expression(P) --> print(P).
expression(ea(I, E)) --> identifier(I), ['='], expression(E).
expression(F) --> function_call(F).
expression(F) --> function_declaration(F).
expression(R) --> return(R).

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
statement(stmt(E)) --> branch(E).
statement(stmt(E)) --> loop(E).
statement(stmt(E)) --> expression(E).

% If
branch(if(Cond, SL)) -->
    ['if'], ['('], boolean_expression(Cond), [')'], ['{'],
      statement_list(SL),
    ['}'].

% If-Else
branch(if(Cond, SL, SL2)) -->
    ['if'], ['('], boolean_expression(Cond), [')'], ['{'],
      statement_list(SL),
    ['}'], ['else'], ['{'],
      statement_list(SL2),
    ['}'].

% If-ElseIf
branch(if(Cond, SL, ElseIf)) -->
    ['if'], ['('], boolean_expression(Cond), [')'], ['{'],
      statement_list(SL),
    ['}'], branch(ElseIf).

% ElseIf
branch(elseif(Cond, SL)) -->
  ['else'], ['if'], ['('], boolean_expression(Cond), [')'], ['{'],
    statement_list(SL),
  ['}'].

% ElseIf-ElseIf
branch(elseif(Cond, SL, ElseIf)) -->
  ['else'], ['if'], ['('], boolean_expression(Cond), [')'], ['{'],
    statement_list(SL),
  ['}'], branch(ElseIf).

% ElseIf-Else
branch(elseif(Cond, SL, SL2)) -->
  ['else'], ['if'], ['('], boolean_expression(Cond), [')'], ['{'],
    statement_list(SL),
  ['}'], ['else'], ['{'],
    statement_list(SL2),
  ['}'].

loop(while(Cond, SL)) -->
  ['while'], ['('], boolean_expression(Cond), [')'], ['{'],
  statement_list(SL),
  ['}'].

loop(for(Initializer, Cond, Increment, SL)) -->
  ['for'], ['('], statement(Initializer), [';'], boolean_expression(Cond), [';'], statement(Increment), [')'], ['{'],
    statement_list(SL),
  ['}'].

function_declaration(function(Identifier, IdentifierList, SL)) -->
  ['function'], identifier(Identifier), ['('], identifier_list(IdentifierList), [')'], ['{'],
    statement_list(SL),
  ['}'].

function_declaration(function(Identifier, il(void), SL)) -->
  ['function'], identifier(Identifier), ['('], [')'], ['{'],
    statement_list(SL),
  ['}'].

function_call(function(Identifier, ValueList)) -->
  identifier(Identifier), ['('], value_list(ValueList), [')'].

function_call(function(Identifier, vl(void))) -->
  identifier(Identifier), ['('], [')'].

return(return(E)) -->
  ['return'], expression(E).

% identifier_list(il('')) --> [''].
identifier_list(il(I)) --> identifier(I).
identifier_list(il(I, IL)) --> identifier(I), [','], identifier_list(IL).

value_list(vl(V)) --> expression(V).
value_list(vl(V, VL)) --> expression(V), [','], value_list(VL).

statement_list(sl(S)) --> statement(S).
statement_list(sl(S, SL)) --> statement(S), statement_list(SL).
parser(program(SL)) --> statement_list(SL).

% ----------------------------------------------------------------------
%   INTERPRETER
% ----------------------------------------------------------------------

%
%   Runner
%   These eval predicates are the top level drivers of the interpreter.
%

%  Evaluate a program given a statement list and an empty environment.
eval(program(SL)) :- eval(SL, [], _, _).

eval(stmt(return(E)), Environment, ReturnValue, ExecutionHalted) :-
  eval_expr(E, Environment, ReturnValue),
  ExecutionHalted = halted.

eval(stmt(return(E), _), Environment, ReturnValue, ExecutionHalted) :-
  eval_expr(E, Environment, ReturnValue),
  ExecutionHalted = halted.

eval(stmt(Expression), Environment, NewEnv, ExecutionHalted) :-
  eval_expr(Expression, Environment, NewEnv),
  ExecutionHalted = continue.

% Evaluate S in Environment and produce InterimEnv, then excecute the
% statement list using InterimEnv.
eval(sl(S, SL), Environment, NewEnv, ExecutionHalted) :-
  eval(S, Environment, InterimEnv, continue),
  eval(SL, InterimEnv, NewEnv, ExecutionHalted).

eval(sl(S, _), Environment, NewEnv, ExecutionHalted) :-
  eval(S, Environment, NewEnv, halted),
  ExecutionHalted = halted.

eval(sl(S), Environment, NewEnv, ExecutionHalted) :-
  eval(S, Environment, NewEnv, ExecutionHalted).

% Evaluate a single statement in an environment.
eval(stmt(S), Environment, NewEnv, ExecutionHalted) :-
  eval(S, Environment, NewEnv, ExecutionHalted).

% Evaluate a single statement in an environment.


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
  eval_expr(E, Environment, Value),
  env(I, Value, Environment, NewEnv).

% Evaluate integers.
eval_expr(int(I), _, R) :-
    atom_number(I, R).

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
eval_expr(Identifier, Env, Value) :-
  env(Identifier, Value, Env).

% Print.
eval_expr(print(E), Env, Env) :-
  eval_expr(E, Env, R),
  write(R), nl.

% If true
eval(if(Cond, SL), Env, NewEnv, ExecutionHalted) :-
  eval_expr(Cond, Env, bool('true')),
  eval(SL, Env, NewEnv, ExecutionHalted).

% If false
eval(if(Cond, _), Env, Env, continue) :-
  eval_expr(Cond, Env, bool('false')).

% If true else
eval(if(Cond, SL, _), Env, NewEnv, ExecutionHalted) :-
  eval_expr(Cond, Env, bool('true')),
  eval(SL, Env, NewEnv, ExecutionHalted).

% If false else
eval(if(Cond, _, Else), Env, NewEnv, ExecutionHalted) :-
  eval_expr(Cond, Env, bool('false')),
  eval(Else, Env, NewEnv, ExecutionHalted).

% If false else if
eval(if(Cond, _, ElseIf), Env, NewEnv, ExecutionHalted) :-
  eval_expr(Cond, Env, bool('false')),
  eval(ElseIf, Env, NewEnv, ExecutionHalted).

% Else If false
eval(elseif(Cond, SL), Env, NewEnv, ExecutionHalted) :-
  eval_expr(Cond, Env, bool('true')),
  eval(SL, Env, NewEnv, ExecutionHalted).

% Else if false
eval(elseif(Cond, _), Env, Env, continue) :-
  eval_expr(Cond, Env, bool('false')).

% Else if false
eval(elseif(Cond, _, Else), Env, NewEnv, ExecutionHalted) :-
  eval_expr(Cond, Env, bool('false')),
  eval(Else, Env, NewEnv, ExecutionHalted).

% While loop is true
eval(while(Cond, SL), Env, NewEnv, ExecutionHalted) :-
  eval_expr(Cond, Env, bool('true')),
  eval(SL, Env, InterimEnv, continue),
  eval(while(Cond, SL), InterimEnv, NewEnv, ExecutionHalted).

eval(while(Cond, SL), Env, NewEnv, halted) :-
  eval_expr(Cond, Env, bool('true')),
  eval(SL, Env, NewEnv, halted).

% While loop is false
eval(while(Cond, _), Env, Env, continue) :-
  eval_expr(Cond, Env, bool('false')).

% For loop, first pass succedes conditional.
eval(for(Initializer, Cond, Increment, SL), Env, NewEnv, ExecutionHalted) :-
  eval(Initializer, Env, Env1, continue),
  eval_expr(Cond, Env1, bool('true')),
  eval(SL, Env1, Env2, continue),
  eval(Increment, Env2, Env3, continue),
  eval(for(Cond, Increment, SL), Env3, NewEnv, ExecutionHalted).

eval(for(Initializer, Cond, _, SL), Env, NewEnv, halted) :-
  eval(Initializer, Env, Env1, continue),
  eval_expr(Cond, Env1, bool('true')),
  eval(SL, Env1, NewEnv, halted).

% For loop, first pass fails conditional.
eval(for(Initializer, Cond, _, _), Env, NewEnv, continue) :-
  eval(Initializer, Env, InitializedEnv, continue),
  eval_expr(Cond, InitializedEnv, bool('false')),
  NewEnv = InitializedEnv.

% For loop, after first pass success.
eval(for(Cond, Increment, SL), Env, NewEnv, ExecutionHalted) :-
  eval_expr(Cond, Env, bool('true')),
  eval(SL, Env, Env1, continue),
  eval(Increment, Env1, Env2, continue),
  eval(for(Cond, Increment, SL), Env2, NewEnv, ExecutionHalted).

eval(for(Cond, _, SL), Env, NewEnv, halted) :-
  eval_expr(Cond, Env, bool('true')),
  eval(SL, Env, NewEnv, halted).

% For loop, after first pass fails.
eval(for(Cond, _, _), Env, Env, continue) :-
  eval_expr(Cond, Env, bool('false')).

% Function Declaration
eval_expr(function(Identifier, IdentifierList, SL), Env, NewEnv) :-
  env(Identifier, [IdentifierList, SL], Env, NewEnv).

% Function Call
eval_expr(function(Identifier, ValueList), Env, ReturnValue) :-
  env(Identifier, [IdentifierList, SL], Env),
  bind_parameters(IdentifierList, ValueList, BoundParameters, Env),
  insert_globals(BoundParameters, Env, FinalEnvironment),
  eval(SL, FinalEnvironment, ReturnValue, _).

bind_parameters(il(void), vl(void), Env, Env).
bind_parameters(IL, VL, Target, Env) :-
  bind_parameters(IL, VL, Target, Env, []).
bind_parameters(il(I), vl(V), Target, Env, Accumulator) :-
  eval_expr(V, Env, Value),
  append(Accumulator, [[I, Value]], Target).
bind_parameters(il(I, IL), vl(V, VL), Target, Env, Accumulator) :-
  eval_expr(V, Env, Value),
  append(Accumulator, [[I, Value]], NewAccumulator),
  bind_parameters(IL, VL, Target, Env, NewAccumulator).

% insert_globals([], Env, FinalEnvironment, Accumulator) :-
%   append(Env, Accumulator, FinalEnvironment).
% insert_globals(BoundParameters, [], FinalEnvironment, Accumulator) :-
%   append(BoundParameters, Accumulator, FinalEnvironment).
insert_globals([], Env, Env).
insert_globals([[BName, BValue] | BT], Env, FinalEnvironment) :-
  env(BName, BValue, Env, NewEnv),
  insert_globals(BT, NewEnv, FinalEnvironment).
