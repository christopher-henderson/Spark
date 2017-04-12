% First block of lines represent the parser
exp(ep(T,E)) --> int(T), ['+'], exp(E).
exp(em(T,E)) --> int(T), ['*'], exp(E).
exp(es(T,E)) --> int(T), ['-'], exp(E).
exp(ed(T,E)) --> int(T), ['/'], exp(E).
exp(er(T,E)) --> int(T), ['%'], exp(E).
exp(int(D, I)) --> digit(D), exp(I).
exp(int(D)) --> digit(D).
exp(negative(Integer)) --> [-], exp(Integer).

% Single zero is fine.
int(digit(0)) --> [0].
% Negative zero is just zero.
int(digit(0)) --> [-], [0].
% Otherwise, no integer can begin with a zero.
int(Integer) --> \+ [0], int_(Integer).
% Negative integers are integers that start with "-"
int(negative(Integer)) --> [-], int(Integer).
% We use int_ here because now we can have arbitrary zeroes without
% worrying about the leading zero case.
int_(int(H, T)) --> digit(H), int_(T).
int_(int(Digit)) --> digit(Digit).

digit(digit(0)) --> [0].
digit(digit(1)) --> [1].
digit(digit(2)) --> [2].
digit(digit(3)) --> [3].
digit(digit(4)) --> [4].
digit(digit(5)) --> [5].
digit(digit(6)) --> [6].
digit(digit(7)) --> [7].
digit(digit(8)) --> [8].
digit(digit(9)) --> [9].

ev_expr(int(digit(Digit)), Digit, 10).
ev_expr(int(digit(Digit), Integer), R, X) :-
    ev_expr(Integer, R1, X1),
    X is X1 * 10,
    R is Digit * X1 + R1.
ev_expr(int(digit(D), Integer), R) :-
    ev_expr(Integer, R1, X),
    R is D * X + R1.
ev_expr(int(digit(D)), R) :- R is D.
ev_expr(negative(Integer), R) :-
  ev_expr(Integer, R1),
  R is R1 * -1.

% The next few lines are the evaluation functions
ev_expr(ep(T, E),R) :-
    ev_expr(T, R1),
    ev_expr(E, R2),
    R is R1 + R2.
ev_expr(em(T, E),R) :-
    ev_expr(T, R1),
    ev_expr(E, R2),
    R is R1 * R2.
ev_expr(es(T, E),R) :-
    ev_expr(T, R1),
    ev_expr(E, R2),
    R is R1 - R2.
ev_expr(ed(T, E),R) :-
    ev_expr(T, R1),
    ev_expr(E, R2),
    R is R1 / R2.
ev_expr(er(T, E),R) :-
    ev_expr(T, R1),
    ev_expr(E, R2),
    R is R1 mod R2.
ev_expr(et(T),R) :- 	ev_expr(T, R).
ev_expr(t(T), T).

% Query starts here eg. eval([7,'-',3],X)
eval(Exp, Result) :-
    exp(T, Exp, []),
    ev_expr(T, Result).
