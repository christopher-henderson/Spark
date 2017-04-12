% exp(integer(Integer)) --> int(integer).

% Single zero is fine.
int(int(digit(0))) --> [0].
% Negative zero is just zero.
int(int(digit(0))) --> [-], [0].
% Otherwise, no integer can begin with a zero.
int(Integer) --> \+ [0], int_(Integer).
% Negative integers are integers that start with "-"
int_(negative(Integer)) --> [-], int(Integer).
% We use int_ here because now we can have arbitrary zeroes without
% worrying about the leading zero case.
int_(int(H, T)) --> digit(H), int_(T).
int_(Digit) --> digit(Digit).

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

% ev_expr(int(Integer), R) :- ev_term(T, R1),


ev_expr(addition(T, E), R) :- 	ev_term(T, R1),
    				  	ev_expr(E, R2),
    					R is R1 + R2.

ev_term(terminal(T), T).

% Query starts here eg. eval([7,'-',3],X)
eval(Exp, Result) 	:- 	exp(T, Exp, []),
    					ev_expr(T, Result).
