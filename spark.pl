% First block of lines represent the parser
exp(ep(T,E)) --> term(T), ['+'], exp(E).
exp(em(T,E)) --> term(T), ['*'], exp(E).
exp(es(T,E)) --> term(T), ['-'], exp(E).
exp(ed(T,E)) --> term(T), ['/'], exp(E).
exp(er(T,E)) --> term(T), ['%'], exp(E).
exp(et(T)) --> term(T).

term(t(0)) --> [0].
term(t(1)) --> [1].
term(t(2)) --> [2].
term(t(3)) --> [3].
term(t(4)) --> [4].
term(t(5)) --> [5].
term(t(6)) --> [6].
term(t(7)) --> [7].
term(t(8)) --> [8].
term(t(9)) --> [9].

% The next few lines are the evaluation functions
ev_expr(ep(T, E),R) :- 	ev_term(T, R1),
    				  	ev_expr(E, R2),
    					R is R1 + R2.
ev_expr(em(T, E),R) :- 	ev_term(T, R1),
    				  	ev_expr(E, R2),
    					R is R1 * R2.
ev_expr(es(T, E),R) :- 	ev_term(T, R1),
    				  	ev_expr(E, R2),
    					R is R1 - R2.
ev_expr(ed(T, E),R) :- 	ev_term(T, R1),
    				  	ev_expr(E, R2),
    					R is R1 / R2.
ev_expr(er(T, E),R) :- 	ev_term(T, R1),
    				  	ev_expr(E, R2),
    					R is R1 mod R2.
ev_expr(et(T),R)	:- 	ev_term(T, R).
ev_term(t(T), T).

% Query starts here eg. eval([7,'-',3],X)
eval(Exp, Result) 	:- 	exp(T, Exp, []),
    					          ev_expr(T, Result).
