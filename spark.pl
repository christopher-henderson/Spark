int([0], _).
int --> \+ [0], int_.
int_ --> digit, int_.
int_ --> digit.
digit --> [0].
digit --> [1].
digit --> [2].
digit --> [3].
digit --> [4].
digit --> [5].
digit --> [6].
digit --> [7].
digit --> [8].
digit --> [9].

addition_symbol --> [+].
subtraction_symbol --> [-].
multiplication_symbol --> [*].
division_symbol --> [/].
% modulus_symbol --> ["%"].

expression --> terminal, expression.
expression --> terminal.

terminal --> add.
terminal --> subtract.
terminal --> multiply.
terminal --> divide.

add --> int, addition_symbol, expression.
add --> int.

subtract --> int, subtraction_symbol, expression.
subtract --> int.

multiply --> int, multiplication_symbol, expression.
multiply --> int.

divide --> int, division_symbol, expression.
divide --> int.
