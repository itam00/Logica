:- op(300,fx,¬). %operador que marca que un elemento debe ser eliminado
:- op(300,fx,@). %operador que marca que un elemento debe aumentar su tamaño


desplazar(Dir,Num,Cant,Tablero,EvolTablero):- guardarTablero(Tablero),mover(Dir,Num,Cant),combinarElementos(EvolTablero).

% guarda todos los elementos de la lista como predicados de la forma
% tabla/3
guardarTablero(_):-dynamic tablero/3.

%realiza el desplazamiento correspondiente sobre el tablero almacenado
mover(_,_,_).


combinarElementos(_).


