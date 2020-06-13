:- op(300,fx,¬). %operador que marca que un elemento debe ser eliminado
:- op(300,fx,@). %operador que marca que un elemento debe aumentar su tamaño
:- dynamic tabla/3.

desplazar(Dir,Num,Cant,Tablero,EvolTablero):- guardarTablero(Tablero),mover(Dir,Num,Cant),combinarElementos(EvolTablero).

% guardarTablero (+Tablero).
% guarda todos los elementos de la lista como predicados de la forma
% tabla/3, pasa 1 como numero de fila al predicado pasarAHechos ya que
% la primer fila en pasarse a hechos es la numero 1.
guardarTablero(Tablero):-pasarAHechos(Tablero,1).

pasarAHechos([],_).
pasarAHechos([FilaActual|Filas],NumFila):-pasarFila(FilaActual,NumFila,1),SigFila is (NumFila+1),pasarAHechos(Filas,SigFila).

pasarFila([],_,_).
pasarFila([Elem1|Elementos],NumFila,NumColumna):-pasarElemento(Elem1,NumFila,NumColumna),SigColumna is (NumColumna+1),pasarFila(Elementos,NumFila,SigColumna).

pasarElemento(Elemento,NumFila,NumColumna):-assert(tabla(NumFila,NumColumna,Elemento)).

% pasar():-forall(tabla(X,Y,Z),imprimir),forall(tabla(X,Y,Z),write(Y)),forall(tabla(X,Y,Z),write(Z)).


%realiza el desplazamiento correspondiente sobre el tablero almacenado

mover(izq,Fila,N):-X is 0-N,desplazarFila(Fila,X).
mover(der,Fila,N):-desplazarFila(Fila,N).
mover(arriba,Col,N):-X is 0-N,desplazarCol(Col,X).
mover(abajo,Col,N):-desplazarCol(Col,N).

desplazarFila(F,N):-forall(tabla(F,C,E),(retract(tabla(F,C,N)),NuevaColumna is ((N+C) mod 4), assert(tabla(F,NuevaColumna,E)))).

desplazarCol(C,N):-forall(tabla(F,C,E),(retract(tabla(F,C,N)),NuevaFila is ((N+F) mod 4), assert(tabla(NuevaFila,C,E)))).

elimitarTodo:-forall(tabla(X,Y,Z),retract(tabla(X,Y,Z))).






combinarElementos(_).


