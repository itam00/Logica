:- op(300,fx,�). %operador que marca que un elemento debe ser eliminado
:- op(300,fx,@). %operador que marca que un elemento debe aumentar su tama�o
:- dynamic tabla/3.

desplazar(Dir,Num,Cant,Tablero,EvolTablero):- guardarTablero(Tablero),mover(Dir,Num,Cant),combinarElementos(EvolTablero).

% guardarTablero (+Tablero).
% guarda todos los elementos de la lista como predicados de la forma
% tabla/3, pasa 1 como numero de fila al predicado pasarAHechos ya que
% la primer fila en pasarse a hechos es la numero 1.
guardarTablero(Tablero):-pasarAHechos(Tablero,1).
guardarTablero(_):-dynamic tablero/3.

pasarAHechos([],_).
pasarAHechos([FilaActual|Filas],NumFila):-pasarFila(FilaActual,NumFila,1),SigFila is NumFila+1,pasarAHechos(Filas,SigFila).

pasarFila([],_,_).
pasarFila([Elem1|Elementos],NumFila,NumColumna):-pasarElemento(Elem1,NumFila,NumColumna),SigColumna is NumColumna+1,pasarFila(Elementos,NumFila,SigColumna).

pasarElemento(Elemento,NumFila,NumColumna):-assert(tabla(NumFila,NumColumna,Elemento)).

pasar():-forall(tabla(X,Y,Z),imprimir(X,Y,Z)).

imprimir(X,Y,Z):-write(' La fila es: '),write(X),write(' La columna es: '),write(Y),write(' La mu�eca es:'),writeln(Z).


%realiza el desplazamiento correspondiente sobre el tablero almacenado
mover(_,_,_).


combinarElementos(_).

pasarTableroAListas(Fila,[X|Xs]):-tabla(Fila,_,_),pasarFilaALista(Fila,1,X),NuevaFila is Fila+1,pasarTableroAListas(NuevaFila,Xs).
pasarTableroAListas(_,[]).

%pasarFilaALista(+Fila,-Res)
pasarFilaALista(Fila,Col,[X|Xs]):-tabla(Fila,Col,X),NuevaCol is Col+1,pasarFilaALista(Fila,NuevaCol,Xs).
pasarFilaALista(_,_,[]).
