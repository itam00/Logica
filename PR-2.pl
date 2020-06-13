:- op(300,fx,/). %operador que marca que un elemento debe ser eliminado

:- op(300,fx,~). %operador que marca que un elemento debe aumentar su tamaño
:- dynamic tabla/3.

desplazar(Dir,Num,Cant,Tablero,EvolTablero):- guardarTablero(Tablero),Desplazamiento is Num-1,mover(Dir,Desplazamiento,Cant),combinarElementos(EvolTablero).

% guardarTablero (+Tablero).
% guarda todos los elementos de la lista como predicados de la forma
% tabla/3, pasa 1 como numero de fila al predicado pasarAHechos ya que
% la primer fila en pasarse a hechos es la numero 0.
guardarTablero(Tablero):-pasarAHechos(Tablero,0).

pasarAHechos([],_).
pasarAHechos([FilaActual|Filas],NumFila):-pasarFila(FilaActual,NumFila,0),SigFila is (NumFila+1),pasarAHechos(Filas,SigFila).

pasarFila([],_,_).
pasarFila([Elem1|Elementos],NumFila,NumColumna):-pasarElemento(Elem1,NumFila,NumColumna),SigColumna is (NumColumna+1),pasarFila(Elementos,NumFila,SigColumna).

pasarElemento(Elemento,NumFila,NumColumna):-assert(tabla(NumFila,NumColumna,Elemento)).

pasar():-forall(tabla(X,Y,Z),imprimir(X,Y,Z)).

imprimir(X,Y,Z):-write(' La fila es: '),write(X),write(' La columna es: '),write(Y),write(' La muñeca es:'),writeln(Z).

%realiza el desplazamiento correspondiente sobre el tablero almacenado

mover(izq,Fila,N):-X is 5-N,desplazarFila(Fila,X).
mover(der,Fila,N):- desplazarFila(Fila,N).
mover(arriba,Col,N):-X is 5-N,desplazarCol(Col,X).
mover(abajo,Col,N):-desplazarCol(Col,N).



desplazarCol(C,N):-forall(tabla(F,C,E),(retract(tabla(F,C,E)),NuevaFila is ((N+F) mod 5), assert(tabla(NuevaFila,C,E)))).

desplazarFila(F,N):-forall(tabla(F,C,E),(retract(tabla(F,C,E)),NuevaColumna is (N+C mod 5), assert(tabla(F,NuevaColumna,E)))).

desplazarCol(C,N):-forall(tabla(F,C,E),(retract(tabla(F,C,E)),NuevaFila is ((N+F) mod 5), assert(tabla(NuevaFila,C,E)))).



eliminarTodo:-forall(tabla(X,Y,Z),retract(tabla(X,Y,Z))).



eliminarColapsados():-forall(tabla(F,C,~Z),(retract(tabla(F,C,~Z)),gravedad(F,C))).

gravedad(F,C):-forall((tabla(Fila,C,X),Fila=<F),(retract(tabla(Fila,C,X)),NuevaFila is Fila+1,assert(tabla(NuevaFila,C,X)))),assert(tabla(0,C,x1)).



combinarElementos(_).

pasarTableroAListas(Fila,[X|Xs]):-tabla(Fila,_,_),pasarFilaALista(Fila,0,X),NuevaFila is Fila+1,pasarTableroAListas(NuevaFila,Xs).
pasarTableroAListas(_,[]).

%pasarFilaALista(+Fila,-Res)
pasarFilaALista(Fila,Col,[X|Xs]):-tabla(Fila,Col,X),NuevaCol is Col+1,pasarFilaALista(Fila,NuevaCol,Xs).
pasarFilaALista(_,_,[]).



buscarCombFila(Fil):-buscarDerecha(Fil,0).

buscarDerecha(Fil,Col):-buscarDerechaAux(Fil,Col,_,0). %no se llama al recursivo ya que solo puede haber una combinacion por col
buscarDerecha(Fil,Col):-NuevaCol is Col+1,buscarDerecha(Fil,NuevaCol).
buscarDerecha(_,_).

buscarDerechaAux(Fil,Col,E,Encontrados):-tabla(Fil,Col,E),Nuevos is Encontrados+1,NuevaCol is Col+1,!,buscarDerechaAux(Fil,NuevaCol,E,Nuevos),marcar(Fil,Col).
buscarDerechaAux(_,_,_,Encontrados):-Encontrados>2.


buscarCombColumna(Col):-buscarAbajo(0,Col).

buscarAbajo(Fil,Col):-buscarAbajoAux(Fil,Col,_,0). %no se llama al recursivo ya que solo puede haber una combiacion por col
buscarAbajo(Fil,Col):-NuevaFil is Fil+1,buscarAbajo(NuevaFil,Col).
buscarAbajo(_,_).

buscarAbajoAux(Fil,Col,E,Encontrados):-tabla(Fil,Col,~E),Nuevos is Encontrados+1,NuevaFil is Fil+1,!,buscarAbajoAux(NuevaFil,Col,E,Nuevos),marcar(Fil,Col).
buscarAbajoAux(Fil,Col,E,Encontrados):-tabla(Fil,Col,E),Nuevos is Encontrados+1,NuevaFil is Fil+1,!,buscarAbajoAux(NuevaFil,Col,E,Nuevos),marcar(Fil,Col).

buscarAbajoAux(_,_,_,Encontrados):-Encontrados>2.


marcar(Fil,Col):- retract(tabla(Fil,Col,~E)),assert(tabla(Fil,Col,/E)).
marcar(Fil,Col):-retract(tabla(Fil,Col,E)),assert(tabla(Fil,Col,~E)).

marcado(~_).
