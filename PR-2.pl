:- op(300,fx,/). %operador que marca que un elemento debe ser eliminado

:- op(300,fx,~). %operador que marca que un elemento debe aumentar su tama�o
:- dynamic tabla/3.

desplazar(Dir,Num,Cant,Tablero,_EvolTablero):- guardarTablero(Tablero),Desplazamiento is Num-1,mover(Dir,Desplazamiento,Cant).

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

pasar:-forall(tabla(X,Y,Z),imprimir(X,Y,Z)).

imprimir(X,Y,Z):-write(' La fila es: '),write(X),write(' La columna es: '),write(Y),write(' La mu�eca es:'),writeln(Z).

%realiza el desplazamiento correspondiente sobre el tablero almacenado

mover(izq,Fila,N):-X is 5-N,desplazarFila(Fila,X).
mover(der,Fila,N):- desplazarFila(Fila,N).
mover(arriba,Col,N):-X is 5-N,desplazarCol(Col,X).
mover(abajo,Col,N):-desplazarCol(Col,N).



desplazarCol(C,N):-forall(tabla(F,C,E),(retract(tabla(F,C,E)),NuevaFila is ((N+F) mod 5), assert(tabla(NuevaFila,C,E)))),combinarElementosCol(C),eliminarColapsados.

desplazarFila(F,N):-forall(tabla(F,C,E),(retract(tabla(F,C,E)),NuevaColumna is ((N+C) mod 5), assert(tabla(F,NuevaColumna,E)))),combinarElementosFil(F),eliminarColapsados.



combinarElementosCol(Col):-forall(member(X,[0,1,2,3,4]),buscarCombFila(X)),buscarCombColumna(Col),marcarColapsoCol(Col),marcarColapsoColumnaDes(0,Col).
combinarElementosFil(Fil):-forall(member(X,[0,1,2,3,4]),buscarCombColumna(X)),buscarCombFila(Fil),marcarColapsoFil(Fil),marcarColapsoFilaDes(Fil,0).

% aca falta lo que hace todo en bucle, solo hay que recorrer todas las
% filas y columnas, marcar y dps agregar los colapsar hasta que no se
% puede eliminar nada
%

%de aca para abajo es todo para encontrar y marcar combiaciones

eliminarTodo:-forall(tabla(X,Y,Z),retract(tabla(X,Y,Z))).

eliminarColapsados:-forall(tabla(F,C,~Z),(retract(tabla(F,C,~Z)),imprimirTablero,gravedad(F,C))).

gravedad(F,C):-forall((tabla(Fila,C,X),Fila=<F),(retract(tabla(Fila,C,X)),NuevaFila is Fila+1,assert(tabla(NuevaFila,C,X)),writeln(X))),random_member(Random, [a1,v1,r1]),assert(tabla(0,C,x)).

imprimirTablero:-pasarTableroAListas(0,R),writeln(R).

pasarTableroAListas(Fila,[X|Xs]):-tabla(Fila,_,_),pasarFilaALista(Fila,0,X),NuevaFila is Fila+1,pasarTableroAListas(NuevaFila,Xs).
pasarTableroAListas(_,[]).

%pasarFilaALista(+Fila,-Res)
pasarFilaALista(Fila,Col,[X|Xs]):-tabla(Fila,Col,X),NuevaCol is Col+1,pasarFilaALista(Fila,NuevaCol,Xs).
pasarFilaALista(_,_,[]).



buscarCombFila(Fil):-buscarDerecha(Fil,0).

buscarDerecha(Fil,Col):-buscarDerechaAux(Fil,Col,_,0). %no se llama al recursivo ya que solo puede haber una combinacion por col
buscarDerecha(Fil,Col):-Col<5,NuevaCol is Col+1,buscarDerecha(Fil,NuevaCol).
buscarDerecha(_,_).

buscarDerechaAux(Fil,Col,E,Encontrados):-tabla(Fil,Col,~E),Nuevos is Encontrados+1,NuevaCol is Col+1,!,buscarDerechaAux(Fil,NuevaCol,E,Nuevos),marcar(Fil,Col).
buscarDerechaAux(Fil,Col,E,Encontrados):-tabla(Fil,Col,E),Nuevos is Encontrados+1,NuevaCol is Col+1,!,buscarDerechaAux(Fil,NuevaCol,E,Nuevos),marcar(Fil,Col).
buscarDerechaAux(_,_,_,Encontrados):-Encontrados>2.


buscarCombColumna(Col):-buscarAbajo(0,Col).

buscarAbajo(Fil,Col):-buscarAbajoAux(Fil,Col,_,0). %no se llama al recursivo ya que solo puede haber una combiacion por col
buscarAbajo(Fil,Col):-Fil<5,NuevaFil is Fil+1,buscarAbajo(NuevaFil,Col).
buscarAbajo(_,_).

buscarAbajoAux(Fil,Col,E,Encontrados):-tabla(Fil,Col,~E),N is Encontrados+1,Sig is Fil+1,!,buscarAbajoAux(Sig,Col,E,N),marcar(Fil,Col).
buscarAbajoAux(Fil,Col,E,Encontrados):-tabla(Fil,Col,E),N is Encontrados+1,Sig is Fil+1,!,buscarAbajoAux(Sig,Col,E,N),marcar(Fil,Col).
buscarAbajoAux(_,_,_,Encontrados):-Encontrados>2.


marcar(Fil,Col):- retract(tabla(Fil,Col,~E)),assert(tabla(Fil,Col,/E)).
marcar(Fil,Col):-retract(tabla(Fil,Col,E)),assert(tabla(Fil,Col,~E)).

marcado(~_).
marcado(/_).


marcarColapsoFil(Fil):-marcarColapsoFil(Fil,0,_).

marcarColapsoFil(Fil,Col,E):-tabla(Fil,Col,~X),E\=X,marcarCentroFil(Fil,Col,E,1).
marcarColapsoFil(Fil,Col,_):-Col<4,tabla(Fil,Col,/E),Sig is Col+1,marcarColapsoFil(Fil,Sig,E).
marcarColapsoFil(Fil,Col,E):-Col<4,Sig is Col+1,marcarColapsoFil(Fil,Sig,E).
marcarColapsoFil(_,_,_).

% solo se verifica hasta la tercera columna ya que no van a haber
% combiaciones de 3 si no hay un elemento marcado en la tercera columna
% Si se encuentra un elemento con marca de colapsar se envia por
% parametro para evitar agregar otra marca de colapsar

marcarCentroFil(Fil,Col,E,Cant):-Col<6,Sig is Col+1,tabla(Fil,Sig,~E),C is Cant+1,marcarCentroFil(Fil,Sig,E,C).%falla cuando llega al final o encuentra aldo distinto
marcarCentroFil(Fil,Col,E,_):-tabla(Fil,Col,/E).%si la combiacion ya estaba marcada entonces no se debe marcar el centro
marcarCentroFil(Fil,Col,_,Cant):-Cant>2,Centro is Col - (Cant//2),marcar(Fil,Centro).


marcarColapsoCol(Fil):-marcarColapsoCol(Fil,0,_).

marcarColapsoCol(Fil,Col,E):-tabla(Fil,Col,~X),E\=X,marcarCentroCol(Fil,Col,E,1).
marcarColapsoCol(Fil,Col,_):-Fil<4,tabla(Fil,Col,/E),Sig is Fil+1,marcarColapsoCol(Sig,Col,E).
marcarColapsoCol(Fil,Col,E):-Fil<4,Sig is Fil+1,marcarColapsoCol(Sig,Col,E).
marcarColapsoCol(_,_,_).

% solo se verifica hasta la tercera columna ya que no van a haber
% combiaciones de 3 si no hay un elemento marcado en la tercera columna
% Si se encuentra un elemento con marca de colapsar se envia por
% parametro para evitar agregar otra marca de colapsar

marcarCentroCol(Fil,Col,E,Cant):-Fil<6,Sig is Fil+1,tabla(Sig,Col,~E),C is Cant+1,marcarCentroCol(Sig,Col,E,C).%falla cuando llega al final o encuentra aldo distinto
marcarCentroCol(Fil,Col,E,_):-tabla(Fil,Col,/E).%si la combiacion ya estaba marcada entonces no se debe marcar el centro
marcarCentroCol(Fil,Col,_,Cant):-Cant>2,Centro is Fil - (Cant//2),marcar(Centro,Col).



% Recorre las filas fijandose, si el elemento en esa fila y columna esta
% marcado, si arriba o abajo hay otro elemento marcado, para poner el
% colapso ahi.
marcarColapsoFilaDes(Fil,Col):-tabla(Fil,Col,~M),verificarCombinacionFilaDes(Fil,Col,~M),marcar(Fil,Col).
marcarColapsoFilaDes(Fil,Col):-Fil<5,NuevaFila is Fil+1,marcarColapsoFilaDes(NuevaFila,Col).
marcarColapsoFilaDes(_,_).

verificarCombinacionColumnaDes(Fil,Col,Marcada):-ColNueva is Col+1,tabla(Fil,ColNueva,Marcada).
verificarCombinacionColumnaDes(Fil,Col,Marcada):-ColNueva is Col-1,tabla(Fil,ColNueva,Marcada).

marcarColapsoColumnaDes(Fil,Col):-tabla(Fil,Col,~M),verificarCombinacionColumnaDes(Fil,Col,~M),marcar(Fil,Col).
marcarColapsoColumnaDes(Fil,Col):-Col<5,NuevaCol is Col+1,marcarColapsoColumnaDes(Fil,NuevaCol).
marcarColapsoColumnaDes(_,_).

verificarCombinacionFilaDes(Fil,Col,Marcada):-FilNueva is Fil+1,tabla(FilNueva,Col,Marcada).
verificarCombinacionFilaDes(Fil,Col,Marcada):-FilNueva is Fil-1,tabla(FilNueva,Col,Marcada).
