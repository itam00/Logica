:- op(300,fx,/). %operador que marca que un elemento debe ser eliminado

:- op(300,fx,~). %operador que marca que un elemento debe aumentar su tamaño

:- dynamic tabla/3.
:- dynamic evol/1.


desplazar(Dir,Num,Cant,Tablero,EvolTablero):- guardarTablero(Tablero),Desplazamiento is Num-1,mover(Dir,Desplazamiento,Cant),recuperarTableros(EvolTablero),writeln(' '),imprimirTablero,eliminarTodo.%bucleCombinacionesAux.

% recuperarTableros/1
% Mete en una lista todos los tableros guardados en forma de hechos
% evol(Tablero) y luegos los elimina (usando
% retract), y devuelve esta lista.
% recuperarTableros(-Lista).
recuperarTableros(Lista):-findall(Tablero,evol(Tablero),Lista),forall(evol(X),retract(evol(X))).

% guardarTablero (+Tablero).
% guarda todos los elementos de la lista como predicados de la forma
% tabla/3, pasa 1 como numero de fila al predicado pasarAHechos ya que
% la primer fila en pasarse a hechos es la numero 0.
guardarTablero(Tablero):-pasarAHechos(Tablero,0).



% pasarAHechos/2
% pasarAHechos(+ListaDeFilas,+NumFila).
% pasa cada fila dentro de la lista de filas a hechos, empezando por la
% fila NumFila.
pasarAHechos([],_).
pasarAHechos([FilaActual|Filas],NumFila):-pasarFila(FilaActual,NumFila,0),SigFila is (NumFila+1),pasarAHechos(Filas,SigFila).

% pasarFila/3
% pasarFila(+Fila,+NumFila,NumColumna)
% pasarFila(Fila,NumFila,NumColumna)
% pasa cada elemento de la fila a hechos, indicando el numero de fila y
% numero de columna donde se encuentra el elemento.
pasarFila([],_,_).
pasarFila([Elem1|Elementos],NumFila,NumColumna):-pasarElemento(Elem1,NumFila,NumColumna),SigColumna is (NumColumna+1),pasarFila(Elementos,NumFila,SigColumna).

% pasarElemento/3
% pasarElemento(+Elemento,+NumFila,+NumColumna).
% pasa el elemento en la fila NumFila y columna NumColumna a un hecho
% tabla(Elemento,NumFila,NumColumna)
pasarElemento(Elemento,NumFila,NumColumna):-assert(tabla(NumFila,NumColumna,Elemento)).

pasar:-forall(tabla(X,Y,Z),imprimir(X,Y,Z)).

imprimir(X,Y,Z):-write(' La fila es: '),write(X),write(' La columna es: '),write(Y),write(' La muñeca es:'),writeln(Z).

% mover/3
% realiza el desplazamiento correspondiente sobre el tablero almacenado
% mueve la fila o columna pasada por parametr en la direccion indicada N
% lugares
% mover(+Direccion,+(Fila o columna),+Cantidad)
mover(izq,Fila,N):-X is 5-N,desplazarFila(Fila,X).
mover(der,Fila,N):- desplazarFila(Fila,N).%,eliminarColapsadosAux,bucleCombinacionesAux.
mover(arriba,Col,N):-X is 5-N,desplazarCol(Col,X).%,eliminarColapsadosAux,bucleCombinacionesAux.
mover(abajo,Col,N):-desplazarCol(Col,N).%,eliminarColapsadosAux,bucleCombinacionesAux.



desplazarCol(C,N):-forall(tabla(F,C,E),(retract(tabla(F,C,E)),NuevaFila is ((N+F) mod 5), assert(tabla(NuevaFila,C,E)))),guardarEvol,combinarElementosCol(C).
desplazarFila(F,N):-forall(tabla(F,C,E),(retract(tabla(F,C,E)),NuevaColumna is ((N+C) mod 5), assert(tabla(F,NuevaColumna,E)))),guardarEvol,combinarElementosFil(F).
cantElem:-forall(tabla(_,_,Z),writeln(Z)).

combinarElementosCol(Col):-forall(member(X,[0,1,2,3,4]),buscarCombFila(X)),buscarCombColumna(Col),marcarColapsoColumnaDes(0,Col),marcarColapsoCol(Col),agrandarColapsados,guardarEvol,eliminarColapsadosAux.
combinarElementosFil(Fil):-forall(member(X,[0,1,2,3,4]),buscarCombColumna(X)),buscarCombFila(Fil),marcarColapsoFilaDes(Fil,0),marcarColapsoFil(Fil),agrandarColapsados,guardarEvol,eliminarColapsadosAux.

bucleCombinacionesAux(TableroViejo):-pasarTableroAListas(0,TableroNuevo),TableroViejo=TableroNuevo.
bucleCombinacionesAux(_):-imprimirTablero,bucleCombinaciones,pasarTableroAListas(0,TableroNuevo),bucleCombinacionesAux(TableroNuevo).

bucleCombinaciones:-forall(member(X,[0,1,2,3,4]),(buscarCombFila(X),buscarCombColumna(X),marcarColapsoFil(X),marcarColapsoCol(X))),eliminarColapsadosAux.

% aca falta lo que hace todo en bucle, solo hay que recorrer todas las
% filas y columnas, marcar y dps agregar los colapsar hasta que no se
% puede eliminar nada
%

%de aca para abajo es todo para encontrar y marcar combiaciones


% eliminarTodo/0
% elimina todos los hechos tabla/3
eliminarTodo:-forall(tabla(X,Y,Z),retract(tabla(X,Y,Z))).

% eliminarColapsadosAux/0
% Elimina todos los elementos del tablero marcados con ~ y guarda el
% tablero resultante después de haberlos eliminado todos.
eliminarColapsadosAux:-eliminarColapsados,eliminarColapsadosAux.
eliminarColapsadosAux:-guardarEvol.%,reemplazarPorRandom,guardarEvol.


% eliminarColapsados/0
% elimina un elemento del tablero marcado con ~ y aplica gravedad para
% que los elementos que habia arriba de el caigan tapando el lugar
% "vacio" resultado de eliminarlo.
eliminarColapsados:-tabla(F,C,~Z),retract(tabla(F,C,~Z)),gravedad(F,C).


% eliminarColapsados:-forall(tabla(F,C,~Z),(retract(tabla(F,C,~Z)),imprimirTablero,gravedad(F,C))).
%

% gravedad/2
% gravedad(+F,+C).
% mueve todos los elementos arriba de una fila y columna pasados por
% parametro una fila para abajo, ya que el elemento que antes se
% encontraba en esa fila y columna fue eliminado. A demás agrega una x
% (que representa un lugar vacio), en la parte mas alta de la columna ya
% que ese lugar debe ser rellenado con un nuevo elemento)
gravedad(F,C):-forall((tabla(Fila,C,X),Fila=<F),(retract(tabla(Fila,C,X)),NuevaFila is Fila+1,assert(tabla(NuevaFila,C,X)),writeln(X))),assert(tabla(0,C,x)).

reemplazarPorRandom:-forall(tabla(X,Y,x),(retract(tabla(X,Y,x)),random_member(Random, [a1,v1,r1]),assert(tabla(X,Y,Random)))).

% imprimirTablero/0
% Muestra el tablero actual en formato lista de listas.
imprimirTablero:-pasarTableroAListas(0,R),writeln(R).

% pasarTableroAListas/2
% pasarTableroAListas(+Fila,-Lista)
% Devuelve una Lista de Listas en Lista que representa el tablero
% actual, donde cada sublista es una fila que contiene los elementos del
% tablero ubicados en esa fila y ordenados por columna (el primer
% elemento está en la columna 1, el segundo en la 2 y así
% sucesivamente).
pasarTableroAListas(Fila,[X|Xs]):-tabla(Fila,_,_),pasarFilaALista(Fila,0,X),NuevaFila is Fila+1,pasarTableroAListas(NuevaFila,Xs).
pasarTableroAListas(_,[]):-!.

% pasarFilaALista/3
% pasarFilaALista(+Fila,+Col,-Res)
% pasa una fila a una lista de elementos, ordenados por columna (el
% primer elemento está en la columna 1, el segundo en la 2, y así
% sucesivamente).
pasarFilaALista(Fila,Col,[x|Xs]):-tabla(Fila,Col,~_),NuevaCol is Col+1,pasarFilaALista(Fila,NuevaCol,Xs).
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


marcarColapsoFil(Fil):-marcarColapsoFil(Fil,0,x). %x puede ser cualquier elemento lo importante es q sea distinto a todas las muñecas del tablero

marcarColapsoFil(Fil,Col,E):-tabla(Fil,Col,~X),E\=X,marcarCentroFil(Fil,Col,X,1).
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
% HAY QUE AGREGAR ALGO PARA Q SIGA RECORRIENDO YA SEA SI HIZO LA MARCA O
% NO PARA EL CASO DEL BUCLE


marcarColapsoCol(Col):-marcarColapsoCol(0,Col,x). %x puede ser cualquier elemento lo importante es q sea distinto a todas las muñecas del tablero

marcarColapsoCol(Fil,Col,E):-tabla(Fil,Col,~X),E\=X,marcarCentroCol(Fil,Col,X,1).
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
% HAY QUE AGREGAR ALGO PARA Q SIGA RECORRIENDO YA SEA SI HIZO LA MARCA O
% NO PARA EL CASO DEL BUCLE


% Recorre las filas fijandose, si el elemento en esa fila y columna esta
% marcado, si arriba o abajo hay otro elemento marcado, para poner el
% colapso ahi.
marcarColapsoFilaDes(Fil,Col):-tabla(Fil,Col,~M),verificarCombinacionFilaDes(Fil,Col,~M),marcar(Fil,Col),marcarColapsoFilaDes(Fil,Col).
marcarColapsoFilaDes(Fil,Col):-Col<5,NuevaCol is Col+1,marcarColapsoFilaDes(Fil,NuevaCol).
marcarColapsoFilaDes(_,_).

verificarCombinacionColumnaDes(Fil,Col,Marcada):-ColNueva is Col+1,tabla(Fil,ColNueva,Marcada).
verificarCombinacionColumnaDes(Fil,Col,Marcada):-ColNueva is Col-1,tabla(Fil,ColNueva,Marcada).

marcarColapsoColumnaDes(Fil,Col):-tabla(Fil,Col,~M),verificarCombinacionColumnaDes(Fil,Col,~M),marcar(Fil,Col),marcarColapsoColumnaDes(Fil,Col).
marcarColapsoColumnaDes(Fil,Col):-Fil<5,NuevaFil is Fil+1,marcarColapsoColumnaDes(NuevaFil,Col),imprimirTablero.
marcarColapsoColumnaDes(_,_).

verificarCombinacionFilaDes(Fil,Col,Marcada):-FilNueva is Fil+1,tabla(FilNueva,Col,Marcada).
verificarCombinacionFilaDes(Fil,Col,Marcada):-FilNueva is Fil-1,tabla(FilNueva,Col,Marcada).

agrandarColapsados:-forall(tabla(F,C,/M),(retract(tabla(F,C,/M)),agrandar(M,MGrande),assert(tabla(F,C,MGrande)))).
agrandarColapsados.

agrandar(a1,a2).
agrandar(a2,a3).
agrandar(v1,v2).
agrandar(v2,v3).
agrandar(r1,r2).
agrandar(r2,r3).
agrandar(M,M).

guardarEvol:-pasarTableroAListas(0,Tablero),assertz(evol(Tablero)).

generarRandom:-guardarTablero([[x,x,x,x,x],[x,x,x,x,x],[x,x,x,x,x],[x,x,x,x,x],[x,x,x,x,x]]),reemplazarPorRandom.

