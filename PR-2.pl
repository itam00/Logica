:- op(300,fx,/). %operador que marca que un elemento debe ser eliminado
:- op(300,fx,~). %operador que marca que un elemento debe aumentar su tamaño
:- dynamic tabla/3.
:- dynamic evol/1.


desplazar(Dir,Num,Cant,Tablero,EvolTablero):- guardarTablero(Tablero),Desplazamiento is Num-1,mover(Dir,Desplazamiento,Cant),aplicarColisiones(EvolTablero).%,guardarEvol,hayColisiones,eliminarColapsadosAux,guardarEvol,reemplazarPorRandom,writeln(' '),bucleCombinacionesAux,recuperarTableros(EvolTablero),eliminarTodo.

hayColisiones:-tabla(_,_,~_).

aplicarColisiones(EvolTablero):-hayColisiones,eliminarColapsadosAux,guardarEvol,reemplazarPorRandom,writeln(' '),bucleCombinacionesAux,recuperarTableros(EvolTablero),eliminarTodo.
aplicarColisiones(EvolTablero):-recuperarTableros(EvolTablero),eliminarTodo.

recuperarTableros(Lista):-findall(Tablero,evol(Tablero),Lista),forall(evol(X),retract(evol(X))).

% guardarTablero (+Tablero).
% guarda todos los elementos de la lista como predicados de la forma
% tabla/3, pasa 1 como numero de fila al predicado pasarAHechos ya que
% la primer fila en pasarse a hechos es la numero 0.
guardarTablero(Tablero):-pasarAHechos(Tablero,0).

%pasarAHechos/2
% Pasa un tablero en formato lista de listas a hechos (un hecho por
% elemento)
% pasarAHechos(+Tablero,+NumFila)
pasarAHechos([],_).
pasarAHechos([FilaActual|Filas],NumFila):-pasarFila(FilaActual,NumFila,0),SigFila is (NumFila+1),pasarAHechos(Filas,SigFila).

%pasarFila/3
% Pasa una lista que representa una fila de un tablero a hechos (un
% hecho por elemento)
% pasarFila(+Fila,+NumFila,+NumColumna)
pasarFila([],_,_).
pasarFila([Elem1|Elementos],NumFila,NumColumna):-pasarElemento(Elem1,NumFila,NumColumna),SigColumna is (NumColumna+1),pasarFila(Elementos,NumFila,SigColumna).

% pasarElemento/3 pasa un elemento en la fila NumFila y columna
% NumColumna a hecho.
% pasarElemento(+Elemento,+NumFila,+NumColumna)
pasarElemento(Elemento,NumFila,NumColumna):-assert(tabla(NumFila,NumColumna,Elemento)).


% mover/3
% realiza el desplazamiento correspondiente sobre el tablero almacenado
% mueve la fila o columna pasada por parametr en la direccion indicada N
% lugares
% mover(+Direccion,+(Fila o columna),+Cantidad)
mover(izq,Fila,N):-X is 5-N,desplazarFila(Fila,X).
mover(der,Fila,N):- desplazarFila(Fila,N).
mover(arriba,Col,N):-X is 5-N,desplazarCol(Col,X).
mover(abajo,Col,N):-desplazarCol(Col,N).


desplazarCol(C,N):-forall(tabla(F,C,E),(retract(tabla(F,C,E)),NuevaFila is ((N+F) mod 5), assert(tabla(NuevaFila,C,E)))),guardarEvol,combinarElementosCol(C).
desplazarFila(F,N):-forall(tabla(F,C,E),(retract(tabla(F,C,E)),NuevaColumna is ((N+C) mod 5), assert(tabla(F,NuevaColumna,E)))),guardarEvol,combinarElementosFil(F).



% marca todas las posibles combinaciones resultantes luego de realizar
% un desplazamiento en la fila Fil o columna Col, luego las elimina e
% incrementa de tamaño la muñeca correspondiente.

combinarElementosCol(Col):-forall(member(X,[0,1,2,3,4]),buscarCombFila(X)),buscarCombColumna(Col),marcarColapsoColumnaDes(0,Col),marcarColapsoCol(Col),agrandarColapsados.
combinarElementosFil(Fil):-forall(member(X,[0,1,2,3,4]),buscarCombColumna(X)),buscarCombFila(Fil),marcarColapsoFilaDes(Fil,0),marcarColapsoFil(Fil),agrandarColapsados.

%bucleCombinacionesAux/0
% busca repetitivamente nuevas combinaciones, solucionando las
% colisiones encontradas, hasta que el tablero anterior sea igual al
% tablero actual (es decir, hasta que no haya mas combinaciones en el
% tablero)
bucleCombinacionesAux:-pasarTableroAListas(0,TableroViejo),bucleCombinaciones,pasarTableroAListas(0,TableroNuevo),TableroViejo\=TableroNuevo,bucleCombinacionesAux.
bucleCombinacionesAux:-guardarEvol.



bucleCombinaciones:-forall(member(X,[0,1,2,3,4]),(buscarCombFila(X),buscarCombColumna(X))),forall(member(X,[0,1,2,3,4]),(marcarColapsoFil(X),marcarColapsoCol(X))),guardarEvol,eliminarColapsadosAux,guardarEvol,reemplazarPorRandom,agrandarColapsados.

% aca falta lo que hace todo en bucle, solo hay que recorrer todas las
% filas y columnas, marcar y dps agregar los colapsar hasta que no se
% puede eliminar nada
%

%de aca para abajo es todo para encontrar y marcar combiaciones
%

%elimina todo el tablero

eliminarTodo:-forall(tabla(X,Y,Z),retract(tabla(X,Y,Z))).

eliminarColapsadosAux:-eliminarColapsados,eliminarColapsadosAux.
eliminarColapsadosAux.

eliminarColapsados:-tabla(F,C,~Z),retract(tabla(F,C,~Z)),gravedad(F,C).


% gravedad/2
% gravedad(+F,+C).
% mueve todos los elementos arriba de una fila y columna pasados por
% parametro una fila para abajo, ya que el elemento que antes se
% encontraba en esa fila y columna fue eliminado. A demás agrega una x
% (que representa un lugar vacio), en la parte mas alta de la columna ya
% que ese lugar debe ser rellenado con un nuevo elemento)
gravedad(F,C):-forall((tabla(Fila,C,X),Fila=<F),(retract(tabla(Fila,C,X)),NuevaFila is Fila+1,assert(tabla(NuevaFila,C,X)))),assert(tabla(0,C,x)).

% reemplazarPorRandom/0 reemplaza todos los elementos del tablero que
% tengan una x por una muñeca random

reemplazarPorRandom:-forall(tabla(X,Y,x),(retract(tabla(X,Y,x)),random_member(Random, [a1,v1,r1]),assert(tabla(X,Y,Random)))).

imprimirTablero:-pasarTableroAListas(0,R),writeln(R).


% pasarTableroAListas/2
% pasarTableroAListas(+Fila,-Lista)
% Devuelve una Lista de Listas en Lista que representa el tablero
% actual, donde cada sublista es una fila que contiene los elementos del
% tablero ubicados en esa fila y ordenados por columna (el primer
% elemento está en la columna 1, el segundo en la 2 y así
% sucesivamente).
pasarTableroAListas(Fila,[X|Xs]):-tabla(Fila,_,_),pasarFilaALista(Fila,0,X),NuevaFila is Fila+1,pasarTableroAListas(NuevaFila,Xs),!.
pasarTableroAListas(_,[]).

%pasarFilaALista(+Fila,-Res)
pasarFilaALista(Fila,Col,[x|Xs]):-tabla(Fila,Col,~_),NuevaCol is Col+1,pasarFilaALista(Fila,NuevaCol,Xs).
pasarFilaALista(Fila,Col,[X|Xs]):-tabla(Fila,Col,X),NuevaCol is Col+1,pasarFilaALista(Fila,NuevaCol,Xs).

pasarFilaALista(_,_,[]).


% buscarCombFila/1 busca y marca una combinacion de muñecas en la fila
% recibida por parametro
buscarCombFila(Fil):-buscarDerecha(Fil,0).


% buscarAbajo/2 recorre una fila de izquierda a derecha llamando a
% buscarDerecgaAux para marcar las combinaciones de muñecas
buscarDerecha(Fil,Col):-buscarDerechaAux(Fil,Col,_,0). %no se llama al recursivo ya que solo puede haber una combinacion por col
buscarDerecha(Fil,Col):-Col<5,NuevaCol is Col+1,buscarDerecha(Fil,NuevaCol).
buscarDerecha(_,_).

% buscarDerecha/2 busca y marca una combinacion de muñecas de izquierda
% a derecha en una fila recibida por parametro
buscarDerechaAux(Fil,Col,E,Encontrados):-tabla(Fil,Col,~E),Nuevos is Encontrados+1,NuevaCol is Col+1,!,buscarDerechaAux(Fil,NuevaCol,E,Nuevos),marcar(Fil,Col).
buscarDerechaAux(Fil,Col,E,Encontrados):-tabla(Fil,Col,E),Nuevos is Encontrados+1,NuevaCol is Col+1,!,buscarDerechaAux(Fil,NuevaCol,E,Nuevos),marcar(Fil,Col).
buscarDerechaAux(_,_,_,Encontrados):-Encontrados>2.

% buscarCombColumna/1 busca y marca una combinacion de muñecas en la
% columna recibida por parametro

buscarCombColumna(Col):-buscarAbajo(0,Col).

% buscarAbajo/2 recorre una columa de arriba a abajo llamando a
% buscarAbajoAux para marcar las combinaciones de muñecas
buscarAbajo(Fil,Col):-buscarAbajoAux(Fil,Col,_,0). %no se llama al recursivo ya que solo puede haber una combiacion por col
buscarAbajo(Fil,Col):-Fil<5,NuevaFil is Fil+1,buscarAbajo(NuevaFil,Col).
buscarAbajo(_,_).

% buscarDerecha/2 busca y marca una combinacion de muñecas de arriba
% hacia abajo en una columna recibida por parametro

buscarAbajoAux(Fil,Col,E,Encontrados):-tabla(Fil,Col,~E),N is Encontrados+1,Sig is Fil+1,!,buscarAbajoAux(Sig,Col,E,N),marcar(Fil,Col).
buscarAbajoAux(Fil,Col,E,Encontrados):-tabla(Fil,Col,E),N is Encontrados+1,Sig is Fil+1,!,buscarAbajoAux(Sig,Col,E,N),marcar(Fil,Col).
buscarAbajoAux(_,_,_,Encontrados):-Encontrados>2.


% marcar/2 agrega una marca de colapsar a una muñeca que este en la fila
% o columna recibida por parametro, si la muñeca ya estaba marcada
% intercambia la marca de eliminar por una de colapsar
marcar(Fil,Col):- retract(tabla(Fil,Col,~E)),assert(tabla(Fil,Col,/E)).
marcar(Fil,Col):-retract(tabla(Fil,Col,E)),assert(tabla(Fil,Col,~E)).


%marcarColapsoFil/1 predicado auxiliar
marcarColapsoFil(Fil):-marcarColapsoFil(Fil,0,x). %x puede ser cualquier elemento lo importante es q sea distinto a todas las muñecas del tablero

% marcarColapsoFil/3 busca muñecas alineadas y marcadas (con una marca
% de eliminar) en una fila que no tengan una marca de colapsar y se las
% agrega en el centro (si son tres la coloca en la muñeca del medio y
% sino en alguna de las del medio)
marcarColapsoFil(Fil,Col,E):-tabla(Fil,Col,~X),E\=X,marcarCentroFil(Fil,Col,X,1).
marcarColapsoFil(Fil,Col,_):-Col<3,tabla(Fil,Col,/E),Sig is Col+1,marcarColapsoFil(Fil,Sig,E).
marcarColapsoFil(Fil,Col,E):-Col<3,Sig is Col+1,marcarColapsoFil(Fil,Sig,E).
marcarColapsoFil(_,_,_).

% solo se verifica hasta la tercera columna ya que no van a haber
% combiaciones de 3 si no hay un elemento marcado en la tercera columna
% Si se encuentra un elemento con marca de colapsar se envia por
% parametro para evitar agregar otra marca de colapsar

marcarCentroFil(Fil,Col,E,Cant):-Col<5,Sig is Col+1,tabla(Fil,Sig,~E),C is Cant+1,marcarCentroFil(Fil,Sig,E,C).%falla cuando llega al final o encuentra aldo distinto
marcarCentroFil(Fil,Col,E,_):-tabla(Fil,Col,/E).%si la combiacion ya estaba marcada entonces no se debe marcar el centro
marcarCentroFil(Fil,Col,_,Cant):-Cant>2,Centro is Col - (Cant//2),marcar(Fil,Centro).
% HAY QUE AGREGAR ALGO PARA Q SIGA RECORRIENDO YA SEA SI HIZO LA MARCA O
% NO PARA EL CASO DEL BUCLE

%marcarColapsoCol/1 predicado auxiliar
marcarColapsoCol(Col):-marcarColapsoCol(0,Col,x). %x puede ser cualquier elemento lo importante es q sea distinto a todas las muñecas del tablero


% marcarColapsoCol/3 busca muñecas alineadas y marcadas (con una marca
% de eliminar) en un columna que no tengan una marca de colapsar y se
% las agrega en el centro (si son tres la coloca en la muñeca del medio y sino en alguna de las del medio)
marcarColapsoCol(Fil,Col,E):-tabla(Fil,Col,~X),E\=X,marcarCentroCol(Fil,Col,X,1).
marcarColapsoCol(Fil,Col,_):-Fil<3,tabla(Fil,Col,/E),Sig is Fil+1,marcarColapsoCol(Sig,Col,E).
marcarColapsoCol(Fil,Col,E):-Fil<3,Sig is Fil+1,marcarColapsoCol(Sig,Col,E).
marcarColapsoCol(_,_,_).

% solo se verifica hasta la tercera columna ya que no van a haber
% combiaciones de 3 si no hay un elemento marcado en la tercera columna
% Si se encuentra un elemento con marca de colapsar se envia por
% parametro para evitar agregar otra marca de colapsar

marcarCentroCol(Fil,Col,E,Cant):-Fil<5,Sig is Fil+1,tabla(Sig,Col,~E),C is Cant+1,marcarCentroCol(Sig,Col,E,C).%falla cuando llega al final o encuentra aldo distinto
marcarCentroCol(Fil,Col,E,_):-tabla(Fil,Col,/E).%si la combiacion ya estaba marcada entonces no se debe marcar el centro
marcarCentroCol(Fil,Col,_,Cant):-Cant>2,Centro is Fil - (Cant//2),marcar(Centro,Col).
% HAY QUE AGREGAR ALGO PARA Q SIGA RECORRIENDO YA SEA SI HIZO LA MARCA O
% NO PARA EL CASO DEL BUCLE


% marcarColapsoFilaDes/3 verifica si hay una combinacion
% de muñecas marcadas (con marcas de eliminar) en una fila que atraviese
% a la columa desplazada, en caso de encontrar una combinacion
% marca el elemento encontrado en la fila y columna recibidas por
% parametro
marcarColapsoFilaDes(Fil,Col):-tabla(Fil,Col,~M),verificarCombinacionFilaDes(Fil,Col,~M),marcar(Fil,Col),marcarColapsoFilaDes(Fil,Col).
marcarColapsoFilaDes(Fil,Col):-Col<5,NuevaCol is Col+1,marcarColapsoFilaDes(Fil,NuevaCol).
marcarColapsoFilaDes(_,_).


% verificarCombinacionColumna/3 Recorre una columna verificando, si el
% elemento en esa fila y columna esta marcado, si arriba o abajo hay
% otro elemento marcado, para poner una marca de colapsar en ese lugar.
verificarCombinacionColumnaDes(Fil,Col,Marcada):-ColNueva is Col+1,tabla(Fil,ColNueva,Marcada).
verificarCombinacionColumnaDes(Fil,Col,Marcada):-ColNueva is Col-1,tabla(Fil,ColNueva,Marcada).

% MarcarColapsoColumnaDes/3 verifica si hay una combinacion
% de muñecas marcadas (con marcas de eliminar) en una fila que
% atraviese a la columna desplazada, en caso de encontrar una
% combinacion marca el elemento encontrado en la fila y columna
% recibidas por parametro
marcarColapsoColumnaDes(Fil,Col):-tabla(Fil,Col,~M),verificarCombinacionColumnaDes(Fil,Col,~M),marcar(Fil,Col),marcarColapsoColumnaDes(Fil,Col).
marcarColapsoColumnaDes(Fil,Col):-Fil<5,NuevaFil is Fil+1,marcarColapsoColumnaDes(NuevaFil,Col).
marcarColapsoColumnaDes(_,_).

% verificarCombinacionColumna/3 Recorre una fila verificando, si el
% elemento en esa fila y columna esta marcado, si arriba o abajo hay
% otro elemento marcado, para poner una marca de colapsar en ese lugar.
verificarCombinacionFilaDes(Fil,Col,Marcada):-FilNueva is Fil+1,tabla(FilNueva,Col,Marcada).
verificarCombinacionFilaDes(Fil,Col,Marcada):-FilNueva is Fil-1,tabla(FilNueva,Col,Marcada).

% agrandarColapsados/0 incrementa el tamaño de todos lo elementos en el
% tablero que tengan una marca de colapsar
agrandarColapsados:-forall(tabla(F,C,/M),(retract(tabla(F,C,/M)),agrandar(M,MGrande),assert(tabla(F,C,MGrande)))).
agrandarColapsados.

agrandar(a1,a2).
agrandar(a2,a3).
agrandar(v1,v2).
agrandar(v2,v3).
agrandar(r1,r2).
agrandar(r2,r3).
agrandar(M,M).

% guardarEvol/0 guarda el estado del tablero actual en un hecho
% evol(Tablero)
guardarEvol:-pasarTableroAListas(0,Tablero),assertz(evol(Tablero)).
