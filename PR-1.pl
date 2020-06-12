
:- op(300,fx,~). % negacion, prefija, no asociativa.
:- op(400,yfx,(/\)). % conjuncion, infija, asociativa a izquierda.
:- op(500,yfx,(\/)). % disyuncion, infija, asociativa a izquierda.
:- op(600,xfx,=>). % implicacion, infija, no asociativa.
:- op(650,xfx,<=>). % equivalencia, infija, no asociativa.


teorema(X):-fncr(~X,R),!,refutable(R).

%fncr/2
fncr(F,R):-criterio1(F,R1),distributivaBucle(R1,R2),aLista(R2,R4),reducirTopBottom(R4,R5),reducirRepetidosYComplementarios(R5,R6),reducirTopBottom(R6,R7),convertir(R7,R).

%criterio1/2
criterio1(F,R):-desarmar(F,R1),deMorgan(R1,R), writeln("Desarmando implicaciones y  equivalencias, y quitando dobles negaciones, aplicando leyes de De Morgan y reduciendo ~top a bottom y ~bottom a top para cumplir el criterio 1 "),writeln(R).

%desarmar/2
% devuelve el resultado de aplicar equivalencias de implicaciones y
% dobles implicaciones en caso de poder aplicarlas, sino devuelve F.

desarmar(~(F),~Rta1):-desarmar(F,Rta1).
desarmar(F/\Q,Rta1/\Rta2):- desarmar(F,Rta1),desarmar(Q,Rta2).
desarmar(F\/Q,Rta1\/Rta2):- desarmar(F,Rta1),desarmar(Q,Rta2).
desarmar(F=>Q,~Rta1\/Rta2):- desarmar(F,Rta1),desarmar(Q,Rta2).
desarmar(F<=>Q,(~Rta1\/Rta2)/\(~Rta2\/Rta1)):-desarmar(F,Rta1),desarmar(Q,Rta2).
desarmar(F,F).

%deMorgan/2
%Aplica leyes de De Morgan y reduce dobles negaciones
deMorgan((~(~F)),RDO):-deMorgan(F,RDO).
deMorgan(~(P/\Q),(Rta1)\/(Rta2)):-deMorgan(~P,Rta1),deMorgan(~Q,Rta2).
deMorgan(~(P\/Q),(Rta1)/\(Rta2)):-deMorgan(~P,Rta1),deMorgan(~Q,Rta2).
deMorgan(~top,bottom).
deMorgan(~bottom,top).

deMorgan(P/\Q,R1/\R2):-deMorgan(P,R1),deMorgan(Q,R2).
deMorgan(P\/Q,R1\/R2):-deMorgan(P,R1),deMorgan(Q,R2).
deMorgan(F,F).


% distributivaBucle/2
% El predicado distributivaBucle llama iterativamente al
% predicado distributiva hasta que no es posible volver a aplicar la
% propiedad.

distributivaBucle(F,Rta2):-distributiva(F,Rta),distributivaBucle(Rta,Rta2).
distributivaBucle(F,F).


%distributiva/2
%El predicado distributiva intenta aplicar una sola vez
%la propiedad distributiva sobre una fbf

distributiva(P\/(Q/\R),(P\/Q)/\(P\/R)).
distributiva((P/\Q)\/R,(P\/R)/\(Q\/R)).

distributiva(P/\Q,R1/\Q):-distributiva(P,R1).
distributiva(P/\Q,P/\R1):-distributiva(Q,R1).
distributiva(P\/Q,R1\/Q):-distributiva(P,R1).
distributiva(P\/Q,P\/R1):-distributiva(Q,R1).


% aLista/2
% aLista convierte una fbf a formato de listas de listas donde cada
% sublista es una clausula, y las sublistas están formadas unicamente
% por literales (sin \/). De esta forma se eliminan los /\ y los \/ pero
% se mantiene la misma fbf.

aLista(S,R):-aListaConjunciones(S,Raux),aListaDisyunciones(Raux,R).

% aListaConjunciones/2
% aListaConjunciones separa la formula bien formada según las
% conjunciones y retorna una lista que es el resultado de pasar a lista
% F1 y F2.
% Recibe una formula bien formada que tiene conjunciones.

aListaConjunciones(F1 /\ F2, R):- aListaConjunciones(F1, R1), aListaConjunciones(F2, R2), append(R1, R2, R).
aListaConjunciones(F, [F]).

% aListaDisyunciones/2
% aListaDisyunciones separa la formula bien formada según las
% disyunciones retornando una lista con los literales de la misma.
% Recibe una formula bien formada que es una lista que no contiene
% conjunciones.

aListaDisyunciones([],[]).
aListaDisyunciones([X|SL],[R|R2]):-sinparentesis(X,R),aListaDisyunciones(SL,R2).


% sinparentesis/2
% sinparentesis recibe una disyunción y retorna una lista que es
% resultado de concatenar dos listas con los literales involucrados en
% la disyuncion.
sinparentesis(P\/Q,R):-sinparentesis(P,R1),sinparentesis(Q,R2),append(R1,R2,R).
sinparentesis(F,[F]).


%reducirTopBottom/2
reducirTopBottom(S,R):-paso2cCascara(S,R6),paso2d(R6,R7),paso3a(R7,R8),paso3b(R8,R), writeln("Aplicando las propiedades Q"\/"Top=top, Q"\/"bottom=Q, Q"/\"top=Q y Q"/\"bottom=bottom "),imprimir(R).

% paso2cCascara/2
% paso2cCascara llama iterativamente al predicado
% paso2c reduciendo cada subLista de la lista recibida a [top] en caso
% de que la sublista contenga el elemento top y dejando la sublista
% igual en caso contrario. La lista recibida se interpreta como una
% lista de clausulas, donde cada clausula es una sublista que contiene
% literales.

paso2cCascara([Clausula1|SL],[R1|R2]):-paso2c(Clausula1,R1),paso2cCascara(SL,R2).
paso2cCascara(L,L).

%paso2c/2
paso2c(L,[top]):-member(top,L).
paso2c(L,L).

%paso2d/2
%Paso2d elimina todas las apariciones de bottom en cada clausula.

paso2d([],[]).
paso2d([Clausula1|SL],[R1|R2]):-eliminartodas(bottom,Clausula1,R1),paso2d(SL,R2).



%paso3a/2
% elimina todas las apariciones de [top] de una lista de listas (lista
% de clausulas)
paso3a(L,R):-eliminartodas([top],L,R).

% Paso3b reduce la lista de listas a una lista que contiene
% [bottom] en caso de que una sublista contenga solo bottom. Esto es así
% porque se interpreta La lista de listas como la conjunción de las
% sublistas y bottom/\algo se reduce a bottom.
%No hay mas conjunciones, por lo tanto devuelvo la ultima clausula.

paso3b(L,[[bottom]]):-member([bottom],L).
paso3b(L,L).

% Paso4aCascara/2 recibe una Lista de listas, donde cada sublista se
% interpreta como una clausula. Llama iterativamente a paso4a eliminando
% las apariciones repetidas de un mismo literal, ya que P\/P se reduce a
% P

paso4aCascara([],[]).
paso4aCascara([Clausula1|SL],[R1|R2]):-paso4a(Clausula1,R1),paso4aCascara(SL,R2).

eliminartodas(_,[],[]).
eliminartodas(E,[E|SL],R):-eliminartodas(E,SL,R).
eliminartodas(E,[X|SL],[X|R]):-eliminartodas(E,SL,R).

paso4a([P|SL],[P|R1]):-eliminartodas(P,SL,R),paso4a(R,R1).
paso4a(L,L).

% paso4bCascara/2 recibe una lista de listas como primer argumento,
% donde cada sublista se interpreta como una clausula. Llama
% iterativamente a paso4b con cada clausula, y paso4b elimina los
% literales complementarios de una clausula y devuelve el resultado como
% segundo argumento

paso4bCascara([],[]).
paso4bCascara([X|Xs],[R|Rs]):-paso4b(X,R),paso4bCascara(Xs,Rs).

paso4b([],[]).
paso4b([~X|Xs],[top|R]):-eliminar(X,Xs,Raux),paso4b(Raux,R).
paso4b([X|Xs],[top|R]):-eliminar(~X,Xs,Raux),paso4b(Raux,R).
paso4b([X|Xs],[X|R]):-paso4b(Xs,R).

% eliminar/3 intenta eliminar un elemento recibido como primer
% argumento de la lista que recibe como segundo argumento, devuelve como
% tercer argumento la lista sin el elemento en caso de poder hacerlo y
% falso sino.
eliminar(E,[E|Xs],Xs).
eliminar(E,[X|Xs],[X|R]):-eliminar(E,Xs,R).


% convertir/2 Recibe una formula bien formada como primer argumento y
% la pasa a formato lista de listas (donde cada sublista es una
% clausula y solo contiene literales que pueden estar negados o no) al
% formato con /\ y \/.

convertir([X],R):-agregarDisyunciones(X,R).
convertir([X|Xs],Rs/\R):-agregarDisyunciones(X,R),convertir(Xs,Rs).

agregarDisyunciones([X],X).
agregarDisyunciones([X|Xs],Rs\/X):-agregarDisyunciones(Xs,Rs). %append([Xs,[X],L).

%reducirRepetidosYComplementarios/2
reducirRepetidosYComplementarios(S,R):-paso4aCascara(S,R1),paso4bCascara(R1,R), writeln("Reduciendo literales repetidos y complementarios según las reglas que dictan que P"\/"P se reduce a P, P"\/"~P se reduce a top, y ~P"\/"~P se reduce a ~P"),imprimir(R).



imprimir(F):-convertir(F,R),writeln(R).



refutable(S):-aLista(S,R),!,refutar(R).

% refutar/1 verificar si se puede llegar a la clausula vacia a partir de
% un conjunto de clausulas que recibe como argumento, para hacer esto
% obtiene todas las resolventes posibles con un conjunto de clausulas S,
% las une y se llama a si mismo, terminando cuando se encontro la
% clausula vacia o no se pueden agregar nuevas clausulas.

refutar(S):-member([bottom],S),writeln("\nResultado Final"),writeln(S).
refutar(S):-obtenerResolventes(S,Nuevas),writeln("\nConjunto de Clausulas:"),writeln(S),writeln("Resolventes del conjunto: "),writeln(Nuevas),unirListas(S,Nuevas,Union),!,not(iguales(S,Union)),refutar(Union).

% obtenerResolventer/2 Recibe una lista de clausulas en su primer
% argumento y obtiene todas las resolventes posibles de un conjunto de
% clausulas sin usar las nuevas clausulas que se obtienen y lo devuelve
% en su tercer argumento.

obtenerResolventes([Clausula|Sentencia],R):-unaConTodas(Clausula,Sentencia,Resolventes),obtenerResolventes(Sentencia,Nuevas),unirListas(Nuevas,Resolventes,R).
obtenerResolventes([],[]).

% unaConTodas/3 Recibe una clausula como primer argumento,una
% lista como segundo argumento y obtiene todas las resolventes que se
% posibles entre la clausula y las clausulas de la lista, devuelve estas
% clausulas como una lista en su tercer argumento

unaConTodas(_,[],[]).
unaConTodas(Clausula1,[Clausula2|Sentencia],[Resolvente|Rs]):-resolvente(Clausula1,Clausula2,Resolvente),unaConTodas(Clausula1,Sentencia,Rs).
unaConTodas(Clausula,[_|Sentencia],R):-unaConTodas(Clausula,Sentencia,R).

% Resolvente/3 recibe como primer y segundo argumento dos clausulas y
% por cada literal A de Clausula1, si existe el literal negado en
% Clausula2 lo elimina de clausula 2 y devuelve una clausula en su
% tercer argumento que es la union de Clausula1 sin A y Clausula2 sin A
% negado. sino me devuelve falso.

resolvente([A],[~A],[bottom]).
resolvente([~A],[A],[bottom]).
resolvente(X,Y,R):-resolventeAux(X,Y,R).

resolventeAux([A|SL],C2,R):-borrarComplementarios(A,C2,R1),unirListas(SL,R1,R).
resolventeAux([A|SL],C2,R1):-resolventeAux(SL,C2,R1),member(A,R1).
resolventeAux([A|SL],C2,[A|R1]):-resolventeAux(SL,C2,R1).

borrarComplementarios(A,[~A|SL],SL).
borrarComplementarios(~A,[A|SL],SL).
borrarComplementarios(A,[A|SL],[A|R]):-borrarComplementarios(A,SL,R). %si encuentra A en ambas clausulas, deja una sola en el resolvente.
borrarComplementarios(A,[E|SL],[E|R]):-borrarComplementarios(A,SL,R).

% unirListas/3 Recibe en sus primeros dos argumentos dos listas de
% elementos y las une teniendo en cuenta que ambas listas pueden estar
% en distinto orden y que los elementos pueden ser listas que tambien
% pueden estar en otro orden, el resultado lo devuelve en su tercer
% argumento.

unirListas(Lista,[],Lista).
unirListas([],Lista,Lista).
unirListas(Lista,[Y|Ys],R):-agregar(Y,Lista,R2),unirListas(R2,Ys,R).

% agregar/3 Recibe un elmento como primer argumento y lo agrega a una
% lista siempre y cuando este no este repetido en la segunda lista,
% devuelve el resutlado como tercer argumento.

agregar(X,[],[X]).
agregar(X,[Y|Ys],[Y|Ys]):-iguales(X,Y).
agregar(X,[Y|Ys],[Y|R]):-agregar(X,Ys,R).

% iguales/2 verficia si dos elementos don iguales o si dos listas son
% iguales teniendo en cuenta que los elementos podrian estar en otro
% orden
iguales([],[]).
iguales(X,X).
iguales([X|Xs],Y):-eliminarElemento(X,Y,R),!,iguales(Xs,R).

% eliminaElemento/3 Recibe como primer argumento un elemento el elemento
% a eliminar de la lista que recibe en el segundo argumentoy
% devuelve la lista sin el elemento en su tercer argumento.
eliminarElemento(E,[Y|Ys],Ys):-iguales(E,Y).
eliminarElemento(E,[Y|Ys],[Y|Rs]):-eliminarElemento(E,Ys,Rs).


