%--------------------------------------------------
% Load and Save from files
%--------------------------------------------------

%KB open and save

open_kb(Route,KB):-
	open(Route,read,Stream),
	readclauses(Stream,X),
	close(Stream),
	atom_to_term(X,KB).

save_kb(Route,KB):-
	open(Route,write,Stream),
	writeq(Stream,KB),
	close(Stream).

readclauses(InStream,W) :-
        get0(InStream,Char),
        checkCharAndReadRest(Char,Chars,InStream),
	atom_chars(W,Chars).

checkCharAndReadRest(-1,[],_) :- !.  % End of Stream
checkCharAndReadRest(end_of_file,[],_) :- !.

checkCharAndReadRest(Char,[Char|Chars],InStream) :-
        get0(InStream,NextChar),
        checkCharAndReadRest(NextChar,Chars,InStream).

%compile an atom string of characters as a prolog term
atom_to_term(ATOM, TERM) :-
	atom(ATOM),
	atom_to_chars(ATOM,STR),
	atom_to_chars('.',PTO),
	append(STR,PTO,STR_PTO),
	read_from_chars(STR_PTO,TERM).

guardar_variable(X):-
	open_kb('/home/nemis/proyecto2_IA/proyecto2_IA.txt',X).
	%write('KB: '),
	%write(KB),
	%save_kb('/home/nemis/proyecto2_IA/proyecto2_IA.txt',KB).

unificar_kb(X):-open_kb('/home/nemis/proyecto2_IA/proyecto2_IA.txt',X).
convertir_lista(L,X) :-  L =..X.



%Definimos nuestro operador atriburo valor.
:- op(400,xfx,=>).

valor(X,[(X => W)|_],R) :- R = W.
valor(X,[(Y=>_)|T],R) :- not(X=Y),valor(X,T,R).


%Predicados que encuentran las observaciones%
encontrar_observaciones([],_).
encontrar_observaciones([H|T],R):-
	H=..Lista,
	%write(Lista),nl,
	%write(T),nl,
	Lista=[class,observacion,[E1,E2,E3]]->R=[E1,E2,E3],!;
	encontrar_observaciones(T,R).

%Predicados que encuentran las creencias%
encontrar_creencias([],_).
encontrar_creencias([H|T],R):-
	H=..Lista,
	%write(Lista),nl,
	%write(Lista),nl,
	Lista=[class,creencias,[C1,C2,C3]]->R=[C1,C2,C3],!;
	encontrar_creencias(T,R).



%Predicados que encuntra los estantes%
encontrar_estantes([],_).
encontrar_estantes([H|T],R):-
	H=..Lista,
	Lista=[class,estantes,[[E1=>X|Er],[E2=>Y|E2r],[E3=>Z|E3r]]],
	Er=[[Cat=>Val]],
	E2r=[[Cat2=>Val2]],
	E3r=[[Cat3=>Val3]],
	R=[X=>Val,Y=>Val2,Z=>Val3],!;
	encontrar_estantes(T,R).

%Predicado ecnontrar categoria de producto de una lista de listas%
encontrar_categoria_listal([],Producto,R).
encontrar_categoria_listal([H|T],Producto,R):-
	%write("El producto es"),nl,
	%write(Producto),nl,nl,
	%write(H),
	H=[Id=>Product,Categoria=>Valor|Rp],
	Product=Producto -> R=Product=>Valor;
	encontrar_categoria_listal(T,Producto,R).


%Predicado que encuentra productos%
encontrar_productos([],_).
encontrar_productos([H|T],R):-
	H=..Lista,
	%write(Lista),nl,
	Lista=[class,productos,[_],Productos],R=Productos;
	encontrar_productos(T,R)
	.
%Predicado lista productos con categoria%
asociar_producto_categoria([],S,R,[]).
asociar_producto_categoria([H|T],S,R,[Lp|Lps]):-
	%write("Asosciar Producto"),nl,
	encontrar_categoria_listal(S,H,R),R=Lp,nl,
	asociar_producto_categoria(T,S,Rs,Lps).

encontrar_match_categorias2(Excategoria,[],El,[],[]).
encontrar_match_categorias2(Excategoria,[Prod=>Cat|T],El,[Prod|L1],L2):-
	%write("encontrar_match_categorias2"),nl,
	%write(Prod),nl,
	%write(Cat),nl,
	Excategoria=Cat->!,encontrar_match_categorias2(Excategoria,T,El,L1,L2).

encontrar_match_categorias2(Excategoria,[Prod=>Cat|T],El,L1,[Prod|L2]):-
	encontrar_match_categorias2(Excategoria,T,El,L1,L2).	

encontrar_match_categorias(Excategoria,[Estante=>Prod],El,Elmodificado):-
	%write("encontrar_match_categorias"),nl,
	%write(Excategoria),nl,
	%write(Estante),nl,
	%write(Prod),nl,
	%write(El),nl,
	%write(Elmodificado),
	encontrar_match_categorias2(Excategoria,Prod,El,Lcorrectos,Lsobrantes),
	%write("LAs Ls"),nl,
	%write(Lcorrectos),nl,
	%write(Lsobrantes),nl,
	El=[Correctos,Sobrantes,Faltantes],
	%write("Los correctos son los siguientes:"),nl,
	%write(Correctos),
	union(Correctos,Lcorrectos,Lcorrectosmod),
	%write("Los sobrantes son los siguiente:"),nl,
	%write(Sobrantes),nl,
	%write("Los faltantes son los siguientes"),nl,
	%write(Faltantes),
	Elmodificado=[Lcorrectosmod,Lsobrantes,Faltantes].
	%write("El valor de Elmodificado es el siguiente:"),nl,
	%write(Elmodificado)

actualizar_creencias_observaciones([],KB,[],[]).
actualizar_creencias_observaciones([Ex=>El|Er],KB,[Ex=>Excategoria|Exr],[Ex=>Elmodificado|Elmodificador]):-
	%encontrar_estantes(KB,T),
	%write("La categoria"),write(Excategoria),nl,
	%write("Soy Exr"),write(Exr),nl,
	%write("Soy Er"),write(Exr),nl,	
	%write(T),nl,nl,
	encontrar_productos(KB,S),
	%write(S),nl,nl,
	El=[_,Sobrantes,_],
	%write(Sobrantes),nl,nl,
	asociar_producto_categoria(Sobrantes,S,R,R2),
	%write("Sale R2"),nl,
	Relacion_Estante_Sobrantes=Ex=>R2,
	%write(Relacion_Estante_Sobrantes),nl,
	%write(Er),nl,nl,nl,
	encontrar_match_categorias(Excategoria,[Relacion_Estante_Sobrantes],El,Elmodificado),
	actualizar_creencias_observaciones(Er,KB,Exr,Elmodificador).

	

creencias_observaciones(I,R):-
	unificar_kb(KB),
	%convertir_lista(KB,L),
	%write(KB),nl,nl,
	encontrar_observaciones(KB,R_observaciones),
	encontrar_creencias(KB,R_creencias),
	%write(R_observaciones),nl,
	%write(R_creencias),
	validar_creencias(R_observaciones,R_creencias,I),
	encontrar_estantes(KB,Estantes),
	%write("Los estantes son:"),write(Estantes),nl,
	actualizar_creencias_observaciones(I,KB,Estantes,Rfinal),
	%write("El resultado final es el siguiente"),nl,
	%write(Rfinal)
	R=Rfinal
	.


%Predicado que obtiene los elementos comunes%
sub(List,[],List):-!.
sub(List,[X|Sub],Rem) :- 
	select(X,List,Rem0), 
	sub(Rem0,Sub,Rem).


validar_creencias2([],C,_,_,_):-write("aca").
validar_creencias2(Obs,Creen,In,R,T,F):-
	intersection(Obs,Creen,In),
	sub(Obs,In,R),
	sub(Creen,In,T),
	F=[In,R,T],!
	.


validar_creencias([],[],[]).
validar_creencias([X=>Y|Xs],[Z=>W|Zs],[X=>F|Fs]):-
	%write(X),nl,nl,
	%write(Z),
	validar_creencias2(Y,W,RI,R,S,F),
	%write(R),nl,
	%write(S),nl,
	%write(F),nl,
	validar_creencias(Xs,Zs,Fs)
	.

tamanio_estante([],[]).
tamanio_estante([Estante=>Lproductos|Rproductos],[Tamlistaprod|Rtam]):-
	length(Lproductos,Tamlistaprod),
	tamanio_estante(Rproductos,Rtam).

total_deproductos(Tamanioestante,Totaldeproductos):-
	sum_list(Tamanioestante,Totaldeproductos)
	.


tamanio_porestante(Tamanioestante,Totaldeproductos):-
	unificar_kb(X),
	encontrar_creencias(X,Creencias),
	tamanio_estante(Creencias,Tamanioestante),
	total_deproductos(Tamanioestante,Totaldeproductos).
	%write(Tamanioestante),
	%write(Totaldeproductos)
	

tamanio_porestante_obs(TamanioestanteObservacion,TotaldeproductosObservacion):-
	unificar_kb(X),
	encontrar_observaciones(X,Observaciones),
	%write(Observaciones),nl,
	tamanio_estante(Observaciones,TamanioestanteObservacion),
	total_deproductos(TamanioestanteObservacion,TotaldeproductosObservacion)
	.

calcular_diferencias_creencias([],[],L).
calcular_diferencias_creencias([H|T],[R|S],[Diferencia|RestoDiferencia]):-
	H=Estante=>[Correctos,Sobrantes,Faltantes],
	R=Estante=>[CorrectosActualizados,SobrantesActualizados,FaltantesActualizados],
	length(CorrectosActualizados,T1),
	length(Correctos,T2),
	Diferencia is T1-T2,
	calcular_diferencias_creencias(T,S,RestoDiferencia).
	

numero_deproductosnoreportados(Total):-
	creencias_observaciones(I,R),	
	calcular_diferencias_creencias(I,R,L),
	sum_list(L,Total).

estantes_observados2([],[]).	
estantes_observados2([Estante=>L|Restante],S):-
	write(L),nl,
	L=[]-> 
	estantes_observados2(Restante,[Estante|S]).


estantes_observados():-
	unificar_kb(KB),
	encontrar_observaciones(KB,R),
	estantes_observados2(R,Rest),
	write(R),
	write(Rest)
	.


arbol_deseado():-
	tamanio_porestante(Tamanioestante,Totaldeproductos),
	numero_deproductosnoreportados(TotalNoReportados),
	ProductosTotales is Totaldeproductos+TotalNoReportados,
	tamanio_porestante_obs(TamanioestanteObservacion,TotaldeproductosObservacion),
	ProductosTotalesRepartir is ProductosTotales - TotaldeproductosObservacion,
	write(ProductosTotalesRepartir),
	estantes_observados()
	.

filter([],[]).
filter([H|T],S) :-
  H<0,
  filter(T,S).
filter([H|T],S) :-
  H>=0,
  filter(T,[H|S]).