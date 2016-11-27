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
	Lista=[class,observacion,EstantesObservacion]->R=EstantesObservacion,!;
	encontrar_observaciones(T,R).

%Predicados que encuentran las creencias%
encontrar_creencias([],_).
encontrar_creencias([H|T],R):-
	H=..Lista,
	Lista=[class,creencias,EstantesCreencia]->R=EstantesCreencia,!;
	encontrar_creencias(T,R).

encontrar_estantes2([],[]).
encontrar_estantes2([H|T],[Estante=>ValorCategoria|REstante]):-
	H=[_=>Estante,[_=>ValorCategoria]],
	encontrar_estantes2(T,REstante)
	.


%Predicados que encuntra los estantes%
encontrar_estantes1([],_).
encontrar_estantes1([H|T],R):-
	H=..Lista,
	Lista=[class,estantes,Estantes]->R=Estantes,!;
	encontrar_estantes1(T,R)
	.

encontrar_estantes(EstantesConCategoria):-
	unificar_kb(KB),
	encontrar_estantes1(KB,R),
	encontrar_estantes2(R,EstantesConCategoria).




%Predicado ecnontrar categoria de producto de una lista de listas%
encontrar_categoria_listal([],_,_).
encontrar_categoria_listal([H|T],Producto,R):-
	H=[_=>Product,_=>Valor|_],
	Product=Producto -> R=Product=>Valor;
	encontrar_categoria_listal(T,Producto,R).



%Predicado ecnontrar estante de producto de una lista de listas%
encontrar_categoria_estantel([],_,_).
encontrar_categoria_estantel([H|T],Producto,R):-
	H=[_=>Product,_=>Valor|_],
	Product=Producto -> R=Product=>Valor;
	encontrar_categoria_estantel(T,Producto,R).


%Predicado que encuentra productos%
encontrar_productos([],_).
encontrar_productos([H|T],R):-
	H=..Lista,
	Lista=[class,productos,[_],Productos],R=Productos;
	encontrar_productos(T,R)
	.
%Predicado lista productos con estantes%
asociar_producto_estante([],_,_,[]).
asociar_producto_estante([H|T],S,R,[Lp|Lps]):-
	encontrar_categoria_estantel(S,H,R),R=Lp,
	asociar_producto_estante(T,S,_,Lps).


%Predicado lista productos con categoria%
asociar_producto_categoria([],_,_,[]).
asociar_producto_categoria([H|T],S,R,[Lp|Lps]):-
	encontrar_categoria_listal(S,H,R),R=Lp,
	asociar_producto_categoria(T,S,_,Lps).

encontrar_match_categorias2(_,[],_,[],[]).
encontrar_match_categorias2(Excategoria,[Prod=>Cat|T],El,[Prod|L1],L2):-
	Excategoria=Cat->!,encontrar_match_categorias2(Excategoria,T,El,L1,L2).

encontrar_match_categorias2(Excategoria,[Prod=>_|T],El,L1,[Prod|L2]):-
	encontrar_match_categorias2(Excategoria,T,El,L1,L2).	

encontrar_match_categorias(Excategoria,[_=>Prod],El,Elmodificado):-
	encontrar_match_categorias2(Excategoria,Prod,El,Lcorrectos,Lsobrantes),
	El=[Correctos,Sobrantes,Faltantes],
	union(Correctos,Lcorrectos,Lcorrectosmod),
	Elmodificado=[Lcorrectosmod,Lsobrantes,Faltantes].

actualizar_creencias_observaciones([],_,[],[]).
actualizar_creencias_observaciones([Ex=>El|Er],KB,[Ex=>Excategoria|Exr],[Ex=>Elmodificado|Elmodificador]):-
	%encontrar_estantes(KB,T),
	encontrar_productos(KB,S),
	El=[_,Sobrantes,_],
	asociar_producto_categoria(Sobrantes,S,R,R2),
	Relacion_Estante_Sobrantes=Ex=>R2,
	encontrar_match_categorias(Excategoria,[Relacion_Estante_Sobrantes],El,Elmodificado),
	actualizar_creencias_observaciones(Er,KB,Exr,Elmodificador).

	

creencias_observaciones(I,R):-
	unificar_kb(KB),
	%convertir_lista(KB,L),
	encontrar_observaciones(KB,R_observaciones),
	encontrar_creencias(KB,R_creencias),
	validar_creencias(R_observaciones,R_creencias,I),
	encontrar_estantes(Estantes),
	actualizar_creencias_observaciones(I,KB,Estantes,Rfinal),
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
	validar_creencias2(Y,W,RI,R,S,F),
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
	

tamanio_porestante_obs(TamanioestanteObservacion,TotaldeproductosObservacion):-
	unificar_kb(X),
	encontrar_observaciones(X,Observaciones),
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

estantes_noobservados2([],[]).	
estantes_noobservados2([Estante=>L|Restante],Y):-
	not(L=[])->!,estantes_noobservados2(Restante,Y);
	estantes_noobservados2(Restante,Y2),append([Estante],Y2,Y).


estantes_noobservados(EstantesNoObservados):-
	unificar_kb(KB),
	encontrar_observaciones(KB,R),
	estantes_noobservados2(R,EstantesNoObservados)
	.

articulos_faltan([],[]).
articulos_faltan([Estante=>L|Restante],ArticulosFaltantes):-
	L=[Correctos,Sobrantes,Faltantes],
	articulos_faltan(Restante,R2),
	append(Faltantes,R2,ArticulosFaltantes).

articulos_observaciones([],[]).
articulos_observaciones([Estante=>L|Restante],ArticulosObservacion):-
	articulos_observaciones(Restante,R2),
	append(L,R2,ArticulosObservacion).

articulos_a_permutar(ArticulosAPermutar):-
	unificar_kb(KB),
	creencias_observaciones(Iiniciales,Reacomodos),
	articulos_faltan(Reacomodos,ArticulosFaltantes),
	encontrar_observaciones(KB,Observaciones),
	articulos_observaciones(Observaciones,ArticulosObservacion),
	subtract(ArticulosFaltantes,ArticulosObservacion,ArticulosAPermutar)
	.

permutar_objetos2(ArticulosAPermutar,[]).
permutar_objetos2(ArticulosAPermutar,[DEstante|DRestante]):-
	write("Permutar Objetos 2"),
	write(DEstante),
	setof(Conjunto,subset(DEstante,ArticulosAPermutar,Conjunto),L),
	write(L),
	permutar_objetos3(ArticulosAPermutar,L,DRestante).
	

permutar_objetos4(ArticulosAPermutar,[],[]).
permutar_objetos4(ArticulosAPermutar,[PrimerElement|RestElement],[DEstante|DRestante]):-
	write("Entre aqui objetos 4"),
	write(ArticulosAPermutar),
	write(PrimerElement),
	write(DEstante),
	subtract(ArticulosAPermutar,PrimerElement,ArticulosAPermutar3),
	write(ArticulosAPermutar3),
	setof(Conjunto,subset(DEstante,ArticulosAPermutar3,Conjunto),L),
	write(L)
	.
	%permutar_objetos4(ArticulosAPermutar3,RestElement,DRestante).
	

permutar_objetos3(ArticulosAPermutar,[],[]).
permutar_objetos3(ArticulosAPermutar,[PrimerElement|RestElement],[DEstante|DRestante]):-
	write("Entre aqui"),
	write(ArticulosAPermutar),
	write(PrimerElement),
	write(DEstante),
	subtract(ArticulosAPermutar,PrimerElement,ArticulosAPermutar2),
	write(ArticulosAPermutar2),
	setof(Conjunto,subset(DEstante,ArticulosAPermutar2,Conjunto),L),
	write(L),nl.
	%permutar_objetos3(ArticulosAPermutar,RestElement,DEstante).
	%permutar_objetos4(ArticulosAPermutar2,L,DRestante).
	
	
calcular_peso_configuracion2(PrimerElement,[]).
calcular_peso_configuracion2(PrimerElement,[Posibles|Rposibles]):-
	write("Una de las configuraciones es la siguiente"),
	write(PrimerElement),write(Posibles),
	calcular_peso_configuracion2(PrimerElement,Rposibles).

calcular_peso_configuracion(PrimerElement,SegundoElemnt):-
	write("Debo calcular el peso de las siguientes configuraciones"),
	write(PrimerElement),
	write(SegundoElemnt),
	calcular_peso_configuracion2(PrimerElement,SegundoElemnt)
	.




permutar_productos3([],ArticulosAPermutar,DRestante).


permutar_productos3([PrimerElement|RestElement],ArticulosAPermutar,DRestante):-
	write("Permutar productos 3"),
	subtract(ArticulosAPermutar,PrimerElement,ArticulosAPermutar2),
	DRestante=[ValorRestante],
	setof(Conjunto,subset(ValorRestante,ArticulosAPermutar2,Conjunto),L),
	calcular_peso_configuracion(PrimerElement,L),
	permutar_productos3(RestElement,ArticulosAPermutar,DRestante).

	
permutar_productos2([],ArticulosAPermutar).
permutar_productos2([DEstante|DRestante],ArticulosAPermutar):-
	write("Permutar productos 2"),
	write(DEstante),
	setof(Conjunto,subset(DEstante,ArticulosAPermutar,Conjunto),L),
	write(L),
	write(DRestante),
	permutar_productos3(L,ArticulosAPermutar,DRestante),
	permutar_productos2(DRestante,ArticulosAPermutar).

permutar_productos(ArticulosAPermutar,PDistribucion):-
	%,
	permutar_productos2(PDistribucion,ArticulosAPermutar).



permutar_objetos(ArticulosAPermutar,[]).
permutar_objetos(ArticulosAPermutar,[PDistribucion|RDsitribucion]):-
	%permutar_objetos2(ArticulosAPermutar,PDistribucion),
	permutar_productos(ArticulosAPermutar,PDistribucion),
	permutar_objetos(ArticulosAPermutar,RDsitribucion).


%Predicado que  asigna a los articulos a permutar el estante que  pertenece%
articulos_permutarestantes(Producto=>Categoria,[],ProductoEstante).
articulos_permutarestantes(Producto=>Categoria,[Estante|Restante],ProductoEstante):-
		Estante= EstanteIndicador=>Catego,
		Categoria=Catego -> ProductoEstante = Producto=>EstanteIndicador;
		articulos_permutarestantes(Producto=>Categoria,Restante,ProductoEstante).

articulos_a_permutar_con_estante([],EstantesEncontrados,[]).
articulos_a_permutar_con_estante([H|T],EstantesEncontrados,[ProductoEstante|RProductoEstante]):-
	articulos_permutarestantes(H,EstantesEncontrados,ProductoEstante),
	articulos_a_permutar_con_estante(T,EstantesEncontrados,RProductoEstante)
	.

separar_productos_a_permutar([],EstantesNoObservados,[],[]).
separar_productos_a_permutar([ArticuloCategoria|RArticuloCategoria],EstantesNoObservados,[Prod|L1],L2):-
	ArticuloCategoria=Producto=>Estante,
	member(Estante,EstantesNoObservados)->Prod=ArticuloCategoria,!,separar_productos_a_permutar(RArticuloCategoria,EstantesNoObservados,L1,L2).

separar_productos_a_permutar([ArticuloCategoria|RArticuloCategoria],EstantesNoObservados,L1,[Prod|L2]):-
	ArticuloCategoria=Producto=>Estante,
	Prod=ArticuloCategoria,
	separar_productos_a_permutar(RArticuloCategoria,EstantesNoObservados,L1,L2).



substraer_elementos_correctos3([],EstanteNoObservado,ElementoDistribucion,[]).
substraer_elementos_correctos3(X,EstanteNoObservado,0,[]).
substraer_elementos_correctos3([ProductoCorrecto|RProductoCorrecto],EstanteNoObservado,ElementoDistribucion,L):-
	ProductoCorrecto=Producto=>IdEstante,
	not(IdEstante=EstanteNoObservado)->!,substraer_elementos_correctos3(RProductoCorrecto,EstanteNoObservado,ElementoDistribucion,L).

substraer_elementos_correctos3([ProductoCorrecto|RProductoCorrecto],EstanteNoObservado,ElementoDistribucion,[ProductoCorrecto|L]):-
	ElementoDistribucion2 is ElementoDistribucion-1 ,
	substraer_elementos_correctos3(RProductoCorrecto,EstanteNoObservado,ElementoDistribucion2,L).

substraer_elementos_correctos2(ProductosColocarCorrectos,[],[],[]).
substraer_elementos_correctos2(ProductosColocarCorrectos,[EstanteNoObservado|REstanteNoObservado],[ElementoDistribucion|RElementoDistribucion],[Valor|Rvalor]):-
	substraer_elementos_correctos3(ProductosColocarCorrectos,EstanteNoObservado,ElementoDistribucion,Valor),
	substraer_elementos_correctos2(ProductosColocarCorrectos,REstanteNoObservado,RElementoDistribucion,Rvalor)
	.
tamanio_configuracionordenados([],[]).
tamanio_configuracionordenados([H|T],[Tamanio|RTamanio]):-
	length(H,Tamanio),
	tamanio_configuracionordenados(T,RTamanio).

actualizar_distribucion_estantes([],[],[]).
actualizar_distribucion_estantes([H|T],[Z|W],[R|Resto]):-
	R is H-Z,
	actualizar_distribucion_estantes(T,W,Resto).

take(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :- M is N-1, take(M, Xs, Ys).


llenado_restantes([],PoisbleConfiguracionNoOrdenados,[]).
llenado_restantes([H|T],PoisbleConfiguracionNoOrdenados,[L|Ls]):-
	take(H,PoisbleConfiguracionNoOrdenados,L),
	subtract(PoisbleConfiguracionNoOrdenados,L,L2),
	llenado_restantes(T,L2,Ls).

unificar_acomodos([],[],[]).
unificar_acomodos([H|T],[W|Z],[R|Rs]):-
	union(H,W,R),
	unificar_acomodos(T,Z,Rs).

regresar_productos_sinestantes2([],[]).
regresar_productos_sinestantes2([H|T],[Producto|RProducto]):-
	H=Producto=>Estante,
	regresar_productos_sinestantes2(T,RProducto).

regresar_productos_sinestantes([],[]).
regresar_productos_sinestantes([H|T],[Valor|RValor]):-
	regresar_productos_sinestantes2(H,Valor),
	regresar_productos_sinestantes(T,RValor).


construir_configuracion_de_estantes([],[],[]).

construir_configuracion_de_estantes([H|T],[],[L2|L2s]):-
	H=IdEstante=>L,
	L2=L,
	construir_configuracion_de_estantes(T,[],L2s).

construir_configuracion_de_estantes([H|T],[Z|W],[L2|L2s]):-
	H=IdEstante=>L,
	not(L=[])->(L2=L,construir_configuracion_de_estantes(T,[Z|W],L2s));
	L2=Z,construir_configuracion_de_estantes(T,W,L2s).

substraer_elementos_correctos(ProductosColocarCorrectos,ProductosColocarIncorrectos,EstantesNoObservados,[],ArticulosConEstantes,[]).
substraer_elementos_correctos(ProductosColocarCorrectos,ProductosColocarIncorrectos,EstantesNoObservados,[PosibleDistribucion|RPosibleDistribucion],ArticulosConEstantes,[ConfiguracionEstantesFinal|R]):-
	substraer_elementos_correctos2(ProductosColocarCorrectos,EstantesNoObservados,PosibleDistribucion,PoisbleConfiguracionOrdenados),
	flatten(PoisbleConfiguracionOrdenados,PoisbleConfiguracionOrdenadosLista),
	subtract(ArticulosConEstantes,PoisbleConfiguracionOrdenadosLista,PoisbleConfiguracionNoOrdenados),
	tamanio_configuracionordenados(PoisbleConfiguracionOrdenados,ResultadoTamanio),
	actualizar_distribucion_estantes(PosibleDistribucion,ResultadoTamanio,DistribucionActualizada),
	llenado_restantes(DistribucionActualizada,PoisbleConfiguracionNoOrdenados,AcomodosRestantes),
	unificar_acomodos(PoisbleConfiguracionOrdenados,AcomodosRestantes,AcomodosUnificados),
	regresar_productos_sinestantes(AcomodosUnificados,AcomodosUnificadosFinal),
	unificar_kb(KB),
	encontrar_observaciones(KB,Observaciones),
	construir_configuracion_de_estantes(Observaciones,AcomodosUnificadosFinal,ConfiguracionEstantesFinal),
	substraer_elementos_correctos(ProductosColocarCorrectos,ProductosColocarIncorrectos,EstantesNoObservados,RPosibleDistribucion,ArticulosConEstantes,R).

obetener_creencias_cantidad_estante_auxiliar([],[]).
obetener_creencias_cantidad_estante_auxiliar([Estante=>ListaProductos|REstantes],[Tlista|RTlista]):-
	length(ListaProductos,Tlista),
	obetener_creencias_cantidad_estante_auxiliar(REstantes,RTlista)
	.

obetener_creencias_cantidad_estante(TamanioCreenciasPorEstante):-
	unificar_kb(KB),
	encontrar_creencias(KB,Creencias),
	obetener_creencias_cantidad_estante_auxiliar(Creencias,TamanioCreenciasPorEstante)
	.

obtener_cantidad_estante_pconfiguracion([],[]).
obtener_cantidad_estante_pconfiguracion([Estante|REstante],[TEstante|RTEstante]):-
	length(Estante,TEstante),
	obtener_cantidad_estante_pconfiguracion(REstante,RTEstante)
	.
obtener_diferencies_creencias_pconfigruacion([],[],[]).
obtener_diferencies_creencias_pconfigruacion([TamanioEstanteCreencia|REstanteCreencia],[TEstante|RTEstante],[DTamanio|RDTamanio]):-
	DTamanio is abs(TamanioEstanteCreencia-TEstante),
	obtener_diferencies_creencias_pconfigruacion(REstanteCreencia,RTEstante,RDTamanio).

%PC/C%
obtener_pconfiguracion_sin_creencias_aux2([],[]).
obtener_pconfiguracion_sin_creencias_aux2([H|T],[Lproductos|RLproductos]):-
	H=IdEstante=>Lproductos,
	obtener_pconfiguracion_sin_creencias_aux2(T,RLproductos).

obtener_pconfiguracion_sin_creencias_aux(LProductosCreencias):-
	unificar_kb(KB),
	encontrar_creencias(KB,Creencias),
	obtener_pconfiguracion_sin_creencias_aux2(Creencias,LProductosCreencias).	

obtener_pconfiguracion_sin_creencias([],[],[]).
obtener_pconfiguracion_sin_creencias([Estante|REstante],[EstanteCreencias|REstanteCreencias],[TEstante|RTEstante]):-
	subtract(Estante,EstanteCreencias,RestoCreencias),
	length(RestoCreencias,TEstante),
	obtener_pconfiguracion_sin_creencias(REstante,REstanteCreencias,RTEstante).

calcular_peso_configuraciones([],[],[]).
calcular_peso_configuraciones([Configuracion|RestoConfiguraciones],[Configuracion|RConfiguracion],[PesoFinalConfiguracion|RPesoFinalConfiguracion]):-
	obetener_creencias_cantidad_estante(TamanioCreenciasPorEstante),
	obtener_cantidad_estante_pconfiguracion(Configuracion,TamanioCinfiguracionPorEstante),
	obtener_diferencies_creencias_pconfigruacion(TamanioCreenciasPorEstante,TamanioCinfiguracionPorEstante,DiferenciasPorEstante),
	sum_list(DiferenciasPorEstante,SumadiferenciasPorEstante),%Valor importante a considerar
	obtener_pconfiguracion_sin_creencias_aux(LProductosCreencias),
	obtener_pconfiguracion_sin_creencias(Configuracion,LProductosCreencias,TEstante),
	sum_list(TEstante,SumaTEstante),
	PesoFinalConfiguracion is SumadiferenciasPorEstante+SumaTEstante,
	calcular_peso_configuraciones(RestoConfiguraciones,RConfiguracion,RPesoFinalConfiguracion).

funcion_min([],[],Min,Configuracion,R):- R=Configuracion.	
funcion_min([H|T],[Z|W],Min,Configuracion,R):-	
	Min>Z -> (Min2=Z, Configuracion2=H,funcion_min(T,W,Min2,Configuracion2,R));
	funcion_min(T,W,Min,Configuracion,R).

reportar_acciones_productos([]).
reportar_acciones_productos([H|T]):-
	write("Bring:"),write(H),nl,
	reportar_acciones_productos(T).

decripcion_acciones_asistente([],Estante).
decripcion_acciones_asistente([H|T],Estante):-
	H=[]->Estante2 is Estante+1,decripcion_acciones_asistente(T,Estante2);
	write("====The assistent move to shelf ===="),write(" :"),write(Estante),nl,
	reportar_acciones_productos(H),Estante2 is Estante+1,
	decripcion_acciones_asistente(T,Estante2).

formato_diagnostico([],[],[]).
formato_diagnostico([IdEstante=>Producto|Resto],[H|T],[Valor|Rvalor]):-
	Valor=IdEstante=>H,
	formato_diagnostico(Resto,T,Rvalor).

diagnostico(DiagnosticoConfiguracion):-
	tamanio_porestante(Tamanioestante,Totaldeproductos),
	numero_deproductosnoreportados(TotalNoReportados),
	ProductosTotales is Totaldeproductos+TotalNoReportados,
	tamanio_porestante_obs(TamanioestanteObservacion,TotaldeproductosObservacion),
	ProductosTotalesRepartir is ProductosTotales - TotaldeproductosObservacion,
	estantes_noobservados(EstantesNoObservados),
	articulos_a_permutar(ArticulosAPermutar),
	distribucion_por_estante(ArticulosAPermutar,EstantesNoObservados,ListaFinalDistribucion),
	unificar_kb(KB),
	encontrar_productos(KB,S),
	asociar_producto_estante(ArticulosAPermutar,S,R,ProductosAPermutarCategoria),
	encontrar_estantes(EstantesEncontradosCategoria),
	articulos_a_permutar_con_estante(ProductosAPermutarCategoria,EstantesEncontradosCategoria,ArticulosConEstantes),
	separar_productos_a_permutar(ArticulosConEstantes,EstantesNoObservados,ProductosColocarCorrectos,ProductosColocarIncorrectos),
	substraer_elementos_correctos(ProductosColocarCorrectos,ProductosColocarIncorrectos,EstantesNoObservados,ListaFinalDistribucion,ArticulosConEstantes,TodasLasConfiguraciones),
	calcular_peso_configuraciones(TodasLasConfiguraciones,LConfiguracion,LPesoConfig),
	funcion_min(LConfiguracion,LPesoConfig,1000,Configuracion,RConfiguracion),
	decripcion_acciones_asistente(RConfiguracion,1),
	encontrar_creencias(KB,Creencias),
	formato_diagnostico(Creencias,RConfiguracion,DiagnosticoConfiguracion)
	.
	
	
	




posibles_distribuciones([],CantidadEstantNObservado,[]).
posibles_distribuciones([Posible|Resto],CantidadEstantNObservado,Y):-
	length(Posible,TamaniTemporal),
	TamaniTemporal>CantidadEstantNObservado->!,posibles_distribuciones(Resto,CantidadEstantNObservado,Y);
	posibles_distribuciones(Resto,CantidadEstantNObservado,Y2),append([Posible],Y2,Y).




llenado_ceros([],CantidadEstantNObservado,[]).
llenado_ceros([Posible|Resto],CantidadEstantNObservado,[PosiblesAct|PosibleResto]):-
	length(Posible,TamaniTemporal),
    TamaniTemporal<CantidadEstantNObservado-> (
    	%write(TamaniTemporal),
    	Cantidad is CantidadEstantNObservado-TamaniTemporal,fill(LZeros,0,Cantidad),
    	union(Posible,LZeros,PosiblesAct),
    	llenado_ceros(Resto,CantidadEstantNObservado,PosibleResto));
    PosiblesAct=Posible,
    llenado_ceros(Resto,CantidadEstantNObservado,PosibleResto).

%calcular_permutaciones_delistas([[1,2,3],[0,1]]).%

calcular_permutaciones_delistas([],[]).
calcular_permutaciones_delistas([PConfiguracion|RConfiguracion],[LConfiguraciones|Resto]):-
	setof(NConfiguraciones,perm(PConfiguracion,NConfiguraciones),LConfiguraciones),
	calcular_permutaciones_delistas(RConfiguracion,Resto)
	.

without_last([_], []).	
without_last([X|Xs], [X|WithoutLast]) :- 
    without_last(Xs, WithoutLast).

vaciar_listas_alista([],_).
vaciar_listas_alista([H|T],Lista):-
	union(H,Lista,Lista2),
	vaciar_listas_alista(T,Lista2)
	.

distribucion_por_estante(ArticulosAPermutar,EstantesNoObservados,ListaFinal):-
	length(ArticulosAPermutar,CantidadArtPermutar),
	length(EstantesNoObservados,CantidadEstantNObservado),
	add_up_list(CantidadArtPermutar,PosiblesDistribuciones),
	posibles_distribuciones(PosiblesDistribuciones,CantidadEstantNObservado,R),
	llenado_ceros(R,CantidadEstantNObservado,ConfiguracionesProductos),
	calcular_permutaciones_delistas(ConfiguracionesProductos,R2),
	vaciar_listas_alista(R2,ListaFinal),
	without_last(ListaFinal,ListaFinalDist)
	.	


%Todas las formas de sumar un número%
num_split(0, []).
num_split(N, [X | List]) :-
    between(1, N, X),
    plus(X, Y, N),
    num_split(Y, List).

add_up_list(N, Splits) :-
    findall(Split, num_split(N, Split), Splits).


%Subconjuntos de tamanio n de una lista%
subset(N, InList, Out) :-
    splitSet(InList,_,SubList),
    permutation(SubList,Out),
    length(Out, N).

splitSet([ ],[ ],[ ]).
splitSet([H|T],[H|L],R) :-
    splitSet(T,L,R).
splitSet([H|T],L,[H|R]) :-
    splitSet(T,L,R).

%Crear listas de Ceros%
fill(L,_,N) :-
   N =< 0,
   L = [].
fill([H|T],X,N) :-
   NewN = N - 1,
   H = X,
   fill(T,X,NewN).

%Permutar una Lista%
takeout(X,[X|R],R).  
takeout(X,[F |R],[F|S]) :- takeout(X,R,S).

perm([X|Y],Z) :- perm(Y,W), takeout(X,Z,W).  
perm([],[]).

appendlist([], X, X).
appendlist([T|H], X, [T|L]) :- appendlist(H, X, L).

permutation([], []).
permutation([X], [X]) :-!.
permutation([T|H], X) :- permutation(H, H1), appendlist(L1, L2, H1), appendlist(L1, [T], X1), appendlist(X1, L2, X).

%%%=================================Toma de Desicion======================================================%%%%

encontrar_tareas_auxiliar([],R).
encontrar_tareas_auxiliar([H|T],R):-
	H=..Lista,
	Lista=[class,robot,Posicion,[_,_,Tareas]]->R=Tareas,!;
	encontrar_tareas_auxiliar(T,R).

encontrar_tareas(Tareas):-
	unificar_kb(KB),
	encontrar_tareas_auxiliar(KB,Tareas).

verificar_diagnostico([]).
verificar_diagnostico([Estante=>LProductos|REstantes]):-
	LProductos=[]->verificar_diagnostico(REstantes);
	fail.


encontrar_diagnostico_intermedio([],R).
encontrar_diagnostico_intermedio([H|T],R):-
	H=..Lista,
	Lista=[class,diagnostico,EstantesDiagnostico]->R=EstantesDiagnostico,!;
	encontrar_diagnostico_intermedio(T,R).

encontrar_diagnostico(EstantesDiagnostico):-
	unificar_kb(KB),
	encontrar_diagnostico_intermedio(KB,EstantesDiagnostico).


obtener_productos_a_ordenar_parcialmente_aux([],[],[]).
obtener_productos_a_ordenar_parcialmente_aux([ProductosDiagnostico|RProductosDiagnostico],[ProductosCreencias|RProductosCreencias],[DiferenciaProductos|RDiferenciaProductos]):-
	ProductosDiagnostico=Estante=>LProductosDiagnostico,
	ProductosCreencias=EstantesCreencias=>LProductosCreencias,
	%write("Las diferencias lista a lista son los siguientes"),nl,
	%write(LProductosCreencias),write("---"),write(LProductosDiagnostico),nl,
	%write("La diferencia es la siguiente"),nl,
	subtract(LProductosCreencias,LProductosDiagnostico,DiferenciaProductos),
	%write(DiferenciaProductos),nl,
	obtener_productos_a_ordenar_parcialmente_aux(RProductosDiagnostico,RProductosCreencias,RDiferenciaProductos)
	.

obtener_productos_a_ordenar_parcialmente(EstantesDiagnostico2,DiferenciaProductosEstantePorEstante):-
	%write("Los estantes del diagnostico son los siguientes"),nl,
	%write(EstantesDiagnostico2),
	unificar_kb(KB),
	encontrar_creencias(KB,Creencias),
	%write("Las creencias son las siguientes"),nl,
	obtener_productos_a_ordenar_parcialmente_aux(EstantesDiagnostico2,Creencias,DiferenciaProductosEstantePorEstante).
	%write("El valor de R es el siguiente"),nl,
	%write(DiferenciaProductosEstantePorEstante)
	

lista_productos_a_lista_ordenar([],[]).
lista_productos_a_lista_ordenar([H|T],[Valor|RValor]):-
	Valor=ordenar=>H,
	lista_productos_a_lista_ordenar(T,RValor).


crear_lista_productos_ordenar_aux([],[]).
crear_lista_productos_ordenar_aux([H|T],[ListaOrdenar|RListaOrdenar]):-
	lista_productos_a_lista_ordenar(H,ListaOrdenar),
	crear_lista_productos_ordenar_aux(T,RListaOrdenar)
	.

crear_lista_productos_ordenar(DiferenciaProductos,ListaObjetos):-
	crear_lista_productos_ordenar_aux(DiferenciaProductos,Lista),
	flatten(Lista,ListaObjetos).

pasar_lista_a_ordenar([],[]).
pasar_lista_a_ordenar([Actividad=>Producto|Resto],[Valor|Rvalor]):-
	Valor=ordenar=>Producto,
	pasar_lista_a_ordenar(Resto,Rvalor)
	.


lista_tareas_unicas(ListaObjetos,LTareas,LTareasFinal):-
	pasar_lista_a_ordenar(LTareas,LTareasModificada),
	%write("Las tareas modificadas son:"),nl,
	%write(LTareasModificada),nl,
	%write(LTareas),nl,
	subtract(ListaObjetos,LTareasModificada,LTareasFinal)
	.


separar_tareas([],[],[]).
separar_tareas([Actividad=>Producto|Resto],[Prod|L1],L2):-
	Actividad=llevar->Prod=Actividad=>Producto,!,separar_tareas(Resto,L1,L2).

separar_tareas([Actividad=>Producto|Resto],L1,[Prod|L2]):-
	Prod=Actividad=>Producto,
	separar_tareas(Resto,L1,L2).

extraer_llevar([],Contador,[]).
extraer_llevar(_,0,[]).
extraer_llevar([Actividad=>Producto|RActividad],Contador,[TareasLLevar|Resto]):-
	TareasLLevar=Actividad=>Producto,
	Contador2 is Contador-1,
	extraer_llevar(RActividad,Contador2,Resto)
	.

obtener_produtos_llevar([],[]).
obtener_produtos_llevar([Actividad=>Producto|Resto],[Producto|RProducto]):-
	obtener_produtos_llevar(Resto,RProducto).

obtener_estantes_llevar([],[]).
obtener_estantes_llevar([Producto=>Estante|Resto],[Estante|RestoEstantes]):-
	obtener_estantes_llevar(Resto,RestoEstantes).



obtener_posicion_inicial_auxiliar([],R).
obtener_posicion_inicial_auxiliar([H|T],R):-
	H=..Lista,
	Lista=[class,robot,PosicionInicial=>Lugar,[_,_,Tareas]]->R=Lugar,!;
	obtener_posicion_inicial_auxiliar(T,R).

obtener_posicion_inicial(PosicionInicial):-
	unificar_kb(KB),
	obtener_posicion_inicial_auxiliar(KB,PosicionInicial).

armar_ruta(EstantesProductosLlevar,Ruta,Ruta2):-
	%write("Los estantes a productos a llevar son los siguientes"),nl,
	%write(EstantesProductosLlevar),
	obtener_posicion_inicial(PosicionInicial),
	append([PosicionInicial],EstantesProductosLlevar,ParteRuta),
	append(ParteRuta,[mostrador],RutaFinal),
	reverse(EstantesProductosLlevar,EstantesProductosLlevarReverso),
	append([PosicionInicial],EstantesProductosLlevarReverso,ParteRuta2),
	append(ParteRuta2,[mostrador],RutaFinal2),
	Ruta=RutaFinal,
	Ruta2=RutaFinal2
	.
pasar_lugar_a_indice(Valor,Indice):-
	Valor=mostrador->Indice is 1;
	remove_char(Valor,e,IndiceString),
	 atom_number(IndiceString, IndiceParcial),
	 Indice is IndiceParcial+1
	 .
obtener_distancias_de_lugar_auxiliar2([],Lugar,DistanciaEncontrada).
obtener_distancias_de_lugar_auxiliar2([[Identificador=>Valor,Distancias]|Resto],Lugar,DistanciaEncontrada):-
	%write(Distancias),nl,
	Valor=Lugar->DistanciaEncontrada=Distancias;
	obtener_distancias_de_lugar_auxiliar2(Resto,Lugar,DistanciaEncontrada).


obtener_distancias_de_lugar_auxiliar([H|T],Lugar,DistanciaEncontrada):-
	H=..Lista,
	Lista=[class,pesos,Lugares]->obtener_distancias_de_lugar_auxiliar2(Lugares,Lugar,DistanciaEncontrada),!;
	obtener_distancias_de_lugar_auxiliar(T,Lugar,DistanciaEncontrada).

%obtener_distancias_de_lugar(mostrador,DistanciaEncontrada)
obtener_distancias_de_lugar(Lugar,DistanciaEncontrada):-
	unificar_kb(KB),
	obtener_distancias_de_lugar_auxiliar(KB,Lugar,DistanciaEncontrada).


funcion_peso_ruta([X],Peso,Pesofinal):-Pesofinal = Peso.
funcion_peso_ruta([X,Y|Resto],Peso,Pesofinal):-
	obtener_distancias_de_lugar(X,DistanciaEncontrada),
	%write(DistanciaEncontrada),nl,
	pasar_lugar_a_indice(Y,Indice),
	nth1(Indice,DistanciaEncontrada,Distancia),
	%write(Distancia),nl,
	Peso2 is Peso+Distancia,
	%write(Peso2),nl,
	funcion_peso_ruta([Y|Resto],Peso2,Pesofinal).

obtener_ruta_menor(R1PesoFinal,R2PesoFinal,Ruta1,Ruta2,RutaFinal):-
	R1PesoFinal =< R2PesoFinal -> RutaFinal=Ruta1;
	RutaFinal=Ruta2.


valor_mano_derecha(EstadoD,Valor):-
	EstadoD=libre ->Valor is 1;
	Valor is 0.

valor_mano_izquierda(EstadoI,Valor):-
	EstadoI=libre ->Valor is 1;
	Valor is 0.

cantidad_brazos_libres(EstadoD,EstadoI,ManosLibres):-
	valor_mano_derecha(EstadoD,ValorD),
	valor_mano_izquierda(EstadoI,ValorI),
	ManosLibres is ValorD+ValorI,
	write(ManosLibres)
	.

encontrar_brazos_libres_auxiliar([],ManosLibres).
encontrar_brazos_libres_auxiliar([H|T],ManosLibres):-
	H=..Lista,
	Lista=[class,robot,Posicion,[ManoDerecha=>EstadoD,ManoIzquierda=>EstadoI,Tareas]]->cantidad_brazos_libres(EstadoD,EstadoI,ManosLibres),!;
	encontrar_brazos_libres_auxiliar(T,ManosLibres).

encontrar_brazos_libres(ManosLibres):-
	unificar_kb(KB),
	encontrar_brazos_libres_auxiliar(KB,ManosLibres).

acualizar_ruta([X,Y|Resto],RutaFinalActualizada):-
	X=Y -> RutaFinalActualizada=[Y|Resto];
	RutaFinalActualizada=[X,Y|Resto].

funcion_diferencias([],[],[]).
funcion_diferencias([EstanteCreencias=>ProductosCreencias|T],[EstanteDiagnostico=>ProductosDiagnostico|W],[EstanteCreencias=>ProductosMalAcomodados|Resto]):-
	subtract(ProductosDiagnostico,ProductosCreencias,ProductosMalAcomodados),
	funcion_diferencias(T,W,Resto).

recobrar_estantes_mal_acomodados([],[]).
recobrar_estantes_mal_acomodados([Estante=>Lista|Resto],[Estante=>ProductosLLevarConEstantes|Restol]):-
	unificar_kb(KB),
	encontrar_productos(KB,S),
	asociar_producto_estante(Lista,S,R,ProductosLLevar),
	encontrar_estantes(EstantesEncontradosCategoria),
	articulos_a_permutar_con_estante(ProductosLLevar,EstantesEncontradosCategoria,ProductosLLevarConEstantes),
	recobrar_estantes_mal_acomodados(Resto,Restol).

%objetos_a_reacomodar_por_estante(ListaObjetosReacomodarPorEstante,EstantesDiagnostico):-
objetos_a_reacomodar_por_estante(ListaObjetosReacomodarPorEstante):-
	encontrar_diagnostico(EstantesDiagnostico),
	unificar_kb(KB),
	encontrar_creencias(KB,Creencias),
	funcion_diferencias(Creencias,EstantesDiagnostico,EstantesObjetosMalAcomodados),
	recobrar_estantes_mal_acomodados(EstantesObjetosMalAcomodados,ListaObjetosReacomodarPorEstante)
	.


encontrar_productos_estante_final(RProductos,IdEstanteFin,RValor):- a_encontrar_productos_estante_final(RProductos,IdEstanteFin,RValor,[]).
a_encontrar_productos_estante_final([],IdEstanteFin,A,A):-!.
a_encontrar_productos_estante_final([Producto=>IdEstante|RProductos],IdEstanteFin,RValor,A):-
	IdEstante=IdEstanteFin->a_encontrar_productos_estante_final(RProductos,IdEstanteFin,RValor,[Producto|A]);
	a_encontrar_productos_estante_final(RProductos,IdEstanteFin,RValor,A).

objetos_por_reacomodar_entre_estantes([],IdEstanteInicio,IdEstanteFin,ArticulosSeleccionados).
objetos_por_reacomodar_entre_estantes([IdEstante=>ProductosAcomodar|REstantes],IdEstanteInicio,IdEstanteFin,ArticulosSeleccionados):-
	IdEstante=IdEstanteInicio-> encontrar_productos_estante_final(ProductosAcomodar,IdEstanteFin,ArticulosSeleccionados);
	objetos_por_reacomodar_entre_estantes(REstantes,IdEstanteInicio,IdEstanteFin,ArticulosSeleccionados).


desicion_four(ListaObjetosReacomodarPorEstante,RutaFinalActualizada,ArticulosAcomodarFinal):-
	RutaFinalActualizada=[P1,P2,P3,P4],
	obtener_importancia_cliente(ImportanciaCliente),
	ElementosTomar is abs(ImportanciaCliente-2),
	objetos_por_reacomodar_entre_estantes(ListaObjetosReacomodarPorEstante,P1,P2,ArticulosSeleccionadoP1P2),
	objetos_por_reacomodar_entre_estantes(ListaObjetosReacomodarPorEstante,P1,P3,ArticulosSeleccionadoP1P3Parcial),
	length(ArticulosSeleccionadoP1P3Parcial,TamanioP1P3),
	(TamanioP1P3=0->take(0,[],ArticulosSeleccionadoP1P3);
		take(1,ArticulosSeleccionadoP1P3Parcial,ArticulosSeleccionadoP1P3)),
	union(ArticulosSeleccionadoP1P2,ArticulosSeleccionadoP1P3,ArticulosAcomodar),
	take(ElementosTomar,ArticulosAcomodar,ArticulosAcomodarPrevio),
	objetos_por_reacomodar_entre_estantes(ListaObjetosReacomodarPorEstante,P2,P3,ArticulosSeleccionadoParcialP2P3),
	take(1,ArticulosSeleccionadoParcialP2P3,ArticulosSeleccionadoP2P3Pendiente),
	take(ElementosTomar,ArticulosSeleccionadoP2P3Pendiente,ArticulosSeleccionadoP2P3),
	union(ArticulosAcomodarPrevio,ArticulosSeleccionadoP2P3,ArticulosAcomodarFinal).


desicion_three(ListaObjetosReacomodarPorEstante,RutaFinalActualizada,ArticulosAcomodarFinal):-
	RutaFinalActualizada=[P1,P2,P3],
	obtener_importancia_cliente(ImportanciaCliente),
	ElementosTomar is abs(ImportanciaCliente-2),
	objetos_por_reacomodar_entre_estantes(ListaObjetosReacomodarPorEstante,P1,P2,ArticulosSeleccionadoParcialP1P2),
	%take(1,ArticulosSeleccionadoParcialP1P2,ArticulosSeleccionadoP1P2Pendiente),
	take(ElementosTomar,ArticulosSeleccionadoParcialP1P2,ArticulosAcomodarFinal).

desicion_dos(ListaObjetosReacomodarPorEstante,RutaFinalActualizada,ArticulosAcomodarFinal):-
	ArticulosAcomodarFinal=[].


objetos_a_reacomodar_en_ruta(ListaObjetosReacomodarPorEstante,RutaFinalActualizada,ArticulosAcomodarFinal):-
	length(RutaFinalActualizada,TamanioRuta),
	(TamanioRuta=4->desicion_four(ListaObjetosReacomodarPorEstante,RutaFinalActualizada,ArticulosAcomodarFinal);
	 TamanioRuta=3->desicion_three(ListaObjetosReacomodarPorEstante,RutaFinalActualizada,ArticulosAcomodarFinal);
	 TamanioRuta=2->desicion_dos(ListaObjetosReacomodarPorEstante,RutaFinalActualizada,ArticulosAcomodarFinal)).

obtener_importancia_cliente_aux([],R).
obtener_importancia_cliente_aux([H|T],R):-
	H=..Lista,
	Lista=[class,cliente,Importancia=>Valor]->R=Valor,!;
	obtener_importancia_cliente_aux(T,R).

obtener_importancia_cliente(ImportanciaCliente):-
	unificar_kb(KB),
	obtener_importancia_cliente_aux(KB,ImportanciaCliente).

distancia_entre_puntos(P1,P2,Distancia):-
	obtener_distancias_de_lugar(P1,DistanciaEncontrada),
	pasar_lugar_a_indice(P2,Indice),
	nth1(Indice,DistanciaEncontrada,Distancia).


funcion_localidad_producto_auxiliar(Producto,[],Valor).
funcion_localidad_producto_auxiliar(Producto,[IdEstante=>LProductos|Resto],Valor):-
	(member(Producto=>X,LProductos)->Valor=Producto=>IdEstante,!;
	funcion_localidad_producto_auxiliar(Producto,Resto,Valor)).

funcion_localidad_producto([],ListaObjetosReacomodarPorEstante,[]).
funcion_localidad_producto([Producto|RProducto],ListaObjetosReacomodarPorEstante,[Valor|Rvalor]):-
	funcion_localidad_producto_auxiliar(Producto,ListaObjetosReacomodarPorEstante,Valor),
	funcion_localidad_producto(RProducto,ListaObjetosReacomodarPorEstante,Rvalor).



funcion_maximar_productividad_aux_2(RutaFinalActualizada,[],[],[]).
funcion_maximar_productividad_aux_2(RutaFinalActualizada,[Producto=>Estante|Rproductos],[Producto=>Estante|L1],L2):-
	member(Estante,RutaFinalActualizada)->!,funcion_maximar_productividad_aux_2(RutaFinalActualizada,Rproductos,L1,L2).

funcion_maximar_productividad_aux_2(RutaFinalActualizada,[Producto=>Estante|Rproductos],L1,[Producto=>Estante|L2]):-
	funcion_maximar_productividad_aux_2(RutaFinalActualizada,Rproductos,L1,L2).

funcion_maximar_productividad_aux(RutaFinalActualizada,[],[],[]).
funcion_maximar_productividad_aux(RutaFinalActualizada,[IdEstante=>Productos|Resto],[IdEstante=>L1|RL1],[IdEstante=>L2|RL2]):-
	funcion_maximar_productividad_aux_2(RutaFinalActualizada,Productos,L1,L2),
	funcion_maximar_productividad_aux(RutaFinalActualizada,Resto,RL1,RL2).

funcion_maximar_productividad(RutaFinalActualizada,ListaObjetosReacomodarPorEstante,DistanciasAFiltrar):-
	funcion_maximar_productividad_aux(RutaFinalActualizada,ListaObjetosReacomodarPorEstante,Lista1,DistanciasAFiltrar).



obtener_distancias_de_punto_auxiliar2([],Lugar,DistanciaEncontrada).
obtener_distancias_de_punto_auxiliar2([[Identificador=>Valor,Distancias]|Resto],Lugar,DistanciaEncontrada):-
	%write(Distancias),nl,
	Valor=Lugar->DistanciaEncontrada=Distancias;
	obtener_distancias_de_punto_auxiliar2(Resto,Lugar,DistanciaEncontrada).


obtener_distancias_de_punto_auxiliar([H|T],Lugar,DistanciaEncontrada):-
	H=..Lista,
	Lista=[class,distancias,Lugares]->obtener_distancias_de_punto_auxiliar2(Lugares,Lugar,DistanciaEncontrada),!;
	obtener_distancias_de_punto_auxiliar(T,Lugar,DistanciaEncontrada).

%obtener_distancias_de_punto(mostrador,DistanciaEncontrada)
obtener_distancias_de_punto(Lugar,DistanciaEncontrada):-
	unificar_kb(KB),
	obtener_distancias_de_punto_auxiliar(KB,Lugar,DistanciaEncontrada).

encontrar_distancias_dos_puntos(P1,P2,Distancia):-
	obtener_distancias_de_punto(P1,LDistancias),
	pasar_lugar_a_indice(P2,Indice),
	nth1(Indice,LDistancias,Distancia).


filtrar_distancias_aux(IdEstante,[],[],[]).
filtrar_distancias_aux(IdEstante,[Producto=>Estante|Resto],[Producto=>Estante|L1],L2):-
	encontrar_distancias_dos_puntos(IdEstante,Estante,Distancia),
	Distancia=2->!,filtrar_distancias_aux(IdEstante,Resto,L1,L2).

filtrar_distancias_aux(IdEstante,[Producto=>Estante|Resto],L1,[Producto=>Estante|L2]):-
	filtrar_distancias_aux(IdEstante,Resto,L1,L2).

filtrar_distancias([],[]).
filtrar_distancias([IdEstante=>Productos|Resto],[IdEstante=>L2|RestoD]):-
	filtrar_distancias_aux(IdEstante,Productos,L1,L2),
	filtrar_distancias(Resto,RestoD).


union_productos_acomodar_aux([],Val).
union_productos_acomodar_aux([Producto=>Estante|Resto],Val):-
	union(Estante,[],Val2),
	union_productos_acomodar_aux(Resto,Val2).



union_productos_acomodar(Encontrados):-
	union_productos_acomodar_aux(Encontrados,ValoresFiltrados),
	write("ValoresFiltrados"),
	write(ValoresFiltrados).

objetos_filtrar([],[]).
objetos_filtrar([Producto=>Estante|Resto],[Valor|Resto2]):-
	Valor=llevar=>Producto,
	objetos_filtrar(Resto,Resto2).

regresar_estantes_a_objetos_ordenar_auxiliar([],_,_).
regresar_estantes_a_objetos_ordenar_auxiliar([Producto=>Estante|RestoValores],EncontrarProducto,Valor):-
	(Producto=EncontrarProducto->Valor=Estante,!;
	regresar_estantes_a_objetos_ordenar_auxiliar(RestoValores,EncontrarProducto,Valor)).

regresar_estantes_a_objetos_ordenar(Encontrados,[],[]).
regresar_estantes_a_objetos_ordenar(Encontrados,[LLevar=>Producto|Resto],[Producto=>Estante|Resto2]):-
	regresar_estantes_a_objetos_ordenar_auxiliar(Encontrados,Producto,Estante),
	regresar_estantes_a_objetos_ordenar(Encontrados,Resto,Resto2).



actualizar_lista([],[]).
actualizar_lista([Producto=>Estante|Resto],[Val|RVal]) :-
	Val=Producto=>Estante,
    actualizar_lista(Resto,RVal).


proceso_armar_ruta(RutaFinalActualizada,RutaProcesar):-
	subtract(RutaFinalActualizada,[mostrador],RutaProcesar).

proceso_armar_ruta_de_ordenar_aux(Estante,[],Valor).
proceso_armar_ruta_de_ordenar_aux(Estante,[IdEstante=>LProductos|Resto],Valor):-
	(IdEstante=Estante->Valor=LProductos,!;
	 proceso_armar_ruta_de_ordenar_aux(Estante,Resto,Valor)).




proceso_armar_ruta_de_ordenar_aux2(Estante,[],[],[]).
proceso_armar_ruta_de_ordenar_aux2(Estante,[Producto=>IdEstante|Rproductos],[Producto=>Estante|L1],L2):-
	Estante=IdEstante->!,proceso_armar_ruta_de_ordenar_aux2(Estante,Rproductos,L1,L2).

proceso_armar_ruta_de_ordenar_aux2(Estante,[Producto=>IdEstante|Rproductos],L1,[Producto=>Estante|L2]):-
	proceso_armar_ruta_de_ordenar_aux2(Estante,Rproductos,L1,L2).


proceso_armar_ruta_de_ordenar_aux3(Estante,[],[],[]).
proceso_armar_ruta_de_ordenar_aux3(Estante,[Producto=>IdEstante|Rproductos],[Producto=>Estante|L1],L2):-
	Estante=IdEstante->!,proceso_armar_ruta_de_ordenar_aux3(Estante,Rproductos,L1,L2).

proceso_armar_ruta_de_ordenar_aux3(Estante,[Producto=>IdEstante|Rproductos],L1,[Producto=>Estante|L2]):-
	proceso_armar_ruta_de_ordenar_aux3(Estante,Rproductos,L1,L2).

proces_reasignar_estante([],[]).
proces_reasignar_estante([Producto=>IdEstanteInicio|Resto],[Producto|R]):-
	proces_reasignar_estante(Resto,R).


proceso_generar_lista_de_desiciones1([],[]).
proceso_generar_lista_de_desiciones1([Producto=>Estante|Resto],[Valor|Rvalor]):-
	Valor=bring(robot,Producto,Estante),
	proceso_generar_lista_de_desiciones1(Resto,Rvalor).

proceso_generar_lista_de_desiciones2([],[]).
proceso_generar_lista_de_desiciones2([Producto=>Estante|Resto],[Valor|Rvalor]):-
	Valor=bring(robot,Producto,Estante),
	proceso_generar_lista_de_desiciones2(Resto,Rvalor).

proceso_generar_lista_de_desiciones3([],[]).
proceso_generar_lista_de_desiciones3([Producto=>Estante|Resto],[Valor|Rvalor]):-
	Valor=bring(robot,Producto,client),
	proceso_generar_lista_de_desiciones3(Resto,Rvalor).

union_proceso1(LProductosProceso1,LProductosProceso2,LProductosProceso3,UnionProceso1):-
	union(LProductosProceso1,LProductosProceso2,UnionProceso1P),
	union(UnionProceso1P,LProductosProceso3,UnionProceso1).

proceso_armar_ruta_de_ordenar([],DistanciasCorrectas,LFinalProductosAcomodar,ProductosLLevarConEstantes,ProcesoFinal,Rvalor).
proceso_armar_ruta_de_ordenar([Estante|Resto],DistanciasCorrectas,LFinalProductosAcomodar,ProductosLLevarConEstantes,ProcesoFinal,[Valor|Rvalor]):-
	%write("###############################"),nl,
	proceso_armar_ruta_de_ordenar_aux(Estante,DistanciasCorrectas,LProductosParcial),
	obtener_importancia_cliente(ImportanciaCliente),
	ElementosTomar is abs(ImportanciaCliente-2),
	take(ElementosTomar,LProductosParcial,LProductos),
	proceso_generar_lista_de_desiciones1(LProductos,LProductosProceso1),
	%write(Estante),write("->"),write(LProductosProceso1),nl,
	proceso_armar_ruta_de_ordenar_aux2(Estante,LFinalProductosAcomodar,L1,L2),
	proces_reasignar_estante(L1,ListaReasignarEstante),
	articulos_a_permutar(ArticulosAPermutar),
	distribucion_por_estante(ArticulosAPermutar,EstantesNoObservados,ListaFinalDistribucion),
	unificar_kb(KB),
	encontrar_productos(KB,S),
	encontrar_estantes(EstantesEncontradosCategoria),
	asociar_producto_estante(ListaReasignarEstante,S,R,ProductosAPermutarCategoria),
	articulos_a_permutar_con_estante(ProductosAPermutarCategoria,EstantesEncontradosCategoria,ArticulosConEstantes),
	proceso_generar_lista_de_desiciones2(ArticulosConEstantes,LProductosProceso2),
	%write("La lista es la siguiente:"),write(LProductosProceso2),nl,
	proceso_armar_ruta_de_ordenar_aux3(Estante,ProductosLLevarConEstantes,L3,L4),
	proceso_generar_lista_de_desiciones3(L3,LProductosProceso3),
	%write("Las cosas por llevar son las siguientes"),nl,
	%write(LProductosProceso3),nl,
	union_proceso1(LProductosProceso1,LProductosProceso2,LProductosProceso3,UnionProceso1),
	%write("=============================================================="),nl,
	%write(UnionProceso1),nl,
	union(UnionProceso1,ProcesoFinal,UnionProceso2),
	Valor=UnionProceso1,
	proceso_armar_ruta_de_ordenar(Resto,DistanciasCorrectas,LFinalProductosAcomodar,ProductosLLevarConEstantes,UnionProceso2,Rvalor).




actualizar_lista2([],[]).
actualizar_lista2([H|Resto],[Val|RVal]) :-
	Val=H,
    actualizar_lista2(Resto,RVal).


imprimir_desiciones([]).
imprimir_desiciones([Desicion|RDesiciones]):-
	write(Desicion),nl,
	imprimir_desiciones(RDesiciones).

desicion(ListaDeDesiciones):-
	desicion_parcial(ListaDeDesiciones,Valor,RutaFinalActualizada),
	write("=====================Las Desiciones Son las Siguientes =================================================="),nl,
	imprimir_desiciones(ListaDeDesiciones).


function_correc_desicion_aux([],Producto,Valor).
function_correc_desicion_aux([IdEstante=>Productos|Res],Producto,Valor):-
	member(Producto,Productos)->Valor=IdEstante,!;
	function_correc_desicion_aux(Res,Producto,Valor).


function_correc_desicion([],[]).
function_correc_desicion([LLevar=>Producto|RProducots],[Producto=>Valor|RestoProducto]):-
	encontrar_diagnostico(Diagnostico),
	function_correc_desicion_aux(Diagnostico,Producto,Valor),
	function_correc_desicion(RProducots,RestoProducto).

%EstantesDiagnostico
%Valor son las desiciones que se podran hacer por estante.
desicion_parcial(ListaDeDesiciones,Valor,RutaFinalActualizada):-
	encontrar_tareas(Tareas),	
	Tareas=Actividad=>LTareas,
	encontrar_diagnostico(EstantesDiagnostico),
	length(LTareas,TamanioTareas),
	(verificar_diagnostico(EstantesDiagnostico),TamanioTareas=<2 )-> (write("La Desicion es la siguiente :"),write(LTareas));
	encontrar_diagnostico(EstantesDiagnostico2),
	encontrar_tareas(Tareas),	
	Tareas=Actividad=>LTareas,
	obtener_productos_a_ordenar_parcialmente(EstantesDiagnostico2,DiferenciaProductos),
	crear_lista_productos_ordenar(DiferenciaProductos,ListaObjetos),
	lista_tareas_unicas(ListaObjetos,LTareas,LTareasFinal),
	%write("Las tareas llevar"),nl,
	union(LTareas,LTareasFinal,ListaTareasPorHacer),
	%write(ListaTareasPorHacer),nl,
	separar_tareas(ListaTareasPorHacer,TareasLLevar,TareasOrdenar),
	%write(TareasLLevar),nl,
	%write(TareasOrdenar),nl,
	extraer_llevar(TareasLLevar,2,TareasALoMas),
	function_correc_desicion(TareasALoMas,Lista),
	unificar_kb(KB),
	encontrar_productos(KB,S),
	obtener_produtos_llevar(TareasALoMas,TareasALoMasSoloProductos),
	asociar_producto_estante(TareasALoMasSoloProductos,S,R,ProductosLLevar),
	%write("=============ProductosLLevar================"),nl,
	%write(ProductosLLevar),nl,
	encontrar_estantes(EstantesEncontradosCategoria),
	%write("=============EstantesEncontradosCategoria================"),nl,
	%write(EstantesEncontradosCategoria),nl,
	articulos_a_permutar_con_estante(ProductosLLevar,EstantesEncontradosCategoria,ProductosLLevarConEstantes),
	%write("=========ProductosLLevarConEstantes==============="),nl,
	%write(ProductosLLevarConEstantes),nl,
	obtener_estantes_llevar(Lista,EstantesProductosLlevar),
	%write(EstantesProductosLlevar),
	armar_ruta(EstantesProductosLlevar,R1,R2),
	%write("R1 y R2:  "),nl,
	%write(R1),nl,
	%write(R2),nl,
	funcion_peso_ruta(R1,0,R1PesoFinal),
	funcion_peso_ruta(R2,0,R2PesoFinal),
	obtener_ruta_menor(R1PesoFinal,R2PesoFinal,R1,R2,RutaFinal),	
	%write("La ruta que usaremos es la siguiente"),nl,
	%write(RutaFinal),
	acualizar_ruta(RutaFinal,RutaFinalActualizada),
	%write("Ruta Actualizada"),nl,
	%write(RutaFinalActualizada),nl,
	objetos_a_reacomodar_por_estante(ListaObjetosReacomodarPorEstante),
	%write("Los objetos a reacomodar por estante son los siguientes"),nl,
	%write(ListaObjetosReacomodarPorEstante),nl,
	objetos_a_reacomodar_en_ruta(ListaObjetosReacomodarPorEstante,RutaFinalActualizada,ArticulosAcomodarFinal),
	%write("Articulos a Acomodar con IMPORTANCIA"),nl,
	%write(ArticulosAcomodarFinal),nl,
	funcion_localidad_producto(ArticulosAcomodarFinal,ListaObjetosReacomodarPorEstante,Encontrados),
	%write("Los ENCONTRADOS SON:"),nl,
	%write(Encontrados),!,nl,
	funcion_maximar_productividad(RutaFinalActualizada,ListaObjetosReacomodarPorEstante,DistanciasAFiltrar),
	%write(DistanciasAFiltrar),nl,
	filtrar_distancias(DistanciasAFiltrar,DistanciasCorrectas),
	%write(DistanciasCorrectas),nl,
	objetos_filtrar(Encontrados,ArticulosTodosALLevar),
	%write(ArticulosTodosALLevar),nl,
	subtract(ArticulosTodosALLevar,TareasALoMas,ProductosFinalesAcomodar),
	%write(ProductosFinalesAcomodar),nl,
	regresar_estantes_a_objetos_ordenar(Encontrados,ProductosFinalesAcomodar,LFinalProductosAcomodar),
	%write(LFinalProductosAcomodar),nl,
	%write("================================================="),nl,
	%write("LA ruta a Procesar es la siguiente"),nl,
	proceso_armar_ruta(RutaFinalActualizada,RutaProcesar),
	%write(RutaProcesar),nl,
	proceso_armar_ruta_de_ordenar(RutaProcesar,DistanciasCorrectas,LFinalProductosAcomodar,Lista,UnionProceso2,Valor),
	%write("********************************************************************************"),
	%actualizar_lista(UnionProceso2,ListaDeDesiciones),
	actualizar_lista2(UnionProceso2,ListaDeDesiciones)
	.

asignar_estante_a_acciones_estante(_,[],[]).
asignar_estante_a_acciones_estante([Lugar|RLugares],[Acciones|RAcciones],[LugarConAcciones|RestoLugarConAcciones]):-
	LugarConAcciones=Lugar=>Acciones,
	asignar_estante_a_acciones_estante(RLugares,RAcciones,RestoLugarConAcciones).


iterar_ruta_aux(Lugar,[],_).
iterar_ruta_aux(Lugar,[IdLugar=>ListaAcciones|RestoLugares],ListaInteresada):-
	(Lugar=IdLugar->ListaInteresada=ListaAcciones;
	 Lugar=mostrador->ListaInteresada=[];
	iterar_ruta_aux(Lugar,RestoLugares,ListaInteresada)).

limpiar_tareas([],[]).
limpiar_tareas([Actividad=>Producto|Resto],[Producto|RProd]):-
	limpiar_tareas(Resto,RProd).

separar_productos(Estante,[],[],[]).
separar_productos(Estante,[Producto=>IdEstante|Rproductos],[Producto=>IdEstante|L1],L2):-
	Estante=IdEstante->!,separar_productos(Estante,Rproductos,L1,L2).

separar_productos(Estante,[Producto=>IdEstante|Rproductos],L1,[Producto=>IdEstante|L2]):-
	separar_productos(Estante,Rproductos,L1,L2).

leyenda_colocar([],[]).
leyenda_colocar([Producto=>Estante|RestoEstantes],[Leyenda|RLeyenda]):-
	Leyenda=colocar(Producto),
	leyenda_colocar(RestoEstantes,RLeyenda).

colocar_objetos_mano(ArticulosEnMano,Lugar,ListaLeyendas):-
	encontrar_tareas(Tareas),
	Tareas=Tar=>LTareas,
	limpiar_tareas(LTareas,ArticulosSinActividadesDeTareas),
	subtract(ArticulosEnMano,ArticulosSinActividadesDeTareas,ArticulosActividadPorHacer),
	unificar_kb(KB),
	encontrar_productos(KB,S),
	encontrar_estantes(EstantesEncontradosCategoria),
	asociar_producto_estante(ArticulosActividadPorHacer,S,R,ProductosAPermutarCategoria),
	articulos_a_permutar_con_estante(ProductosAPermutarCategoria,EstantesEncontradosCategoria,ArticulosActividadPor),
	separar_productos(Lugar,ArticulosActividadPor,L1,L2),
	leyenda_colocar(L1,ListaLeyendas)
	.


funcion_remplazar(ObjetosEnMano,[],[]).
funcion_remplazar(ObjetosEnMano,[Elemento|Resto],[Valor|RValor]):-
	(member(Elemento,ObjetosEnMano)->Valor=libre;
	Valor=Elemento),
	funcion_remplazar(ObjetosEnMano,Resto,RValor).

acciones_a_realizar_en_estante(Lugar,[],ListaLeyendas,BrazosActualizados).
acciones_a_realizar_en_estante(Lugar,[Acciones|RAcciones],ListaLeyendas,BrazosActualizados):-
	%write("Voy hacer las siguienes acciones"),write(Acciones),nl,
	%write("Estoy en el lugar "),write(Lugar),nl,
	encontrar_brazos_lib(Brazos),
	subtract(Brazos,[libre],ObjetosEnMano),
	unificar_kb(KB),
	encontrar_productos(KB,S),
	encontrar_estantes(EstantesEncontradosCategoria),
	asociar_producto_estante(ObjetosEnMano,S,R,ProductosAPermutarCategoria),
	articulos_a_permutar_con_estante(ProductosAPermutarCategoria,EstantesEncontradosCategoria,ArticulosEnMano),
	%write("***"),
	%write(Lugar),nl,
	colocar_objetos_mano(ObjetosEnMano,Lugar,ListaLeyendas),
	funcion_remplazar(ObjetosEnMano,Brazos,BrazosActualizados)
	%write("Obejros en mano "),write(BrazosActualizados)
	.

obtener_lista_de_filtro(Lugar,[],ValorDeLista).
obtener_lista_de_filtro(Lugar,[IdEstant=>ListaAcciones|RestoEstantes],ValorDeLista):-
	IdEstant=Lugar->ValorDeLista=ListaAcciones;
	obtener_lista_de_filtro(Lugar,RestoEstantes,ValorDeLista).


funcion_filtrar_auxiliar_estantes(RutaFijaActualizada,[],[],[]).
funcion_filtrar_auxiliar_estantes(RutaFijaActualizada,[bring(X,Y,Estante)|Rproductos],[bring(X,Y,Estante)|L1],L2):-
	member(Estante,RutaFijaActualizada)->!,funcion_filtrar_auxiliar_estantes(RutaFijaActualizada,Rproductos,L1,L2).

funcion_filtrar_auxiliar_estantes(RutaFijaActualizada,[bring(X,Y,Estante)|Rproductos],L1,[bring(X,Y,Estante)|L2]):-
	funcion_filtrar_auxiliar_estantes(RutaFijaActualizada,Rproductos,L1,L2).


funcion_filtrar_auxiliar_estantes2(Lugar,[],[],[]).
funcion_filtrar_auxiliar_estantes2(Lugar,[bring(X,Y,Estante)|Rproductos],[bring(X,Y,Estante)|L1],L2):-
	encontrar_distancias_dos_puntos(Lugar,Estante,Distancia),
	Distancia=2->!,funcion_filtrar_auxiliar_estantes2(Lugar,Rproductos,L1,L2).

funcion_filtrar_auxiliar_estantes2(Lugar,[bring(X,Y,Estante)|Rproductos],L1,[bring(X,Y,Estante)|L2]):-
	funcion_filtrar_auxiliar_estantes2(Lugar,Rproductos,L1,L2).


funcion_filtrar(Lugar,ListaAcciones,RutaFijaActualizada,ListaPosiblesObejtosAcomodarCercanos):-
	%write("Estoy en la funcion FILTRAR"),nl,
	obtener_lista_de_filtro(Lugar,ListaAcciones,ValorDeLista),
	subtract(ValorDeLista,[bring(X,Y,client)],ValorDeListaSinClientes),
	funcion_filtrar_auxiliar_estantes(RutaFijaActualizada,ValorDeListaSinClientes,L1,ListaPosiblesObejtosAcomodar),
	funcion_filtrar_auxiliar_estantes2(Lugar,ListaPosiblesObejtosAcomodar,L3,ListaPosiblesObejtosAcomodarCercanos)
	.




movimientos_a_acciones([],[],[]).
movimientos_a_acciones([Movimientos|RMovimientos],[PuntoRuta|RPuntoRuta],[NuevaConfiguracion|RNuevaConfiguracion]):-
	NuevaConfiguracion=PuntoRuta=>Movimientos,
	movimientos_a_acciones(RMovimientos,RPuntoRuta,RNuevaConfiguracion).


iterar_ruta([],LugarConAcciones,[],ListaInteresada,RutaFijaActualizada).
iterar_ruta([Lugar|RestoLugares],LugarConAcciones,[ListaAcciones|RListaAcciones],ListaInteresada,RutaFijaActualizada):-
	RestoLugares=[H|T],
	iterar_ruta_aux(Lugar,LugarConAcciones,ListaInteresada),
	not(RestoLugares=[]),ListaInteresada=[]->ListaAcciones=[],iterar_ruta(RestoLugares,LugarConAcciones,RListaAcciones,ListaInteresada,RutaFijaActualizada);
    RestoLugares=[],ListaInteresada=[]->ListaAcciones=[],iterar_ruta(RestoLugares,LugarConAcciones,RListaAcciones,ListaInteresada,RutaFijaActualizada);
	iterar_ruta_aux(Lugar,LugarConAcciones,ListaInteresada2),
	acciones_a_realizar_en_estante(Lugar,ListaInteresada2,ListaAccion,BrazosActualizados),
	%write("Brazos:"),write(BrazosActualizados),nl,
	funcion_filtrar(Lugar,LugarConAcciones,RutaFijaActualizada,ListaPosiblesObejtosAcomodarCercanos),
	union(ListaPosiblesObejtosAcomodarCercanos,ListaAccion,ListaAcciones),
	iterar_ruta(RestoLugares,LugarConAcciones,RListaAcciones,ListaInteresada,RutaFijaActualizada).


iterar_ruta2([],LugarConAcciones,[],ListaInteresada,RutaFijaActualizada).
iterar_ruta2([Lugar|RestoLugares],LugarConAcciones,[ListaAcciones|RListaAcciones],ListaInteresada,RutaFijaActualizada):-
	RestoLugares=[H|T],
	iterar_ruta_aux(Lugar,LugarConAcciones,ListaInteresada),
	not(RestoLugares=[]),ListaInteresada=[]->ListaAcciones=[],iterar_ruta2(RestoLugares,LugarConAcciones,RListaAcciones,ListaInteresada,RutaFijaActualizada);
    RestoLugares=[],ListaInteresada=[]->ListaAcciones=[],iterar_ruta2(RestoLugares,LugarConAcciones,RListaAcciones,ListaInteresada,RutaFijaActualizada);
	iterar_ruta_aux(Lugar,LugarConAcciones,ListaInteresada2),
	acciones_a_realizar_en_estante(Lugar,ListaInteresada2,ListaAccion,BrazosActualizados),
	%write("Brazos:"),write(BrazosActualizados),nl,
	f_filtrar(Lugar,LugarConAcciones,RutaFijaActualizada,ListaPosiblesObejtosAcomodarCercanos),
	union(ListaPosiblesObejtosAcomodarCercanos,ListaAccion,ListaAcciones),
	iterar_ruta2(RestoLugares,LugarConAcciones,RListaAcciones,ListaInteresada,RutaFijaActualizada).

encontrar_brazos([],Brazos).
encontrar_brazos([H|T],Brazos):-
	H=..Lista,
	Lista=[class,robot,Posicion,[ManoDerecha=>EstadoD,ManoIzquierda=>EstadoI,Tareas]]->Brazos=[EstadoD,EstadoI],!;
	encontrar_brazos(T,Brazos).

encontrar_brazos_lib(Brazos):-
	unificar_kb(KB),
	encontrar_brazos(KB,Brazos).

checar_brings([],Contador,Contador).
checar_brings([H|T],Contador,R):-
	(H=bring(X,Y,Z)-> Contador2 is Contador+1,checar_brings(T,Contador2,R);
	checar_brings(T,Contador,R)).

objetos_a_llevar1([],Lugar,[]).
objetos_a_llevar1([H|T],Lugar,[Actividades|RActividades]):-
	(H=bring(X,Producto,LLevas)->Actividades=[buscar=>(Producto),agarrar=>(Producto),mover(Lugar,LLevas),colocar(Producto),mover(LLevas,Lugar)],objetos_a_llevar1(T,Lugar,RActividades);
	Actividades=H,objetos_a_llevar1(T,Lugar,RActividades)).

objetos_a_llevar2([bring(X,Producto,LLevas),bring(Y,Producto2,LLevas2)],Lugar,Actividades):-
	(LLevas=LLevas2->Actividades=[buscar=>(Producto),agarrar=>(Producto),buscar=>(Producto2),agarrar=>(Producto2),mover(Lugar,LLevas),colocar(Producto),colocar(Producto2),mover(LLevas,Lugar)];
	Actividades=[buscar(Producto),agarrar(Producto),mover(Lugar,LLevas),colocar(Producto),mover(LLevas,Lugar),buscar(Producto2),agarrar(Producto2),mover(Lugar,LLevas2),colocar(Producto2),mover(LLevas2,Lugar)])
	.

funcion_filtrar_brings([],[],[]).
funcion_filtrar_brings([H|Rproductos],[H|L1],L2):-
	%write("Entre a brings"),nl,
	H=bring(X,Y,Estante)->funcion_filtrar_brings(Rproductos,L1,L2),!.

funcion_filtrar_brings([H|Rproductos],L1,[H|L2]):-
	funcion_filtrar_brings(Rproductos,L1,L2).

funcion_entregar_objetos_cercanos([],[]).
funcion_entregar_objetos_cercanos([Lugar=>Acciones|RestoAcciones],[ValorLista|RValorLista]):-
	checar_brings(Acciones,0,NBrings),
	(
	 NBrings=0->ValorLista=Acciones,funcion_entregar_objetos_cercanos(RestoAcciones,RValorLista);
	 NBrings=1->objetos_a_llevar1(Acciones,Lugar,ValorListaP),flatten(ValorListaP,ValorLista),funcion_entregar_objetos_cercanos(RestoAcciones,RValorLista);
	 NBrings=2->funcion_filtrar_brings(Acciones,L1,L2),objetos_a_llevar2(L1,Lugar,ValorListaP),union(L2,ValorListaP,ValorLista),funcion_entregar_objetos_cercanos(RestoAcciones,RValorLista)

	 	 ).

f_filtrar_auxiliar_estantes(RutaFijaActualizada,[],[],[]).
f_filtrar_auxiliar_estantes(RutaFijaActualizada,[bring(X,Y,Estante)|Rproductos],[bring(X,Y,Estante)|L1],L2):-
	member(Estante,RutaFijaActualizada)->!,f_filtrar_auxiliar_estantes(RutaFijaActualizada,Rproductos,L1,L2).

f_filtrar_auxiliar_estantes(RutaFijaActualizada,[bring(X,Y,Estante)|Rproductos],L1,[bring(X,Y,Estante)|L2]):-
	f_filtrar_auxiliar_estantes(RutaFijaActualizada,Rproductos,L1,L2).


f_filtrar_auxiliar_estantes2(Lugar,[],[],[]).
f_filtrar_auxiliar_estantes2(Lugar,[bring(X,Y,Estante)|Rproductos],[bring(X,Y,Estante)|L1],L2):-
	encontrar_distancias_dos_puntos(Lugar,Estante,Distancia),
	Distancia=2->!,f_filtrar_auxiliar_estantes2(Lugar,Rproductos,L1,L2).

f_filtrar_auxiliar_estantes2(Lugar,[bring(X,Y,Estante)|Rproductos],L1,[bring(X,Y,Estante)|L2]):-
	f_filtrar_auxiliar_estantes2(Lugar,Rproductos,L1,L2).


f_filtrar(Lugar,ListaAcciones,RutaFijaActualizada,ListaPosiblesObejtosAcomodarCercanos):-
	%write("Estoy en la funcion FILTRAR"),nl,
	obtener_lista_de_filtro(Lugar,ListaAcciones,ValorDeLista),
	subtract(ValorDeLista,[bring(X,Y,client)],ValorDeListaSinClientes),
	f_filtrar_auxiliar_estantes(RutaFijaActualizada,ValorDeListaSinClientes,ListaPosiblesObejtosAcomodarCercanos,L2)
	.

funcion_lista_acciones2([],[],[]).
funcion_lista_acciones2([Acciones|RAcciones],[Lugar|RLugares],[FormatoRuta|RestoFormatoRuta]):-
	FormatoRuta=Lugar=>Acciones,
	funcion_lista_acciones2(RAcciones,RLugares,RestoFormatoRuta).



objetos_a_llevar_en_ruta1([],Lugar,[]).
objetos_a_llevar_en_ruta1([H|T],Lugar,[Actividades|RActividades]):-
	(H=bring(X,Producto,LLevas)->Actividades=[buscar=>(Producto),agarrar=>(Producto)],objetos_a_llevar_en_ruta1(T,Lugar,RActividades);
	Actividades=[],objetos_a_llevar_en_ruta1(T,Lugar,RActividades)).


objetos_a_llevar_en_ruta2([bring(X,Producto,LLevas),bring(Y,Producto2,LLevas2)],Lugar,Actividades):-
	Actividades=[buscar=>(Producto),agarrar=>(Producto),buscar=>(Producto2),agarrar=>(Producto2)].


funcion_entregar_objetos_en_ruta([],[]).
funcion_entregar_objetos_en_ruta([Lugar=>Acciones|RestoAcciones],[ValorLista|RValorLista]):-
	checar_brings(Acciones,0,NBrings),
	(
	 NBrings=0->ValorLista=Acciones,funcion_entregar_objetos_en_ruta(RestoAcciones,RValorLista);
	 NBrings=1->objetos_a_llevar_en_ruta1(Acciones,Lugar,ValorListaP),flatten(ValorListaP,ValorLista),funcion_entregar_objetos_en_ruta(RestoAcciones,RValorLista);
	 NBrings=2->funcion_filtrar_brings(Acciones,L1,L2),objetos_a_llevar_en_ruta2(L1,Lugar,ValorListaP),union(L2,ValorListaP,ValorLista),funcion_entregar_objetos_en_ruta(RestoAcciones,RValorLista)

	 	 ).

union_penultimo_plan([],[],[]).
union_penultimo_plan([NC3|RNC3],[NC2|RNC2],[NCV|RNCV]):-
	union(NC2,NC3,NCV),
	union_penultimo_plan(RNC3,RNC2,RNCV).

flitrar_clientes_aux([],[],[]).
flitrar_clientes_aux([H|Rproductos],[H|L1],L2):-
	H=bring(X,Y,client)->!,flitrar_clientes_aux(Rproductos,L1,L2).

flitrar_clientes_aux([bring(X,Y,Estante)|Rproductos],L1,[H|L2]):-
	flitrar_clientes_aux(Rproductos,L1,L2).

flitrar_clientes([],[]).
flitrar_clientes([IdEstante=>Lista|Resto],[Valor|RValor]):-
	flitrar_clientes_aux(Lista,L1,L2),
	%write("LAS LS"),nl,
	%write(L1),nl,
	Valor=IdEstante=>L1,
	flitrar_clientes(Resto,RValor).

brings_clientes_acciones_aux([],[]).
brings_clientes_acciones_aux([bring(X,Y,Cliente)|Resto],[AccionesCliente|RAccionesCliente]):-
	AccionesCliente=[buscar(Y),agarrar(Y)],
	%write(AccionesCliente),
	brings_clientes_acciones_aux(Resto,RAccionesCliente).

brings_clientes_acciones([],[]).
brings_clientes_acciones([IdEstante=>Lista2|REstantes],[NewValor|RNewValor]):-
	brings_clientes_acciones_aux(Lista2,AccionesCliente),
	NewValor=IdEstante=>AccionesCliente,
	brings_clientes_acciones(REstantes,RNewValor).

regresar_estantes_configuracion3([],[],[]).
regresar_estantes_configuracion3([Lista|Rlista],[Estante|REstante],[Valor|RValor]):-
	Valor=Estante=>Lista,
	regresar_estantes_configuracion3(Rlista,REstante,RValor).


union_final_configuracion_aux(IdEstante,[],[]).
union_final_configuracion_aux(IdEstante,[IdEst=>Lista|Resto],Valor):-
	IdEstante=IdEst->Valor=Lista,!;
	union_final_configuracion_aux(IdEstante,Resto,Valor).

union_final_configuracion(NewValor,[],[]).
union_final_configuracion(NewValor,[IdEstante=>LConfiguracion|RestoEstantes],[NLConfiguracionFinal|RNLConfiguracionFinal]):-
	union_final_configuracion_aux(IdEstante,NewValor,NLConfiguracion),
	%write("&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"),nl,
	%write(LConfiguracion),nl,
	flatten(NLConfiguracion,NLConfiguracionF),
	%write(NLConfiguracionF),
	union(LConfiguracion,NLConfiguracionF,NLConfiguracionFinalP),
	flatten(NLConfiguracionFinalP,NLConfiguracionFinal),
	union_final_configuracion(NewValor,RestoEstantes,RNLConfiguracionFinal).

separar_bring_axu(Estante,Producto,[],[]).
separar_bring_axu(Estante,Producto,[IdEstante=>Productos|REstantes],[LProductos|RLProductos]):-
	Estante=IdEstante->union([colocar(Producto)],Productos,LProductos),!;
	LProductos=Productos,
	separar_bring_axu(Estante,Producto,REstantes,RLProductos).


separar_bring([bring(X,Producto,Estante)|Rbring],RNLConfiguracionFinalEstantes,[VariableSalida|RVariableSalida]):-
	separar_bring_axu(Estante,Producto,RNLConfiguracionFinalEstantes,VariableSalida),
	separar_bring(Rbring,RNLConfiguracionFinalEstantes,RVariableSalida).

separar_bring([],RNLConfiguracionFinalEstantes,[]).

arreglar_h([],[]).
arreglar_h([H|T],[H|RH]):-
	arreglar_h(T,RH).

pasar_a_entregar([],[]).
pasar_a_entregar([Llevar=>Producto|Resto],[Leyenda|RLeyenda]):-
	Leyenda=colocar(Producto),
	pasar_a_entregar(Resto,RLeyenda).

armar_leyenda_mover([X],[]).
armar_leyenda_mover([X,Y|Resto],[Var|Rvar]):-
	Var=X=>Y,
	armar_leyenda_mover([Y|Resto],Rvar).

unificar_mover_aux(Estante,[],Valor).
unificar_mover_aux(Estante,[IdEstante=>Lugar|Res],Valor):-
	Estante=IdEstante->Valor=[mover(IdEstante,Lugar)],!;
	unificar_mover_aux(Estante,Res,Valor).

unificar_mover([],LeyendaMover,[]).
unificar_mover([Estante=>Lista|Resto],LeyendaMover,[Valor2|RValor2]):-
	unificar_mover_aux(Estante,LeyendaMover,Valor),
	union(Lista,Valor,Valor2),
	%write(Estante),nl,
	%write(LeyendaMover),nl,
	unificar_mover(Resto,LeyendaMover,RValor2).

planeacion():-
	desicion_parcial(ListaDeDesiciones,AccionesPuntoRuta,RutaFinalActualizada),
	%write(AccionesPuntoRuta),nl,
	%write("=============LA RUTA ACTUALIZA ES================"),nl,
	%write(RutaFinalActualizada),nl,
	length(AccionesPuntoRuta,CantidadDeAcciones),
	length(RutaFinalActualizada,TamanioRutaActualizada),
	(CantidadDeAcciones<TamanioRutaActualizada-1 -> RutaFinalActualizada=[X|RestoRuta],NuevaRuta=RestoRuta;
	NuevaRuta=RutaFinalActualizada),
	asignar_estante_a_acciones_estante(NuevaRuta,AccionesPuntoRuta,LugarConAcciones),
	%write("==============AQUIIIIIIIIIIIIIIIIIII======================="),nl,
	%write(NuevaRuta),nl,
	%write(AccionesPuntoRuta),nl,
	%write(LugarConAcciones),nl,
	iterar_ruta(RutaFinalActualizada,LugarConAcciones,ListaAcciones,ListaInteresada,RutaFinalActualizada),
	%write("===La lista de ACCIONES:==="),nl,
	%write(ListaAcciones),nl,
	movimientos_a_acciones(ListaAcciones,RutaFinalActualizada,NuevaConfiguracion),
	funcion_entregar_objetos_cercanos(NuevaConfiguracion,NuevaConfiguracion2),
	%write("********************************"),nl,
	%write(LugarConAcciones),nl,
	iterar_ruta2(RutaFinalActualizada,LugarConAcciones,ListaAcciones2,ListaInteresada2,RutaFinalActualizada),
	%write("===ListaAcciones #####==="),nl,
	%write(ListaAcciones2),nl,
	funcion_lista_acciones2(ListaAcciones2,RutaFinalActualizada,ListaAccionesFormato),
	%write("===####ListaAcciones Formato  #####==="),nl,
	%write(ListaAccionesFormato),nl,
	funcion_entregar_objetos_en_ruta(ListaAccionesFormato,NuevaConfiguracion3),
	%write("===####*****ListaAcciones Formato ***#####==="),nl,
	%write(NuevaConfiguracion3),nl,
	union_penultimo_plan(NuevaConfiguracion3,NuevaConfiguracion2,NuevaConfiguracionP),
	%write(NuevaConfiguracionP),
	flitrar_clientes(LugarConAcciones,ListasClientes),
	brings_clientes_acciones(ListasClientes,NewValor),
	%write("LugarConAcciones"),nl,
	%write(LugarConAcciones),nl,
	%write("ListasClientes"),nl,
	%write(ListasClientes),
	%write("##########################################################33"),
	%write(NewValor),nl,
	regresar_estantes_configuracion3(NuevaConfiguracion3,RutaFinalActualizada,RutaFinalActualizadaEstantes),
	%write(RutaFinalActualizadaEstantes),
	union_final_configuracion(NewValor,RutaFinalActualizadaEstantes,RNLConfiguracionFinal),
	%write("POR FIN"),nl,
	%write(NewValor),nl,
	%write(RutaFinalActualizadaEstantes),nl,
	%write(RNLConfiguracionFinal),
	regresar_estantes_configuracion3(RNLConfiguracionFinal,RutaFinalActualizada,RNLConfiguracionFinalEstantes),nl,nl,nl,
	%write(RNLConfiguracionFinalEstantes),nl,
	flatten(ListaAcciones2,ListaAcciones2F),
	%write(ListaAcciones2F),
	separar_bring(ListaAcciones2F,RNLConfiguracionFinalEstantes,Salida),
	%write("La salida es"),nl,
	%write(Salida),nl,
	(Salida=[]->H=[[]];
	Salida=[H|T]),
	%write("Las HHHHHH"),nl,
	%write(H),nl,
	arreglar_h(H,SalidaFinal),
	%write("Salida Final"),nl,
	%write(SalidaFinal),nl,
	encontrar_tareas(Tareas),
	Tareas = Tar=>L,
	pasar_a_entregar(L,Leyendas),
	append(SalidaFinal,[Leyendas],SalidaFinalD),
	%write(SalidaFinalD),
	armar_leyenda_mover(RutaFinalActualizada,LeyendaMover),
	regresar_estantes_configuracion3(SalidaFinalD,RutaFinalActualizada,SalidaFinalDEstantes),
	%write(LeyendaMover),nl,
	%write(SalidaFinalDEstantes),
	unificar_mover(SalidaFinalDEstantes,LeyendaMover,SalidaFinalDEstantesPorFin),
	write("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"),nl,
	write(SalidaFinalDEstantesPorFin)
	.

remove_char(S,C,X) :- atom_concat(L,R,S), atom_concat(C,W,R), atom_concat(L,W,X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Planeacion%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%planeacion():-
	%encontrar_brazos_libres(ManosLibres),nl,
	%write("La cantidad de manos libres es la siguiente"),nl,
	%write(ManosLibres)