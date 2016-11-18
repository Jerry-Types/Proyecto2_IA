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
	H=[Id=>Estante,[Categoria=>ValorCategoria]],
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
encontrar_categoria_listal([],Producto,R).
encontrar_categoria_listal([H|T],Producto,R):-
	H=[Id=>Product,Categoria=>Valor|Rp],
	Product=Producto -> R=Product=>Valor;
	encontrar_categoria_listal(T,Producto,R).



%Predicado ecnontrar estante de producto de una lista de listas%
encontrar_categoria_estantel([],Producto,R).
encontrar_categoria_estantel([H|T],Producto,R):-
	H=[Id=>Product,Categoria=>Valor|Rp],
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
asociar_producto_estante([],S,R,[]).
asociar_producto_estante([H|T],S,R,[Lp|Lps]):-
	encontrar_categoria_estantel(S,H,R),R=Lp,
	asociar_producto_estante(T,S,Rs,Lps).


%Predicado lista productos con categoria%
asociar_producto_categoria([],S,R,[]).
asociar_producto_categoria([H|T],S,R,[Lp|Lps]):-
	encontrar_categoria_listal(S,H,R),R=Lp,
	asociar_producto_categoria(T,S,Rs,Lps).

encontrar_match_categorias2(Excategoria,[],El,[],[]).
encontrar_match_categorias2(Excategoria,[Prod=>Cat|T],El,[Prod|L1],L2):-
	Excategoria=Cat->!,encontrar_match_categorias2(Excategoria,T,El,L1,L2).

encontrar_match_categorias2(Excategoria,[Prod=>Cat|T],El,L1,[Prod|L2]):-
	encontrar_match_categorias2(Excategoria,T,El,L1,L2).	

encontrar_match_categorias(Excategoria,[Estante=>Prod],El,Elmodificado):-
	encontrar_match_categorias2(Excategoria,Prod,El,Lcorrectos,Lsobrantes),
	El=[Correctos,Sobrantes,Faltantes],
	union(Correctos,Lcorrectos,Lcorrectosmod),
	Elmodificado=[Lcorrectosmod,Lsobrantes,Faltantes].
	.

actualizar_creencias_observaciones([],KB,[],[]).
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
	write("Dejo el producto :"),write(H),nl,
	reportar_acciones_productos(T).

decripcion_acciones_asistente([],Estante).
decripcion_acciones_asistente([H|T],Estante):-
	H=[]->Estante2 is Estante+1,decripcion_acciones_asistente(T,Estante2);
	write("====El asistente se movio al estante===="),write(" :"),write(Estante),nl,
	reportar_acciones_productos(H),Estante2 is Estante+1,
	decripcion_acciones_asistente(T,Estante2).

diagnostico(RConfiguracion):-
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
	decripcion_acciones_asistente(RConfiguracion,1)
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

