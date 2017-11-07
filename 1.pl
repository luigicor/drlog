% BASE DE DATOS
sintoma(dolor).
sintoma(tos).
sintoma(congestion).
sintoma(nauseas).
sintoma(gases).
sintoma(distension).
sintoma(ahogamiento).
sintoma(lagrimeo).
sintoma(constipacion).
sintoma(fiebre).
sintoma(estornudos).
sintoma(jaqueca).
sintoma(sudoracion).
sintoma(escalofrios).
sintoma(malestar).
sintoma(ansiedad).
sintoma(silbidos).
sintoma(agitamiento).
sintoma(ampollas).
sintoma(cansancio).
sintoma(comezon).

causa('alergias').
causa('contagio').
causa('el mal clima').
causa('virus de la influenza').
causa('mala alimentacion').
causa('nervios').
causa('virus').

tratamiento('tomar antibiotico').
tratamiento('descansar').
tratamiento('tomar jarabe').
tratamiento('aplicar mascarilla').
tratamiento('aplicar inyeccion').
tratamiento('aplicar gotas nasales').

preve('Teniendo buena higiene').
preve('Alimentandose bien').
preve('Evitando contacto con afectados').
preve('Protegiendose del mal clima').
preve('Vacunandose').
preve('Descansando').
preve('Disminuyendo la presion').

causas(asma,['alergias','el clima']).
causas(gripe,['el clima','virus de la influenza']).
causas(gastritis,['mala alimentacion']).
causas(rinitis,['el clima','alergias']).
causas(varicela,['contagio','virus']).
causas(faringitis,['contagio','virus']).
causas(estres,['nervios']).




enfermedad(asma,Sintomas):-member(Sintomas,[tos, ahogamiento, silbido, agitamiento]).
enfermedad(gripe,Sintomas):-member(Sintomas,[fiebre, tos, estornudos, jaqueca, congestion]).
enfermedad(gastritis,Sintomas):-member(Sintomas,[dolor, gases, nauseas, constipacion, distension]).
enfermedad(rintis,Sintomas):-member(Sintomas,[congestion, estornudos, lagrimeo, tos]).
enfermedad(varicela,Sintomas):-member(Sintomas,[ampollas, fiebre, cansancio, comezon]).
enfermedad(faringitis,Sintomas):-member(Sintomas,[dolor, irritacion, fiebre,tos]).
enfermedad(estres,Sintomas):-member(Sintomas,[sudoracion, escalofrios, ansiedad, cansancio]).

prevencion(asma,['Vacunandose','Protegiendose del clima']).
prevencion(gastritis,['Alimentandose bien']).
prevencion(gripe,['Vacunandose','Protegiendose del clima','Evitando contacto con afectados','Teniendo buena higiene']).
prevencion(rinitis,['Protegiendose del clima','Teniendo buena higiene']).
prevencion(varicela,['Evitando contacto con afectados']).
prevencion(faringitis,['Teniendo buena higiene','Evitando contacto con afectados']).
prevencion(estres,['Descansando','Disminuyendo la presion']).

tratar(asma,[mascarilla,descanso]).
tratar(gastritis,[jarabe,descanso]).
tratar(gripe,[inyeccion,descanso, jarabe]).
tratar(rinitis,[gotas,descanso]).
tratar(varicela,[descanso]).
tratar(faringitis,[antibiotico, descanso]).
tratar(estres,[descanso]).
trata(X):-tratar(X,L), write(L).
preve(A):-prevencion(A,L), write(L).
caus(B):-causas(B,L), write(L).

busca_enfermedad(_,[]):-!.
busca_enfermedad(Padecimiento,[X|T]):-
	enfermedad(Padecimiento,X), busca_enfermedad(Padecimiento,T).


% Lee la entrada y la convierte en una lista de terminos atomicos
:- [readline].

consulta:- write('Bienvenido a Dr. Log'), nl,
			 write('El doctor realizara una consulta, debes decirle los sintomas'), nl,
			 write('Ademas te dara tratamiento y causas de la enfermedad'), nl,
			 write('La consulta cuesta $500, se acepta Visa, Mastercard y AmericanExpress'), nl,
			 write('escribe "comenzar." para iniciar la consulta. Para terminar la consulta, escribe "adios".').
comenzar :-
	   write('----Consulta iniciada----'), nl,
       %read(Input),
	   readline(Input),
	   drlog(Input),!.

drlog(['adios'|_]) :-
	reply(['Hasta pronto, espero haber sido de ayuda']).
drlog(['gracias'|_]) :-
	reply(['Hasta pronto, espero haber sido de ayuda']).

drlog([_,'adios'|_]) :-
	reply(['Hasta pronto, espero haber sido de ayuda']).
drlog([_,'gracias'|_]) :-
	reply(['Hasta pronto, espero haber sido de ayuda']).
drlog([_,_,'adios'|_]) :-
	reply(['Hasta pronto, espero haber sido de ayuda']).
drlog([_,_,'gracias'|_]) :-
	reply(['Hasta pronto, espero haber sido de ayuda']).
% mantiene la conversacion andando
drlog(Input) :-
	pattern(Stim, Response),
	match(Stim, Dict, Input),
	match(Response, Dict, Output),
	write('Dr.Log: '),
	reply(Output),
	readline(Input1),
	!, drlog(Input1).

% matching the input to the Stimulus pair
match([N|Pattern], Dict, Target) :-
	integer(N), lookup(N,Dict,Lt),
	append(Lt,Rt, Target),
	match(Pattern, Dict, Rt).

match([Word|Pattern], Dictionary, [Word|Target]) :-
	atom(Word), match(Pattern, Dictionary, Target).

match([], _,[]).

% lookup dictionary
lookup(Key, [(Key, Value)|_], Value).
lookup(Key, [(Key1, _)|Dictionary], Value) :-
    \=(Key, Key1), lookup(Key, Dictionary, Value).

% adding_memory
add_mem(X,Predicate, Y) :-
	print(X), nl, print(Predicate), nl, print(Y),
	Fact =.. [Predicate,X,Y],
	asserta(Fact).
agregar(X, L, [X|L]).

% patrones que reconoce drlog.

:- dynamic(pattern/1).
pattern([_,me, gusta,el, A|_], ['Por que', te, gusta, el, A, '?']).
pattern([_,me, gusta,la, A|_], ['Por que', te, gusta, la, A, '?']).
pattern([me, gusta,el, A|_], ['Por que', te, gusta, el, A, '?']).
pattern([me, gusta,la, A|_], ['Por que', te, gusta, la, A, '?']).
pattern([porque,_|_], ['Interesante, continuemos con la consulta']).
pattern([tiene,_|_], ['Interesante, continuemos con la consulta']).
pattern([es,_|_], ['Interesante, continuemos con la consulta']).
pattern([_,_,como, esta |_],['Genial! Gracias por preguntar. Como esta usted?']).
pattern([_,como, esta |_],['Genial! Gracias por preguntar. Como esta usted?']).
pattern([como, esta |_],['Genial! Gracias por preguntar. Como esta usted?']).
pattern([estoy, bien|_], ['A que debo tu visita, en ese caso?']).
pattern([no, tan, bien], ['Por que?']).
pattern([mal],['Te gustaria hablar sobre el tema?']).
pattern([fatal],['Vaya! Por que?']).
pattern([_,necesito,ayuda|_],['Por supuesto, estoy para ayudar, dime mas']).
pattern([mi, X|_], ['Dime', mas, sobre, tu, X,'.']).
pattern([_,lo, siento, _|_], ['No hace falta disculparse']).
pattern([como, esta, el,clima, _|_], ['www.google.com']).
pattern([recuerdame|_], ['Lo siento, no soy tu asistente!']).
pattern([_,_,siento, A,B,y,C|_], ['Parece', que, tienes,':',L]):-busca_enfermedad(L,[A,B,C]).
pattern([_,siento, A,B,y,C|_], ['Vaya',tienes,':',L]):-busca_enfermedad(L,[A,B,C]).
pattern([siento, A,B,y,C|_], ['Tenemos un caso',de,':',L]):-busca_enfermedad(L,[A,B,C]).
pattern([siento, A|_], ['Dime tres sintomas para poder diagnosticarte']).
pattern([siento, A,y,B|_], ['Tenemos un caso',de,':',L]):-busca_enfermedad(L,[A,B]).
pattern([_,siento, A|_], ['Dime tres sintomas para poder diagnosticarte']).
pattern([_,siento, A,y,B|_], ['Vaya',tienes,':',L]):-busca_enfermedad(L,[A,B]).
pattern([_,_,siento, A|_], ['Dime tres sintomas para poder diagnosticarte']).
pattern([_,_,siento, A,y,B|_], ['Parece', que, tienes,':',L]):-busca_enfermedad(L,[A,B]).
pattern([_,_,siento, A,B,y,C|_], ['Parece', que, tienes,':',L]):-busca_enfermedad(L,[A,B,C]).
pattern([_,siento, A,B,y,C|_], ['Vaya',tienes,':',L]):-busca_enfermedad(L,[A,B,C]).
pattern([tengo, A,B,y,C|_], ['Tenemos un caso',de,':',L]):-busca_enfermedad(L,[A,B,C]).
pattern([tengo, A|_], ['Dime tres sintomas para poder diagnosticarte']).
pattern([tengo, A,y,B|_], ['Parece', que, tienes,':',L]):-busca_enfermedad(L,[A,B]).
pattern([_,tengo, A|_], ['Dime tres sintomas para poder diagnosticarte']).
pattern([_,tengo, A,y,B|_], ['Tenemos un caso',de,':',L]):-busca_enfermedad(L,[A,B]).
pattern([_,tengo, A,B,y,C|_], ['Tenemos un caso',de,':',L]):-busca_enfermedad(L,[A,B,C]).
pattern([_,_,tengo, A|_], ['Dime tres sintomas para poder diagnosticarte']).
pattern([_,_,tengo, A,y,B|_], ['Vaya',tienes,':',L]):-busca_enfermedad(L,[A,B]).
pattern([_,_,tengo, A,B,y,C|_], ['Vaya',tienes,':',L]):-busca_enfermedad(L,[A,B,C]).
pattern([cual, es, el, tratamiento, del, A|_], ['Para tratar el',A, se, debe,':'|T]):-tratar(A,T).
pattern([cual, es, el, tratamiento, de, la,A|_], ['Para tratar la',A, se, debe,':'|T]):-tratar(A,T).
pattern([como, se, trata, el, A|_], ['Para tratar el',A, se, debe,':'|T]):-tratar(A,T).
pattern([como, se, trata, la, A|_], ['Para tratar la',A, se, debe,':'|T]):-tratar(A,T).
pattern([como, se, previene, el, A|_], ['Podemos prevenir el',A,':'|T]):-prevencion(A,T).
pattern([como, se, previene, la, A|_], ['Se puede prevenir la',A, se, debe,':'|T]):-prevencion(A,T).
pattern([como, prevenimos, el, A|_], ['Buena pregunta, para prevenir el',A,':'|T]):-prevencion(A,T).
pattern([como, prevenimos, la, A|_], ['Es importante para prevenir la',A,':'|T]):-prevencion(A,T).
pattern([_,cual, es, el, tratamiento, del, A|_], ['Para tratar el',A, se, debe,':'|T]):-tratar(A,T).
pattern([_,cual, es, el, tratamiento, de, la,A|_], ['Para tratar la',A, se, debe,':'|T]):-tratar(A,T).
pattern([_,como, se, trata, el, A|_], ['Podemos tratar el',A, con,':'|T]):-tratar(A,T).
pattern([_,como, se, trata, la, A|_], ['Aconsejaria',':'|T]):-tratar(A,T).
pattern([_,como, se, previene, el, A|_], ['Podemos prevenir el',A,':'|T]):-prevencion(A,T).
pattern([_,como, se, previene, la, A|_], ['Se puede prevenir la',A, ':'|T]):-prevencion(A,T).
pattern([_,como, prevenimos, el, A|_], ['Para prevenir el',A,recomendamos,':'|T]):-prevencion(A,T).
pattern([_,como, prevenimos, la, A|_], ['Es importante para prevenir la',A,':'|T]):-prevencion(A,T).
pattern([_,_,cual, es, el, tratamiento, del, A|_], ['Para tratar el',A, se, debe,':'|T]):-tratar(A,T).
pattern([_,_,cual, es, el, tratamiento, de, la,A|_], ['Para tratar la',A, se, debe,':'|T]):-tratar(A,T).
pattern([_,_,como, se, trata, el, A|_], ['Para el',A, 'prodeceremos a',':'|T]):-tratar(A,T).
pattern([_,_,como, se, trata, la, A|_], ['Para tratar la',A, se, debe,':'|T]):-tratar(A,T).
pattern([_,_,como, se, previene, el, A|_], ['Podemos prevenir el',A,':'|T]):-prevencion(A,T).
pattern([_,_,como, se, previene, la, A|_], ['Se puede prevenir la',A, 'de la siguiente manera',':'|T]):-prevencion(A,T).
pattern([_,_,como, prevenimos, el, A|_], ['Te aconsejaria como prevencion de',A,':'|T]):-prevencion(A,T).
pattern([_,_,como, prevenimos, la, A|_], ['Considera lo siguiente para prevenir la',A,':'|T]):-prevencion(A,T).


pattern([buenas, tardes], ['Buenas tardes,','soy','Dr. Log.','Como puedo ayudarte?']).
pattern([buenos,dias], ['Buenos dias, soy','Dr. Log.','Como puedo ayudarte?']).
pattern([hola|_], ['Hola, soy Dr.Log. Como puedo ayudarte?']).
pattern([1], ['Podias ser mas claro? Por favor']).

% reply
reply([Head|Tail]) :- write(Head), write(' '), reply(Tail).
reply([]) :- nl.

:- consulta.



























































