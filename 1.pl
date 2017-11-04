% eliza.Pl

% Reads a line of input, removes all punctuation characters, and converts
% it into a list of atomic terms
:- [readline].

drlog:- write('Bienvenido a Dr. Log'), nl,
			 write('El doctor realizara una consulta, debes decirle los sintomas'), nl,
			 write('Ademas te dara tratamiento y causas de la enfermedad'), nl,
			 write('escribe comenzar. para iniciar la consulta. Para terminar la consulta, escribe <adios>. Enjoy!').
comenzar :-
	   write('Hola, soy Dr. Log. ¿Como puedo ayudarte?'), nl,
       %read(Input),
	   readline(Input),
	   eliza(Input),!.

eliza(['adios']) :-
	reply(['Hasta pronto, espero haber sido de ayuda']).
% recursive algorithm to conduct the conversation with the user
eliza(Input) :-
	pattern(Stim, Response),
	match(Stim, Dict, Input),
	match(Response, Dict, Output),
	reply(Output),
	readline(Input1),
	!, eliza(Input1).

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

% pattern matching
%pattern([i,am,1], ['How', long, have, you, been, 1,'?']) :-
%	add_mem(i,am,X).

:- dynamic(pattern/1).

pattern([_| [_, tu, B, a, mi]], ['Que te hace pensar eso', '?']).
pattern([_,me, gusta,el, A], ['Por que', te, gusta, el, A, '?']).
pattern([_,me, gusta,la, A], ['Por que', te, gusta, la, A, '?']).
pattern([me, gusta,el, A], ['Por que', te, gusta, el, A, '?']).
pattern([me, gusta,la, A], ['Por que', te, gusta, la, A, '?']).
pattern([_, siento, A, B, C], ['Con', que, frecuencia, sientes, A,',', B,'y', C,'?']).

%% asking about how the bot is doing
pattern([_,como, esta,?,_|_],['Genial! Gracias por preguntar. Como estas tu?']).
pattern([estoy, X|_], ['Por que', estas, X, ?]).

%% similar adjectives that a user can describe
pattern([bien,_|_], ['Me', alegro]).
pattern([no, tan, bien,_|_], ['Por que?']).
pattern([mal,_|_],['Te gustaria hablar sobre el tema?']).
pattern([fatal],['Vaya! Por que?']).

%% User asking about what the chatbot can do.
pattern([what,_,you,do,_|_],['Im here to converse']).
pattern([my, X, _|_], ['Please', tell, me, more, about, your, X,'.']).
pattern([_, sorry, _|_], ['Apologies are not necessary']).
pattern([_, is, the, weather, _|_], ['Ask Google']).
pattern([X, reminds, me, of,Y|_], ['Why does',X,'remind you of',Y]).
pattern([can, you, _|_], ['Probably']).
pattern([do, _|_], ['No thanks!']).
pattern([what, are, you], ['Its Who, not what!']).
pattern([recuerdame, _|_], ['Lo siento, no soy tu asistente!']).
pattern([_, acuerda, _|_], ['Para eso tienes agenda!']).
pattern([i, feel, _|_], ['Do', you, often, feel, that, way, '?']).
pattern([you, are, _|_], ['I know I am, but what are you?']).
pattern([you, _|_], ['Yeah, I get that alot']).
pattern([am, i, _|_], ['Yeah, you are']).
pattern([remember,_|_], ['No, I do not remember. Please elaborate.']).
pattern([_,you, remember, _|_], ['Yeah, I remember. Good times.']).
pattern([if,_|_], ['Yeah, I would think so']).
pattern([hola], ['Hola','soy','Dr. Log.','Como puedo ayudarte?']).
pattern([holi], ['Hola','soy','Dr. Log.','Como puedo ayudarte?']).
pattern([hey], ['Hola','soy','Dr. Log.','Como puedo ayudarte?']).
pattern([hola,_|_], ['Hola','soy','Dr. Log.','Como puedo ayudarte?']).
pattern([_, you, interested, _|_], ['No, I have already told you that I am not interested']).
pattern([please], ['stop']).
pattern([_, dreamt, _|_], ['What does that dream suggest to you?']).
pattern([perhaps, _|_], ['Why the uncertain tone?']).
pattern([why, is, _|_], ['I honestly couldn\'t tell you']).
pattern([why, doesnt, _|_], ['I don\'t know.']).
pattern([_, is, _|_], ['Is it really?']).
pattern([i, love, you], ['I can\'t say I feel the same way']).
pattern([everyone, _|_], ['I think you\'re over-exaggerating']).
pattern([no], ['so....']).
pattern([yes], ['yes, indeed']).
pattern([una, vez, _|_], ['cool story bro']).
pattern([im, _|_], ['I\'m not jealous.']).
pattern([what, are, your, thoughts, _|_], ['I couldn\'t care really. What do you think?']).


pattern([1], ['Cuentame mas.']).

% reply
reply([Head|Tail]) :- write(Head), write(' '), reply(Tail).
reply([]) :- nl.

:- drlog.















