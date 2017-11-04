%Funcion miembro

miembro(X,[X|_]).
miembro(X,[_|R]):- miembro(X,R).

%funcion inversa

inversa(L1,L):-inversa(L1,[],L).
inversa([],L,L).
inversa([X|L1],L2,L3):-inversa(L1,[X|L2],L3).

%longitudlongitud(L,N):-longitud(L,0,N).

longitud([],0).
longitud([_|L],R):-longitud(L,R1), R is R1+1.

%Funcion que agrega un elemento a la lista

agregar(X, L, [X|L]).

%Consulta de Dr.Log
acontecimiento(1505,['Euclides',traducido,al,latin]).
acontecimiento(1523,['Chistian','II',huye,de,'Dinamarca']).

consulta:-
	write(['Que', tiene,  'we ?' ]),
	read(C),
	agregar(C,[],S),
	write('Que mas tiene  we ?' ),
	read(D),
	agregar(D,S,T),
	write(['Que', 'mas tiene',  'we ?' ]),
	read(E),
	agregar(E,T,U),
	write(U).