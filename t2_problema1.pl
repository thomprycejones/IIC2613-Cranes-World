:- discontiguous holds/2,is_negative_effect/2,is_positive_effect/2,poss/2.

%% Object Declaration (problem-specific)
container(C) :- member(C,[c1,c2,c3,c4,c5,c6]).
paleta(P) :- member(P,[p11,p21,p12,p22,p13]).
superficie(S) :- container(S).
superficie(S) :- paleta(S).
lugar(L) :- member(L,[cargo1,cargo2,cargo3]).
conectada(C1,C2) :- member([X1,X2],[[cargo1,cargo2],
                                    [cargo2,cargo3]]),
                    (C1=X1,C2=X2; C1=X2,C2=X1).

grua(G) :- member(G,[g1,g2,g3]).

camion(Cam) :- member(Cam,[cam1]).

%% Initial Situation (problem-specific)
holds(F,s0) :- member(F,[en(cam1,cargo1),

                         en(g1,cargo1),en(g2,cargo2),en(g3,cargo3),
                         disponible(g1),disponible(g2),

                         en(p11,cargo1),en(p21,cargo1),
                         en(c1,cargo1),en(c2,cargo1),
                         sobre(c1,p11),sobre(c2,c1),
                         despejada(c2),despejada(p21),

                         en(p12,cargo2),en(p22,cargo2),
                         en(c3,cargo2),en(c4,cargo2),en(c5,cargo2),
                         sobre(c3,p12),sobre(c4,c3),sobre(c5,c4),
                         sobre(c6,p22),
                         despejada(c5),
                         despejada(c6),

                         en(p13,cargo3),despejada(p13)]).

%%% Accion manejar
poss(manejar(Cam,L1,L2),S) :-
    camion(Cam),conectada(L1,L2),lugar(L1),lugar(L2), % Facts
    holds(en(Cam,L1),S). % El camion Cam esta en lugar L1

is_positive_effect(manejar(Cam,_,L2), en(Cam,L2)). % El camion Cam pasa a estar en L2
is_negative_effect(manejar(Cam,L1,_), en(Cam,L1)). % El camion Cam deja de estar en L1

%%% Accion levantar
poss(levantar(G,C,Sup,L),S) :-
    grua(G),container(C),superficie(Sup),lugar(L), % Facts
    holds(disponible(G), S), % La grua esta disponible
    holds(despejada(C), S), % El container C esta despejada
    holds(sobre(C, Sup), S), % El cargo C esta sobre Sup
    holds(en(G, L), S), % La grua G esta en L
    holds(en(C, L), S), % El cargo C esta en L
    holds(en(Sup, L), S), % La superficie Sup esta en L
    C \= Sup. % Diferencio variables

is_positive_effect(levantar(G,C,_,_),levantando(G,C)). % La grua G levantando a container C
is_positive_effect(levantar(_,_,Sup,_),despejada(Sup)). % La superficie Sup queda despejada
is_negative_effect(levantar(G,_,_,_), disponible(G)). % La grua G deja de estar disponible
is_negative_effect(levantar(_,C,Sup,_),sobre(C,Sup)). % El container C deja de estar sobre Sup
is_negative_effect(levantar(_,C,_,L),en(C,L)). % El container C deja de estar en lugar L

%%% Accion soltar
poss(soltar(G,C,Sup,L),S) :-
    grua(G),container(C),superficie(Sup),lugar(L), % Facts
    holds(despejada(Sup), S), % La Sup esta despejada
    \+ holds(disponible(G), S), % La grua G no esta disponible
    holds(levantando(G, C), S), % La grua G esta levantando al container C
    holds(en(G, L), S), % La grua G esta en L
    holds(en(Sup, L), S), % La superficie Sup esta en L
    C \= Sup. % Diferencio variables

is_positive_effect(soltar(_,C,Sup,_), sobre(C, Sup)). % El container C pasa a estar sobre Sup
is_positive_effect(soltar(_,C,_,L), en(C, L)). % El container C pasa a estar en lugar L
is_positive_effect(soltar(G,_,_,_), disponible(G)). % La grua G pasa a estar disponible
is_positive_effect(soltar(_,C,_,_), despejada(C)). % El container C pasa a estar despejada
is_negative_effect(soltar(G,C,_,_), levantando(G,C)). % La grua G ya no levantando a container C
is_negative_effect(soltar(_,_,Sup,_),despejada(Sup)). % La superficie Sup deja de estar despejada

%%% Accion cargar
poss(cargar(G,Co,Ca,L),S) :-
    grua(G),container(Co),camion(Ca),lugar(L), % Facts
    holds(levantando(G, Co), S), % La grua G esta levantando al container C
    holds(en(G, L), S), % La grua G esta en L
    \+ holds(en(Co, L), S), % El container C no esta en L
    holds(en(Ca, L), S). % El camion Camion esta en L

is_positive_effect(cargar(_,Co,Ca,_), dentro(Co, Ca)). % El container Container pasa a estar dentro del camion Camion
is_positive_effect(cargar(G,_,_,_), disponible(G)). % La grua G pasa a estar disponible
is_positive_effect(cargar(_,Co,_,_), despejada(Co)). % El container Co queda despejado (redundante?)
is_negative_effect(cargar(G,Co,_,_), levantando(G, Co)). % La grua G ya no levanta a container Container
is_negative_effect(cargar(_,Co,_,L), en(Co, L)). % El container C deja de estar en lugar L

%%% Accion descargar
poss(descargar(G,Co,Ca,L),S) :-
    grua(G),container(Co),camion(Ca),lugar(L), % Facts
    holds(dentro(Co, Ca), S), % El camion Camion tiene dentro a container Container
    holds(despejada(Co), S), % El container Co tiene que estar despejada
    holds(disponible(G), S), % La grua G esta disponible
    holds(en(G, L), S), % La grua G esta en L
    holds(en(Ca, L), S). % El camion Camion esta en L

is_positive_effect(descargar(G,Co,_,_), levantando(G, Co)). % La grua G levanta al container Container
is_negative_effect(descargar(_,Co,Ca,_), dentro(Co, Ca)). % El container Container ya no esta dentro camion Camion
is_negative_effect(descargar(G,_,_,_), disponible(G)). % La grua G deja de estar disponible
is_negative_effect(descargar(_,Co,_,_), despejada(Co)). % El container Co ya no esta despejado (redundante?)

%%%%% Situation Calculus Successor State Axiom a la Reiter (domain-independent)
holds(F,do(A,S)) :-
    holds(F,S),
    \+ is_negative_effect(A,F).

holds(F,do(A,_)) :-
    is_positive_effect(A,F).

%%%%% Legal Situations are those produced by executing
%%%%% generates situations in a breadth-first manner

legal(s0).
legal(do(A,S)) :-
    legal(S),
    poss(A,S).

% If you want to generate a plan use a query like
% legal(S),holds(on(b,a),S).

% ---------------------------- PART 2 -----------------------------------------

goal_condition([en(c1,cargo2),en(c5,cargo1)]).
%goal_condition([sobre(c3,c2)]).
%goal_condition([en(c1,cargo2),sobre(c5,c2)]).
%goal_condition([dentro(c1,cam1),dentro(c4,cam1)]).
%goal_condition([dentro(c1,cam1),en(cam1,cargo3)]).

%% heuristics for A*

null_heuristic(_,0).

astar_heuristic1(State,N) :-
    goal_condition(Goal),
    findall(C,(member(sobre(C,Gpos),Goal), member(sobre(C,Pos), State), Gpos\=Pos),List),
    length(List,N).

astar_heuristic2(State,N) :-
    goal_condition(Goal),
    findall(sobre(A,B),(member(sobre(A, B),Goal), \+ member(sobre(A,B), State), member(sobre(_, B), State)),List),
    length(List,N).

the_good_heuristic(State, N) :-
    goal_condition(Goal),
    findall(sobre(A,B),(member(sobre(A, B),Goal), \+ member(sobre(A,B), State), member(sobre(_, B), State)),L1),
    findall(en(A, B),(member(en(A, B), Goal), \+ member(en(A, B), State), member(en(_, B), State)), L2),
    findall(disponible(G),(\+ member(disponible(G), State)), L3),
    append(L1,L2, L12),
    append(L12,L3, L123),
    length(L123, N).

%% We could do better :)

astar_heuristic(State,N) :-
  the_good_heuristic(State,M),
  N is 3*M.
