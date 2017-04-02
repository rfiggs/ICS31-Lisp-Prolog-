%listlength/2
%arg1, a list
%arg2, the length of the list
%as predicate eg.
%listlength([1,2],2).
%true.
%as function eg.
%listlength([1,2], L).
%L = 2.
%as generator
%listlength(L, 3).
%L = (_, _, _).
listlength([],0).
listlength([_|T], L):-
    listlength(T,L1),
    L is L1 +1.

sum([],0).
sum([H|T], S):-
    sum(T, S2),
    S is S2 + H.

%apend/3
%arg1, the first list to append
%arg2, the second list to append
%arg3, the two lists appeneded together
apend([], L2, L2).
apend([H1|T1], L2, [H1|L]):-
    apend(T1,L2,L).

%split3/2
%returns true if the list of integers can be split into 3 sublists such that
%the sum of each sublist is less than arg1
%arg1, the number which the sum of sublists must be smaller
%arg2, the list of integers
split3(N, [H1, H2|T]):-
    split3(N, [H1], [H2], T).

%split3/3
%this is a helper function for split3/2
%arg1, the number that, N, the sum of the sublists must be smaller than
%arg2, the first sublist
%arg3, the second sublist
%arg4, the third sublist
split3(N, L1, L2, L3):-
    L1 \= [],
    L2 \= [],
    L3 \= [],
    sum(L1,S1),
    sum(L2,S2),
    sum(L3,S3),
    N >= S1,
    N >= S2,
    N >= S3.
    %format('base~w~w~w~n',[L1,L2,L3]).

split3(N, L1, L2, [H3|T3]):-
    T3 \= [],
    apend(L2, [H3], L2A),
    split3(N, L1, L2A , T3).

split3(N, L1, [H2|T2], L3):-
    T2 \= [],
    apend(L1, [H2], L1A),
    split3(N, L1A, T2, L3).


%remove/3
%arg1 the type of ball to remove
%arg2 a list of tuples where the first is the type of ball
%and the second is the number of that type of ball
%arg3 the new list with the ball removed if it exists
%decrements the given balls count by 1 as long as there is at least 1 remaining
remove(Type,[[Type,N]|T],[[Type,N2]|T]):-
    N2 is N - 1,
    N2 >= 0.
remove(Type, [H|T], [H|L]):-
    remove(Type, T, L).

permutate(L,[]):-
    not(remove(_,L,_)).
permutate([H|T], [Type|L3]):-
    remove(Type,[H|T],L2),
    permutate(L2,L3).


%situation1([black, black, red, red, blue, blue], L).
%situation1([black, black, red, red, blue, blue]). false.
%situation1([black, red, blue, red, black, blue]). true.
nonAdjacent([H|T]):-
    nonAdjacent(H,T).
nonAdjacent(_,[]).
nonAdjacent(P,[H|T]):-
    P \= H,
    nonAdjacent(H,T).

situation1(L):-
    Balls = [[red,2],[black,2],[blue,2]],
    permutate(Balls,L),
    nonAdjacent(L).

situation2(L):-
    Balls = [[red,4],[black,1],[blue,2]],
    permutate(Balls,L),
    noMoreThan2InARow(L).

noMoreThan2InARow([H1,H2|T]):-
    noMoreThan2InARow(H1,H2,T).
noMoreThan2InARow(_,_,[]).
noMoreThan2InARow(LastLast,Last,[H|T]):-
    Last = LastLast,
    H \= Last,
    noMoreThan2InARow(Last,H,T).
noMoreThan2InARow(LastLast,Last,[H|T]):-
    LastLast \= Last,
    noMoreThan2InARow(Last,H,T).

situation3(L):-
    Balls = [[yellow, 1],[black, 2],[blue,2],[purple, 3]],
    permutate(Balls,L),
    verify3(L).

verify3(L):-
    L = [I1,I2,I3,I4,_,I6,I7,I8],
    I2 \= purple,
    I3 \= purple,
    I4 = I8,
    I1 \= I7,
    I1 \= black,
    I8 \= black,
    I6 \= blue,
    I7 \= blue,
    leftOfBlueIsPurple(L).

leftOfBlueIsPurple([H|T]):-
    H \= blue,
    leftOfBlueIsPurple(H,T).
leftOfBlueIsPurple(P,[H|T]):-
    H = blue,
    P = purple,
    leftOfBlueIsPurple(H,T).
leftOfBlueIsPurple(_,[H|T]):-
    H \= blue,
    leftOfBlueIsPurple(H,T).
leftOfBlueIsPurple(_,[]).
