% listlength/2
% gives the lenght of a list.
% arg1, a list
% arg2, the length of the list
% at least one argument must be instantiated for this predicate to work.
% as predicate eg.
% listlength([1,2],2).
% true.
% as function eg.
% listlength([1,2], L).
% L = 2.
% as generator
% listlength(L, 3).
% L = (_, _, _).
listlength([],0).
listlength([_|T], L):-
    listlength(T,L1),
    L is L1 +1.

% sum/2
% sums the values of an integer list
% arg1, a list of integers to sum
% arg2, the sum of the integers.
% This function does not work as a generator
% This means that all values in the list must be instantiated
% as a predicate eg.
% sum([1,2,3], 6).
% true
% as a function
% sum([1,2,3], N).
% N=6.

sum([],0).
sum([H|T], S):-
    sum(T, S2),
    S is S2 + H.

% apend/3
% appends to lists together
% arg1, the first list to append
% arg2, the second list to append
% arg3, the two lists appeneded together
% at lesat one argument must be instantiated for this predicate to work
% as a predicate eg.
% apend([1,2], [3,4],[1,2,3,4]).
% true
% as a function
% apend([1,2],[3,4],L).
% L= [1,2,3,4].
% or
% apend(L, [3,4], [1,2,3,4]).
% L= [1,2].
% or
% apend([1,2], L, [1,2,3,4]).
% L= [3,4].
% as a generator ()
% apend(L,L2,[1,2]).
% L = [],
% L2 = [1, 2] ;
% L = [1],
% L2 = [2] ;
% L = [1, 2],
% L2 = [] ;
% false.
apend([], L2, L2).
apend([H1|T1], L2, [H1|L]):-
    apend(T1,L2,L).


% split3/2
% returns true if the list of integers can be split into 3 sublists such that
% the sum of each sublist is less than arg1
% this predicate does not work unless the firist argument is instantiated
% arg1, the number which the sum of sublists must be smaller
% arg2, the list of integers
split3(N, [H1, H2|T]):-
    split3(N, [H1], [H2], T).

% split3/3
% this is a helper function for split3/2 and is not inteded for external use
% the first argument must be instatiated for this prediate to work
% arg1, the number that, N, the sum of the sublists must be smaller than
% arg2, the first sublist
% arg3, the second sublist
% arg4, the third sublist
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

split3(N, L1, L2, [H3|T3]):-
    T3 \= [],
    apend(L2, [H3], L2A),
    split3(N, L1, L2A , T3).

split3(N, L1, [H2|T2], L3):-
    T2 \= [],
    apend(L1, [H2], L1A),
    split3(N, L1A, T2, L3).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BALL SITUATIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% for each of the followiwng problems the balls are stored in 2d array
% such that each element in the fist array represents a ball type and is ammount
% the inner array has two elements where the first is the ball type,
% and the second is the amount of balls of that type
% for example the first problem has 3 ball types and two of each ball
% This is how it would look in the list
% [[red,2],[black,2],[blue,2]]
% I chose to do it this way because when I used a list with one elmenet for each ball eg.
% [red,red,blue,blue,black,black]
% I would get a lot of duplicate solutions.

% remove/3
% arg1 the type of ball to remove
% arg2 a list of tuples where the first is the type of ball
% and the second is the number of that type of ball
% arg3 the new list with the ball removed if it exists
% decrements the given balls count by 1 as long as there is at least 1 remaining
remove(Type,[[Type,N]|T],[[Type,N2]|T]):-
    N2 is N - 1,
    N2 >= 0.
remove(Type, [H|T], [H|L]):-
    remove(Type, T, L).

% permutate/2
% arg1, is a 2d list as described above
% arg2, is a list of ball types eg [black,red]
% the first argument must be instantiated for this predicate to work
% as a predicate eg.
% permutate([[red,2],[blue,1]],[red,red,blue]).
% true.
% Where this predicate will return true if the two lists contain the same amount
% of balls for every color
% as a generator eg.
% permutate([[red,2],[blue,1]],L).
% L = [red, red, blue] ;
% L = [red, blue, red] ;
% L = [blue, red, red] ;
% false.
% as a generator this predicate will return every possible combination of the balls

permutate(L,[]):-
    not(remove(_,L,_)).
permutate([H|T], [Type|L3]):-
    remove(Type,[H|T],L2),
    permutate(L2,L3).


% situation1/1
% arg1, a list of colors representing colored balls
% This predicate finds solutions to the following logic problem
% Given 2 red balls, 2 black balls, and 2 blue balls, arrange them such that
% no two balls of the same color are adjacent
% as a predicate this function will return true if the list matches the requirements
% eg.
% situation1([red,black,red,blue,black,blue]).
% true.
% as a generator, this function will generate all possible solutions to the problem
situation1(L):-
    Balls = [[red,2],[black,2],[blue,2]],
    permutate(Balls,L),
    nonAdjacent(L).

% nonAdjacent/1
% arg1, a list
% this predicate returns true when none of the elements on the list are adjacent to the same elements
% eg. for our ball problem, there will be no two of the same color in a row.
% this predicate must have its argument instantiated for it to work
% eg.
% nonAdjacent([1,2,3]).
% true
% nonAdjacent([1,1,3]).
% false
nonAdjacent([H|T]):-
    nonAdjacent(H,T).
% nonAdjacent/2
% This is a helper function for nonAdjacent/1 and is not intended for external use
% arg1, the last item looked at
% arg2, the rest of the items to lookat
nonAdjacent(_,[]).
nonAdjacent(P,[H|T]):-
    P \= H,
    nonAdjacent(H,T).

% situation2/1
% arg1, a list of colors representing ball colors
% This predicate solves the following logic problem
% You are given 4 red balls, 1 black ball, and 1 blue ball
% arrange them such that no more than 2 red balls are in a row
% if given a list, this predicate will return true if it matches these requirements
% if a list is given as a variable, this predicate will return all the possible solutions
% one by one
situation2(L):-
    Balls = [[red,4],[black,1],[blue,1]],
    permutate(Balls,L),
    noMoreThan2InARow(L).


% noMoreThan2InARow/1
% arg1, a list
% this predicate returns true when there are no more than two of the same element in a row
% this predicate will not work unless the argument is instantiated
noMoreThan2InARow([H1,H2|T]):-
    noMoreThan2InARow(H1,H2,T).
% noMoreThan2InARow/3
% this is a helper for noMoreThan2InARow/1 and is not inteded for external use
% arg1, the element before the previous
% arg2, the previous element
% arg3, the list
noMoreThan2InARow(_,_,[]).
noMoreThan2InARow(LastLast,Last,[H|T]):-
    Last = LastLast,
    H \= Last,
    noMoreThan2InARow(Last,H,T).
noMoreThan2InARow(LastLast,Last,[H|T]):-
    LastLast \= Last,
    noMoreThan2InARow(Last,H,T).

% situation3/1
% arg1, a list of colors representing colored balls
% this predicate solves the following logic problem
% You have eight colored balls: 1 yellow, 2 black, 2 blue and 3 purple.
% The balls in positions 2 and 3 are not purple.
% The balls in positions 4 and 8 are the same color.
% The balls in positions 1 and 7 are of different colors.
% There is a purple ball to the left of every blue ball.
% A black ball is neither first nor last.
% The balls in positions 6 and 7 are not blue.
% This predicate returns true when the list matches these requirements
% if the list is a variable, this predicate will generate the correct solutions,
% one at a time (you'll have to reject each answer until it generates all of them)
situation3(L):-
    Balls = [[yellow, 1],[black, 2],[blue,2],[purple, 3]],
    permutate(Balls,L),
    verify3(L).

% verify3/1
% arg1, a list of colored balls
% This is a helper for situation3/1
% this predicate retunrs true when
% The balls in positions 2 and 3 are not purple.
% The balls in positions 4 and 8 are the same color.
% The balls in positions 1 and 7 are of different colors.
% There is a purple ball to the left of every blue ball.
% for this predicate to work the argument must be instantiated
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

% leftOfBlueIsPurple/1
% arg1, a list of colors representing colored balls
% this is a helper for verify3
% This predicate returns true when every blue ball has a purple ball to the left of it
% this predicate will not work unless the argument is instantiated
leftOfBlueIsPurple([H|T]):-
    H \= blue,
    leftOfBlueIsPurple(H,T).
% leftOfBlueIsPurple/2
% arg1, the previous elements
% arg2, the list of elelments
% this is a helper for the leftOfBlueIsPurple/1 predicate and is not intended for external use
leftOfBlueIsPurple(P,[H|T]):-
    H = blue,
    P = purple,
    leftOfBlueIsPurple(H,T).
leftOfBlueIsPurple(_,[H|T]):-
    H \= blue,
    leftOfBlueIsPurple(H,T).
leftOfBlueIsPurple(_,[]).
