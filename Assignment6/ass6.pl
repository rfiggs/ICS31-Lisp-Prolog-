% a state in the 15 tile puzzle is represented using a flat list in the following form
% [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
% This state represents the following game board
% |12|13|14|15|
% |8 |9 |10|11|
% |4 |5 |6 |7 |
% |0 |1 |2 |3 |
% where 0 is the blank space
% nodes for this puzzle are represented as 2d lists as follows
% [state,parent-state]
% start nodes are special in that they must have an empty list as the parent
% bfs/2 and astar/2 will not work correctly if you do not set the parent of your start node to []

% Usefull Commands
% use bfs to find easy solution
% easy(X),bfs(X,Path).
% use bfs to find hard solution
% hard(X),bfs(X,Path).
% use astar to find easy solution
% easy(X),astar(X,Path).
% use astar to find hard solution
% hard(X),astar(X,Path).

% easy/1
% this is a start node for the 15 tile puzzle with an easy solution
easy([[13,14,11,0,9,10,12,15,5,6,7,8,1,2,3,4],[]]).

% hard/1
% this is a start node for the 15 tile puzzle with a harder solution
hard([[13,14,11,15,9,6,0,10,5,7,3,12,1,2,4,8],[]]).

% goal/1
% this is the goal node for the 15 tile puzzle
goal([[13,14,15,0,9,10,11,12,5,6,7,8,1,2,3,4],_]).

% isin/2
% checks if an element exits in a list
% arg1 the element to check for
% arg2 the list of elements to search through
isin(X,[X|_]).
isin(X,[_|T]):-
    isin(X,T).

% apend/3
% appends to lists together
% arg1, the first list to append
% arg2, the second list to append
% arg3, the two lists appeneded together
% at lesat one argument must be instantiated for this predicate to work
apend([], L2, L2).
apend([H1|T1], L2, [H1|L]):-
    apend(T1,L2,L).

% bfs/2
% this predicate solves the 15 tile puzzle using a breadth first search
% the goal state is defined above in the goal predicate
% arg1 this is the start node of the puzzle
% arg2 this is the solution path found by performing a bfs search
% the solution path consits of a squence of states from the start to the goal
bfs(Start, Path):-
    bfs(Start,FinalClosed,[],[]),
    makepath(FinalClosed,Path).

% bfs/4
% this is a helper for the bfs/2 predicate
% arg1 the current state of the puzzle
% arg2 the closed list after the bfs has found a solution
% arg3 the open list
% arg4 the closed list
% note that arg3 and arg4 must be bound for this predicate to work
bfs(Start,FinalClosed,Open,Closed):-
    goal(Start),
    FinalClosed = [Start|Closed].
bfs([Node,Parent],FinalClosed,[H|T],Closed):-
    isin([Node,_],Closed),
    bfs(H, FinalClosed, T, Closed).
bfs(Start, FinalClosed, Open, Closed):-
    bagof(Child,child(Start,Child),Children),
    apend(Open,Children,NewOpen),
    NewOpen = [H|T],
    NewClosed = [Start|Closed],
    bfs(H, FinalClosed, T, NewClosed).

% makepath/2
% makes a path to the solution from the final closed list
% arg1 the list of nodes to complete
% arg2 the solution path
% note that this predicate assumes that the first element in the list is the goal state
makepath([H|T],Path):-
    makepath(H,T,Path,[]).

% makepath/4
% this is a helper for the makepath/2 predicate
% arg1 the current state
% arg2 the closed list with the parent child relationships
% arg3 the final solution path
% arg4 a list to hold the solution path as we build it
% arg1 arg2 and arg4 must be bound fo this predicate to work
makepath([Current,[]], _, [Current|Build], Build).
makepath(Node, List, Path, Build):-
    Node = [Current,Parent],
    NewBuild = [Current|Build],
    parentnode(Parent, List, Next),
    makepath(Next, List, Path, NewBuild).

% parentnode/3
% this is a helper for the makepath predicate
% this predicate gives us the node for a given state
% the node contains the child and parent relationship
% arg1 the state
% arg2 the list of nodes containing the child-parent relationships
% arg3 the node whose child is the given state.
parentnode(State, [H|T], Node):-
    H = [Child,Parent],
    State = Child,
    H = Node.
parentnode(State, [H|T], Node):-
    parentnode(State, T, Node).

% xypos/3
% xypos is a predicate used to convert between the position in a flat list
% and the position in a 4x4 grid
% The X Y position is based off of (0,0) being the bottom left corner
% arg1 the position in a flat list
% arg2 the X position in a 4x4 grid
% arg3 the Y position in a 4x4 grid
% note for this predicate to work either arg1 needs to be bound or both arg2 and arg3 need to be bound.
xypos(P,X,Y):-
    nonvar(P),
    X is mod(P,4),
    Y is (P - X) / 4.

xypos(P,X,Y):-
    nonvar(X),
    nonvar(Y),
    P is (4 * Y) + X.


% child/2
% this is a helper for the bfs/2 and astar/2 predicates
% this predicate will generate a child for the given state
% note that in both bfs and astar we use bagof/3 to generate all the child states as this predicate will only unify one at a time
% arg1 the node whose child we are generating
% arg2 a child of the given state
child([Parent|_], [Child|[Parent]]):-
    position(0,Parent,P),
    xypos(P,X,Y),
    child(Parent,Child,X,Y).

% child/4
% this is a helper for the child/2 predicate
% these 4 predicates give us the 4 possible moves you can make from any state
% arg1 the node whose child we are finding
% arg2 the child of the given node
% arg3 the X position 0 in the given node
% arg4 the Y position 0 in the given node
child(Parent,Child,X,Y):-
    X > 0,
    Cx is X - 1,
    xypos(Cp,Cx,Y),
    position(C,Parent,Cp),
    swap(Parent,C,0, Child).
child(Parent,Child,X,Y):-
    X < 3,
    Cx is X + 1,
    xypos(Cp,Cx,Y),
    position(C,Parent,Cp),
    swap(Parent,C,0, Child).
child(Parent,Child,X,Y):-
    Y > 0,
    Cy is Y - 1,
    xypos(Cp,X,Cy),
    position(C,Parent,Cp),
    swap(Parent,C,0, Child).
child(Parent,Child,X,Y):-
    Y < 3,
    Cy is Y + 1,
    xypos(Cp,X,Cy),
    position(C,Parent,Cp),
    swap(Parent,C,0, Child).

% swap/4
% this predicate swaps the two given elements in the list
% note that for this prediate to work, there can be no duplicates in the list
% arg1 the original list
% arg2 the first element to swap
% arg3 the second element to swap
% arg4 a list with the two element swapped.
swap([],_,_,[]).
swap([Y|T],X,Y,Nl):-
    swap(T,X,Y,Nll),
    Nl =[X|Nll],!.
swap([X|T],X,Y,Nl):-
    swap(T,X,Y,Nll),
    Nl =[Y|Nll],!.
swap([H|T],X,Y,Nl):-
    swap(T,X,Y,Nll),
    Nl = [H|Nll].

% postion/3
% this predicate relates an element to its position in a list
% where the first element is at position 0 and the last element has a postion equal to the length of the lists mimnus 1.
% This is similar to how arrays work in java and c
% arg1 the element in the lit
% arg2 the list
% arg3 the position of the element in the list
% note if the element does not exist in the list, or if the given position is out of bounds this predicate will return false
% for this predicate to work, the second argument must be bound
position(Element, List, Position):-
    position(Element, List, Position, 0).
position(Element, [Element|_], Position, Position).
position(Element, [_|T], Position, Count):-
    N is Count + 1,
    position(Element, T, Position, N).

% mdistance/2
% this predicate calculates the manhattan distance of a given 15 tile puzzle state.
% distance is calculated based off of the goal predicate above
% arg1 the state
% arg2 the manhattan distance of the given state
mdistance(State, Distance):-
    mdistance(State, Distance, 0).

% mdistance/3
% a helper to mdistance/2
% arg1 the state or part of the state
% arg2 the distance
% arg3 the current position in the state
mdistance([],0,_).
mdistance([H|T], Distance, Count):-
    NewCount is Count + 1,
    mdistance(T, NewDistance, NewCount),
    xypos(Count, X1, Y1),
    goal([Goal,_]),
    position(H,Goal,P2),
    xypos(P2,X2,Y2),
    D is (abs(X1 - X2) + abs(Y1-Y2)),
    Distance is NewDistance + D.

% reorder/2
% This predicate reorders a list of nodes so that the first node in the list is the one with the smallest manhattan distance.
% note that this is not a full sort, it only garuntees that the first element will have the smallest manhattan score
% This prediate is used in the astar/2 predicate.
% arg1 the list to reorder
% arg2 the resulting list
reorder([H],[H]).
reorder([H|T], Result):-
    H = [Node,Parent],
    reorder(T,[H2|T2]),
    H2 = [Node2,Parent2],
    mdistance(Node,D1),
    mdistance(Node2,D2),
    D2  < D1,
    Result = [H2,H|T2].
reorder([H|T], Result):-
    reorder(T,[H2|T2]),
    Result = [H,H2|T2].

% astar/2
% this predicate preforms an a star search over the 15 tile puzzle
% it uses the manhattan distance predicate as a heursitic to judge the states
% arg1 the start node
% arg2 the solution Path
% note that the solution path is a sequence of states from start to goal
astar(Start, Path):-
    astar(Start,FinalClosed,[],[]),
    makepath(FinalClosed,Path).

% astar/4
% this predicate is a helper for astar/2
% arg1 the start node
% arg2 the final closed list containing the child-parent relationships
% arg3 the Open list
% arg4 the Closed list
% note that arg1, arg3, and arg4 must be bound for this predicate to work
astar(Start,FinalClosed,Open,Closed):-
    goal(Start),
    FinalClosed = [Start|Closed].

astar([Node,Parent],FinalClosed,[H|T],Closed):-
    isin([Node,_],Closed),
    reorder(T,NewOpen),
    astar(H, FinalClosed, NewOpen, Closed).

astar(Start, FinalClosed, Open, Closed):-
    bagof(Child,child(Start,Child),Children),
    apend(Open,Children,ExtraOpen),
    reorder(ExtraOpen, NewOpen),
    NewOpen = [H|T],
    NewClosed = [Start|Closed],
    astar(H, FinalClosed, T, NewClosed).
