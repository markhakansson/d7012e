% Package delivery lab
% by Mark Hakansson

% Some information:
% 
% --State--
% Contains the following locations:
%   current_room | steel_key_location | brass_key_location | package_location;
%
% --Item--
% Item locations are:
%   inventory | r1 | r2 | r3;
%
% --Move--
% Works like this:
% State is the next state if Condition is true. To reach the next State, a Move is done.
% Take actions also check that the inventory is not full (i.e. more than 2 items)

% Move to room 2 (from room1)
action(Condition, State, Action) :-
    Condition = state(r1, inventory, Brass, Package),
    State = state(r2, inventory, Brass, Package),
    Action = "move to r2".

% Move to room 1 (from room2)
action(Condition, State, Action) :-
    Condition = state(r2, inventory, Brass, Package),  
    State = state(r1, inventory, Brass, Package),
    Action = "move to r1".

% Move to room 1 (from room3)
action(Condition, State, Action) :-
    Condition = state(r3, Steel, inventory, Package),
    State = state(r1, Steel, inventory, Package),
    Action = "move to r1".

% Move to room 3 (from room 1)
action(Condition, State, Action) :-
    Condition = state(r1, Steel, inventory, Package),
    State = state(r3, Steel, inventory, Package),
    Action = "move to r3".

% Take steel key
action(Condition, State, Action) :-
    Condition = state(Room, Room, Brass, Package),
    State = state(Room, inventory, Brass, Package),
    State \= state(_, inventory, inventory, inventory),
    Action = "take steel key".
% Take brass key 
action(Condition, State, Action) :-
    Condition = state(Room, Steel, Room, Package),
    State = state(Room, Steel, inventory, Package),
    State \= state(_, inventory, inventory, inventory),
    Action = "take brass key".
% Take package
action(Condition, State, Action) :-
    Condition = state(Room, Steel, Brass, Room),
    State = state(Room, Steel, Brass, inventory),
    State \= state(_, inventory, inventory, inventory),
    Action = "take package".

% Drop steel key
action(Condition, State, Action) :-
    Condition = state(Room, inventory, Brass, Package),
    State = state(Room, Room, Brass, Package),
    Action = "drop steel key".
% Drop brass key
action(Condition, State, Action) :-
    Condition = state(Room, Steel, inventory, Package),
    State = state(Room, Steel, Room, Package),
    Action = "drop brass key".
% Drop package
action(Condition, State, Action) :-
    Condition = state(Room, Steel, Brass, inventory),
    State = state(Room, Steel, Brass, Room),
    Action = "drop package".

% Solves the problem for a state, with a max recursion step of N.
% Default state = state(r1,r1,r2,r3)
solveR(state(_, _, _, r2), _, ["finished"]). 
solveR(State, N, [Action|Trace]) :-
    N > 0,
    action(State, NextState, Action),
    solveR(NextState, N - 1, Trace).

