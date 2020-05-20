% Smallest k sets

% Haskell-helper functions
% Removes the last item in list (like init in haskell)
init(X, L) :-
    append(L, [_], X).

sum([X],X).
sum([X|XS], Size) :-
    sum(XS,I),
    Size is X + I.

take(1, [X], [X]).
take(1, [X|_], [X]).
take(Num, [X|XS], L) :-
    append([X], L2, L),
    NumDec is Num - 1,
    take(NumDec, XS, L2).

% Starting from the last element and its index,
% generate a tuple (ListLength, LastIndex, Subset)
% for all subsets possible starting at last index.
subsetsWithLastIndex([X], EndIndex, [(X, EndIndex, EndIndex, [X])]).
subsetsWithLastIndex([X|XS], EndIndex, L) :-
    sum([X|XS], Size),
    length([X|XS], Length),
    StartIndex is EndIndex - (Length - 1),
    append([(Size, StartIndex, EndIndex, [X|XS])], L1, L),
    subsetsWithLastIndex(XS, EndIndex, L1).

% Generates all possible subsets from a list
% together with the sublist length and ending index.
generateSubsetsWithIndex([X], _, [(X, 0, 0,[X])]).
generateSubsetsWithIndex(XS, EndIndex, L) :-
    subsetsWithLastIndex(XS, EndIndex, L1),
    init(XS, XS1),
    PrevIndex is EndIndex - 1,
    generateSubsetsWithIndex(XS1, PrevIndex, L2),
    append(L1,L2,L).

% Generate subsets with their last indices and sizes
generateSubsets(XS, L) :-
    length(XS, Length),
    Index is Length - 1,
    generateSubsetsWithIndex(XS, Index, L).

% Get the smallest K sets
smallestKSets(Num, List, Return) :-
    take(Num, SortedSublists, Return),
    generateSubsets(List, Sublists),
    sort(Sublists, SortedSublists).

formatTuple((Size, StartIndex, EndIndex, List)) :-
    write(Size),
    write("\t"),
    write(StartIndex),
    write("\t"),
    write(EndIndex),
    write("\t"),
    write(List),
    nl.

printHeader :-
    write("size\ti\tj\tsublist\n").

formatTuples([]).
formatTuples([X]) :- formatTuple(X).
formatTuples([X|XS]) :-
    formatTuple(X),
    formatTuples(XS).

% Enter a K and the list
program(K, List) :-
    printHeader,
    smallestKSets(K, List, Return),
    formatTuples(Return).

testProgram2 :-
    program(6, [24,-11,-34,42,-24,7,-19,21]).

testProgram3 :-
    program(8, [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3]).

