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
subsetsWithLastIndex([X], Index, [(X, Index, [X])]).
subsetsWithLastIndex([X|XS], Index, L) :-
    sum([X|XS], Size),
    append([(Size, Index, [X|XS])], L1, L),
    subsetsWithLastIndex(XS, Index, L1).

% Generates all possible subsets from a list
% together with the sublist length and ending index.
generateSubsetsWithIndex([X], _, [(X, 0,[X])]).
generateSubsetsWithIndex(XS, Index, L) :-
    subsetsWithLastIndex(XS, Index, L1),
    init(XS, XS1),
    PrevIndex is Index - 1,
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
