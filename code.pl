%Open data file with facts.
?-consult('attends.pl').

%Predicate that removes duplcate elements from a list.
remove_duplicates([],[]).
remove_duplicates([H|T],Result) :-
	member(H,T),
	remove_duplicates(T,Result), !.
remove_duplicates([H|T],[H|Result]) :-
	remove_duplicates(T,Result).

%Predicate that creates a list "Result" with all the separate exam courses.
courses(Result) :-
	findall(X, attends(_,X),L),
	remove_duplicates(L,Result).

%Predicate that returns all possible orders of k elements.
k_permutation(0,_,[]).
k_permutation(K,L1,[X|T2]) :-
    K > 0,
    K1 is K - 1,
    delete(X,L1,L2),
    k_permutation(K1,L2,T2).