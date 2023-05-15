%Open data file with facts.
?-consult('attends.pl').

%Predicate that removes duplicate elements from a list.
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
	select(X,L1,L2),
	k_permutation(K1,L2,T2).

%Predicate that generates all possible permutations of 8 courses
permutations_list(Result) :-
    courses(Courses),
    k_permutation(8, Courses, Result).

%Predicate that splits a list of permutations into 3 lists
divide_list([],[],[],[]).
divide_list([X],[X],[],[]).
divide_list([X,Y],[X],[Y],[]).
divide_list([X,Y,Z|L],[X|L1],[Y|L2],[Z|L3]) :-
    divide_list(L,L1,L2,L3).

%Predicate schedule
schedule(A, B, C) :-
    permutations_list(Permutations),
    nl,
    divide_list(Permutations, A, B, C).

%Predicate that turns a list with one element to a variable.
%Fails if list has more than one element.
list_to_var([X],X).

%Auxiliary predicate that checks if student S is attending all lesson in week W.
%The predicate divides the list with the lessons to 3 sublists with one element each
%and then turns the list to variables.
%In the end checks if student attends all lessons in the week.
check_student(S,W) :-
	divide_list(W,L1,L2,L3),
	list_to_var(L1,E1),
	list_to_var(L2,E2),
	list_to_var(L3,E3),
	attends(S,E1),
	attends(S,E2),
	attends(S,E3).

%Auxiliary predicate that counts the number E of students that are dissatisfied with week W.
%Computes the number of student that attending all lessons in given week.
check_week(W,E) :-
	findall(S, (check_student(S,W)),Students),
	length(Students,E).

%Predicate that computes the error in given schedule A,B,C.
%Computes the total number of students that attend more than two lessons in a week.
schedule_errors(A,B,C,E) :-
	check_week(A,E1),
	check_week(B,E2),
	check_week(C,E3),
	E is E1 + E2 + E3.
