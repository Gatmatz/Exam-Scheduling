%Open data file with facts.
?-consult('attends.pl').

courses(Courses) :-
	setof(Course, S^attends(S,Course),Courses).

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
    divide_list(Permutations, A, B, C).

%Predicate that turns a list with one element to a variable.
%Fails if list has more than one element.
list_to_var([H],H).

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
    schedule(A,B,C),
	check_week(A,E1),
	check_week(B,E2),
	check_week(C,E3),
	E is E1 + E2 + E3.

%Predicate to find the minimal error of all schedules
minimal_error(E) :-
    setof(Error-A-B-C, schedule_errors(A, B, C, Error), List),
    List=[E-A-B-C|_].

%Predicate that finds the schedules with minimal error
minimal_schedule_errors(A, B, C, E) :-
    minimal_error(E),
    schedule_errors(A, B, C, E).

%Predicate that finds the minimum possible error E through minimal_schedule_errors predicate.
find_min_error(E) :-
	minimal_schedule_errors(_,_,_,E),!.

%Predicate that finds the maximum possible score between exams that have the minimum error.
find_max_score(S) :-
	find_min_error(E),
	setof(S,A^B^C^(minimal_schedule_errors(A,B,C,E),score_schedule(A,B,C,S)),Scores),
	reverse(Scores,[S|_]).

%Final predicate that calculates the exam schedule that has the minimum error and the maximum score.
%First, it finds the minimum possible error and maximum score and then finds the schedules that have
%that minimum error and maximum score.
maximum_score_schedule(A,B,C,E,S) :-
	find_min_error(E),
	find_max_score(S),
	minimal_schedule_errors(A,B,C,E),
	score_schedule(A,B,C,S).



    



