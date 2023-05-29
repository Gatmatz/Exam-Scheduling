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

%Predicate that splits a list of permutations into 2 lists
divide_list2([],[],[]).
divide_list2([X],[X],[]).
divide_list2([X,Y],[X],[Y]).
divide_list2([X,Y|L],[X|L1],[Y|L2]) :-
    divide_list2(L,L1,L2).

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

%Auxiliary predicate that checks which lessons student S is attending in a week with 3 lessons and calculates the score.
%First it finds all the students that attend lesson in X days with the help of findall , and then it calculates the score based on some criterias.
score_calculator(A,B,C,SCORE):-
	findall(S,(attends(S,A),attends(S,B),not(attends(S,C))),Students0),			% Monday-Wednesday +1.
	length(Students0,E0),

	findall(S,(attends(S,C),attends(S,B),not(attends(S,A))),Students1),			% Wednesday-Friday +1.
	length(Students1,E1),

	findall(S,(attends(S,A),attends(S,C),not(attends(S,B))),Students2),			% Monday-Friday +3.
	length(Students2,E2),

	findall(S,(attends(S,A),attends(S,B),attends(S,C)),Students3),				% Monday-Wednesday-Friday -7.
	length(Students3,E3),

	findall(S,(not(attends(S,A)),not(attends(S,B)),not(attends(S,C))),Students4),		% None 0.
	length(Students4,E4),

	findall(S,(attends(S,A),not(attends(S,B)),not(attends(S,C))),Students5),		% Monday +7.
	length(Students5,E5),

	findall(S,(attends(S,B),not(attends(S,A)),not(attends(S,C))),Students6),		% Wednesday +7.
	length(Students6,E6),

	findall(S,(attends(S,C),not(attends(S,A)),not(attends(S,B))),Students7),		% Friday +7.
	length(Students7,E7),


	SCORE is E0+E1+(E2*3)-(7*E3)+0*E4+(E5+E6+E7)*7.

%Auxiliary predicate that checks which lessons student S is attending in a week with 2 lessons and calculates the score.
score_calculator2(A,B,SCORE):-
	findall(S,(attends(S,A),attends(S,B)),Students),	%Monday-Wednesday +1
	length(Students,E),

	findall(S,(attends(S,A),not(attends(S,B))),Students1),	%Monday +7
	length(Students1,E1),

	findall(S,(attends(S,B),not(attends(S,A))),Students2),	%Friday +7
	length(Students2,E2),

	SCORE is E+(E1+E2)*7.


%Auxiliary predicate that divides the list with the lessons to 3 sublists with one element each
%and then turns the list to variables.
week_score(A,E):-
	divide_list(A,A1,A2,A3),
	list_to_var(A1,AA1),
	list_to_var(A2,AA2),
	list_to_var(A3,AA3),
	score_calculator(AA1,AA2,AA3,E).


%Auxiliary predicate that divides the list with the lessons to 2 sublists with one element each
%and then turns the list to variables.
week_score2(A,E):-
	divide_list2(A,A1,A2),
	list_to_var(A1,AA1),
	list_to_var(A2,AA2),
	score_calculator2(AA1,AA2,E).


%Predicate that calculates the schedule score based on the lessons that student attends.
%First it calculates the scores for each week and then adds them up.
score_schedule(A,B,C,S) :-
	week_score(A,E1),
	week_score(B,E2),
	week_score2(C,E3),
	S is E1 + E2 + E3.


    



