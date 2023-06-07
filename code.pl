%------------------------STUDENTS--------------------------
%Ατματζίδης Γεώργιος ΑΕΜ:3908
%Λιτσίδης Δημήτριος AEM:3930
%Καραχούς Χαλιδήν AEM:3869

%Open data file with facts.
?-consult('attends.pl').

%Courses predicate that finds all exam lessons.
%The predicate uses the setof to gather the exam lessons for all students.
%The exam lessons are retrieved in a sorted way and they are unique.
courses(Courses) :-
	setof(Course, S^attends(S,Course),Courses).

%Predicate that returns all possible orders of k elements.
k_permutation(0,_,[]).    %If K is 0, the permutation list is empty.
k_permutation(K,L1,[X|T2]) :-
	K > 0,           %Ensure K is greater than 0.
	K1 is K - 1,     %Decrement K by 1.
	select(X,L1,L2), %Selects an element X from the list L1, resulting in a new list L2 that contains all elements of L1 except the selected element X.
	k_permutation(K1,L2,T2).   %Recursively generate a permutation list T2 with K1 elements from L2.

%Predicate that generates all possible permutations of courses.
%The predicate computes the list with the courses from courses predicate,
%finds its length and finally computes all permutations of the courses.
permutations_list(Result) :-
    courses(Courses),	%Find all courses and store them in courses list
    length(Courses,L),	%Find the length L of the course list
    k_permutation(L, Courses, Result).	%Produce all permutations of the course list

%Predicate that splits a single list into 3 separate lists.
divide_list([],[],[],[]).	%If the list is empty, return three empty lists
divide_list([X],[X],[],[]).	%If the list has one element, put the element in the first list and return other 2 lists
divide_list([X,Y],[X],[Y],[]).	%If the list has two elements, put the elements in the first and second list and return one empty
divide_list([X,Y,Z|L],[X|L1],[Y|L2],[Z|L3]) :- %If the list has more than 2 elements 
    divide_list(L,L1,L2,L3).	%then divide their heads and recursively call for their tails

%Predicate that splits a list into 2 separate lists.
divide_list2([],[],[]).	%If the list is empty, return two empty lists
divide_list2([X],[X],[]).	%If the list has one element, put the element in the first list and return one more empty list
divide_list2([X,Y|L],[X|L1],[Y|L2]) :- %If the list has more than 1 elements 
    divide_list2(L,L1,L2).	%then divide their heads and recursively call for their tails

%Schedule predicate that builds all possible exam schedules.
%The predicate fetches each permutation from the permutations_list predicate
%and the divides the list to 3 lists, one for each week.
schedule(A, B, C) :-
    permutations_list(Permutations),	%Produce a new course schedule
    divide_list(Permutations, A, B, C).	%Divide the list with exams to three list of each week

%Predicate that turns a list with one element to a variable.
%Fails if list has more than one element.
list_to_var([H],H).

%Auxiliary predicate that checks if student S is attending all lesson in week W.
%The predicate divides the list with the lessons to 3 sublists with one element each
%and then turns the list to variables.
%In the end checks if student attends all lessons in the week.
check_student_error(S,W) :-
	divide_list(W,L1,L2,L3),	%Divide the week with three courses to three lists with one element
	list_to_var(L1,E1),	%Turn the list with one element to a variable, representing a course
	list_to_var(L2,E2),	%Turn the list with one element to a variable, representing a course
	list_to_var(L3,E3),	%Turn the list with one element to a variable, representing a course
	attends(S,E1),	%Check if student S attends course E1
	attends(S,E2),	%Check if student S attends course E2
	attends(S,E3).	%Check if student S attends course E3

%Auxiliary predicate that counts the number E of students that are dissatisfied with week W.
%Computes the number of student that attending all lessons in given week.
find_week_error(W,E) :-
	findall(S, (check_student_error(S,W)),Students),	%Find all dissatisfied students and store them in Students list
	length(Students,E).	%Find the number of dissatisfied students

%Predicate that computes the error in given schedule A,B,C.
%Computes the total number of students that attend more than two lessons in a week.
schedule_errors(A,B,C,E) :-
    schedule(A,B,C),	%Produce the schedule
	find_week_error(A,E1),	%Find error in first week
	find_week_error(B,E2),	%Find error in second week
	find_week_error(C,E3),	%Find error in third week
	E is E1 + E2 + E3.	%Sum up all errors

%Predicate to find the minimal error of all schedules.
%With the use of setof the predicate finds all schedules and stores them in a list in a sorted way by the error.
%In the end, it keeps the error from the first element of the list(which means the minimal error)-through unification.
minimal_error(E) :-
    setof(Error-A-B-C, schedule_errors(A, B, C, Error), List),	%Produce each schedule and find its error and store them in List list in a sorted way
    List=[E-A-B-C|_].	%Keep the minimal error 

%Predicate that finds the schedules with minimal error.
%Because there may be multiple schedules with minimal error, the predicate finds all schedules with the minimal error. 
minimal_schedule_errors(A, B, C, E) :-
    minimal_error(E),	%Find the minimal error E
    schedule_errors(A, B, C, E).	%Produce all schedules with the given error E

%Predicate that finds the maximum possible score between exams that have the minimum error.
find_max_score(S) :-
	minimal_error(E),	%Find the minimal error E
	setof(S,A^B^C^(minimal_schedule_errors(A,B,C,E),score_schedule(A,B,C,S)),Scores),	%Score each schedule and store the schedule with the error and the score in Scores list
	reverse(Scores,[S|_]).	%Reverse the list to get the maximal score and through unification keep only the score S

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


	SCORE is E0+E1+(E2*3)-(7*E3)+0*E4+(E5+E6+E7)*7.	%Calculate the final score based on given criteria weights and the number of students in each criteria

%Auxiliary predicate that checks which lessons student S is attending in a week with 2 lessons and calculates the score.
score_calculator2(A,B,SCORE):-
	findall(S,(attends(S,A),attends(S,B)),Students),	%Monday-Wednesday +1
	length(Students,E),

	findall(S,(attends(S,A),not(attends(S,B))),Students1),	%Monday +7
	length(Students1,E1),

	findall(S,(attends(S,B),not(attends(S,A))),Students2),	%Friday +7
	length(Students2,E2),

	SCORE is E+(E1+E2)*7.	%Calculate the final score based on given criteria weights and the number of students in each criteria


%Auxiliary predicate that divides the list with the lessons to 3 sublists with one element each
%and then turns the list to variables.
%Finally score the schedule and return the score through unification in variable E.
week_score(A,E):-
	divide_list(A,A1,A2,A3),	%Divide the list to 3 sublists
	list_to_var(A1,AA1),	%Turn the list with one element to a variable, representing a course
	list_to_var(A2,AA2),	%Turn the list with one element to a variable, representing a course
	list_to_var(A3,AA3),	%Turn the list with one element to a variable, representing a course
	score_calculator(AA1,AA2,AA3,E).	%Score the week with three courses


%Auxiliary predicate that divides the list with the lessons to 2 sublists with one element each
%and then turns the list to variables.
week_score2(A,E):-
	divide_list2(A,A1,A2),	%Divide the list to 3 sublists
	list_to_var(A1,AA1),	%Turn the list with one element to a variable, representing a course
	list_to_var(A2,AA2),	%Turn the list with one element to a variable, representing a course
	score_calculator2(AA1,AA2,E).	%Score the week with two courses


%Predicate that calculates the schedule score based on the lessons that student attends.
%First it calculates the scores for each week and then adds them up.
score_schedule(A,B,C,S) :-
	week_score(A,E1),	%Find score for the first week
	week_score(B,E2),	%Find score for the second week
	week_score2(C,E3),	%Find score for the third week
	S is E1 + E2 + E3.	%Sum up all scores.

%Final predicate that calculates the exam schedule that has the minimum error and the maximum score.
%First, it finds the minimum possible error and maximum score and then finds the schedules that have
%that minimum error and maximum score.
maximum_score_schedule(A,B,C,E,S) :-
	minimal_error(E),	%Find minimum error E.
	find_max_score(S),	%Find maximum possible score S with the minimum error E.
	minimal_schedule_errors(A,B,C,E),	%Produce the exam schedules with the minimum error E.
	score_schedule(A,B,C,S). %Finally, score those exam schedules.