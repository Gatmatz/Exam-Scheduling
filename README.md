# Exam-Scheduling
## Overview
An exam scheduling program written in SWI-Prolog. <br />
The scheduling is optimal in terms of facilitation for students. <br />
The program ensures that no student is been examined in more than two lessons in the same week.<br />
If this possibility does not exist, then the number of students who happen to be examined in more than 2 courses at the same time is been minimized.<br />

## Predicates
- `remove_duplicates(L,NL)`: Removes duplicate elements from a list L and saves the new list to NL. <br />
-`courses(Result)`: Creates a list Result with all the separate exam courses. <br />
-`k_permutation(K,L,T)`: Returns all possible orders of k elements of L list to list T. <br />
-`permutations_list(Result)`:Generates all possible permutations of 8 courses to list Result. <br />
-`divide_list(L,L1,L2,L3)`:Splits a list L1 into 3 lists L1,L2 and L3. <br />
-`schedule(A, B, C)`: Generates a program of 8 exam courses in 3 weeks. <br />
-`list_to_var(L,X)`:Turns a list L with one element to a variable X. <br />
-`check_student(S,W)`:Checks if student S is attending all lesson in week W. <br />
-`check_week(W,E)`: Counts the number E of students that are dissatisfied with week W. <br />
-`schedule_errors(A,B,C,E)`:Computes the error in given schedule A,B,C. <br />