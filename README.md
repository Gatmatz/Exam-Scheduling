# Exam-Scheduling
## Overview
An exam scheduling program written in SWI-Prolog. <br />
The scheduling is optimal in terms of facilitation for students. <br />
The 8 courses are examined over 2.5 weeks, according to the following schedule:
- Week 1: Monday, Wednesday, Friday.<br />
- Week 2: Monday, Wednesday, Friday.<br />
- Week 3: Monday, Wednesday. <br />

## Criterion
The schedule should ensure that no student should be tested in more than two courses in the same week. 
If this is not possible, then the number of students who happen to be tested in more than 2 courses in the same week should be minimized. <br />
If more than one programme is similar in terms of the above criterion, 
i.e. they are equivalent in terms of the number of students taking more than 2 courses in a week, then a second criterion for comparing preference between programmes is the number of days that days between the courses examined. <br />
## Scoring
If a student is tested on the same week Monday-Wednesday, then the program gets a score of +1 for that student and for that week. <br />
If the second week is given Monday-Friday then the program gets a score of +3 for that student for that week as well. <br />
If a student gives only one class in a week then that week gets a score of +7 for that student. <br />
If a student gives no classes any week then that week does not add to the score. <br />
If a student gives three lessons some week then that week gets a score of -7 for that student. <br />
The total score for a program is the sum of the scores for each student and each week. <br />