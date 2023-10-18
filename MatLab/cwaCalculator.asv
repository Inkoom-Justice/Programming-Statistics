%%This is a program for calcuting CWA and also showing if a student had
%first class or any other class. It was programmed by Justice Inkoom
clear;
clc;
%__________________________________________________________________________

%%
%FIRST CASE: Gather information from student.
name=input('Enter fullname: ', 's');
nCourses=input('Enter number of courses taken: ');
numClass=nCourses;
totalMarks=0;
totalCredit=0;
t=0; %END

%%
%For Loop meant to gather information on the marks and credit hours 
for i=1:nCourses
    t=t+1;
    fprintf("\nCourse %d\n",t);
    M(i)=input('Enter MARKS for course: ');
    C(i)=input('Enter CREDIT HOURS for course: ');
    
    totalMarks=totalMarks + (M(i)*C(i));
    totalCredit=totalCredit + C(i);
    
end
CWA = totalMarks/totalCredit;
fprintf("\n----------------------------\n");
fprintf("Name: %s\n", name);

fprintf("CWA : \t%0.2f\n",CWA);
fprintf("----------------------------\n");
if (CWA>=80 && CWA<=100)
   fprintf('First Class\n');
   
elseif (CWA>=70 && CWA<80)
    fprintf('Second Class Upper\n');
    
elseif (CWA>=60 && CWA<70)
    fprintf('Second Class Lower\n');
    
elseif (CWA>=50 && CWA<60)
    fprintf('Pass\n');
    
elseif (CWA<50)
    fprintf('Fail\n');
end
fprintf("----------------------------\n");