%SIR FOR MUMPS DISEASE MODELLING


clear all;
b=input('Enter Natural Birth Rate, B: ');
a=input('Enter Contact Rate, a: ');
u=input('Enter Natural Death Rate, u: ');
g=input('Enter Recovery Rate, g: ');
d=input('Enter Disease Death Rate, d: ');
s_not=input('Enter Number of Susceptible, S: ');
i_not=input('Enter Number of Infected, I: ');
R=input('Enter Number of Recovered, R: ');


t=0;
for i=1:10
    
    t=t+1;
    
    S1=s_not+(b-(a*s_not*I)-(u*s_not))*t;
    S2=-(a*s_not*I)*((a*s_not)-(g+d+u));
    S3=-(b-(a*s_not*I)-(u*s_not))*((a*I)+u);
    
    I1=I+(I*((a*s_not)-(g+d+u)))*t;
    I2=I*((a*s_not)-(g+d+u))^2;
    I3=(a*I)*(b-(a*s_not*I)-(u*s_not));
    
    R1=R+((g*I)-(u*R))*(t);
    R2=(g*I)*((a*s_not)-(g+d+u));
    R3=-u*((g*I)-(u*R));
    
    s_not(i)=S1+((S2+S3)*(t^2)/2);
    I(i)=I1+((I2+I3)*(t^2)/2);
    R(i)=R1+((R2+R3)*(t^2)/2);
    
    fprintf('S(%.1f)= %.6f\n',i,s_not(i));
    fprintf('I(%.1f)= %.6f\n',i,I(i));
    fprintf('R(%.1f)= %.6f\n',i,R(i));
    
    
end
