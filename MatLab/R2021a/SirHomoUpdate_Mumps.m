%SIR FOR MUMPS DISEASE MODELLING


clear all;
b=input('Enter Natural Birth Rate, B: ');
a=input('Enter Contact Rate, a: ');
u=input('Enter Natural Death Rate, u: ');
g=input('Enter Recovery Rate, g: ');
d=input('Enter Disease Death Rate, d: ');
s_not=input('Enter Number of Susceptible, S: ');
i_not=input('Enter Number of Infected, I: ');
r_not=input('Enter Number of Recovered, R: ');

S1=b-(a*s_not*i_not)-(u*s_not);
    S2=(-a*s_not*i_not)*(a*s_not - (g+d+u));
    S3=-(b-(a*s_not*i_not)-(u*s_not))*((a*i_not)+u);


    I1=(i_not*((a*s_not)-(g+d+u)));
    I2=i_not*((a*s_not)-(g+d+u))^2;
    I3=(a*i_not)*(b-(a*s_not*i_not)-(u*s_not));

    R1=((g*i_not)-(u*r_not));
    R2=(g*i_not)*((a*s_not)-(g+d+u));
    R3=-u*((g*i_not)-(u*r_not));
t=0;
for i=1:30
    
    
       
    
    S(i)= s_not + (S1*t) +((S2+S3)*(t^2)/2);
    I(i)= i_not + (I1*t)+ ((I2+I3)*(t^2)/2);
    R(i)= r_not + (R1*t)+((R2+R3)*(t^2)/2);
    
    fprintf('S(%.1f)= %.6f\n',i,S(i));
    fprintf('I(%.1f)= %.6f\n',i,I(i));
    fprintf('R(%.1f)= %.6f\n',i,R(i));
    t=t+1;
    xx(i)=i;
end

plot(xx,S(1:i),xx,I(1:i),xx,R(1:i));
hold on
xlabel('Time')
ylabel('Susceptible/Infection/Recovery')
legend('Susceptible','Infected','Recovery')