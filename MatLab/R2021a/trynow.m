%% Homotopy Perturbation method for solving SIR Infectious Disease Model by
% Incorporating Vaccination.

clear all;
s_not=110;
i_not=55;
r_not=35;

a=0.001;
g=0.16;
B=0.15;
z=0.002;
d=0.01;
u=0.015;
p=0.02;

t=0;
for i=1:20
    
    
    
    S1=s_not+(B+(z*r_not)-((a*i_not)+p+u)*(s_not))*(t);
    S2=(z*((g*i_not)-(u+z)*r_not+(p*s_not)));
    S3=-(a*s_not)*((a*s_not*i_not)-(g+d+u)*i_not);
    S4=-((a*i_not)+p+u)*(B+(z*r_not)-((a*i_not)+p+u)*s_not);
    
    I1=i_not+((a*s_not)-(g+d+u))*i_not*(t);
    I2=((a*i_not)*(B+(z*r_not)-((a*i_not)+p+u)*s_not));
    I3=((a*s_not)-g-d-u)^(2)*i_not;
    
    R1=r_not+((g*i_not)-(u+z)*r_not+(p*s_not))*(t);
    R2=((g*i_not)*((a*s_not)-g-d-u)-(u+z)*((g*i_not)-(u+z)*r_not+(p*s_not)));
    R3=-p*(B-(z*r_not)-((a*i_not)+p+u)*s_not);
    
    S(i)=S1+((S2+S3+S4)*(t^2)/2);
    I(i)=I1+((I2+I3)*(t^2)/2);
    R(i)=R1+((R2+R3)*(t^2)/2);
    
    fprintf('\nS(%.1f)= %.4f\n',i,S(i));
    fprintf('I(%.1f)= %.4f\n',i,I(i));
    fprintf('R(%.1f)= %.4f\n',i,R(i));
    t=t+1;
    %S=S(i);
    %I=I(i);
    %R=R(i);
    xx(i) = t;
end

plot(xx,S(1:t),xx,I(1:t),xx,R(1:t));
hold on
xlabel('Time')
ylabel('Susceptible/Infection/Recovery')
legend('Susceptible','Infected','Recovery')
