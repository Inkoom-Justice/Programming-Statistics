%% Approximate Solution of SIR DISEASE MODEL 
% using HOMOTOPY PERTURBATION METHOD (HPM)


clear all;


t=0;
for i=1:11
    
    
    
    S1=60+(0.2-(0.001*60*25)-(0.015*60))*(t);
    S2=-(0.001*60*25)*((0.001*60)-(0.15+0.01+0.015));
    S3=-(0.2-(0.001*60*25)-(0.015*60))*((0.001*25)+0.015);
    
    I1=25+(25*((0.001*60)-(0.15+0.01+0.015)))*(t);
    I2=25*((0.001*60)-(0.15+0.01+0.015))^2;
    I3=(0.001*25)*(0.2-(0.001*60*25)-(0.015*60));
    
    R1=15+((0.15*25)-(0.015*15))*(t);
    R2=(0.15*25)*((0.001*60)-(0.15+0.01+0.015));
    R3=-0.015*((0.15*25)-(0.015*15));
    
    S(i)=S1+((S2+S3)*(t.^2)/2);
    I(i)=I1+((I2+I3)*(t.^2)/2);
    R(i)=R1+((R2+R3)*(t.^2)/2);
    
    fprintf('\nS(%.1f)= %.4f\n',i,S(i));
    fprintf('I(%.1f)= %.4f\n',i,I(i));
    fprintf('R(%.1f)= %.4f\n',i,R(i));
    
    %S=S(i);
    %I=I(i);
    %R=R(i);
    t=t+1;
    xx(i) = t;
end

plot(xx,S(1:t),xx,I(1:t),xx,R(1:t));
hold on
xlabel('Time')
ylabel('Susceptible/Infection/Recovery')
legend('Susceptible','Infected','Recovery')
