clear all;
clc;

%% Initial parameters
a = 0.00007;     %rate at which a baby is born into a susceptible class
B = 0.02500;     %exrate at which a person is exposed to the Covid-19 infection
k = 0.07142;     %rate at which an exposed persone becomes infected with Covid-19 virus
d_1 = 0.0008;    %rate at which quarantined person dies from his or her Covid-19 infection
d_2 = 0.00729;   %rate at which an infected person dies from his or her Covid-19 infection
v = 0.01430;     %rate at which an exposed person is quarantined
g_1 = 0.00025;   %rate at which an infected person recovers from his or her Covid-19 infection
g_2 = 0.00051;   %rate at which an exposed person recovers from his or her Covid-19 infection
g_3 = 0.00178;   %rate at which a quarantined person recovers from his or her Covid-19 infection
phi = 0.00288;   %rate at which an infected person is quarantined
u = 0.00002;

s0 = 0.95631246;          %initial proportion of Susceptible
e0 = 3.6448*(10^(-02));   %initial proportion of Exposed
i0 = 3.0487*(10^(-03));   %initial proportion of Infected
q0 = 1.2195*(10^(-03));   %initial proportion of Quarantined
r0 = 2.9709*(10^(-03));

f_1 = u+k+d_2+v;
f_2 = u+d_2+g_1+phi;
f_3 = u+d_1+g_3;




n=1500;
t=0;
for i=1:n
    t=t+1;
   
    
    xx(i)=i;
    
end

plot(xx,s(1:i),'r',xx,e(1:i),'b',xx,I(1:i),'g',xx,q(1:i),'y',xx,r(1:i),'v');
hold on
xlabel('Time/days')
ylabel('SEIQR')
legend('Susceptible','Exposed','Infected','Quarantined','Recovered')