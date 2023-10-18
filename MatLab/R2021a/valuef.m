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

s_not = 0.95631246;          %initial proportion of Susceptible
e_not = 3.6448*(10^(-02));   %initial proportion of Exposed
i_not = 3.0487*(10^(-03));   %initial proportion of Infected
q_not = 1.2195*(10^(-03));   %initial proportion of Quarantined
r_not = 2.9709*(10^(-03));

f_1 = u+k+d_2+v;
f_2 = u+d_2+g_1+phi;
f_3 = u+d_1+g_3;

s_1 = a-(B*s_not)*(i_not+q_not)-(u*s_not);
e_1 = (B*s_not)*(i_not+q_not)-(f_1*e_not);
i_1 = (k*e_not)-(f_2*i_not);
q_1 = (v*e_not)+(phi*i_not)-(f_3*q_not);
r_1 = (g_1*i_not)+(g_2*e_not)+(g_3*q_not)-(u*r_not);

s_2 = -(B*((s_not*(i_1+q_1))+(s_1*(i_not+q_not)))+(s_1*u));
e_2 = B*(s_not*(i_1+q_1)+(s_1*(i_not+q_not)))-(f_1*e_1);
i_2 = (k*e_1)-(f_2*i_1);
q_2 = (v*e_1)+(phi*i_1)-(f_3*q_1);
r_2 = (g_1*i_1)+(g_2*e_1)+(g_3*q_1)-(u*r_1);

t=0;
for i=1:10
    t=t+1;
   p(i)=(0.95631246*t - (0.00004292386043*t*t))/(1 - 0.00002244230973*t + (0.00007744489531*t*t))
   xx(i)=i;
end
plot(xx,p(1:i))