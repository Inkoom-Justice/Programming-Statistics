%% Initial parameters
A = 1.5;
a_0 = 0.012;
r_0 = 0.035;
B_0 = 0.04;
k_0 = 0.125;
b_0 = 0.0062;
r_1 = 0.412;
B_1 = 0.21;
k_1 = 0.025;
b_1 = 0.021;
g = 0.02;
B = 0.03;
u = 0.05;
a = 0.001;
b = 0.5;
c = 0.003;
d = 0.04;
k = 0.5;
Ba = 2;

s0 = 0.9;
x0 = 0.0001;
y0 = 0.0003;
z0 = 0.00021;
w0 = 0.002;

L0 = [s0; x0; y0; z0; w0];                % Initial conditions
T = 10;
tspan = [0 T];                        % Interval of Integration
[t,y] = ode45(@(t,y) sxyzw_model(t,y,A,a_0,r_0,B_0,k_0,b_0,r_1,B_1,k_1,b_1,g,B,u,a,b,c,d,k,Ba),tspan,L0);

plot(t,y,'LineWidth',2);
xlabel('Days'); 
ylabel('Number of individuals');
legend('S','X','Y', 'Z','W');
title('SXYZW Model Predictions');

function dydt = sxyzw_model(t,y,A,a_0,r_0,B_0,k_0,b_0,r_1,B_1,k_1,b_1,g,B,u,a,b,c,d,k,Ba)
  S = y(1);
  X = y(2);
  Y = y(3);
  Z = y(4);
  W = y(5);
  % Equations of the model described above
  dS = (A+(a*S)-(a_0*S)-(g*S*X)) ;
  dX = ((g*S*X)+(c*X)-(r_0+(r_1*W)+(B*Y))*X) ;
  dY = ((B*X*Y)+(d*Y)-(B_0+(B_1*W)+(u*Z))*Y);
  dZ = ((u*Y*Z)+(k*Z)-(k_0+(k_1*W))*Z);
  dW = (Ba+(b*W)-(b_0*W)-(b_1*Z*X)-(b_1*W*Y)-(b_1*W*Z));
  dydt = [dS;dX;dY;dZ;dW];
end


% Changing values of miu
[tt1,yy1] = ode45(@(tt1,yy1) xyzw_model(tt1,yy1,A,gamma,Beta(1),Beta_1,k_1,k(1),miu(2),B(1),b(1),b_1),tspan,L0);
[tt2,yy2] = ode45(@(tt2,yy2) xyzw_model(tt2,yy2,A,gamma,Beta(1),Beta_1,k_1,k(1),miu(3),B(1),b(1),b_1),tspan,L0);
[tt3,yy3] = ode45(@(tt3,yy3) xyzw_model(tt3,yy3,A,gamma,Beta(1),Beta_1,k_1,k(1),miu(4),B(1),b(1),b_1),tspan,L0);

% Changing values of beta
[ttt1,yyy1] = ode45(@(ttt1,yyy1) xyzw_model(ttt1,yyy1,A,gamma,Beta(2),Beta_1,k_1,k(1),miu(1),B(1),b(1),b_1),tspan,L0);
[ttt2,yyy2] = ode45(@(ttt2,yyy2) xyzw_model(tyt2,yyy2,A,gamma,Beta(3),Beta_1,k_1,k(1),miu(1),B(1),b(1),b_1),tspan,L0);
[ttt3,yyy3] = ode45(@(ttt3,yyy3) xyzw_model(ttt3,yyy3,A,gamma,Beta(4),Beta_1,k_1,k(1),miu(1),B(1),b(1),b_1),tspan,L0);

% Changing values of B
[tttt,yyyy] = ode45(@(tttt,yyyy) xyzw_model(tttt,yyyy,A,gamma,Beta(1),Beta_1,k_1,k(1),miu(1),B(1),b(1),b_1),tspan,L0);
[tttt1,yyyy1] = ode45(@(tttt1,yyyy1) xyzw_model(tttt1,yyyy1,A,gamma,Beta(1),Beta_1,k_1,k(1),miu(1),B(2),b(1),b_1),tspan,L0);
[tttt2,yyyy2] = ode45(@(tttt2,yyyy2) xyzw_model(tttt2,yyyy2,A,gamma,Beta(1),Beta_1,k_1,k(1),miu(1),B(3),b(1),b_1),tspan,L0);
[tttt3,yyyy3] = ode45(@(tttt3,yyyy3) xyzw_model(tttt3,yyyy3,A,gamma,Beta(1),Beta_1,k_1,k(1),miu(1),B(4),b(1),b_1),tspan,L0);

% Changing values of b
[ttttt,yyyyy] = ode45(@(ttttt,yyyyy) xyzw_model(ttttt,yyyyy,A,gamma,Beta(1),Beta_1,k_1,k(1),miu(1),B(1),b(1),b_1),tspan,L0);
[ttttt1,yyyyy1] = ode45(@(ttttt1,yyyyy1) xyzw_model(ttttt1,yyyyy1,A,gamma,Beta(1),Beta_1,k_1,k(1),miu(1),B(2),b(1),b_1),tspan,L0);
[ttttt2,yyyyy2] = ode45(@(ttttt2,yyyyy2) xyzw_model(ttttt2,yyyyy2,A,gamma,Beta(1),Beta_1,k_1,k(1),miu(1),B(3),b(1),b_1),tspan,L0);
[ttttt3,yyyyy3] = ode45(@(ttttt3,yyyyy3) xyzw_model(ttttt3,yyyyy3,A,gamma,Beta(1),Beta_1,k_1,k(1),miu(1),B(4),b(1),b_1),tspan,L0);