clc;
clear all;

a=input('Enter the value of Contact rate, a: ');
b=input('Enter the value of natural birth rate, b: ');
z=input('Enter the value of loss of immunity rate, z:');
g=input('Enter the value of recovery rate, g:');
p=input('Enter the value of vacinnation rate, p:');
u=input('Enter the value of natural death, u:');
d=input('Enter the value of typh oid death rate, d:');

s=input('Number of Susceptible, s:');
i=input('Number of Infected, i:');
r=input('Number of Recovered, r:');

fprintf('s(0)= %.6f\n',s);
fprintf('i(0)= %.6f\n',i);
fprintf('r(0)= %.6f\n',r);

m=0;
t=0;
for ty=1:10
   
   t=t+1;
   m=m+1;
   

    

   %i2=(a*s)*((a*s)-g-d-u);
   %i3=a*(b-(z*r)-(a*s*i)-(p*s)-(u*s));
   %i4=-(g+d+u)*((a*s)-g-d-u);
   

   %r2=(2*u)*((r*z)+(p*s)-(g*i))*(g*i)*((a*s)-d-g);
   %r3=r*((z*z)-(z*p)-(u^2));
   %r4=(p*s)*((a*i)+p+u)-((z*g*i)+(b*p));
    
    
   
   
   s(m)=(s+(b+(z*r)-(a*s*i)-(p*s)-(u*s))*t)+(((((g*i)+(p*s)-(u*r)-(z*r))*z)+(-(b+(z*r)-(a*s*i)-(p*s)-(u*s))*(u+p+(a*i)))+(-((a*s*i)*((a*s)-g-d-u))))*((t^2)/2));
   %i(m)=i1+(i2+i3)*((t^2)/2);
   i(m)=(i+(((a*s)-g-d-u)*i*t))+((((a*i)*(b+(z*r)-(a*s*i)-(p*s)-(u*s)))+(i*(((a*s)-g-d-u)^2)))*((t^2)/2));
   r(m)=(r+((g*i)+(p*s)-(u*r)-(z*r))*t)+((((g*i)*((a*s)-g-d-u))+(-(u+z)*((g*i)-((u+z)*r)+(p*s)))+(-p*(b-(z*r)-(((a*i)+p+u)*s))))*((t^2)/2));
   
   q=s(m);
   w=i(m);
   c=r(m);
   
   fprintf("\ns(%0.2f)= %.6f\n",t,q);
   fprintf("i(%0.2f)= %.6f\n",t,w);
   fprintf("r(%0.2f)= %.6f\n",t,c);
   
   
end
