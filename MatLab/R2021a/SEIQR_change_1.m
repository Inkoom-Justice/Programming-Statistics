clear all;
clc;
t=0;
for i=1:1500
    t=t+1;
s0 = 5/2 - (77184377*exp(-t/50000))/50000000;
e0 = (1139*exp(-(69*t)/800))/31250;
a0 = (30487*exp(-(261*t)/25000))/10000000;
q0 = (2439*exp(-(261*t)/100000))/2000000;
r0 = (29709*exp(-t/50000))/10000000;

s1 = ((213409*exp(-(521*t)/50000))/8336000 - (3877470101599*exp(-(261*t)/25000))/208800000000000 - (34466966167*exp(-(261*t)/100000))/1160000000000 + (2439*exp(-(259*t)/100000))/59200 - 74531826777249457/4025037600000000000)*exp(-t/50000);
e1 = (5691*exp(-(261*t)/100000))/4460800 - (310202695503*exp(-(263*t)/100000))/334480000000000 - (3877470101599*exp(-(523*t)/50000))/1515800000000000 + (30487*exp(-(261*t)/25000))/8664000 - (12525628603579148031427*exp(-(69*t)/800))/9567826705179600000000000;
a1 = -(4067369*exp(-(69*t)/800))/118453125 + (4067369*exp(-(261*t)/25000))/118453125;
q1 = -(9581*exp(-(69*t)/800))/1537500 - (30487*exp(-(261*t)/25000))/27187500 + (683016*exp(-(261*t)/100000))/92890625;
r1 = -(58089*exp(-(69*t)/800))/269468750 - (30487*exp(-(261*t)/25000))/416800000 - (217071*exp(-(261*t)/100000))/259000000 + (1048920814591*exp(-t/50000))/930863197600000;

s2 = (-(788104451*exp(-(8623*t)/100000))/19144353450 + (96739488693*exp(-(1303*t)/100000))/321509516800000 + (929457169*exp(-(1043*t)/50000))/9936512000000 + (2282033371*exp(-(521*t)/50000))/8181523500 - (118212430987448713*exp(-(261*t)/12500))/1743897600000000000000 - (1050794397533329*exp(-(261*t)/20000))/4844160000000000000 - (8599565304525637118382401593*exp(-(261*t)/25000))/42475223583475200000000000000 - (9340547831257*exp(-(261*t)/50000))/53824000000000000 - (686733235203517393411127*exp(-(261*t)/100000))/3828615765120000000000000 + (1366032*exp(-(259*t)/100000))/5499125 + (100234573611362027*exp(-(69*t)/800))/3351038906250000000 + (5948721*exp(-(13*t)/2500))/24627200000 - 2262088368016273773487172215757789699/16827542105219399980108800000000000000)*exp(-t/50000);

e2 = (-(788104451*t)/222015000000 - (1050794397533329*exp((3659*t)/50000))/27164416000000000000 + (1982907*exp((8103*t)/100000))/127919360000 - (686733235203517393411127*exp((4181*t)/50000))/122662394743040000000000000 + (32246496231*exp((183*t)/2500))/602059264000000 + (796852*exp((2091*t)/25000))/103591625 - (118212430987448713*exp((1307*t)/20000))/5458032000000000000000 + (6506200183*exp((6537*t)/100000))/217969728000000 - (8599565304525637118382401593*exp((7579*t)/100000))/308352221780803200000000000000 + (2282033371*exp((7581*t)/100000))/59524116750 - (84064930481313*exp((8101*t)/100000))/7517728000000000000 - (100234573611362027*exp(-t/50000))/777052500000000 + 213401164850965605807037227415712950661961001578680668417/1654519629381831089216118156358371197808000000000000000)*exp(-(69*t)/800);

a2 = ((4555810841*exp((783*t)/100000))/391500000000 - (6623617709*exp((781*t)/100000))/781000000000 + (1826948903*exp(-t/50000))/200000000 + (628284147*t)/2500000000000 + (164032997*exp(-(7581*t)/100000))/133000000000 - 743309131720965151/81332559000000000)*exp(-(261*t)/25000);

q2 = (364873117/20000000000000*t + 294030847/209100000000*exp(-2091/25000*t) - 17150651/900000000*exp(-783/100000*t) + 1828995331/392500000000*exp(-157/20000*t) + 132620741/200000000*exp(-1/50000*t) - 3201356962505651/4924305000000000)*exp(-261/100000*t);

r2 = (254301927*exp(-(69*t)/800))/1077875000000 - (8382915401*exp(-(261*t)/25000))/10420000000000 + (326149517*exp(-(523*t)/50000))/2610000000000 + (16423023*exp(-(263*t)/100000))/90625000000 - (137388199*exp(-(261*t)/100000))/25900000000 + (169065181831494161867*exp(-t/50000))/30369411821700000000000;

S(t)=s0+s1+s2;
E(t)=e0+e1+e2;
I(t)=a0+a1+a2;
Q(t)=q0+q1+q2;
R(t)=r0+r1+r2;

    fprintf('S(%.1f)= %.6f\n',t,S(t));
    fprintf('E(%.1f)= %.6f\n',t,E(t));
    fprintf('I(%.1f)= %.6f\n',t,I(t));
    fprintf('Q(%.1f)= %.6f\n',t,Q(t));
    fprintf('R(%.1f)= %.6f\n',t,R(t));

xx(i)=i;
end

plot(xx,S(1:i),xx,E(1:i),xx,I(1:i),xx,Q(1:i),xx,R(1:i));
hold on
xlabel('Time/days')
ylabel('SEIQR')
legend('Susceptible','Exposed','Infected','Quarantined','Recovered')


