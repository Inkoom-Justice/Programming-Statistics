my_data <- read.csv("TabbysStarFlux.csv")
x<-my_data$Uncertainty
x
summary(x)
my_data<-

  
  lambda=a
beta=b
alpha=
  

library(moments)
library(zipfR)
install.packages("goftest")
library(goftest)

# Basic data 
x<-c(2,5,10,25,30,35,45,50,75,100, 150, 200, 250, 300)
hist(r)

hist(x,probability =T ,main="",xlab="Survival time(days)")

z<- 1.23+((3.6/7)*(((-log(1-(r^-1)))^-2.3)-1)) 
z
hist(r, probability = T)
# plots the histrogram 
hist(x)



install.packages("psych")
library(psych)

install.packages("goodness.fit")
install.packages("goftest")
library(goftest)

# basic statistics 
cv(x)
skewness(x)
kurtosis(x)
summary(x)
sd(x)
describe(x)


plot(r , main="lung cancer ",xlab="Time (Days) ", ylab="frequency", pch=19)

# plot
plot(y,x , main="Maximum Annual Rainfall in Koforidua  ",
     xlab="Time (Years) ", ylab="Annual Maximum Rainfall in cm ", pch=19)

##################


########################## ER-Kum CDF/PDF/LL ###############################

ECDF<-function(x,a,b){
  A<-Ibeta(x^a, 1/a, b+1, lower=TRUE, log=!missing(base), base=exp(1))
  D<-b*beta(1+1/a,b)
  CDF<-A/D
  return(CDF)
}

EPDF<-function(x,a,b){
  A<-(1-x^a)^b
  D<-b*beta(1+1/a,b)
  PDF<-A/D
  return(PDF)
}

Ekum_LL<-function(a,b){
  A<-(1-x^a)^b
  D<-b*beta(1+1/a,b)
  EK<-A/D
  LL<--sum(log(EK))
  return(LL)
}


Ibeta(x, a, b, lower=TRUE, log=!missing(base), base=exp(1))

install.packages("fitdistrplus")
library("fitdistrplus")
install.packages("fitdist")
library(fitdist)

#fit_w <-fitdist(x,"CTLW_WPDF", start =NULL)
#start=NULL
#fit_w <-fitdist(x,"CTLW_WPDF", start =list(alpha=1,beta=1,lambda=1))
#summary(fit_w)
#plot(fit_w)
#profile(fit)

###################### CosTopp-Leone-frechet PDF ###############################


CTLF_FCDF<-function(x,alpha,beta,lambda){
  D<-exp((-1*lambda)*(x^(-1*beta)))
  FCDF<-(1-cos(((pi/2)*((1-(1-D)^2)^(alpha)))))
  return(FCDF)
}

CTLF_FPDF<-function(x,alpha,beta,lambda){
  D<-exp((-1*lambda)*(x^(-1*beta)))
  FPDF<-((pi*alpha*beta*lambda)*(x^(-beta-1))*(1-D)*((1-(1-D)^2)^(alpha-1))*sin(((pi/2)*((1-(1-D)^2)^(alpha)))))
  return(FPDF)
}

CTLF_LL<-function(x,alpha,beta,lambda){
  D<-exp((-1*lambda)*(x^(-1*beta)))
  FPDF<-(1-cos(((pi/2)*((1-(1-D)^2)^(alpha)))))
  LL<--sum(log(FPDF))
  return(LL)
}



###################### CosTopp-Leone-chen PDF ###############################

CTLC_CCDF<-function(x,alpha,beta,lambda){
  A<-exp(2*beta*(1-(exp(x^lambda))))
  CCDF<-(1-cos(((pi/2)*((1-A)^(alpha)))))
  return(CCDF)
}


CTLC_CPDF<-function(x,alpha,beta,lambda){
  A<-exp(2*beta*(1-(exp(x^lambda))))
  B<-exp(x^lambda)
  CPDF<-((pi*alpha*beta*lambda)*(x^(lambda-1))*A*B*(1-A)^(alpha-1)*sin(((pi/2)*((1-A)^(alpha)))))
  return(CPDF)
}


CTLC_LL<-function(x,alpha,beta,lambda){
  A<-exp(2*beta*(1-(exp(x^lambda))))
  B<-exp(x^lambda)
  CPDF<-((pi*alpha*beta*lambda)*(x^(lambda-1))*A*B*(1-A)^(alpha-1)*sin(((pi/2)*((1-A)^(alpha)))))
  LL<--sum(log(CPDF))
  return(LL)
}



###################### CosTopp-Leone-cauchy PDF ###############################


CTLCa_CaCDF<-function(x,alpha,mu,lambda){
  C<-(1-(1-((1/pi)*atan((x-mu)/lambda)+0.5))^2)
  CaCDF<-(1-cos((pi/2)*(C^alpha)))
  return(CaCDF)
}



CTLCa_CaPDF<-function(x,alpha,mu,lambda){
  C<-(1-(1-((1/pi)*atan((x-mu)/lambda)+0.5))^2)
  D<-(((1-((1/pi)*atan((x-mu)/lambda)+0.5))))
  E<-((pi*alpha*lambda))/(pi*((lambda)^2)+(x-mu)^2)
  CaPDF<-(E*D*(C^(alpha-1))*sin((pi/2)*(C^alpha)))
  return(CaPDF)
}


CTLCa_LL<-function(x,alpha,mu,lambda){
  C<-(1-(1-((1/pi)*atan((x-mu)/lambda)+0.5))^2)
  D<-(((1-((1/pi)*atan((x-mu)/lambda)+0.5))))
  E<-((pi*alpha*lambda))/(pi*((lambda)^2)+(x-mu)^2)
  CaPDF<-(E*D*(C^(alpha-1))*sin((pi/2)*(C^alpha)))
  LL<--sum(log(CaPDF))
  return(LL)
}



########################## CosTopp-Leone-Burr XII CDF/PDF/LL ###############################


CTLB_BCDF<-function(x,alpha,beta,lambda,c){
  L<-(1-((1+(x/lambda)^c))^(-2*beta))
  BCDF<-(1-(cos((pi/2)*(L^(alpha)))))
  return(BCDF)
}


CTLB_BPDF<-function(x,alpha,beta,lambda,c){
  H<-((pi*alpha*beta*c)/(lambda))
  I<-((x/alpha)^(c-1))
  J<-(1+((x/lambda)^c))
  K<-(1-((1+(x/lambda)^c))^(-beta))
  L<-(1-((1+(x/lambda)^c))^(-2*beta))
  BPDF<-(H*I*(J^(-beta-1))*K*(L^(alpha-1))*(sin((pi/2)*(L^(alpha)))))
  return(BPDF)
}


CTLB_LL<-function(x,alpha,beta,lambda,c){
  H<-((pi*alpha*beta*c)/(lambda))
  I<-((x/alpha)^(c-1))
  J<-(1+((x/lambda)^c))
  K<-(1-((1+(x/lambda)^c))^(-beta))
  L<-(1-((1+(x/lambda)^c))^(-2*beta))
  BPDF<-(H*I*(J^(-beta-1))*K*(L^(alpha-1))*(sin((pi/2)*(L^(alpha)))))
  LL<--sum(log(BPDF))
  return(LL)
}


fit_k <-fitdist(x,"MEC_PDF", start =NULL)
start=NULL
fit_w <-fitdist(x,"MEC_PDF", start =list(a=1,b=1,rho=1))
summary(fit_w)
plot(fit_w)
profile(fit)


####################### MEC PDF ########################################
dMEC_PDF<-function(x,a,b,rho){
  A<-a*b*rho*x^(-b-1)*exp(x^(-b))
  B<-(rho*(exp(x^(-b))-1)+1)^(-a-1)
  PDF<-A*B
  return(PDF)
}

pMEC_PDF<-function(q,a,b,rho){
  B<-(rho*(exp(q^(-b))-1)+1)^(-a)
  PDF<-B
  return(PDF)
}

qMEC_PDF<-function(p,a,b,rho){
  B<-(log((p^(1/a)-1)/rho)+1)^(-1/a)
  PDF<-B
  return(PDF)
}


fit_w <-fitdist(x,"MEC_PDF", start =NULL)
start=NULL
fit_w <-fitdist(x,"MEC_PDF", start =list(a=1,b=1,rho=1))
summary(fit_w)
plot(fit_w)
profile(fit)
################################### 1 MEC-Distribution CDF/PDF/LL #########################

MEC_CDF<-function(x,a,b,rho){
  B<-(rho*(exp(x^(-b))-1)+1)^(-a)
  CDF<-B
  return(CDF)
}


MEC_PDF<-function(x,a,b,rho){
  A<-a*b*rho*x^(-b-1)*exp(x^(-b))
  B<-(rho*(exp(x^(-b))-1)+1)^(-a-1)
  PDF<-A*B
  return(PDF)
}


MEC_LL<-function(a,b,rho){
  A<-a*b*rho*x^(-b-1)*exp(x^(-b))
  B<-(rho*(exp(x^(-b))-1)+1)^(-a-1)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}

######################## 2 GUMBEL DISTRIBUTION CDF/PDF/LL #####################
GUM_CDF<-function(x,a,b){
  A<-exp(-exp(-(x-a)/b))
  CDF<-A
  return(CDF)
}


GUM_PDF<-function(x,a,b){
  A<-1/b*exp(-(x-a)/b-exp(-(x-a)/b))
  PDF<-A
  return(PDF)
}


GUM_LL<-function(a,b){
  A<-1/b*exp(-(x-a)/b-exp(-(x-a)/b))
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}


#################### 3 BURR 3-P DISTRIBUTION CDF/PDF/LL ##########################
BURR3P_CDF<-function(x,a,b,c){
  B<-1-(1+(x/c)^a)^(-b)
  CDF<-B
  return(CDF)
}


BURR3P_PDF<-function(x,a,b,c){
  A<-(a*b)/c*(x/c)^(a-1)
  B<-(1+(x/c)^a)^(-b-1)
  PDF<-A*B
  return(PDF)
}


BURR3P_LL<-function(a,b,c){
  A<-(a*b)/c*(x/c)^(a-1)
  B<-(1+(x/c)^a)^(-b-1)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}

##################### 4 CAUCHY DISTRIBUTION CDF/PDF/ LL ##########################
CAUCHY_CDF<-function(x,a,b){
  A<-1/2+1/pi*atan((x-b)/a)
  CDF<-A
  return(CDF)
}


CAUCHY_PDF<-function(x,a,b){
  A<-(pi*a*(1+((x-b)/a)^2))^(-1)
  PDF<-A
  return(PDF)
}

CAUCHY_LL<-function(a,b){
  A<-(pi*a*(1+((x-b)/a)^2))^(-1)
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}


################################ 5 LOG-LOGISTIC DISTRIBUTION CDF/PDF/LL ###############################


LL_CDF<-function(x,a,b,c){
  B<-(1+((x-c)/b)^(-a))^(-1)
  CDF<-B
  return(CDF)
}


LL_PDF<-function(x,a,b,c){
  A<- a/b*((x-c)/b)^(a-1)
  B<-(1+((x-c)/b)^(a))^(-2)
  PDF<-A*B
  return(PDF)
}


LL_LL<-function(a,b,c){
  A<- a/b*((x-c)/b)^(a-1)
  B<-(1+((x-c)/b)^(a))^(-2)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}

################## 6 INVERSE WEIBULL DISTRIBUTION CDF/PDF/LL ####################
INW_CDF<-function(x,a,b,n){
  A<-exp((-1/n)*(1/(x-a))^b)
  CDF<-A
  return(CDF)
}


INW_PDF<-function(x,a,b,n){
  A<- b/n*(1/(x-a))^(b+1)
  B<-exp((-1/n)*(1/(x-a))^b)
  PDF<-A*B
  return(PDF)
}


INW_LL<-function(a,b,n){
  A<- b/n*(1/(x-a))^(b+1)
  B<-exp((-1/n)*(1/(x-a))^b)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}


################ 7 Exponentiated inverse flexible Weibull extension distribution CDF/PDF/LL #############

EINFW_CDF<-function(x,a,b,c){
  A<- exp(-c*exp(a/x-b*x))
  CDF<-A
  return(CDF)
}



EINFW_PDF<-function(x,a,b,c){
  A<- c*(b+a/x^2)*exp(a/x-b*x)*exp(-c*exp(a/x-b*x))
  PDF<-A
  return(PDF)
}


EINFW_LL<-function(a,b,c){
  A<- c*(b+a/x^2)*exp(a/x-b*x)*exp(-c*exp(a/x-b*x))
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}


############################### 8 NAKAGAMI DISTRIBUTION CDF/PDF/LL ###############################
NAK_CDF<-function(x,a,m){
  t<-((m*x^2)/a)
  A<-Igamma(m,t,lower=TRUE)
  B<-1/gamma(m)
  CDF<-A*B
  return(CDF)
}

NAK_PDF<-function(x,a,m){
  A<-(2*m^m)/(gamma(m)*a^m)
  B<-x^(2*m-1)*exp(-m/a*x^2)
  PDF<-A*B
  return(PDF)
}

NAK_LL<-function(a,m){
  A<-(2*m^m)/(gamma(m)*a^m)
  B<-x^(2*m-1)*exp(-m/a*x^2)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}

#################### 9 EXTENDED BURR XII  DISTRIBUTION CDF/PDF/LL  ##################################
EB_CDF<-function(x,a,c,k){
  A<-1-(1-k*(x/a)^(c))^(1/k)
  CDF<-A
  return(CDF)
}


EB_PDF<-function(x,a,c,k){
  A<-c*a^(-1)*(x/a)^(c-1)*(1-k*(x/a)^(c))^(1/k-1)
  PDF<-A
  return(PDF)
}

EB_LL<-function(a,c,k){
  A<-c*a^(-1)*(x/a)^(c-1)*(1-k*(x/a)^(c))^(1/k-1)
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}


############################## 10 BETA GUMBEL DISTRIBUTION CDF/PDF/LL  ##################################
BG_PDF<-function(x,a,b,d,e){
  y<-exp(-(x-e)/d)
  A<-1/(d*beta(a,b))*y*exp(-a*y)*(1-exp(-y))^(b-1)
  PDF<-A
  return(PDF)
}

BG_LL<-function(a,b,d,e){
  u<-exp(-(x-e)/d)
  A<-1/(d*beta(a,b))*u*exp(-a*u)*(1-exp(-u))^(b-1)
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}


############################### 11 LPT3 DISTRIBUTION ###############################

LPT3_PDF<-function(x,a,b,c){
  A<-a*b*c*x^-(b+1)*exp(-a*x^(-b))
  B<-(c-(c-1)*exp(-a*x^(-b)))^2
  PDF<-A/B
  return(PDF)
}

LPT3_LL<-function(a,b,c){
  A<-a*b*c*x^-(b+1)*exp(-a*x^(-b))
  B<-(c-(c-1)*exp(-a*x^(-b)))^2
  PDF<-A/B
  LL<--sum(log(PDF))
  return(LL)
}


################# 12 EXP WEIBULL DISTRIBUTION CDF/PDF/LL ##########################

EW_CDF<-function(x,a,b,n){
  A<-(1-exp(-x/n)^b)^(a)
  CDF<-A
  return(CDF)
}

EW_PDF<-function(x,a,b,n){
  A<-((a*b)/n)*(x/n)^(b-1)*exp(-(x/n)^b)*(1-exp(-x/n)^b)^(a-1)
  PDF<-A
  return(PDF)
}

EW_LL<-function(a,b,n){
  A<-((a*b)/n)*(x/n)^(b-1)*exp(-(x/n)^b)*(1-exp(-x/n)^b)^(a-1)
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}

################# 12 GIW DISTRIBUTION CDF/PDF/LL ##########################

GIW_CDF<-function(x,a,b,c){
  A<-exp(-c*(a/x)^b)
  CDF<-A
  return(CDF)
}



GIW_PDF<-function(x,a,b,c){
  A<-b*c*a^(b)*x^(-b-1)*exp(-c*(a/x)^b)
  PDF<-A
  return(PDF)
}

GIW_LL<-function(a,b,c){
  A<-b*c*a^(b)*x^(-b-1)*exp(-c*(a/x)^b)
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}

################# 12 LINDLEY DISTRIBUTION CDF/PDF/LL ##########################

LI_PDF<-function(x,a,b){
  A<-(b^2/b+1)*(1+x)*exp(-b*x)
  B<-(1-a+2*a)*((b+1+b*x)/(b+1))*exp(-b*x)
  PDF<-A*B
  return(PDF)
}

LI_LL<-function(a,b){
  A<-(b^2/b+1)*(1+x)*exp(-b*x)
  B<-(1-a+2*a)*((b+1+b*x)/(b+1))*exp(-b*x)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}



################# 12 LOGISTIC DISTRIBUTION CDF/PDF/LL ##########################

LOG_PDF<-function(x,a,b){
  A<-exp(-(x-a)/b)
  B<-b*(1-exp(-(x-a)/b))^2
  PDF<-A/B
  return(PDF)
}
LOG_LL<-function(a,b){
  A<-exp(-(x-a)/b)
  B<-b*(1-exp(-(x-a)/b))^2
  PDF<-A/B
  LL<--sum(log(PDF))
  return(LL)
}


##### Exponential distribution 

EXP_PDF<-function(x,a,b){
  A<-a*exp(-a*(x-b))
  PDF<-A
  return(PDF)
}
EXP_LL<-function(a,b){
  A<-a*exp(-a*(x-b))
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}

########## Exponential Relayh ####
ERL_PDF<-function(x,a,b,m){
  A<-((x-a)^(m-1))/b^m*gamma(m)
  B<-exp(-(x-a)/b)
  PDF<-A*B
  return(PDF)
}
ERL_LL<-function(a,b,m){
  A<-((x-a)^(m-1))/b^m*gamma(m)
  B<-exp(-(x-a)/b)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}





fit90<-mle2(ERL_LL,start=list(a=0.1,b=0.1,m=1),data=list(x),method="BFGS")
summary(fit90)
AIC(fit90)


library(hypergeo)
############################# BETA LINDLEY DISTRIBUTION ########################
BL_CDF<-function(x,a,b,c){
  A<-((1-((a+1+a*x)/(a+1))*exp(-a*x))^c)/c*beta(c,b)
  B<-hypergeo(c,1-b,c+1,1-(((a+1+a*x)/(a+1))*exp(-a*x)))
  CDF<-1-A*B
  return(CDF)
}


BL_PDF<-function(x,a,b,c){
  A<-(a^2*(a+1+a*x)^(b-1)*(1+x)*exp(-a*b*x))/(beta(c,b)*(a+1)^b)
  B<-(1-(a+1+a*x)/(a+1)*exp(-a*x))^(c-1)
  PDF<-A*B
  return(PDF)
}

BL_LL<-function(a,b,c){
  A<-(a^2*(a+1+a*x)^(b-1)*(1+x)*exp(-a*b*x))/(beta(c,b)*(a+1)^b)
  B<-(1-(a+1+a*x)/(a+1)*exp(-a*x))^(c-1)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}










############################ CODES FOR FITTING DATA #####################

library (bbmle)
install.packages(bbmle)
#1.1110e+03 2.5058e+00 1.1047e+03 GHN_LL
fitz<-mle2(CTLW_LL,start=list(alpha=7, beta=0.2, lambda=1),data=list(x),method="BFGS")
#profile(fit)
summary(fitz)
AIC1<-AIC(fitz)
BIC2<-BIC(fitz)
CAIC=AIC1+((2*3*(3+1))/(418-3-1))
HQIC=2*log(log(137)*(3+2*CTLW_LL(1.069772,0.608439,0.044424)))
logLik(fitz)
AIC1
BIC2
CAIC
HQIC
install.packages("goftest")
library(goftest)
install.packages("EnvStats")
library(EnvStats)
cvm.test(x,"CTLW_WCDF",alpha=1.069772,beta=0.608439,lambda=0.044424)
ad.test(x,"CTLW_WCDF",alpha=1.069772,beta=0.608439,lambda=0.044424)
ks.test(x,"CTLW_WCDF",alpha=1.069772,beta=0.608439,lambda=0.044424)








library (bbmle)  #1.1110e+03 2.5058e+00 1.1047e+03 GHN_LL
fit<-mle2(Ekum_LL,start=list(a=0.5, b=0.09),data=list(x),method="SANN")
#profile(fit)
summary(fit)
AIC1<-AIC(fit)
BIC2<-BIC(fit)
CAIC=AIC1+((2*3*(3+1))/(100-3-1))
HQIC=2*log(log(100)*(3+2*MEC_LL(4.2719e+03,5.7431e+00,7.9190e+03)))
logLik(fit)
AIC1
BIC2
CAIC
HQIC
library(EnvStats)
cvm.test(x,"MEC_CDF",a=4.2719e+03,b=5.7431e+00,rho=7.9190e+03)
ad.test(x,"MEC_CDF",a=4.2719e+03,b=5.7431e+00,rho=7.9190e+03)
ks.test(x,"MEC_CDF",a=4.2719e+03,b=5.7431e+00,rho=7.9190e+03)




#Bolga: a=99,b=0.1,c=0.01  Ho:a=9,b=0.1,c=0.01  

fit0<-mle2(GIW_LL,start=list(a=0.1,b=7,c=500),data=list(x),method="BFGS")
summary(fit0)
AIC(fit0)
AIC1<-AIC(fit0)
BIC2<-BIC(fit0)
CAIC=AIC1+((2*3*(3+1))/(100-3-1))
HQIC=2*log(log(100)*(3+2*GIW_LL(6.933899,5.743041,500.508617)))
logLik(fit0)
AIC1
BIC2
CAIC
HQIC
cvm.test(x,"GIW_CDF",a=6.933899 ,b=5.743041,c=500.508617)
ad.test(x,"GIW_CDF",a=6.933899 ,b=5.743041,c=500.508617)
ks.test(x,"GIW_CDF",a=6.933899 ,b=5.743041,c=500.508617)



fit2<-mle2(GUM_LL,start=list(a=6,b=2.6),data=list(x),method="BFGS")
summary(fit2)
AIC(fit2)
AIC2<-AIC(fit2)
logLik(fit2)
BIC2<-BIC(fit2)
CAIC2=AIC6+((2*3*(3+1))/(100-3-1))
HQIC2=2*log(log(100)*(3+2*GUM_LL(22.07684,5.34725)))
AIC2
BIC2
CAIC2
HQIC2
cvm.test(x,"GUM_CDF",a=22.07684 ,b=5.34725)
ad.test(x,"GUM_CDF",a=22.07684 ,b=5.34725)
ks.test(x,"GUM_CDF",a=22.07684 ,b=5.34725)


fit10<-mle2(BURR3P_LL,start=list(a=0.2,b=1,c=1),data=list(x),method="BFGS")
summary(fit10)
AIC(fit10)
AIC10<-AIC(fit10) 
logLik(fit10)
BIC10<-BIC(fit10)
CAIC10=AIC10+((2*3*(3+1))/(100-3-1))
HQIC10=2*log(log(100)*(3+2*BURR3P_LL(15.70295 ,0.31387,18.89884)))
profile(fit)
BIC10
CAIC10
HQIC10
cvm.test(x,"BURR3P_CDF",a=15.70295 ,b=0.31387, c=18.89884)
ad.test(x,"BURR3P_CDF",a=15.70295 ,b=0.31387, c=18.89884)
ks.test(x,"BURR3P_CDF",a=15.70295 ,b=0.31387, c=18.89884)


fit11<-mle2(CAUCHY_LL,start=list(a=1,b=1),data=list(x),method="BFGS")
summary(fit11)
AIC(fit11)
AIC11<-AIC(fit11) #1 and 2
logLik(fit11)
BIC11<-BIC(fit11)
CAIC11=AIC11+((2*3*(3+1))/(100-3-1))
HQIC11=2*log(log(100)*(3+2*CAUCHY_LL(2.79732 ,21.07031)))
profile(fit)
BIC11
CAIC11
HQIC11
cvm.test(x,"CAUCHY_CDF",a=2.79732 ,b=21.07031)
ad.test(x,"CAUCHY_CDF",a=2.79732 ,b=21.07031)
ks.test(x,"CAUCHY_CDF",a=2.79732 ,b=21.07031)






fit19<-mle2(LL_LL,start=list(a=0.1,b=0.1, c=0.1),data=list(x),method="BFGS")
summary(fit19)
AIC(fit19)
AIC19<-AIC(fit19) #1 and 2
logLik(fit19)
BIC19<-BIC(fit19)
CAIC19=AIC19+((2*3*(3+1))/(100-3-1))
HQIC19=2*log(log(100)*(3+2*LL_LL(3.3763,8.8843,13.0823)))
profile(fit)
BIC19
CAIC19
HQIC19
cvm.test(x,"LL_CDF",a=3.3763 ,b=8.8843,c=13.0823)
ad.test(x,"LL_CDF",a=3.3763 ,b=8.8843, c=13.0823)
ks.test(x,"LL_CDF",a=3.3763 ,b=8.8843, c=13.0823)


fit20<-mle2(INW_LL,start=list(a=2.00,b=1.22,n=3.00),data=list(x),method="BFGS")
summary(fit20)
AIC(fit20)
AIC20<-AIC(fit20) #1 and 2
logLik(fit20)
BIC20<-BIC(fit20)
CAIC20=AIC20+((2*3*(3+1))/(100-3-1))
HQIC20=2*log(log(100)*(3+2*INW_LL(2.00 ,1.22,3.00)))
profile(fit)
BIC20
CAIC20
HQIC20

INW_CDF<-function(x,a,b,c){
  A<-exp((-1/c)*(1/(x-a))^b)
  CDF<-A
  return(CDF)
}

cvm.test(x,"INW_CDF", a=11.4854790, b=2.3484959,c=0.0063314)
ad.test(x,"INW_CDF",a=11.4854790,b=2.3484959,c=0.0063314)
ks.test(x,"INW_CDF",a=11.4854790,b=2.3484959,c=0.0063314)

fit22<-mle2(EINFW_LL,start=list(a=10,b=10.22,c=1),data=list(x),method="BFGS")
summary(fit22)
AIC(fit22)



fit21<-mle2(INFW_LL,start=list(a=0.1,b=0.22),data=list(x),method="BFGS")
summary(fit21)
AIC(fit21)

fit23<-mle2(IE_LL,start=list(a=34.387 ,b=24.622),data=list(x),method="BFGS")
summary(fit23)
AIC(fit23)


fit25<-mle2(NAK_LL,start=list(a=2 ,m=0.1),data=list(x),method="BFGS")
summary(fit25)
AIC(fit25)
AIC25<-AIC(fit25) #1 and 2
logLik(fit25)
BIC25<-BIC(fit25)
CAIC25=AIC25+((2*3*(3+1))/(100-3-1))
HQIC25=2*log(log(100)*(3+2*NAK_LL(2,0.1)))
profile(fit)
BIC25
CAIC25
HQIC25
cvm.test(x,"NAK_CDF",a=525.3651 ,m=6.3686)
ad.test(x,"NAK_CDF",a=525.3651 ,m=6.3686)
ks.test(x,"NAK_CDF",a=525.3651 ,m=6.3686)


fit26<-mle2(LPT3_LL,start=list(a=1 ,b= 0.01,c=1),data=list(x),method="BFGS")
summary(fit26)
AIC(fit26)


fit29<-mle2(BL_LL,start=list(a=6,b=2.4,c=1.4),data=list(x),method="BFGS")
summary(fit29)
AIC29<-AIC(fit29)
AIC29
BIC(fit29)
logLik(fit29)
CAIC29=AIC29+((2*3*(3+1))/(100-3-1))
CAIC29
HQIC29=2*log(log(100)*(3+2*BL_LL(6,2.4,1.4)))
HQIC29

fit34<-mle2(EB_LL,start=list(a=0.6,c=0.2,k=0.1),data=list(x),method="BFGS")
summary(fit34)
AIC(fit34)
AIC34<-AIC(fit34)
BIC(fit34)
logLik(fit34)
CAIC34=AIC34+((2*3*(3+1))/(100-3-1))
CAIC34
HQIC34=2*log(log(100)*(3+2*EB_LL(20.3492,15.6814,-3.1802)))
HQIC34
cvm.test(x,"EB_CDF",a=20.3492,c=15.6814,k=-3.1802)
ad.test(x,"EB_CDF",a=20.3492,c=15.6814,k=-3.1802)
ks.test(x,"EB_CDF",a=20.3492,c=15.6814,k=-3.1802)


fit36<-mle2(BG_LL,start=list(a=1,b=2,d=1,e=20),data=list(x),method="BFGS")
summary(fit36)
AIC(fit36)
AIC36<-AIC(fit36)
BIC(fit36)
logLik(fit36)
CAIC36=AIC36+((2*3*(3+1))/(100-3-1))
CAIC36
HQIC36=2*log(log(100)*(3+2*BG_LL(1,2,1,20)))
HQIC36

fit39<-mle2(LPT3_LL,start=list(a=1,b=0.05,c=0.0365),data=list(x),method="BFGS")
summary(fit39)
AIC(fit39)
AIC(fit39)
AIC39<-AIC(fit39)
BIC(fit39)
logLik(fit39)
CAIC39=AIC39+((2*3*(3+1))/(100-3-1))
CAIC39
HQIC39=2*log(log(100)*(3+2*LPT3_LL(1,0.05,0.0365)))
HQIC39



#############################  PDF CURVES ####################################
hist(x,probability =T ,main="",xlab="Survival time(days)",breaks = 3)
curve( EPDF(x,0.42131,24.98241 ),col="RED",lwd=2,add=TRUE,lty=1)
curve(GIW_PDF(x,6.933899 ,5.743041,500.508617  ),col="blue",lwd=2,add=TRUE,lty=3)
curve(NAK_PDF(x,525.3651,6.3686),col="cyan",lwd=2,add=TRUE,lty=3)
curve(LL_PDF(x,3.3763,8.8843,13.0823),col="YELLOW",lwd=2,add=TRUE,lty=3)
curve(EB_PDF(x,20.3492,15.6814 ,-3.1802 ),col="PURPLE",lwd=2,add=TRUE,lty=3)
curve(BURR3P_PDF(x,15.70295  , 0.31387 , 18.89884 ),col="cornsilk4",lwd=2,add=TRUE,lty=3)
curve(INW_PDF(x,11.4854790,2.3484959,0.0063314),col="GREEN",lwd=2,add=TRUE,lty=3)
curve(GUM_PDF(x,21.71462  ,3.63778  ),col="PINK",lwd=2,add=TRUE,lty=3)
curve(CAUCHY_PDF(x,2.79732,21.07031),col="ORANGE",lwd=2,add=TRUE,lty=3)
legend("topright",inset=c(0.001,0.1),cex=0.8,legend=c("Histogram",
                                                      "MEC","GIW","NAK", "LL","EB XII", "BURR(3P)","IW",
                                                      "GUM","CAUCHY"),lty=c(1,1,3,3,3,3,3,3,3,3),lwd=1,
       col=c("black","red","blue","cyan","yellow","purple","cornsilk4","green","pink"
             ,"orange"))





#legend("topright",inset=c(0.001,0.1),cex=0.5,legend=c("Histogram",
#        "MEC","MOW","EW","APTA Weibull"),lty=1,lwd=3,
#     col=c("black","red","blue","green","grey"))

#curve(KAPPA_PDF(x,942.971465,233.487452 ,0.485218),col="GREEN",lwd=2,add=TRUE)
#curve(LPT3_PDF(x,591.97287 ,4.14444  , 582.47150 ),col="GREEN",lwd=2,add=TRUE)



############################# CDF CURVES ######################################

plot(ecdf(x),main=" ",ylab="F(x)", xlab="Rainfall (cm)")
curve(MEC_CDF(x,4.8775e+03   , 5.744206 ,6.9030e+03 ),col="RED",lwd=2,add=TRUE,lty=1)
curve(GIW_CDF(x,6.933899 ,5.743041,500.508617  ),col="blue",lwd=2,add=TRUE,lty=3)
curve(NAK_CDF(x,525.3651,6.3686),col="cyan",lwd=2,add=TRUE,lty=3)
curve(LL_CDF(x,3.3763,8.8843,13.0823),col="YELLOW",lwd=2,add=TRUE,lty=3)
curve(EB_CDF(x,20.3492,15.6814,-3.1802 ),col="PURPLE",lwd=2,add=TRUE,lty=3)
curve(BURR3P_CDF(x,15.70295  , 0.31387 , 18.89884 ),col="cornsilk4",lwd=2,add=TRUE,lty=3)
curve(INW_CDF(x,11.4854790,2.3484959,0.0063314),col="GREEN",lwd=2,add=TRUE,lty=3)
curve(GUM_CDF(x,21.71462  ,3.63778  ),col="PINK",lwd=2,add=TRUE,lty=3)
curve(CAUCHY_CDF(x,2.79732,21.07031),col="ORANGE",lwd=2,add=TRUE,lty=3)
legend("bottomright",inset=c(0.04,0.1),cex=0.7,legend=c("Emperical",
                                                        "MEC","GIW","LL","NAK","EB XII", "BURR(3P)","IW",
                                                        "GUM","CAUCHY"),lty=c(1,1,3,3,3,3,3,3,3,3),lwd=1,
       col=c("black","red","blue","cyan","yellow","purple","cornsilk4","green","pink"
             ,"orange"))


pp.plot(MEC_CDF(x,4.8775e+03, 5.744206 ,6.9030e+03),ref.line = TRUE,col="BLUE")
pp.plot(GIW_CDF(x,6.933899 ,5.743041,500.508617),ref.line = TRUE,col="RED")
pp.plot(NAK_CDF(x,525.3651,6.3686),ref.line = TRUE,col="GREEN")
pp.plot(LL_CDF(x,3.3763,8.8843,13.0823),ref.line = TRUE,col="ORANGE")







############################ OTHER DISTRIBUTIONS #######################################

#curve(WEIBULL_PDF(x,1.74462 , 0.89322, 1.50410),col="BLUE",lwd=2,add=TRUE)
#curve(LOG_NORMAL_PDF(x,73.20051 ,5.25022,  0.28777 ),col="PINK",lwd=2,add=TRUE)
#curve(HN_PDF(x,3.9602e-16 , 9.4389e+01 ),col="BLACK",lwd=2,add=TRUE)
#curve(GHN_PDF(x,-4.9016e-19 , 9.4389e+00, 8.0000e-02 ),col="BLACK",lwd=2,add=TRUE)
#curve(FRECH_PDF(x,0.362672,0.044691),col="PINK",lwd=2,add=TRUE)
#curve(LPT3_PDF(x,1.1131e+03,4.5445e+00,4.5445e+00),col="PINK",lwd=2,add=TRUE)
#curve(LPT3_PDF(x,1.1131e+03,4.5445e+00 ,1.1046e+03 ),col="GREEN",lwd=2,add=TRUE)




fit2<-mle2(GAMMA_LL,start=list(a=0.9,beta=100 ,c=200 ),data=list(x),method="BFGS")
summary(fit2)
AIC(fit2)
fit3<-mle2(WEIBULL_LL,start=list(a=1.1,b=3.6,rho=4.2),data=list(x),method="BFGS")
summary(fit3)
AIC(fit3)
fit4<-mle2(LOG_NORMAL_LL,start=list(a=0.86109 ,b= 0.42101, rho=7.39237),data=list(x),method="BFGS")
summary(fit4)
AIC(fit4)
fit5<-mle2(HN_LL,start=list(a=1,b=20),data=list(x),method="BFGS")
summary(fit5)
AIC(fit5)
fit7<-mle2(PT3_LL,start=list(a=2,b=0.1,r=1),data=list(x),method="BFGS")
summary(fit7)
AIC(fit7)
fit9<-mle2(GHN_LL,start=list(a=1,b=1),data=list(x),method="BFGS")
summary(fit9)
AIC(fit9)
fit13<-mle2(Chi_LL,start=list(a=2.1,b=1.1),data=list(x),method="BFGS")
summary(fit13)
AIC(fit13)
fit14<-mle2(DAG_LL,start=list(a=1,b=1,c=1,k=1 ),data=list(x),method="BFGS")
summary(fit14)
AIC(fit14)
fit15<-mle2(FRECH_LL,start=list(a=50,b=60),data=list(x),method="BFGS")
summary(fit15)
AIC(fit15)
fit16<-mle2(INVERSEG_LL,start=list(a=1.1 ,b=4.7, c=9.98 ),data=list(x),method="BFGS")
summary(fit16)
AIC(fit16)
fit17<-mle2(LEVY_LL,start=list(a=0.1 ,b=2),data=list(x),method="BFGS")
summary(fit17)
AIC(fit17)
fit18<-mle2(DAGUMI_LL,start=list(a=0.3813 ,b=0.05552 , c=0.00147),data=list(x),method="BFGS")
summary(fit18)
AIC(fit18)
fit24<-mle2(PT5_LL,start=list(a=20 ,b=0.0001, r=3),data=list(x),method="BFGS")
summary(fit24)
AIC(fit24)
fit27<-mle2(PAR_LL,start=list(a=6,b=70,c=400),data=list(x),method="BFGS")
summary(fit27)
AIC(fit27)
fit28<-mle2(GEV_LL,start=list(a=0.5,b=1,c=10),data=list(x),method="BFGS")
summary(fit28)
AIC(fit28)
fit30<-mle2(EW_LL,start=list(a=0.209640,b=0.149112,n=1092.978899),data=list(x),method="BFGS")
summary(fit30)
AIC(fit30)
fit31<-mle2(IC_LL,start=list(a=310.212279,b=1.69),data=list(x),method="BFGS")
summary(fit31)
AIC(fit31)
confint(fit31)
# fit32<-mle2(EXCHEN_LL,start=list(a=90.1,b=0.9,c=9.6),data=list(x),method="BFGS")
summary(fit32)
AIC(fit32)
fit33<-mle2(TIW_LL,start=list(a=40,b=0.97,c=26.85687),data=list(x),method="BFGS")
summary(fit33)
AIC(fit33)
fit35<-mle2(GP_LL,start=list(a=0.2,b=0.2,k=0.01),data=list(x),method="BFGS")
summary(fit35)
AIC(fit35)
fit37<-mle2(GP_LL,start=list(a=0.66,c=20.7702),data=list(x),method="BFGS")
summary(fit37)
AIC(fit37)
confint(fit37)
fit37<-mle2(KAPPA_LL,start=list(a=5.0826,b=10.8663,c=0.1215),data=list(x),method="BFGS")
summary(fit37)
AIC(fit37)
fit38<-mle2(MW_LL,start=list(a=0.219	,b=0.05,c=0.0365),data=list(x),method="BFGS")
summary(fit38)
AIC(fit37)
fit40<-mle2(EB3_LL,start=list(a=4,b=30.1,c=200),data=list(x),method="BFGS")
summary(fit40)
AIC(fit40)




####################### GAMMA PDF ########################################
GAMMA_PDF<-function(x,a,beta,c){
  A<-((x-c)^(a-1))/(beta^(a)*gamma(a))
  B<-exp(-(x-c/beta))
  PDF<-A*B
  return(PDF)
}


################################### GAMMA-Negative log-likelihood #########################
GAMMA_LL<-function(a,beta,c){
  A<-((x-c)^(a-1))/(beta^(a)*gamma(a))
  B<-exp(-(x-c/beta))
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}


####################### NORMAL PDF ########################################
NORMAL_PDF<-function(x,a,b){
  A<-exp(-1/2*((x-a)/b)^2)
  B<-b*sqrt(2*pi)
  PDF<-A/B
  return(PDF)
}

################################### NORMAL-Negative log-likelihood #########################
NORMAL_LL<-function(a,b){
  A<-exp(-1/2*((x-a)/b)^2)
  B<-b*sqrt(2*pi)
  PDF<-A/B
  LL<--sum(log(PDF))
  return(LL)
}

####################### LOG-NORMAL PDF ########################################
LOG_NORMAL_PDF<-function(x,a,b,rho){
  A<- -1/2*((log(x-a)-b)/rho)^2 
  B<-exp(A)
  C<-(x-a)*rho*sqrt(2*pi)
  PDF<-B/C
  return(PDF)
}

################################### NORMAL-Negative log-likelihood #########################
LOG_NORMAL_LL<-function(a,b,rho){
  A<- -1/2*((log(x-a)-b)/rho)^2 
  B<-exp(A)
  C<-(x-a)*rho*sqrt(2*pi)
  PDF<-B/C
  LL<--sum(log(PDF))
  return(LL)
}


####################### LOG-GAMMA PDF ########################################
LOG_GAMMA_PDF<-function(x,a,b){
  A<-(log(x))^(a-1)/x*b^a*gamma(a)
  B<-exp(-(log(x)/b))
  PDF<-A*B
  return(PDF)
}

################################### LOG-GAMMA-Negative log-likelihood #########################
LOG_GAMMA_LL<-function(a,b){
  A<-(log(x))^(a-1)/x*b^a*gamma(a)
  B<-exp(-(log(x)/b))
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}

####################### WEIBULL PDF ########################################
WEIBULL_PDF<-function(x,a,b,rho){
  A<-a/b*((x-rho)/b)^(a-1)
  B<-exp(-((x-rho)/b)^a)
  PDF<-A*B
  return(PDF)
}
################################### WEIBULL-Negative log-likelihood #########################
WEIBULL_LL<-function(a,b,rho){
  A<-a/b*((x-rho)/b)^(a-1)
  B<-exp(-((x-rho)/b)^a)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}


#################### LOG-PEARSON TYPE III   CDF/PDF/LL #########################

LPT_CDF<-function(x,a,b){
  A<-1/gamma()
  B<-exp((-log(x)-x)/b)
  PDF<-A*B
  return(PDF)
}


LPT_PDF<-function(x,a,b){
  A<-(1/x*abs(b)*gamma(a))*((log(x)-x)/b)^(a-1)
  B<-exp((-log(x)-x)/b)
  PDF<-A*B
  return(PDF)
}


LPT_LL<-function(a,b){
  A<-(1/x*abs(b)*gamma(a))*((log(x)-x)/b)^(a-1)
  B<-exp((-log(x)-x)/b)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}



##################### HALF-NORMAL DISTRIBUTION ###############################
HN_PDF<-function(x,a,b){
  A<-2/a*b*(x/a)
  PDF<-A
  return(PDF)
}

########################### HALF NORMAL NEGATIVE LOG-LIKLEHOOD #########

HN_LL<-function(a,b){
  A<-2/a*b*(x/a)
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}

##################### GENERAL-HALF-NORMAL DISTRIBUTION ###############################
GHN_PDF<-function(x,a,b,r){
  A<-(2/a)*b*((x-r)/a)
  PDF<-A
  return(PDF)
}

########################### GENERAL-HALF NORMAL NEGATIVE LOG-LIKLEHOOD #########

GHN_LL<-function(a,b,r){
  A<-(2/a)*b*((x-r)/a)
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}

##################### PEARSON TYPE 3 DISTRIBUTION  ###############################
PT3_PDF<-function(x,a,b,r){
  A<-(1/(abs(a)*gamma(b)))*((x-r)/a)^(b-1)
  B<-exp(-(x-r)/a)
  PDF<-A*B
  return(PDF)
}

##################### PEARSON TYPE 3 DISTRIBUTION LOG LIKELIHOOD ###############################
PT3_LL<-function(a,b,r){
  A<-(1/(abs(a)*gamma(b)))*((x-r)/a)^(b-1)
  B<-exp(-(x-r)/a)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}



##################### GENERAL HALF NORMAL DISTRIBUTION LOG LIKLIHOOD ###############################
GHN_PDF<-function(x,a,b,r){
  A<-(1/(abs(a)*gamma(b)))*((x-r)/a)^(b-1)
  B<-exp(-(x-r)/a)
  PDF<-A*B
  return(PDF)
}

##################### GENERAL HALF NORMAL DISTRIBUTION ###############################
GHN_LL<-function(a,b,r){
  A<-(1/(abs(a)*gamma(b)))*((x-r)/a)^(b-1)
  B<-exp(-(x-r)/a)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}


################################# GUMBEL DISTRIBUTION ##########################
GUM_PDF<-function(x,a,b){
  A<-1/b*exp(-(x-a)/b-exp(-(x-a)/b))
  PDF<-A
  return(PDF)
}

################################# GUMBEL DISTRIBUTION LOGLIKELIHOOD  ##########################
GUM_LL<-function(a,b){
  A<-1/b*exp(-(x-a)/b-exp(-(x-a)/b))
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}


################################# BURR 3-P DISTRIBUTION ##########################
BURR3P_PDF<-function(x,a,b,c){
  A<-(a*b)/c*(x/c)^(a-1)
  B<-(1+(x/c)^a)^(-b-1)
  PDF<-A*B
  return(PDF)
}

################################# BURR 3-P DISTRIBUTION LOG-LHOOD##########################
BURR3P_LL<-function(a,b,c){
  A<-(a*b)/c*(x/c)^(a-1)
  B<-(1+(x/c)^a)^(-b-1)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}

################################# CAUCHY DISTRIBUTION ##########################
CAUCHY_PDF<-function(x,a,b){
  A<-(pi*a*(1+((x-b)/a)^2))^(-1)
  PDF<-A
  return(PDF)
}

################################# CAUCHY DISTRIBUTION LOG LIKELIHOOD ###########
CAUCHY_LL<-function(a,b){
  A<-(pi*a*(1+((x-b)/a)^2))^(-1)
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}

################################# CHI-SQUARE DISTRIBUTION ######################
Chi_PDF<-function(x,a,b){
  A<-(x-a)^(b/2-1)*exp((x-a)/2)
  B<-(2)^(b/2-1)*gamma(b/2)
  PDF<-A/B
  return(PDF)
}

################################# CHI-SQUARE DISTRIBUTION LOG ##########################
Chi_LL<-function(a,b){
  A<-(x-a)^(b/2-1)*exp((x-a)/2)
  B<-(2)^(b/2-1)*gamma(b/2)
  PDF<-A/B
  LL<--sum(log(PDF))
  return(LL)
}

################################ DAGUM DISTRIBUTION ###############################
DAG_PDF<-function(x,a,b,c,k){
  A<- a*k*((x-c)/b)^(a*k-1)
  B<-b*((1+(x-c)/b))^{k+1}
  PDF<-A*B
  return(PDF)
}

################################ DAGUM DISTRIBUTION LOG LIKELIHOOD ###############################
DAG_LL<-function(a,b,c,k){
  A<-a*k*((x-c)/b)^(a*k-1)
  B<-b*((1+(x-c)/b)^a)^{k+1}
  PDF<-A/B
  LL<--sum(log(PDF))
  return(LL)
}



################################ FRECH DISTRIBUTION ###############################
FRECH_PDF<-function(x,a,b){
  A<- (a*b)^(a)*x^(-a-1)
  B<-exp(-(b/x)^(a))
  PDF<-A*B
  return(PDF)
}


#fit_w <-fitdist(x,"FRECH_PDF", start =list(a=1.79,b=5.7442,rho=1.886))
#summary(fit_w)
#plot(fit_w)

################################ FRECH DISTRIBUTION LOG LIKELIHOOD ###############################
FRECH_LL<-function(a,b){
  A<- (a*b)^(a)*x^(-a-1)
  B<-exp(-(b/x)^(a))
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}

################################ INVERSE_G DISTRIBUTION ###############################
INVERSEG_PDF<-function(x,a,b,c){
  A<- sqrt(a/(2*pi*(x-b)^3))
  B<-exp((-a*(x-b-c)^2)/(2*c^2*(x-b)))
  PDF<-A*B
  return(PDF)
}
################################ INVERSE_G DISTRIBUTION LOG LIKELIHOOD ###############################
INVERSEG_LL<-function(a,b,c){
  A<- sqrt(a/(2*pi*(x-b)^3))
  B<-exp((-a*(x-b-c)^2)/(2*c^2*(x-b)))
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}

fit00<-mle2(INVERSEG_LL,start=list(a=1.1234,b=10,c=10),data=list(x),method="BFGS")
summary(fit00)
AIC(fit00)









################################ LEVY DISTRIBUTION ###############################
LEVY_PDF<-function(x,a,b){
  A<- sqrt(a/2*pi)
  B<-(exp((-0.5*b)/x))/(x)^{3/2}
  PDF<-A*B
  return(PDF)
}
################################ LEVY DISTRIBUTION LOG LIKELIHOOD ###############################
LEVY_LL<-function(a,b){
  A<- sqrt(a/2*pi)
  B<-(exp((-0.5*b)/x))/(x)^{3/2}
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}



################################ DAGUMI DISTRIBUTION ###############################
DAGUMI_PDF<-function(x,a,b,c){
  A<- a*c*(x/b)^(a*c-1)
  B<-b*(1+(x/b)^a)^(c+1)
  PDF<-A/B
  return(PDF)
}
################################ DAGUMI DISTRIBUTION LOG LIKELIHOOD ###############################
DAGUMI_LL<-function(a,b,c){
  A<- a*c*(x/b)^(a*c-1)
  B<-b*(1+(x/b)^a)^(c+1)
  PDF<-A/B
  LL<--sum(log(PDF))
  return(LL)
}

################################ LOG-LOGISTIC DISTRIBUTION ###############################
LL_PDF<-function(x,a,b,c){
  A<- a/b*((x-c)/b)^(a-1)
  B<-(1+((x-c)/b)^(a))^(-2)
  PDF<-A*B
  return(PDF)
}

################################ LOG-LOGISTIC DISTRIBUTION LOG LIKELIH ###############################
LL_LL<-function(a,b,c){
  A<- a/b*((x-c)/b)^(a-1)
  B<-(1+((x-c)/b)^(a))^(-2)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}

################################ INVERSE WEIBULL DISTRIBUTION ###############################
INW_PDF<-function(x,a,b,n){
  A<- b/n*(1/(x-a))^(b+1)
  B<-exp((-1/n)*(1/(x-a))^b)
  PDF<-A*B
  return(PDF)
}

################################ INVERSE WEIBULL DISTRIBUTION LOG LIKELIHOO ###############################
INW_LL<-function(a,b,n){
  A<- b/n*(1/(x-a))^(b+1)
  B<-exp((-1/n)*(1/(x-a))^b)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}

############################### INVERSE FLEXIBLE WEIBULL DISTRIBUTION ###############################
EINFW_PDF<-function(x,a,b,c){
  A<- c*(b+a/x^2)*exp(a/x-b*x)*exp(-c*exp(a/x-b*x))
  PDF<-A
  return(PDF)
}#

################################ INVERSE FLEXIBLE WEIBULL DISTRIBUTION LOG ###############################
EINFW_LL<-function(a,b,c){
  A<- c*(b+a/x^2)*exp(a/x-b*x)*exp(-c*exp(a/x-b*x))
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}
############################### INVERSE EXPONENTIAL DISTRIBUTION ###############################
IE_PDF<-function(x,a,b){
  A<-a/b^2*exp(-a/b)
  PDF<-A
  return(PDF)
}
################################ INVERSE EXPONENTIAL DISTRIBUTION LOG ###############################
IE_LL<-function(a,b){
  A<-a/b^2*exp(-a/b)
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}
############################### PEARSON TYPE 5 DISTRIBUTION ###############################
PT5_PDF<-function(x,a,b,r){
  A<-exp(-b/(x-r))
  B<-b*gamma(a)*((x-r)/b)^(a+1)
  PDF<-A/B
  return(PDF)
}
############################### PEARSON TYPE 5 DISTRIBUTION LOG LIK ###############################
PT5_LL<-function(a,b,r){
  A<-exp(-b/(x-r))
  B<-b*gamma(a)*((x-r)/b)^(a+1)
  PDF<-A/B
  LL<--sum(log(PDF))
  return(LL)
}
############################### NAKAGAMI DISTRIBUTION ###############################
NAK_PDF<-function(x,a,m){
  A<-(2*m^m)/(gamma(m)*a^m)
  B<-x^(2*m-1)*exp(-m/a*x^2)
  PDF<-A*B
  return(PDF)
}

############################### NAKAGAMI DISTRIBUTION ###############################
NAK_LL<-function(a,m){
  A<-(2*m^m)/(gamma(m)*a^m)
  B<-x^(2*m-1)*exp(-m/a*x^2)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}

############################### LPT3 DISTRIBUTION ###############################
LPT3_PDF<-function(x,a,b,c){
  A<-a*b*c*x^-(b+1)*exp(-a*x^(-b))
  B<-(c-(c-1)*exp(-a*x^(-b)))^2
  PDF<-A/B
  return(PDF)
}

LPT3_LL<-function(a,b,c){
  A<-a*b*c*x^-(b+1)*exp(-a*x^(-b))
  B<-(c-(c-1)*exp(-a*x^(-b)))^2
  PDF<-A/B
  LL<--sum(log(PDF))
  return(LL)
}


############################# PARETO DISTRIBUTION ########################
PAR_PDF<-function(x,a,b,c){
  A<-1/x*gamma(a)*c^a*(b/x)^(1/c)
  B<-(log(x/b))^(a-1)
  PDF<-A*B
  return(PDF)
}

PAR_LL<-function(a,b,c){
  A<-1/x*gamma(a)*c^a*(b/x)^(1/c)
  B<-(log(x/b))^(a-1)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}



############################# GEV DISTRIBUTION ########################
GEV_PDF<-function(x,a,b,c){
  A<-exp(-((1+(c*(x-a)/b))^(-1/c)))
  PDF<-A
  return(PDF)
}

GEV_LL<-function(a,b,c){
  A<-exp(-((1+(c*(x-a)/b))^(-1/c)))
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}





############################# BETA LINDLEY DISTRIBUTION ########################
BL_PDF<-function(x,a,b,c){
  A<-(a^2*(a+1+a*x)^(b-1)*(1+x)*exp(-a*b*x))/(beta(c,b)*(a+1)^b)
  B<-(1-(a+1+a*x)/(a+1)*exp(-a*x))^(c-1)
  PDF<-A*B
  return(PDF)
}

BL_LL<-function(a,b,c){
  A<-(a^2*(a+1+a*x)^(b-1)*(1+x)*exp(-a*b*x))/(beta(c,b)*(a+1)^b)
  B<-(1-(a+1+a*x)/(a+1)*exp(-a*x))^(c-1)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}




############################## INVERSE CHEN  DISTRIBUTION  ##################################
IC_PDF<-function(x,a,b){
  A<-a*b*x^(-b+1)*exp(x^(-b)+a*(1-exp(x^(-b))))
  PDF<-A
  return(PDF)
}

IC_LL<-function(a,b){
  A<-a*b*x^(-b+1)*exp(x^(-b)+a*(1-exp(x^(-b))))
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}
############################## CHEN  DISTRIBUTION  ##################################
CHEN_PDF<-function(x,a,b){
  A<-a*b*x^(b-1)*exp(x^(b))*exp(a*(1-exp(x^(b))))
  PDF<-A
  return(PDF)
}

CHEN_LL<-function(a,b){
  A<-a*b*x^(b-1)*exp(x^(b))*exp(a*(1-exp(x^(b))))
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}

############################## EXTENDED CHEN  DISTRIBUTION  ##################################
EXCHEN_PDF<-function(x,a,b){
  A<-a*b*c*x^(b-1)*exp(x^(b))
  B<- (1+c*(exp(x^b-1)))^(-a-1)
  PDF<-A*B
  return(PDF)
}

EXCHEN_LL<-function(a,b,c){
  A<-a*b*c*x^(b-1)*exp(x^(b))
  B<- (1+c*(exp(x^b-1)))^(-a-1)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}

############################## EXPONENTIAL INVERSE WEIBULL  DISTRIBUTION  ##################################
EIW_PDF<-function(x,a,b,c){
  A<-c*b*a^b*x^(-(b+1))*exp(-(a*x^(-1)))^b
  B<- (1-exp(-(a*x^(-1))^b))^(c-1)
  PDF<-A*B
  return(PDF)
}

EIW_LL<-function(a,b,c){
  A<-c*b*a^b*x^(-(b+1))*exp(-(a*x^(-1)))^b
  B<- (1-exp(-(a*x^(-1))^b))^(c-1)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}
############################## MOIE   DISTRIBUTION  ##################################
MOIE_PDF<-function(x,a,b,c){
  A<-a*b*c^b*x^(-(b+1))*exp(-(c*x^(-1)))^b
  B<- (a+(1-a)*exp(-(b*x-1)^b))^(-2)
  PDF<-A*B
  return(PDF)
}

MOIE_LL<-function(a,b,c){
  A<-a*b*c^b*x^(-(b+1))*exp(-(c*x^(-1)))^b
  B<- (a+(1-a)*exp(-(b*x-1)^b))^(-2)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}

############################## TIW  DISTRIBUTION  ##################################
TIW_PDF<-function(x,a,b,c){
  A<-b*a^b*x^(-(b+1))*exp(-(a*x^(-1)))^b
  B<-1+c-2*c*exp(-(a*x^(-1))^b)
  PDF<-A*B
  return(PDF)
}

TIW_LL<-function(a,b,c){
  A<-b*a^b*x^(-(b+1))*exp(-(a*x^(-1)))^b
  B<-1+c-2*c*exp(-(a*x^(-1))^b)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}

############################## KIW  DISTRIBUTION  ##################################
TIW_PDF<-function(x,a,b,c,n){
  A<-b*a^b*x^(-(b+1))*exp(-(a*x^(-1)))^b
  B<-1+c-2*c*exp(-(a*x^(-1))^b)
  PDF<-A*B
  return(PDF)
}

TIW_LL<-function(a,b,c){
  A<-b*a^b*x^(-(b+1))*exp(-(a*x^(-1)))^b
  B<-1+c-2*c*exp(-(a*x^(-1))^b)
  PDF<-A*B
  LL<--sum(log(PDF))
  return(LL)
}

############################## EXTENDED BURR XII  DISTRIBUTION  ##################################
EB_PDF<-function(x,a,c,k){
  A<-c*a^(-1)*(x/a)^(c-1)*(1-k*(x/a)^(c))^(1/k-1)
  PDF<-A
  return(PDF)
}

EB_LL<-function(a,c,k){
  A<-c*a^(-1)*(x/a)^(c-1)*(1-k*(x/a)^(c))^(1/k-1)
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}

############################## GP DISTRIBUTION  ##################################
GP_PDF<-function(x,a,b,k){
  A<-a^(-1)*exp(-(1-k)*y)
  y<--k^(-1)*log(1-(k*(x-b))/a)
  PDF<-A
  return(PDF)
}

GP_LL<-function(a,b,k){
  A<-a^(-1)*exp(-(1-k)*y)
  y<--k^(-1)*log(1-(k*(x-b))/a)
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}


############################## BETA GAMMA DISTRIBUTION  ##################################
BG_PDF<-function(x,a,b,d,e){
  y<-exp(-(x-e)/d)
  A<-1/(d*beta(a,b))*y*exp(-a*y)*(1-exp(-y))^(b-1)
  PDF<-A
  return(PDF)
}

BG_LL<-function(a,b,d,e){
  u<-exp(-(x-e)/d)
  A<-1/(d*beta(a,b))*u*exp(-a*u)*(1-exp(-u))^(b-1)
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}


############################## GENERALISED PARETO DISTRIBUTION  ##################################
GP_PDF<-function(x,a,c){
  A<-1/a*(1+((c*x)/a))^(-(1+c)/c)
  PDF<-A
  return(PDF)
}

GP_LL<-function(a,b,c){
  A<-1/a*(1+((c*x)/a))^(-(1+c)/c)
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}
############################## MOKKAPPA DISTRIBUTION  ##################################
KAPPA_PDF<-function(x,a,b,c,d){
  A<-d*(a*c)/b*(x/b)^(c-1)*(a+(x/b)^(a*c))^((a+1)/a)
  B<-(d+(1-d)*(((x/b)^(a*c))/a+(x/b)^(a*c))^(1/a))^2
  PDF<-A/B
  return(PDF)
}

KAPPA_LL<-function(a,b,c,d){
  A<-d*(a*c)/b*(x/b)^(c-1)*(a+(x/b)^(a*c))^((a+1)/a)
  B<-(d+(1-d)*(((x/b)^(a*c))/a+(x/b)^(a*c))^(1/a))^2
  PDF<-A/B
  LL<--sum(log(PDF))
  return(LL)
}

############################## KAPPA DISTRIBUTION  ##################################
KAPPA_PDF<-function(x,a,b,c){
  A<-(a*c/b)*(x/b)^(c-1)*(a+(x/b)^a*c)^(-(a+1)/a)
  PDF<-A
  return(PDF)
}

KAPPA_LL<-function(a,b,c){
  A<-(a*c/b)*(x/b)^(c-1)*(a+(x/b)^a*c)^(-(a+1)/a)
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}

############################## MODIFIED WEIBULL DISTRIBUTION  ##################################
MW_PDF<-function(x,a,b,c){
  A<-(c+a*b*x^(b-1))*exp(-c*x-a*x^b)
  PDF<-A
  return(PDF)
}

MW_LL<-function(a,b,c){
  A<-(c+a*b*x^(b-1))*exp(-c*x-a*x^b)
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}

############################## EXTENDED BURR XII DISTRIBUTION  ##################################
EB3_PDF<-function(x,a,b,c){
  A<-a*c^(-1)*(x/c)^(a-1)*(1-b*(x/c)^a)^(1/(b-1))
  PDF<-A
  return(PDF)
}

EB3_LL<-function(a,b,c){
  A<-a*c^(-1)*(x/c)^(a-1)*(1-b*(x/c)^a)^(1/(b-1))
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}



EB3_PDF<-function(x,a,b,c){
  A<-a*c^(-1)*(x/c)^(a-1)*(1-b*(x/c)^a)^(1/(b-1))
  PDF<-A
  return(PDF)
}

EB3_LL<-function(a,b,c){
  A<-a*c^(-1)*(x/c)^(a-1)*(1-b*(x/c)^a)^(1/(b-1))
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}

