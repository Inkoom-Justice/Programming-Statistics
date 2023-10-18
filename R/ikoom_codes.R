#CDF MEC#
y=seq(0,1,0.01)
gm1=c(1.5,3.40,4.85,5,0.78)
rho1=c(0.07,0.25,6.5,0.00075,0.002)
mu1=c(0.9,0.25,0.01,3,10.0)
fy1<-((rho1[1]*(exp((y^(-gm1[1])))-1)+1))^(-mu1[1])
fy2<-((rho1[2]*(exp((y^(-gm1[2])))-1)+1))^(-mu1[2])
fy3<-((rho1[3]*(exp((y^(-gm1[3])))-1)+1))^(-mu1[3])
fy4<-((rho1[4]*(exp((y^(-gm1[4])))-1)+1))^(-mu1[4])
fy5<-((rho1[5]*(exp((y^(-gm1[5])))-1)+1))^(-mu1[5])
plot(y,fy1,type="l",ylim=c(0,1),lty=1,lwd=2,ylab="F(y)", col="blue")
lines(y,fy2,type="l",lwd=2, col="green")
lines(y,fy3,type="l",lwd=2, col="yellow")
lines(y,fy4,type="l",lwd=2, col="red")
#lines(y,fy5,type="l",lwd=2, col="brown")
#legend("topleft",c("theta=0.1","theta=0.25","theta=0.5","theta=0.75","theta=1"),
 #    fill=c("blue","green", "yellow", "red"))
legend("topleft", legend = c(expression(paste(gamma, " = ", 1.5  , rho, "=",0.07 , mu, "=",0.90)),
                              expression(paste(gamma, " = ", 3.40 , rho, "=",0.25 , mu, "=",0.25)),
                              expression(paste(gamma, " = ", 4.85  , rho, "=",6.50 , mu, "=",0.01)),
                             expression(paste(gamma, " = ", 5.0 , rho, "=",0.00075 , mu, "=",3.00))),
      fill=c("blue","green" ,"yellow","red"), ncol = 1,
       cex = 0.5)

#CDF MEC#
y=seq(0,1,0.01)
gm1=c(1.5,3.40,4.85,5,0.78)
rho1=c(0.07,0.25,6.5,0.00075,0.002)
mu1=c(0.9,0.25,0.01,3,10.0)
fy1<-((rho1[1]*(exp((y^(-gm1[1])))-1)+1))^(-mu1[1])
fy2<-((rho1[2]*(exp((y^(-gm1[2])))-1)+1))^(-mu1[2])
fy3<-((rho1[3]*(exp((y^(-gm1[3])))-1)+1))^(-mu1[3])
fy4<-((rho1[4]*(exp((y^(-gm1[4])))-1)+1))^(-mu1[4])
fy5<-((rho1[5]*(exp((y^(-gm1[5])))-1)+1))^(-mu1[5])
plot(y,fy1,type="l",ylim=c(0,1),lty=1,lwd=1,ylab="F(y)", col=1)
lines(y,fy2,type="l",lwd=1, col=2)
lines(y,fy3,type="l",lwd=1, col=3)
lines(y,fy4,type="l",lwd=1, col=4)
#lines(y,fy5,type="l",lwd=2, col="brown")
#legend("topleft",c("theta=0.1","theta=0.25","theta=0.5","theta=0.75","theta=1"),
#    fill=c("blue","green", "yellow", "red"))
legend("topleft", legend = c(expression(paste(gamma, " = ", "1.50"  , ~~rho, "=","0.07" , ~~mu, "=","0.90")),
                             expression(paste(gamma, " = ", "3.40" , ~~rho, "=","0.25" , ~~mu, "=","0.25")),
                             expression(paste(gamma, " = ", "4.85"  , ~~rho, "=","6.50" , ~~mu, "=","0.01")),
                             expression(paste(gamma, " = ", "5.0" , ~~rho, "=","0.00075" , ~~mu, "=","3.00"))),
       #fill=c("blue","green" ,"yellow","red"), 
       ncol = 1, col = 1:5,lwd = 1, 
       cex = 0.5)

#SF MEC#
y=seq(0,1,0.01)
gm1=c(0.5,0.40,0.85,0.32,0.7)
rho1=c(0.07,0.25,0.53,0.09,0.08)
mu1=c(0.59,0.87,0.01,0.64,0.61)
fy1<-1-((rho1[1]*(exp((y^(-gm1[1])))-1)+1))^(-mu1[1])
fy2<-1-((rho1[2]*(exp((y^(-gm1[2])))-1)+1))^(-mu1[2])
fy3<-1-((rho1[3]*(exp((y^(-gm1[3])))-1)+1))^(-mu1[3])
fy4<-1-((rho1[4]*(exp((y^(-gm1[4])))-1)+1))^(-mu1[4])
fy5<-1-((rho1[5]*(exp((y^(-gm1[5])))-1)+1))^(-mu1[5])
plot(y,fy1,type="l",ylim=c(0,1),lty=1,lwd=1,ylab="S(y)", col=1)
lines(y,fy2,type="l",lwd=1, col=2)
lines(y,fy3,type="l",lwd=1, col=3)
lines(y,fy4,type="l",lwd=1, col=4)
lines(y,fy5,type="l",lwd=1, col=5)
#legend("topleft",c("theta=0.1","theta=0.25","theta=0.5","theta=0.75","theta=1"),
#    fill=c("blue","green", "yellow", "red"))
legend("topright", legend = c(expression(paste(gamma, " = ", "0.50", ~rho, "=","0.07" ,  ~mu, "=","0.90")),
                             expression(paste(gamma, " = ", "0.40" ,  ~rho, "=","0.25" ,  ~mu, "=","0.25")),
                             expression(paste(gamma, " = ", "0.85"  , ~rho, "=","0.53" , ~mu, "=","0.01")),
                             expression(paste(gamma, " = ", "0.32" , ~rho, "=","0.00075" , ~mu, "=","3.20")),
                             expression(paste(gamma, " = ", "0.70" , ~rho, "=","0.00075" , ~mu, "=","4.10"))),
      # fill=c("blue","green" ,"yellow","red","brown"), 
      col = 1:5,lwd = 1, 
       cex = 0.5)


#PDF MEC#
y=seq(0,2,0.01)
gm1=c(0.30,3.40,0.85,0.50,3)
rho1=c(0.10,0.5,6.5,0.10,0.90)
mu1=c(4,0.6,0.01,3,0.87)
fy1<-mu1[1]*gm1[1]*rho1[1]*y^(-gm1[1]-1)*exp(y^(-gm1[1]))*((rho1[1]*(exp((y^(-gm1[1])))-1)+1))^(-mu1[1]-1)
fy2<-mu1[2]*gm1[2]*rho1[2]*y^(-gm1[2]-1)*exp(y^(-gm1[2]))*((rho1[2]*(exp((y^(-gm1[2])))-1)+1))^(-mu1[2]-1)
fy3<-mu1[3]*gm1[3]*rho1[3]*y^(-gm1[3]-1)*exp(y^(-gm1[3]))*((rho1[3]*(exp((y^(-gm1[3])))-1)+1))^(-mu1[3]-1)
fy4<-mu1[4]*gm1[4]*rho1[4]*y^(-gm1[4]-1)*exp(y^(-gm1[4]))*((rho1[4]*(exp((y^(-gm1[4])))-1)+1))^(-mu1[4]-1)
fy5<-mu1[5]*gm1[5]*rho1[5]*y^(-gm1[5]-1)*exp(y^(-gm1[5]))*((rho1[5]*(exp((y^(-gm1[5])))-1)+1))^(-mu1[5]-1)
plot(y,fy1,type="l",ylim=c(0,2),lty=1,lwd=1,ylab="f(y)", col=1)
lines(y,fy2,type="l",lwd=1, col=2)
lines(y,fy3,type="l",lwd=1,  col=3)
lines(y,fy4,type="l",lwd=1,  col=4)
lines(y,fy5,type="l",lwd=1,  col=5)
#legend("topleft",c("theta=0.1","theta=0.25","theta=0.5","theta=0.75","theta=1"),
#       cex=0.8,lty=1:5, lwd=2, bty="n")
legend("topright", legend = c(expression(paste(gamma, " = ", "0.30"  , ~~rho, "=","0.10" , ~~mu, "=","4.00")),
                             expression(paste(gamma, " = ", "3.40" , ~~rho, "=","0.50" , ~~mu, "=","0.6")),
                             expression(paste(gamma, " = ", "0.85"  , ~~rho, "=","6.50" , ~~mu, "=","0.01")),
                             expression(paste(gamma, " = ", "0.50" , ~~rho, "=","0.10" , ~~mu, "=","3.00")),
                             expression(paste(gamma, " = ", "3.01" , ~~rho, "=","0.90" , ~~mu, "=","0.87"))),
    ncol = 1,
       cex = 0.5,  col = 1:5,lwd = 1)

#HRF MEC#
y=seq(0,1.8,0.001)
gm1=c(0.49, 0.09, 0.05, 0.50)
rho1=c(0.3, 0.001, 3.12, 0.10)
mu1=c(1.3, 1.4, 0.8, 3)
fy1<-(mu1[1]*gm1[1]*rho1[1]*y^(-gm1[1]-1)*exp(y^(-gm1[1]))*(rho1[1]*(exp((y^(-gm1[1])))-1)+1)^(-mu1[1]-1))/(1-(((rho1[1]*(exp((y^(-gm1[1])))-1)+1))^(-mu1[1])))
fy2<-(mu1[2]*gm1[2]*rho1[2]*y^(-gm1[2]-1)*exp(y^(-gm1[2]))*((rho1[2]*(exp((y^(-gm1[2])))-1)+1))^(-mu1[2]-1))/(1-((rho1[2]*(exp((y^(-gm1[2])))-1)+1))^(-mu1[2]))
fy3<-(mu1[3]*gm1[3]*rho1[3]*y^(-gm1[3]-1)*exp(y^(-gm1[3]))*((rho1[3]*(exp((y^(-gm1[3])))-1)+1))^(-mu1[3]-1))/(1-((rho1[3]*(exp((y^(-gm1[3])))-1)+1))^(-mu1[3]))
fy4<-(mu1[4]*gm1[4]*rho1[4]*y^(-gm1[4]-1)*exp(y^(-gm1[4]))*((rho1[4]*(exp((y^(-gm1[4])))-1)+1))^(-mu1[4]-1))/(1-((rho1[3]*(exp((y^(-gm1[3])))-1)+1))^(-mu1[3]))
plot(y,fy1,type="l",ylim=c(0,1.8),lty=1,lwd=1,ylab="h(y)",col=1)
lines(y,fy2,type="l",col=2,lwd=1)
lines(y,fy3,type="l",col=3,lwd=1)
lines(y,fy4,type="l",col=4,lwd=1)
#legend("topleft",c("theta=0.1","theta=0.25","theta=0.5","theta=0.75","theta=1"),
#       cex=0.8,lty=1:5, lwd=2, bty="n")

legend("topright", legend = c(expression(paste(gamma, " = ", "0.49"  , ~~rho, "=","0.30" , ~~mu, "=","1.30")),
                              expression(paste(gamma, " = ", "0.09" , ~~rho, "=","0.001" , ~~mu, "=","1.40")),
                              expression(paste(gamma, " = ", "0.05"  , ~~rho, "=","3.12" , ~~mu, "=","0.80")),
                              expression(paste(gamma, " = ", "0.5" , ~~rho, "=","0.10" , ~~mu, "=","3.00"))),
      ncol = 1, col = 1:5,lwd = 1, 
       cex = 0.5)

y=seq(0,2,0.01)
gm1=c(1.6,0.7,5,0.50,2)
rho1=c(0.7,0.6,1.80,0.10,0.90)
mu1=c(0.20,0.5,0.7,3,1)
fy1<-mu1[1]*gm1[1]*rho1[1]*y^(-gm1[1]-1)*exp(y^(-gm1[1]))*((rho1[1]*(exp((y^(-gm1[1])))-1)+1))^(-mu1[1]-1)
fy2<-mu1[2]*gm1[2]*rho1[2]*y^(-gm1[2]-1)*exp(y^(-gm1[2]))*((rho1[2]*(exp((y^(-gm1[2])))-1)+1))^(-mu1[2]-1)
fy3<-mu1[3]*gm1[3]*rho1[3]*y^(-gm1[3]-1)*exp(y^(-gm1[3]))*((rho1[3]*(exp((y^(-gm1[3])))-1)+1))^(-mu1[3]-1)
fy4<-mu1[4]*gm1[4]*rho1[4]*y^(-gm1[4]-1)*exp(y^(-gm1[4]))*((rho1[4]*(exp((y^(-gm1[4])))-1)+1))^(-mu1[4]-1)
fy5<-mu1[5]*gm1[5]*rho1[5]*y^(-gm1[5]-1)*exp(y^(-gm1[5]))*((rho1[5]*(exp((y^(-gm1[5])))-1)+1))^(-mu1[5]-1)
plot(y,fy1,type="l",ylim=c(0,2.0),lty=1,lwd=2,ylab="PDF")
lines(y,fy2,type="l",lty=2,lwd=2)
lines(y,fy3,type="l",lty=3,lwd=2)
lines(y,fy4,type="l",lty=4,lwd=2)
lines(y,fy5,type="l",lty=5,lwd=2)
#legend("topleft",c("theta=0.1","theta=0.25","theta=0.5","theta=0.75","theta=1"),
#       cex=0.8,lty=1:5, lwd=2, bty="n")

##

#len<-24
#x = c(3.70, 2.74, 2.73, 2.50, 3.60, 3.11, 3.27, 2.87, 1.47, 3.11, 3.56,
#      4.42, 2.41, 3.19, 3.22, 1.69, 3.28, 3.09, 1.87, 3.15, 4.90, 1.57,
#      2.67, 2.93, 3.22, 3.39, 2.81, 4.20, 3.33, 2.55, 3.31, 3.31, 2.85,
#      1.25, 4.38, 1.84, 0.39, 3.68, 2.48, 0.85, 1.61, 2.79, 4.70, 2.03,
#      1.89, 2.88, 2.82, 2.05, 3.65, 3.75, 2.43, 2.95, 2.97, 3.39, 2.96,
#      2.35, 2.55, 2.59, 2.03, 1.61, 2.12, 3.15, 1.08, 2.56, 1.80, 2.53)


#y = pnorm(x,2.7,0.8)+ runif(len, min = -0.1, max = 0.1)

#plot(x, y)

#s<- seq(from =min(x), to = max(x), length = length(x))
#lines(s, pnorm(s,2.7,0.8)
#      , lwd= 2,col="blue")

#exp.eq <- function(x, a, b) {
#  pnorm(x,a,b)
#}

#df <- data.frame(x, y)
#m.sinexp <- nls(y ~ exp.eq(x, a, b), data = df, start = list(a = 2.7, b = 0.8), 
#               trace = F)

#summary(m.sinexp)
#lines(s, predict(m.sinexp, list(x = s)), col = "red", lwd=2)

#############################MLE Codes######################################################
####################### Quantile ######################################################
quantile<-function(a,b,d,u){
  A<-(log((u^(-1/a)-1)/d+1))^(-1/b)
  quant<-A
  return(quant)
}
######################### Negative Log-likelihood ##################################
ANW_LL<-function(par){
  a=par[1]
  b=par[2]
  d=par[3]
  A<-a*b*d*x^(-b-1)*exp(x^(-b))*((d*(exp((x^(-b)))-1)+1))^(-a-1)
  PDF<-A
  LL<--sum(log(PDF))
  return(LL)
}
###Algorithm for Monte Carlo Simulation Study
library(numDeriv)
library(Matrix)
a=3.5
b=0.5
d=0.01
n1=300
for(j in 1:length(n1)){
  n=n1[j]
  N=2000
  mle_a<-c(rep(0,N))
  mle_b<-c(rep(0,N))
  mle_alpha<-c(rep(0,N))
  LC_a<-c(rep(0,N))
  UC_a<-c(rep(0,N))
  LC_b<-c(rep(0,N))
  UC_b<-c(rep(0,N))
  LC_alpha<-c(rep(0,N))
  UC_alpha<-c(rep(0,N))
  count_a=0
  count_b=0
  count_alpha=0
  temp=1
  HH1<-matrix(c(rep(2,9)),nrow=3,ncol=3)
  HH2<-matrix(c(rep(2,9)),nrow=3,ncol=3)
  for(i in 1:N)
  {
    print(i)
    flush.console()
    repeat{
      x<-c(rep(0,n))
      # Generate a random variable from uniform distribution
      u<-0
      u<-runif(n,min=0,max=1)
      for(k in 1:n){
        x[k]<-quantile(a,b,d,u[k])
      }
     
      #Maximum likelihood estimation
      mle.result<-nlminb(c(a,b,d),ANW_LL,lower=c(0,0,0))
      temp=mle.result$convergence
      if(temp==0){
        temp_a<-mle.result$par[1]
        temp_b<-mle.result$par[2]
        temp_alpha<-mle.result$par[3]
        HH1<-hessian(ANW_LL, c(temp_a,temp_b,temp_alpha))
        if(sum(is.nan(HH1))==0&(diag(HH1)[1]>0)&(diag(HH1)[2]>0)&(diag(HH1)[3]>0)){
          HH2<-solve(HH1)
          #print(det(HH1))
        }
        else{
          temp=1}
      }
      if((temp==0)&(diag(HH2)[1]>0)&(diag(HH2)[2]>0)&(diag(HH2)[3]>0)&(sum(is.nan(HH2))==0)){
        break
      }
      else{
        temp=1}
    }
    temp=1
    mle_a[i]<-mle.result$par[1]
    mle_b[i]<-mle.result$par[2]
    mle_alpha[i]<-mle.result$par[3]
    HH<-hessian(ANW_LL,c(mle_a[i],mle_b[i],mle_alpha[i]))
    H<-solve(HH)
    LC_a[i]<-mle_a[i]-qnorm(0.975)*sqrt(diag(H)[1])
    UC_a[i]<-mle_a[i]+qnorm(0.975)*sqrt(diag(H)[1])
    if((LC_a[i]<=a)&(a<=UC_a[i])){
      count_a=count_a+1
    }
    LC_b[i]<-mle_b[i]-qnorm(0.975)*sqrt(diag(H)[2])
    UC_b[i]<-mle_b[i]+qnorm(0.975)*sqrt(diag(H)[2])
    if((LC_b[i]<=b)&(b<=UC_b[i])){
      count_b=count_b+1
    }
    LC_alpha[i]<-mle_alpha[i]-qnorm(0.975)*sqrt(diag(H)[3])
    UC_alpha[i]<-mle_alpha[i]+qnorm(0.975)*sqrt(diag(H)[3])
    if((LC_alpha[i]<=d)&(d<=UC_alpha[i])){
      count_alpha=count_alpha+1
    }
    
  }
  #Calculate Average Bias
  ABias_a<-sum(mle_a-a)/N
  ABias_b<-sum(mle_b-b)/N
  ABias_alpha<-sum(mle_alpha-d)/N
  print(cbind(ABias_a,ABias_b,ABias_alpha))
  #Calculate RMSE
  RMSE_a<-sqrt(sum((a-mle_a)^2)/N)
  RMSE_b<-sqrt(sum((b-mle_b)^2)/N)
  RMSE_alpha<-sqrt(sum((d-mle_alpha)^2)/N)
  print(cbind(RMSE_a,RMSE_b,RMSE_alpha))
}

#########################################################################
#fy<-function(y,mu,gm,rho){
#  fxn<- mu*gm*rho*y^(-gm-1)*exp(y^(-gm))*((rho*(exp((y^(-gm)))-1)+1))^(-mu-1)
#  return(fxn)
#  }

#Moment<- function(mu, gm, rho, r){
#  f<- function(y, mu, gm, rho, r){(y^r)*(fy(y, mu, gm, rho))}
#  results <- integrate(f, inf, 0, subdivisions= 1000)$value
#  return(results)
#  }

#Moment(2,1,2,1)



#gm=0.6
#rho=0.7
#mu=0.5
#r=1
#f1<- y^r*(mu*gm*rho*y^(-gm-1)*exp(y^(-gm))*((rho*(exp((y^(-gm)))-1)+1))^(-mu-1))
#results <- integrate(f1, 0, Inf, subdivisions= 1000) 

#integrate( f1 , 0 , Inf)



#fy<-function(y,mu,gm,rho){
#  fxn<- (y^r)*mu*gm*rho*y^(-gm-1)*exp(y^(-gm))*((rho*(exp((y^(-gm)))-1)+1))^(-mu-1)
#  return(fxn)
#}

#integrate( fy , 0 , Inf)
