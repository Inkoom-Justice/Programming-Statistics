######################## Quantile Function for EJ #######################

quantile<-function(alpha,beta,lambda,p){
  Q1<-(log(1-p))/lambda
  Q2<-(log(1-Q1))^(1/beta)
  Q3<-alpha*log(1+Q2)
  result<-Q3
  return(result)
}

quantile(2,2,9,0.1)



f1<- ((lambda*beta*exp(-x/alpha)*(1 - exp(-x/alpha))^(beta - 1))/alpha)
f2<- (exp(-x/alpha)^(-beta - 1)*exp(((1 - exp(-x/alpha))/exp(-x/alpha))^beta))
f3<- (exp(lambda*(1 - exp(((1 - exp(-x/alpha))/exp(-x/alpha))^beta))))

pdf1<- (f1*f2*f3)


quantile<- (alpha*ln(1 + ln(1 - ln(1 - p)/lambda)^(1/beta)))

######### Moments of the EJ ##########

OCE_PDF<-function(x,alpha,beta,lambda){
  
  OCE_PDF1<- (lambda*beta*x^(-alpha)*alpha*exp(-x^(-alpha))*exp(-x^(-alpha))^(beta - 1)*(1 - exp(-x^(-alpha)))^(-beta - 1)*exp((exp(-x^(-alpha))/(1 - exp(-x^(-alpha))))^beta)*exp(lambda*(1 - exp((exp(-x^(-alpha))/(1 - exp(-x^(-alpha))))^beta)))/x)
  return(OCE_PDF1)
}
OCE_PDF(10,0.1,1,1)

Moment<-function(alpha,beta,lambda){
  results<-1
  r<-seq(1,6,1)
  for(i in 1:length(r)){
    f<-function(x,alpha,beta,lambda,r){(x^r[i])*(OCE_PDF(x,alpha,beta,lambda))}
    results[i]<-integrate(f,lower=0,upper=100000,subdivisions=1000,alpha=alpha,beta=beta,lambda=lambda,r=r)$value
  }
  return(results)
}
r<-seq(1,6,1)
print(r,Moment(0.01,0.01,1))
print(cbind(r,Moment(0.1,1,1),Moment(0.1,0.1,1),Moment(0.1,0.1,1),Moment(0.1,0.1,1)))

