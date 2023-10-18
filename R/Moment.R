######################## Quantile Function for EJ #######################

quantile<-function(alpha,beta,lambda,p){
  Q1<-(log(1-p))/lambda
  Q2<-(log(1-Q1))^(1/beta)
  Q3<-alpha*log(1+Q2)
  result<-Q3
  return(result)
}

quantile(2,2,9,0.1)




######### Moments of the EJ ##########

OCE_PDF<-function(x,alpha,beta,lambda){
  
  OCE_PDF1<- (lambda*beta*exp(-x/alpha)*(1 - exp(-x/alpha))^(beta - 1)*exp(-x/alpha)^(-beta - 1)*exp(((1 - exp(-x/alpha))/exp(-x/alpha))^beta)*exp(lambda*(1 - exp(((1 - exp(-x/alpha))/exp(-x/alpha))^beta)))/alpha)
  return(OCE_PDF1)
}
OCE_PDF(1,1,1,1)

Moment<-function(alpha,beta,lambda){
  results<-1
  r<-seq(1,6,1)
  for(i in 1:length(r)){
    f<-function(x,alpha,beta,lambda,r){(x^r[i])*(OCE_PDF(x,alpha,beta,lambda))}
    results[i]<-integrate(f,lower=0,upper=Inf,subdivisions=1000,alpha=alpha,beta=beta,lambda=lambda,r=r)$value
  }
  return(results)
}
r<-seq(1,6,1)
print(r,Moment(10000,0.0001,1))
print(cbind(r,Moment(8000,0.00005,0.0003),Moment(5000,0.00008,0.0031),Moment(10000,0.00005,0.0001),Moment(5000,0.000085,0.0001)))

