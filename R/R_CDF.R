#CDF OCG#
x=seq(0,1.5,0.001)

alpha=c(0.81,1,1.44,1.64,1)
beta=c(5,7,4.8,2,10)
lambda=c(0.21,0.1,1,1,1)
mu=c(1,1,1,1,1)

fg1<- (1 - exp(lambda[1]*(1 - exp((((1 - exp(-exp((x - alpha[1])/mu[1])))/exp(-exp((x - alpha[1])/mu[1])))^beta[1])))))
fg2<- (1 - exp(lambda[2]*(1 - exp((((1 - exp(-exp((x - alpha[2])/mu[2])))/exp(-exp((x - alpha[2])/mu[2])))^beta[2])))))
fg3<- (1 - exp(lambda[3]*(1 - exp((((1 - exp(-exp((x - alpha[3])/mu[3])))/exp(-exp((x - alpha[3])/mu[3])))^beta[3])))))
fg4<- (1 - exp(lambda[4]*(1 - exp((((1 - exp(-exp((x - alpha[4])/mu[4])))/exp(-exp((x - alpha[4])/mu[4])))^beta[4])))))
fg5<- (1 - exp(lambda[5]*(1 - exp((((1 - exp(-exp((x - alpha[5])/mu[5])))/exp(-exp((x - alpha[5])/mu[5])))^beta[5])))))

#fy1<- (1 - exp(lambda[1]*(1 - exp(((1 - exp(-x/alpha[1]))/exp(-x/alpha[1]))^beta[1]))))
#fy2<- (1 - exp(lambda[2]*(1 - exp(((1 - exp(-x/alpha[2]))/exp(-x/alpha[2]))^beta[2]))))
#fy3<- (1 - exp(lambda[3]*(1 - exp(((1 - exp(-x/alpha[3]))/exp(-x/alpha[3]))^beta[3]))))
#fy4<- (1 - exp(lambda[4]*(1 - exp(((1 - exp(-x/alpha[4]))/exp(-x/alpha[4]))^beta[4]))))


#fy1<- (1 - (exp(lambda[1]*(1 - exp((1 - exp(-x/alpha[1]))/exp(-x/alpha[1]))^beta[1]))))
#fy2<- (1 - exp(lambda[2]*(1 - exp((1 - exp(-x/alpha[2]))/exp(-x/alpha[2]))^beta[2])))
#fy3<- (1 - exp(lambda[3]*(1 - exp((1 - exp(-x/alpha[3]))/exp(-x/alpha[3]))^beta[3])))
#fy4<- (1 - exp(lambda[4]*(1 - exp((1 - exp(-x/alpha[4]))/exp(-x/alpha[4]))^beta[4])))
#fy1<-(1-exp(del1[1]-(del1[1]*exp((1-exp(-exp((y-gm1[1])/mu1[1])))/(exp(-exp((y-gm1[1])/mu1[1]))))^(rho1[1]))))
#fy2<-(1-exp(del1[2]-(del1[2]*exp((1-exp(-exp((y-gm1[2])/mu1[2])))/(exp(-exp((y-gm1[2])/mu1[2]))))^(rho1[2]))))
#fy3<-(1-exp(del1[3]-(del1[3]*exp((1-exp(-exp((y-gm1[3])/mu1[3])))/(exp(-exp((y-gm1[3])/mu1[3]))))^(rho1[3]))))
#fy4<-(1-exp(del1[4]-(del1[4]*exp((1-exp(-exp((y-gm1[4])/mu1[4])))/(exp(-exp((y-gm1[4])/mu1[4]))))^(rho1[4]))))
#fy5<-(1-exp(del1[5]-(del1[5]*exp((1-exp(-exp((y-gm1[5])/mu1[5])))/(exp(-exp((y-gm1[5])/mu1[5]))))^(rho1[5]))))
plot(x,fg1,type="l",ylim=c(0,1.2),lty=1,lwd=2,ylab="F(x)", col="blue")
lines(x,fg2,type="l",lwd=2, col="green")
lines(x,fg3,type="l",lwd=2, col="yellow")
lines(x,fg4,type="l",lwd=2, col="red")
lines(x,fg5,type="l",lwd=2, col="brown")
#legend("topleft",c("theta=0.1","theta=0.25","theta=0.5","theta=0.75","theta=1"),
#    fill=c("blue","green", "yellow", "red"))
legend("topleft", legend = c(expression(paste(lambda,"=",0.21," ", alpha, "=", 0.81 ," ", beta, "=", 5 ," ", mu,"=",1)),                             
                             expression(paste(lambda,"=",0.1," ", alpha, "=", 1 ," ", beta, "=", 7," ", mu,"=",1)),
                             expression(paste(lambda,"=",1," ", alpha, "=", 1.44 ," ", beta, "=", 4.8," ", mu,"=",1)),
                             expression(paste(lambda,"=",1," ", alpha, "=", 1.64 ," ", beta, "=", 3," ", mu,"=",1)),
                             expression(paste(lambda,"=",1," ", alpha, "=", 1 ," ", beta, "=", 10," ", mu,"=",1))),
       fill=c("blue","green" ,"yellow","red","brown"), ncol = 3, cex = 0.7)


