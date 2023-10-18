#CDF OCG#
x=seq(0,9,0.01)
lambda=c(1,1,1,1)
alpha=c(10,10,8.44,6.64)
beta=c(10,3.1,3.58,4.03)
#mu=c(1,1,1,1)



fy1<- (exp(lambda[1]*(1 - exp(((1 - exp(-x/alpha[1]))/exp(-x/alpha[1]))^beta[1]))))
fy2<- (exp(lambda[2]*(1 - exp(((1 - exp(-x/alpha[2]))/exp(-x/alpha[2]))^beta[2]))))
fy3<- (exp(lambda[3]*(1 - exp(((1 - exp(-x/alpha[3]))/exp(-x/alpha[3]))^beta[3]))))
fy4<- (exp(lambda[4]*(1 - exp(((1 - exp(-x/alpha[4]))/exp(-x/alpha[4]))^beta[4]))))


#fy1<- (1 - (exp(lambda[1]*(1 - exp((1 - exp(-x/alpha[1]))/exp(-x/alpha[1]))^beta[1]))))
#fy2<- (1 - exp(lambda[2]*(1 - exp((1 - exp(-x/alpha[2]))/exp(-x/alpha[2]))^beta[2])))
#fy3<- (1 - exp(lambda[3]*(1 - exp((1 - exp(-x/alpha[3]))/exp(-x/alpha[3]))^beta[3])))
#fy4<- (1 - exp(lambda[4]*(1 - exp((1 - exp(-x/alpha[4]))/exp(-x/alpha[4]))^beta[4])))
#fy1<-(1-exp(del1[1]-(del1[1]*exp((1-exp(-exp((y-gm1[1])/mu1[1])))/(exp(-exp((y-gm1[1])/mu1[1]))))^(rho1[1]))))
#fy2<-(1-exp(del1[2]-(del1[2]*exp((1-exp(-exp((y-gm1[2])/mu1[2])))/(exp(-exp((y-gm1[2])/mu1[2]))))^(rho1[2]))))
#fy3<-(1-exp(del1[3]-(del1[3]*exp((1-exp(-exp((y-gm1[3])/mu1[3])))/(exp(-exp((y-gm1[3])/mu1[3]))))^(rho1[3]))))
#fy4<-(1-exp(del1[4]-(del1[4]*exp((1-exp(-exp((y-gm1[4])/mu1[4])))/(exp(-exp((y-gm1[4])/mu1[4]))))^(rho1[4]))))
#fy5<-(1-exp(del1[5]-(del1[5]*exp((1-exp(-exp((y-gm1[5])/mu1[5])))/(exp(-exp((y-gm1[5])/mu1[5]))))^(rho1[5]))))
plot(x,fy1,type="l",ylim=c(0,1.2),lty=1,lwd=2,ylab="S(x)", col="blue")
lines(x,fy2,type="l",lwd=2, col="green")
lines(x,fy3,type="l",lwd=2, col="yellow")
lines(x,fy4,type="l",lwd=2, col="red")
#lines(y,fy5,type="l",lwd=2, col="brown")
#legend("topleft",c("theta=0.1","theta=0.25","theta=0.5","theta=0.75","theta=1"),
#    fill=c("blue","green", "yellow", "red"))
legend("topright", legend = c(expression(paste(lambda,"=",1," ", alpha, "=", 10 ," ", beta, "=", 10)),                             
                             expression(paste(lambda,"=",1," ", alpha, " = ", 10 ," ", beta, "=", 3.1)),
                             expression(paste(lambda,"=",1," ", alpha, " = ", 8.44 ," ", beta, "=", 3.58)),
                             expression(paste(lambda,"=",1," ", alpha, "=", 6.64 ," ", beta, "=", 4.03))),
       fill=c("blue","green" ,"yellow","red"), ncol = 1,
       cex = 0.7)



