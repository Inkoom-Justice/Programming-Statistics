

#PDF OF OCG#
x=seq(0,2,0.00001)
alpha=c(1.8,2,1.99,1,1.61)
beta=c(2.44,1.8,3.1,1,2)
lambda=c(1,2.6,1,1,3)
mu=c(1,1,1,1,1)

#mu=c(4.5,5.85,3.85,5.85)

fg1<- (lambda[1]*beta[1]*exp((x - alpha[1])/mu[1])*exp(-exp((x - alpha[1])/mu[1]))*(1 - exp(-exp((x - alpha[1])/mu[1])))^(beta[1] - 1)*exp(-exp((x - alpha[1])/mu[1]))^(-beta[1] - 1)*exp(((1 - exp(-exp((x - alpha[1])/mu[1])))/exp(-exp((x - alpha[1])/mu[1])))^beta[1])*exp(lambda[1]*(1 - exp(((1 - exp(-exp((x - alpha[1])/mu[1])))/exp(-exp((x - alpha[1])/mu[1])))^beta[1])))/mu[1])
fg2<- (lambda[2]*beta[2]*exp((x - alpha[2])/mu[2])*exp(-exp((x - alpha[2])/mu[2]))*(1 - exp(-exp((x - alpha[2])/mu[2])))^(beta[2] - 1)*exp(-exp((x - alpha[2])/mu[2]))^(-beta[2] - 1)*exp(((1 - exp(-exp((x - alpha[2])/mu[2])))/exp(-exp((x - alpha[2])/mu[2])))^beta[2])*exp(lambda[2]*(1 - exp(((1 - exp(-exp((x - alpha[2])/mu[2])))/exp(-exp((x - alpha[2])/mu[2])))^beta[2])))/mu[2])
fg3<- (lambda[3]*beta[3]*exp((x - alpha[3])/mu[3])*exp(-exp((x - alpha[3])/mu[3]))*(1 - exp(-exp((x - alpha[3])/mu[3])))^(beta[3] - 1)*exp(-exp((x - alpha[3])/mu[3]))^(-beta[3] - 1)*exp(((1 - exp(-exp((x - alpha[3])/mu[3])))/exp(-exp((x - alpha[3])/mu[3])))^beta[3])*exp(lambda[3]*(1 - exp(((1 - exp(-exp((x - alpha[3])/mu[3])))/exp(-exp((x - alpha[3])/mu[3])))^beta[3])))/mu[3])
fg4<- (lambda[4]*beta[4]*exp((x - alpha[4])/mu[4])*exp(-exp((x - alpha[4])/mu[4]))*(1 - exp(-exp((x - alpha[4])/mu[4])))^(beta[4] - 1)*exp(-exp((x - alpha[4])/mu[4]))^(-beta[4] - 1)*exp(((1 - exp(-exp((x - alpha[4])/mu[4])))/exp(-exp((x - alpha[4])/mu[4])))^beta[4])*exp(lambda[4]*(1 - exp(((1 - exp(-exp((x - alpha[4])/mu[4])))/exp(-exp((x - alpha[4])/mu[4])))^beta[4])))/mu[4])
fg5<- (lambda[5]*beta[5]*exp((x - alpha[5])/mu[5])*exp(-exp((x - alpha[5])/mu[5]))*(1 - exp(-exp((x - alpha[5])/mu[5])))^(beta[5] - 1)*exp(-exp((x - alpha[5])/mu[5]))^(-beta[5] - 1)*exp(((1 - exp(-exp((x - alpha[5])/mu[5])))/exp(-exp((x - alpha[5])/mu[5])))^beta[5])*exp(lambda[5]*(1 - exp(((1 - exp(-exp((x - alpha[5])/mu[5])))/exp(-exp((x - alpha[5])/mu[5])))^beta[5])))/mu[5])

#fy1<- (lambda[1]*beta[1]*exp(-x/alpha[1])*(1 - exp(-x/alpha[1]))^(beta[1] - 1)*exp(-x/alpha[1])^(-beta[1] - 1)*exp(((1 - exp(-x/alpha[1]))/exp(-x/alpha[1]))^beta[1])*exp(lambda[1]*(1 - exp(((1 - exp(-x/alpha[1]))/exp(-x/alpha[1]))^beta[1])))/alpha[1])
#fy2<- (lambda[2]*beta[2]*exp(-x/alpha[2])*(1 - exp(-x/alpha[2]))^(beta[2] - 1)*exp(-x/alpha[2])^(-beta[2] - 1)*exp(((1 - exp(-x/alpha[2]))/exp(-x/alpha[2]))^beta[2])*exp(lambda[2]*(1 - exp(((1 - exp(-x/alpha[2]))/exp(-x/alpha[2]))^beta[2])))/alpha[2])
#fy3<- (lambda[3]*beta[3]*exp(-x/alpha[3])*(1 - exp(-x/alpha[3]))^(beta[3] - 1)*exp(-x/alpha[3])^(-beta[3] - 1)*exp(((1 - exp(-x/alpha[3]))/exp(-x/alpha[3]))^beta[3])*exp(lambda[3]*(1 - exp(((1 - exp(-x/alpha[3]))/exp(-x/alpha[3]))^beta[3])))/alpha[3])
#fy4<- (lambda[4]*beta[4]*exp(-x/alpha[4])*(1 - exp(-x/alpha[4]))^(beta[4] - 1)*exp(-x/alpha[4])^(-beta[4] - 1)*exp(((1 - exp(-x/alpha[4]))/exp(-x/alpha[4]))^beta[4])*exp(lambda[4]*(1 - exp(((1 - exp(-x/alpha[4]))/exp(-x/alpha[4]))^beta[4])))/alpha[4])
#fy5<- (lambda[5]*beta[5]*exp(-x/alpha[5])*(1 - exp(-x/alpha[5]))^(beta[5] - 1)*exp(-x/alpha[5])^(-beta[5] - 1)*exp(((1 - exp(-x/alpha[5]))/exp(-x/alpha[5]))^beta[5])*exp(lambda[5]*(1 - exp(((1 - exp(-x/alpha[5]))/exp(-x/alpha[5]))^beta[5])))/alpha[5])



plot(x,fg1,type="l",ylim=c(0,2.5),lty=1,lwd=2,ylab="f(x)", col="blue")
lines(x,fg2,type="l",lwd=2, col="green")
lines(x,fg3,type="l",lwd=2, col="yellow")
lines(x,fg4,type="l",lwd=2, col="red")
lines(x,fg5,type="l",lwd=2, col="brown")

legend("topleft", legend = c(expression(paste(lambda,"=",1," ", alpha, "=", 1.8 ," ", beta, "=", 4.4," ", mu,"=",1)),                             
                             expression(paste(lambda,"=",1," ", alpha, "=", 2.4 ," ", beta, "=", 3.41," ", mu,"=",1)),
                             expression(paste(lambda,"=",1," ", alpha, " = ", 1.9 ," ", beta, "=", 3.1," ", mu,"=",1)),
                             expression(paste(lambda,"=",1," ", alpha, " = ", 1 ," ", beta, "=", 1," ", mu,"=",1)),
                             expression(paste(lambda,"=",3," ", alpha, " = ", 1.61 ," ", beta, "=", 2," ", mu,"=",1))),
       
       fill=c("blue","green" ,"yellow","red","brown"), ncol = 1, cex = 0.6)



