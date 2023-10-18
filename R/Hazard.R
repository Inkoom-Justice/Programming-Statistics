#Hazard OCE#
x=seq(0,4,0.001)

lambda=c(0.48,11,0.5,1)
alpha=c(0.47,10,2.5,6.64)
beta=c(0.08,3.1,0.12,4.03)

#mu=c(1,1,1,1)


fy1<- (lambda[1]*beta[1]*exp(-x/alpha[1])*(1 - exp(-x/alpha[1]))^(beta[1] - 1)*exp(-x/alpha[1])^(-beta[1] - 1)*exp(((1 - exp(-x/alpha[1]))/exp(-x/alpha[1]))^beta[1])/alpha[1])
fy2<- (lambda[2]*beta[2]*exp(-x/alpha[2])*(1 - exp(-x/alpha[2]))^(beta[2] - 1)*exp(-x/alpha[2])^(-beta[2] - 1)*exp(((1 - exp(-x/alpha[2]))/exp(-x/alpha[2]))^beta[2])/alpha[2])
fy3<- (lambda[3]*beta[3]*exp(-x/alpha[3])*(1 - exp(-x/alpha[3]))^(beta[3] - 1)*exp(-x/alpha[3])^(-beta[3] - 1)*exp(((1 - exp(-x/alpha[3]))/exp(-x/alpha[3]))^beta[3])/alpha[3])
fy4<- (lambda[4]*beta[4]*exp(-x/alpha[4])*(1 - exp(-x/alpha[4]))^(beta[4] - 1)*exp(-x/alpha[4])^(-beta[4] - 1)*exp(((1 - exp(-x/alpha[4]))/exp(-x/alpha[4]))^beta[4])/alpha[4])
fy5<- (lambda[5]*beta[5]*exp(-x/alpha[5])*(1 - exp(-x/alpha[5]))^(beta[5] - 1)*exp(-x/alpha[5])^(-beta[5] - 1)*exp(((1 - exp(-x/alpha[5]))/exp(-x/alpha[5]))^beta[5])/alpha[5])


#fy1<- (((lambda*beta)/(alpha))*(exp(-x/alpha))*((1-exp(-x/alpha))^(beta-1))*((exp(-x/alpha))^(-beta-1))*((exp((exp(-x/alpha)-1)^(beta)))))
#fy2<- (((lambda*beta)/(alpha))*(exp(-x/alpha))*((1-exp(-x/alpha))^(beta-1))*((exp(-x/alpha))^(-beta-1))*((exp((exp(-x/alpha)-1)^(beta)))))
#fy3<- (((lambda*beta)/(alpha))*(exp(-x/alpha))*((1-exp(-x/alpha))^(beta-1))*((exp(-x/alpha))^(-beta-1))*((exp((exp(-x/alpha)-1)^(beta)))))
#fy4<- (((lambda*beta)/(alpha))*(exp(-x/alpha))*((1-exp(-x/alpha))^(beta-1))*((exp(-x/alpha))^(-beta-1))*((exp((exp(-x/alpha)-1)^(beta)))))
#fy1<-(1-exp(del1[1]-(del1[1]*exp((1-exp(-exp((y-gm1[1])/mu1[1])))/(exp(-exp((y-gm1[1])/mu1[1]))))^(rho1[1]))))
#fy2<-(1-exp(del1[2]-(del1[2]*exp((1-exp(-exp((y-gm1[2])/mu1[2])))/(exp(-exp((y-gm1[2])/mu1[2]))))^(rho1[2]))))
#fy3<-(1-exp(del1[3]-(del1[3]*exp((1-exp(-exp((y-gm1[3])/mu1[3])))/(exp(-exp((y-gm1[3])/mu1[3]))))^(rho1[3]))))
#fy4<-(1-exp(del1[4]-(del1[4]*exp((1-exp(-exp((y-gm1[4])/mu1[4])))/(exp(-exp((y-gm1[4])/mu1[4]))))^(rho1[4]))))
#fy5<-(1-exp(del1[5]-(del1[5]*exp((1-exp(-exp((y-gm1[5])/mu1[5])))/(exp(-exp((y-gm1[5])/mu1[5]))))^(rho1[5]))))
plot(x,fy1,type="l",ylim=c(0,1.5),lty=1,lwd=2,ylab="h(x)", col="blue")
lines(x,fy2,type="l",lwd=2, col="green")
lines(x,fy3,type="l",lwd=2, col="yellow")
lines(x,fy4,type="l",lwd=2, col="red")
lines(x,fy5,type="l",lwd=2, col="brown")
#legend("topleft",c("theta=0.1","theta=0.25","theta=0.5","theta=0.75","theta=1"),
#    fill=c("blue","green", "yellow", "red"))
legend("top", legend = c(expression(paste(lambda,"=",0.48," ", alpha, "=", 0.47 ," ", beta, "=", 0.08)),                             
                             expression(paste(lambda,"=",11," ",alpha, "=", 10 ," ",beta, "=", 3.1)),
                             expression(paste(lambda,"=",0.5," ",alpha, "=", 2.5 ," ",beta, "=", 0.12)),
                             expression(paste(lambda,"=",1," ",alpha, "=", 6.64 ," ",beta, "=", 4.03))
                             #expression(paste(lambda,"=",3, alpha, " = ", 1.61 , beta, "=", 2))
                             ),
       
       fill=c("blue","green" ,"yellow","red","brown"), ncol = 1.2,
       cex = 0.9)



