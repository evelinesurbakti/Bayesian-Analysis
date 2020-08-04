library(rjags)

## EXERCISE 1
#data list
misprints <- list(N=6, x = c(3, 4, 2, 1, 2, 3),alpha=9,beta=6)
#JAGS posterior
jmodel=jags.model(file="misprints.model", data=misprints)

samps=jags.samples(jmodel,"lambda",n.iter=1e3)

plot(density(samps$lambda),col=3,lwd=2,lty=1,xlab="date",main="xxx", xlim=c(0,5))

# (b) Show that the result is approximately equal to the Gamma(24; 12) posterior obtained using conjugacy.
# Do this by plotting a density estimate of your sampled lambda values together with the Gamma(24; 12)
# posterior.

#theoretical
alpha=24
beta=12

curve(dgamma(x, alpha, beta),add=T,col=2,lwd=2,lty=2) # likelihood (renormalised for comparison with prior and posterior; not actually a function of the date)
legend("topright", c("JAGS posterior","theoretical posterior"), lty=c(1,2), col=c(3,2), lwd=c
       (2,2))
#total= 3+4+2+1+2+3 #15# a) Use the posterior distribution Gammma(24; 12) from Exercise 1 as a prior distribution for this new data

#n=6
#alpha0=alpha+total
#beta0=beta+n

#curve(dgamma(x, alpha0, beta0),add=T,col=1,lwd=2,lty=2) # theoretical posterior
#>curve(dpois(x, lambda),add=T,col=4,lwd=2,lty=2) # prior
#we dont need prior and likelihood, right?
#edit legend("topleft", c("prior", "likelihood", "theoretical posterior","JAGS posterior"), lty=c(2,2,2,1), col=c(4,2,3,5), lwd=c(2,2,2,2))
#they are the same because of the conjugacy
t

## EXERCISE 2
#2a asks you to clearly state what the posterior distribution will be by updating the distribution achieved in
  # the previous question, and show that this will be the same as though all the daa was used simultaneously.
  # A few sentences will answer this part.

#2b involves running the code to fit the model as requested, and producing a plot of the results.
# and derive the Gamma posterior. Then show that this posterior distribution is the same as that if the
# data had all been observed simultaneously and a Gamma(9; 6) prior is used.

tenmisprints <- list(N=10, x = c(2, 2, 3, 5, 2, 5, 6, 4, 3, 1),alpha=24,beta=12)

x = c(2, 2, 3, 5, 2, 5, 6, 4, 3, 1)
total1=sum(2, 2, 3, 5, 2, 5, 6, 4, 3, 1)
n1=10

#alpha1=alpha0+total1
#beta1=beta0+n1

#posterior distribution gamma 24,12 which gamma(24+15,12+6)?
#curve(dgamma(x, alpha1, beta1),add=T,col=1,lwd=2,lty=2,xlim=c(0,10))

#versus
#alpha2=9+total1
#beta2=6+n1

#posterior distribution gamma 9,6 which gamma(9+33,6+10)?
#curve(dgamma(x, alpha2, beta2),add=T,col=2,lwd=2,lty=2,xlim=c(0,10))

#(b) Now use JAGS to sample lambda based on the Gammma(24; 12) and these 10 pages of counts.

jmodel=jags.model(file="misprints.model", data=tenmisprints)
samps=jags.samples(jmodel,"lambda",n.iter=1e3)

plot(density(samps$lambda),col=3,lwd=2,xlab="x",main="Density Estimation Sample 24,12", xlim=c(0,6))

alpha3=24+total1
beta3=12+n1
#posterior distribution gamma 24,12 which gamma(24+33,12+10) < theoretical posterior
curve(dgamma(x, alpha3, beta3),add=T,col=2,lwd=2,lty=2)
legend("topright", c("JAGS posterior","theoretical posterior"), lty=c(1,2), col=c(3,2), lwd=c
       (2,2))
#mean(samps$lambda)
#sd(samps$lambda)

#plot(samps$lambda,t='l')
#plot(density(samps$lambda))


## EXERCISE 3
#data list
exe3 <- list(N=16, x = rpois(16,samps$lambda,))
#JAGS posterior
jmodel=jags.model(file="misprints.model", data=exe3)

set.seed(16)
x<-rpois(16,samps$lambda[2])

y = rpois(16, lambda = samps$lambda)
summary(y)
hist(y)


set.seed(16)
list1 = list() # Make an empty list to save output in
for (i in 1:1000) { # Indicate number of iterations with "i"
  list1[[i]] = rpois(16, lambda = samps$lambda[i]) # Save output in list for each iteration
}

ccc<-(t(simplify2array(list1)))

meansam<- apply(ccc,1,mean)
minsam<- apply(ccc,1,min)
maxsam<- apply(ccc,1,max)
sdsam<- apply(ccc,1,sd)

library(ggplot2)
sixteen<-c(misprints$x,tenmisprints$x)


barplot(table(minsam))
minsam

barplot(table(maxsam))

plot(density(meansam))
abline(v=mean(sixteen),col=2)

plot(density(sdsam))
abline(v=sd(sixteen),col=2)


## Create a vector of colors selected based on whether x is <0 or >0
## (FALSE + 1 -> 1 -> "blue";    TRUE + 1 -> 2 -> "red")
cols <- c("red")[(min(sixteen) == 0)]
cols <- ifelse(min(sixteen) == 0,1)

## Pass the colors in to barplot()
barplot(x, col = cols)
barplot(table(minsam),col = cols)


