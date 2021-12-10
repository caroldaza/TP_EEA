# La distribución teórica es una Beta(5,7). Por Bayes la posterior es la likelihood x prior.
#Prior es beta(1,1) no informativa y likelihood es una binomial con p=0.45
posterior <- function(x){
  dbeta(x,5,5)*dbinom(3,10,x)
}

# proposed distribution, we know how to sample from it easily. N(mu,1)
proposal <- function(x){
  min(max(rnorm(1, x, 0.1),0.001),0.999)
}

pasos<-100000
stored <- rep(NA, pasos)
x_viejo <- 0.1

set.seed(10)
for(i in 1:pasos){
  x_nuevo <- proposal(x_viejo)
  ratio <- min(1, (posterior(x_nuevo)/posterior(x_viejo)))
  accept <- runif(1) < ratio
  stored[i] <- ifelse(accept, x_nuevo, x_viejo)
  x_viejo <- stored[i]
}

plot(stored, type = 'p', ylab='Prob')

#hist(stored, breaks = 30)
#curve(1700*dnorm(x,0.46,0.1),add=TRUE)
#curve(5000*dbeta(x,5,7),add=TRUE)

ggplot() + aes(stored)+ geom_histogram(aes(y=..count../sum(..count..)),binwidth=0.01, colour="black", fill="white")+
  labs(x = "Probabilidad")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
 geom_function(fun = dnorm, args = list(mean = 0.4, sd = 0.2), inherit.aes = TRUE)


df <- data.frame(stored)
ggplot(df,aes(stored))+
  geom_histogram(aes(y=..density..), bins = 50, colour= "#000000", fill = "#0099F8")+
  geom_function(fun = dnorm, args = list(mean = 0.4, sd = 0.14), colour="black", size=0)+
  geom_function(fun = dbeta, args = list(shape1 = 5, shape2 = 7), colour="red", size=1)




base <- ggplot() + xlim(-5, 5)

base + geom_function(fun = dnorm, args = list(mean = 0.4, sd = 0.2))

#define range
p = seq(1, 10, length=10)

#create plot of Beta distribution with shape parameters 2 and 10
plot(p, dbinom(p, 10, 0.5), type='l')
