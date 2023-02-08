library(actuar)

c=1.3 #premium rate
u0=5 #capital at t=0
n=1000 #number of trials
rn=100 #numer of ruin trials
t=100 #time
lambda=1 #poison parameter
alpha=4 #pareto paramter1
beta=3 #pareto paramter2
mu=beta/(alpha-1) # mean of pareto
u=numeric()
r=numeric()
prob.r=numeric()

for (j in 1:n) {
a=rexp(1)
u[1]=u0+a*c - rpareto(1,4,3)
for (i in 2:t) {
a=rexp(1)
u[i]=u[i-1]+a*c-rpareto(1,4,3)
}
ruin=any(u<0)
if(ruin=="TRUE"){
r[j]=1}
else {
r[j]=0}}
prob.ruin=sum(r)/n
prob.ruin


se=prob.ruin/sqrt(n)
se


##### Question 2f - R code

library(actuar)
c=0.88 #premium rate
u0=5 #capital at t=0
n=1000 #number of trials
rn=100 #numer of ruin trials
t=100 #time
lambda=1 #Poisson parameter
alpha=4 #Pareto paramter1
beta=3 #Pareto paramter2
mu=beta/(alpha-1) # mean of pareto
u=numeric()
r=numeric()
prob.r=numeric()

for(k in 1:rn){ #create a loop to run rn trials

	for (j in 1:n){		#create a loop to run n trials
		simcomp=function(t,lambda,mu){ 	#set a function to simulate compound poisson distribution values of claim amounts
			N=rpois(t,lambda)
			aggclaims=rep(0,t)
			for (i in 1:t)
			aggclaims[i]=sum(rpareto(N[i],alpha,beta))
			aggclaims}
		S<-simcomp(t,lambda,mu)		#name a vector of the claim amounts

		for (i in 1:t){ 		#create a loop to track capital over time t
			u[i]=u0 +c*i-0.7*cumsum(S)[i]
			x=any(u<0)}
			if (x=="TRUE"){ 	#record whether the ruin occurs 
				r[j]=1 
		}else if (x=="FALSE"){
				r[j]=0
	
		}
	}
prob.r[k]=sum(r)/n 	#calculate the probability of ruin after 1000 trials

}

mean(prob.r) #calculate the estimate of ruin


se=sd(prob.r)/sqrt(n) #calculate standard error
se




################
rpareto(1,4,3)
