'            LAB EX 4              '
'
1. Generate random numbers from X~Binom(n=8,p=0.5). 
Also generate samples of size 1000 from Binomial(n=8,p1=0.5) distribution
'
b<-rbinom(10,size=8,0.5)
set.seed(1)
x1<-sample(1000)
b1<-rbinom(x1,size=8,0.5)
print(b1)

'
2. Generate random numbers from X???Poisson(lambda=4). 
Also generate samples of size 1000 from Poisson(lambda=4). distribution
'
p<-rpois(10,4)  
print(p)
p1<-rpois(x1,4)
print(p1)



'              LAB EX 5                 '

'1.In a certain city district, the need for money to buy drugs is stated 
as the reason for 75% of all thefts. 
Find the probability that among the next 5 theft cases reported in 
this district, (a) exactly 2 resulted from the need for money to buy drugs; 
(b) at most 3 resulted from the need for money to buy drugs.'

exactly_twice=dbinom(2,5,0.75)
print(exactly_twice)
atmost_thrice=pbinom(3,5,0.75)
print(atmost_thrice)

'2.It is conjectured that an impurity exists in 30% of all drinking wells
 in a certain rural community. In order to gain some insight into the true 
 extent of the problem, it is determined that some testing is necessary. 
 It is too expensive to test all of the wells in the area, so 10 are 
 randomly selected for testing.
(a) Using the binomial distribution, what is the probability that 
exactly 3 wells have the impurity,  assuming that the conjecture is correct? 
(b) What is the probability that more than 3 wells are impure?'

exactly_3_wells=dbinom(3,10,0.3)
print(exactly_3_wells)
more_than_3_well=1-pbinom(3,10,0.3)
print(more_than_3_well)




'3.Plot PMF and CDF when n=10, p=0.6'
library(plotly)
k<- seq(0,10,by=1)
plot(k,dbinom(k,10,0.6),type="h")
#ggplot2::qplot(k,dbinom(k,10,0.6))

plot(k,pbinom(k,10,0.6),type = "s")

'4. number of heads : 0    1    2  3
         frequency : 36   40   22  2
FIT DIST'
n<-100
x<-seq(0,3,by=1)
f<-c(36,40,22,2)
x_bar<-sum(f*x)/sum(f)

print(x_bar)
p<-x_bar/n
print(p)
q<-1-p
x1<-seq(0,3,by=1)
f1<-dbinom(x1,n,p)
exp_f<-sum(f)*f1
sum(exp_f)
barplot(matrix(c(f,exp_f),nr=2,byrow=TRUE), beside=T,col=c("orange","blue"), names.arg=x)
legend("topright", c("Observed frequency","Expected frequency of Binomial"), pch=15,col=c("orange","blue"), bty="n")








'              LAB EX 6                 '
'1.The following mistakes per page were observed in a book
MISTAKES   : 0    1  2 3 4
NO.OF.PAGES: 211 90 19 5 0
'

x<-seq(0,4,by=1)
f<-c(211,90, 19, 5, 0)
x_bar<-sum(f*x)/sum(f)
lambda<-x_bar
x1<-seq(0,4,by=1)
f1<-dpois(x,lambda)
exp_f<-sum(f)*f1
sum(exp_f)
barplot(matrix(c(f,exp_f),nr=2, byrow=TRUE),beside=T,col=c("yellow","red"), names.arg=x)
legend("topright", c("Observed frequency","Expected frequency of Poisson"), pch=21, col=c("yellow","red"), bty="n")

'2.In a certain industrial facility, accidents occur infrequently. It is known that the 
probability of an accident on any given day is 0.005 and accidents are independent of 
each other. (a) What is the probability that in any given period of 400 days there will 
be an accident on one day? (b) What is the probability that there are at most three days
with an accident?'

p<-0.005
n<-400
x1<-1
x2<-3
one_day<-dbinom(1,400,0.005)
print(one_day)
atmost_three_days<-pbinom(3,400,0.005)
print(atmost_three_days)

'3.On average, 3 traffic accidents per month occur at a certain intersection. 
What is the probability that in any given month at this intersection 
(a) exactly 5 accidents will occur? 
(b) fewer than 3 accidents will occur? 
(c) at least 2 accidents will occur?'

mean_acc=3
lamda=3
exactly_five_accidents=dpois(5,lambda = lamda)
print(exactly_five_accidents)

fewer_than_3_accidents=ppois(2,lambda = lamda)
print(fewer_than_3_accidents)

atleast_2_accidents=ppois(2,lambda = lamda)
print(atleast_2_accidents)





'              LAB EX 7                 '

'1.The daily amount of coffee, in liters, dispensed by a machine located in an 
airport lobby is a random variable X having a continuous uniform distribution 
with a = 7 and b = 10. Find the probability that on a given day the amount of 
coffee dispensed by this machine will be 
(a) at most 8.8 liters; 
(b) more than 7.4 liters but less than 9.5 liters;
(c) at least 8.5 liters'

print(punif(8.8, min = 7, max = 10, log = FALSE))

print(punif(9.5,min=7,max=10,log=FALSE)-punif(7.4,min=7,max=10,log=FALSE))

print(1-punif(8.5,min=7,max=10,log=FALSE))


'2.Given a standard normal distribution, find the area under the curve that lies
(a) to the left of z = -1.39; 
(b) to the right of z = 1.96;
(c) find the value of k such that P(Z>k)=0.2946'

print(pnorm(1.39))
print(1-pnorm(1.96))
print(qnorm(1-0.2946))




'3.The loaves of rye bread distributed to local stores by a certain bakery 
have an average length of 30 centimeters and a standard deviation of 2 centimeters.
Assuming that the lengths are normally distributed, what percentage of the loaves are 
(a) longer than 31.7 centimeters? 
(b) between 29.3 and 33.5 centimeters in length? 
(c) shorter than 25.5 centimeters?'

mean=30
sd=2
print(pnorm(31.7,30,2))
print(pnorm(33.5,30,2)-pnorm(29.3,30,2))
print(1-pnorm(25.5,30,2))


'4.Fit normal distribution

ci 60-65 65-70 70-75 75-80 80-85 85-90 90-95 95-100
f5 25 150 335 326 135 26 8'


x5<-c(62.5,67.5,72.5,77.5,82.5,87.5,92.5,97.5)
f5<-c(5,25,150,335,326,135,26,8 )
x_bar5=sum(f5*x5)/sum(f5)
sd<- sqrt(sum(f5*(x5-x_bar5)^2)/sum(f5))
x6<-c(-Inf,60,65,70,75,80,85,90,95,100,Inf) 
z=(x6-x_bar5)/sd
z1<-pnorm(z,0,1)
z2<-0 
for(i in 2:10){
  z2[i-1] <- z1[i]-z1[i-1]
}
print(z2) 
exp_f=sum(f5)*z2  #expected frequency
print(exp_f)
z3<-0
for(i in 2:9){
  z3[i-1] <- exp_f[i]
}
print(z3) 
barplot(matrix(c(f5,z3),nr=2, byrow = TRUE), beside=T,col=c("blue","green"), names.arg=x5) 
legend("topright", c("Observed frequency","Expected frequency of normal distribution"), pch=15, col=c("blue","green"), bty="n")

















