 # pmf of Binomial distribution when n=15 & p= 0.4
x <- seq(0,15,by = 1)
y <- dbinom(x,15,0.4)
plot(x,y, type="h", main="Binomial pmf when n =15 & p =0.4")
print(x)
print(y)

x <- seq(0,15,by = 1)
y <- pbinom(x,15,0.4)
plot(x,y, type="s", main="Binomial CDF when n =15 & p =0.4")


x <- seq(0,15,by = 1)
y <- dpois(x,3)
plot(x,y, type="h", main=" Poisson pmf when lambda=3")

x <- seq(0,15,by = 1)
y <- ppois(x,3)
plot(x,y, type="s", main=" Poisson CDF when lambda=3")

# Let X be the R.V follows Binomial distribution with n=15 & p=0.4
# (i) P(X=4) (ii)P(X<=4) (iii)P(X>4)
#(i)
dbinom(4,15,0.4)
#(ii)
pbinom(4,15,0.4)
dbinom(0,15,0.4)+dbinom(1,15,0.4)+dbinom(2,15,0.4)+dbinom(3,15,0.4)+dbinom(4,15,0.4)
#(iii)
1-pbinom(4,15,0.4)
pbinom(4,15,0.4,lower.tail=FALSE)

# Let X be the R.V follows Poisson distribution with lambda=2.5
#find (i)p(x=2) (ii)p(x<=3) (iii) p(x>4)
#(1)
dpois(2,2.5)
#(2)
dpois(3,2.5)
#(3)
ppois(4,2.5,lower.tail=FALSE)

#geometric distribution

dgeom(4,0.05)
pgeom(4,0.05)

#pmf and cdf
x<-seq(0,20,length=21)
y<-dgeom(x,0.2)
plot(x,y,type="h")
y1=pgeom(x,0.2)
plot(x,y1,type="s")

#A representative from the NFL Marketing division randomly selects people on a random street in Chicago loop, until he/she finds a person who attended the last home football game. Let p, the probability that she succeedsin finding such a person, is 0.2 and X denote the number of people asked until the first success. (a)What is the probability that the representative must select 4 .(b)What is the probability that the representative must select more than 6 people before finding one who attended the last home game?
#(a)
dgeom(3,0.2)
#(b)
1-pgeom(5,0.2)

#Each of 12 refrigerators of a certain type has been returned to a distributor because of an audible, high-pitched, oscillating noise when the refrigerator is running. Suppose that 7 of these refrigerators have a defective compressor and the other 5 have less serious problems. If the refrigerators are examined in random order, let X be the number among the first 6 examined that have a defective compressor. Find p(X=5), p(X<=4).

dhyper(5,7,5,6,log=FALSE)
phyper(4,7,5,6,log=FALSE)

#An instructor who taught two sections of engineering statistics last term, the first with 20 students and the second with 30, decided to assign a term project. After all projects had been turned in, the instructor randomly ordered them before grading. Consider the first 15 graded projects.(a)What is the probability that exactly 10 of these are from the second section? b. What is the probability that at least 10 of these are from the second section?
dhyper(10,30,20,15,log=FALSE)
1-phyper(9, 30, 20, 15, log = FALSE)

# Pat is required to sell candy bars to raise money for the 6th grade field trip. There is a 40% chance of him selling a candybar at each house. He has to sell 5 candy bars in all. Let X be of number of houses it takes (a) What is the probability he sells his last candy bar at the 11th house? (b) What is the probability of Pat finishing on or before the 8th house?

dnbinom(6,5,0.4,log=FALSE)
pnbinom(3,5,0.4,log=FALSE)

# Suppose that p = P(male birth) = .5. A couple wishes to have exactly two female children in their family. They will have children until this condition is fulfilled.(a)What is the probability that the family has four children?(b) What is the probability that the family has at most four children.

dnbinom(2,2,0.5,log=FALSE)
pnbinom(2,2,0.5,log=FALSE)


#prob 1
1-dnbinom(6,2,0.05,log=FALSE)


#for random numbers
x = rnorm(10000, mean=0, sd=1)
hist(x,breaks=20, col="red")

x = rnorm(n = 10000)
hist(x, probability = TRUE, col =gray(0.9), main = "normal mu=0,sigma=1")
curve(dnorm(x),col=gray(0.1), add = TRUE)

data_binomial=rbinom(p=0.5,size=10,n=1000)
hist(data_binomial,breaks=100, col="red")

data_poisson = rpois(n=10000,lambda=3)
hist(data_poisson,breaks=100, col="red")

data_negbinom = rnbinom(10000, 10, 0.5)
hist(data_negbinom,breaks=100, col="red")

data_uniform =runif(n=100000,min=0,max=10)
hist(data_uniform,breaks=100, col="red")


#fitting of binomial distribution
x<-c(0,1,2,3,4,5,6,7)
f<-c(0,4,13,28,42,20,6,2)
x_bar<-sum(f*x)/sum(f)
n=7
print(x_bar)
p<-x_bar/n
print(p)
q<-1-p
x1<-seq(0,7,by=1)
f1<-dbinom(x1,n,p)
exp_f<-sum(f)*f1
sum(exp_f)
barplot(matrix(c(f,exp_f),nr=2,byrow=TRUE), beside=T,col=c("orange","blue"), names.arg=x)
legend("topright", c("Observed frequency","Expected frequency of Binomial"), pch=15,col=c("orange","blue"), bty="n")


#fitting of possion distribution
x<-c(0,1,2,3,4,5,6,7,8)
f<-c(162, 193, 115, 83, 44, 24, 19,8,2)
x_bar<-sum(f*x)/sum(f)
lambda<-x_bar
x1<-seq(0,8,by=1)
f1<-dpois(x,lambda)
exp_f<-sum(f)*f1
sum(exp_f)
barplot(matrix(c(f,exp_f),nr=2, byrow=TRUE),beside=T,col=c("yellow","red"), names.arg=x)
legend("topright", c("Observed frequency","Expected frequency of Poisson"), pch=21, col=c("yellow","red"), bty="n")


#another example of fittinf possion distribution
x<-c(0,1,2,3,4,5,6)
f<-c(153, 169, 72, 31, 12, 6, 2)
x_bar<-sum(f*x)/sum(f)
lambda<-x_bar
x1<-seq(0,6,by=1)
f1<-dpois(x,lambda)
exp_f<-sum(f)*f1
sum(exp_f)
barplot(matrix(c(f,exp_f),nr=2, byrow = TRUE), beside=T,col=c("yellow","red"), names.arg=x)
legend("topright", c("Observed frequency","Expected frequency of Poisson"), pch=21, col=c("yellow","red"), bty="n")
 

 #fitting of geometric distribution
x<-c(1,2,3,4,5,6)
f<-c(140,42,12,3,2,1)
x_bar<-sum(f*x)/sum(f)
p<- (1/x_bar)
print(p)
x1<-seq(0,5,by=1)
f1<-dgeom(x1,p)
print(f1)
exp_f<-sum(f)*f1
sum(f)
barplot(matrix(c(f,exp_f),nr=2, byrow = TRUE), beside=T,col=c("blue","green"), names.arg=x)
legend("topright", c("Observed frequency","Expected frequency of geometric distribution"), pch=15, col=c("blue","green"), bty="n")
print(f)
print(exp_f)

#fitting of negative binomial
x<-c(1,2,3,4,5,6,7)
f<-c(131,131,79,37,14,5,2,1)
q<-c(x*x)
x_bar<-sum(f*x)/sum(f)
var<-sum(f*q)/sum(f) - (x_bar)**2
p<-x_bar/var
x1<-seq(0,7,by=1)
f1<-dnbinom(x1,7,p)
barplot(matrix(c(f,exp_f),nr=2, byrow = TRUE), beside=T,col=c("blue","green"), names.arg=x)
legend("topright", c("Observed frequency","Expected frequency of geometric distribution"), pch=15, col=c("blue","green"), bty="n")



#if X follows Normal with mean = 0 and s.d=1 find (i) P(X>1.2),(ii)p(-2<X<2),(iii)p(-1.2<z<1)

 #P(X>1.2)

 x=seq(-3.5,3.5,length=100)

 y=dnorm(x,mean=0,sd=1)

 plot(x,y,type="l",lwd=2,col="red")

 x1=seq(1.2,3.5,length=100)

 y1=dnorm(x1,mean=0,sd=1)

 polygon(c(1.2,x1,3.5),c(0,y1,0),col="green")

 pnorm(1.2, mean = 0, sd = 1, lower.tail=FALSE)

 # #

 # # P(-2<X<2)

x=seq(-3.5,3.5,length=100)
y=dnorm(x,mean=0,sd=1)
plot(x,y,type="l",lwd=2,col="red")
x1=seq(-2,2,length=100)
y1=dnorm(x1,mean=0,sd=1)
polygon(c(-2,x1,2),c(0,y1,0),col="green")
pnorm(2, mean = 0, sd = 1)-pnorm(-2, mean = 0, sd = 1)


 # # p(-1.2<z<1)

 # x=seq(-3.5,3.5,length=100)

 # y=dnorm(x,mean=0,sd=1)

 # plot(x,y,type="l",lwd=2,col="red")

 # x1=seq(-1.2,1,length=100)

 # y1=dnorm(x1,mean=0,sd=1)

 # polygon(c(-1.2,x1,1),c(0,y1,0),col="green")

 # pnorm(1, mean = 0, sd = 1)-pnorm(-1.2, mean = 0, sd = 1)
 
 
 # Fit normal distribution for the following data

 # Class 60-65 65-70 70-75 75-80 80-85 85-90 90-95 95-100

 # Frequency 3 21 150 335 326 135 26

 x<-c(155,165,175,185,195,205,215,225,265)

 f<-c(9,24,51,66,72,48,21,6,3 )

 x_bar=sum(f*x)/sum(f)

 print(x_bar)

 sd<- sqrt(sum(f*(x-x_bar)^2)/sum(f))

 print(sd)

 x1<-c(-Inf,155,165,175,185,195,205,215,225,265,Inf)

 z=(x1-x_bar)/sd

 print(z)

 z1<-pnorm(z,0,1)

 print(z1)

 z2<-0

 for(i in 2:10){

 z2[i-1] <- z1[i]-z1[i-1]

 }

 print(z2)

 exp_f=sum(f)*z2

 print(exp_f)

 z3<-0

 for(i in 2:9){

 z3[i-1] <- exp_f[i]

 }

 print(z3)

 barplot(matrix(c(f,z3),nr=2, byrow = TRUE), beside=T,

 col=c("blue","green"), names.arg=x)

 legend("topright", c("Observed frequency","Expected frequency of normal distribution"), pch=15, col=c("blue","green"), bty="n")
 
 
 # fit exponential distribution to the data

 # 0-3 3-6 6-9 9-12 12-15 5 & above

 # 190 70 25 10 4 1

 x<-c(1.5,4.5,7.5,10.5,13.5,16.5)

 f<-c(190,70,25,10,4,1)

 x_bar=sum(f*x)/sum(f)

 print(x_bar)

 x<-c(0,3,6,9,12,15,18)

 f1=pexp(x,1/x_bar)

 z2<-0

 for(i in 2:7){

 z2[i-1]<-sum(f)*(f1[i]-f1[i-1])

 }

 print(z2)

 barplot(matrix(c(f,z2),nr=2, byrow = TRUE), beside=T, col=c("blue","green"))

#example
x<-c(10,30,50,75,105,150)

 f<-c(41,19,16,13,9,2)

 x_bar=sum(f*x)/sum(f)

 print(x_bar)

 x<-c(0,20,40,60,90,120,180)

 f1=pexp(x,1/x_bar)

 z2<-0

 for(i in 2:7){

 z2[i-1]<-sum(f)*(f1[i]-f1[i-1])

 }

 print(z2)

 barplot(matrix(c(f,z2),nr=2, byrow = TRUE), beside=T, col=c("blue","green"))
library (intoo)
library (bivariate)
library (MASS)
B=matrix(c(1/12,0,1/18,1/6,1/9,1/4,0,1/5,2/15),nrow=3,ncol=3,byrow = TRUE)
rownames (B)=c ("1", "2", "3")
colnames (B)=c ("1", "2", "3")
B
f = cbvpmf (B)
par(mar=c(1,1,1,1))
plot (f, TRUE)

dbinom(7,10,0.50)

x<-seq(7,10,0.50)
p1=dbinom(x,10,0.50)
print(p1)
sum(p1)
dpois(6,3)
sum(dpois(0:6,lambda=5))