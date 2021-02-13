'              LAB EX 8             '

'1.The length of time for one individual to be served at a 
cafeteria is a random variable having an exponential distribution
with a mean of 4 minutes. What is the probability that a person is
served in less than 3 minutes?'

print(pexp(3,1/4))

'2.The life, in years, of a certain type of electrical switch 
has an exponential distribution with an average life 2 years.
What is the probability it fails during the first year?'

print(pexp(1,1/2))

'3.The study of divorced cases in the western countries, the following distribution is obtained for the time interval (in years) between the day of their marriage and day of their divorce
No .of years: 0-4 4-8 8-12 12-16 12-16 16 & above
No. of persons 192 75 25 11 5 2
Fit an exponential distribution'
x<-c(2,6,10,14,14,18)

f<-c(192,75,25,11,5,2)

x_bar=sum(f*x)/sum(f)

print(x_bar)

x2<-c(0,4,8,12,16,20)
f1=pexp(x2,1/x_bar)

z2<-0

for(i in 2:7){
  
  z2[i-1]<-sum(f)*(f1[i]-f1[i-1])
  
}

print(z2)

barplot(matrix(c(f,z2),nr=2, byrow = TRUE), beside=T, col=c("blue","green"))


'4.Apply fitdistrplus package to fit exponential distribution
for your data (take suitable data)'

library(MASS)
library(fitdistrplus)


fexp=fitdist(mtcars$mpg,"exp")
par(mar=c(1,1,1,1)) 
plot.legend=c("exponential") 

denscomp(fexp,legendtext = plot.legend)
qqcomp(fexp,legendtext = plot.legend)
cdfcomp(fexp,legendtext = plot.legend)
ppcomp(fexp,legendtext = plot.legend)

       
'         LAB EX 9      '
'1.If a random variable X has the gamma distribution 
with ?? = 2 and ?? = 1, find P(1.8 <X< 2.4).'

alpha<-2
beta<-1
find_p=(pgamma(2.4,alpha)-pgamma(1.8,alpha))
print(find_p)

'2.Plot the pdf of gamma distribution with 
(i) ?? = 4 and ?? = 1, 
(ii) ?? = 3 and ?? = 1, 
(iii) ?? = 2 and ?? = 1, 
(iv) ?? = 1 and ?? = 1,'

bayesAB::plotGamma(4,1)
bayesAB::plotGamma(3,1)
bayesAB::plotGamma(2,1)
bayesAB::plotGamma(1,1)

'3.Apply fitdistrplus package to fit gamma distribution 
for your data (take suitable data)'

library(fitdistrplus)
fgam=fitdist(mtcars$mpg,"gamma")
par(mar=c(1,1,1,1)) 
plot.legend=c("gamma") 

denscomp(fgam,legendtext = plot.legend)
qqcomp(fgam,legendtext = plot.legend)
cdfcomp(fgam,legendtext = plot.legend)
ppcomp(fgam,legendtext = plot.legend)

'    LAB EX 10    '
'1.Suppose the random variable X follows a beta distribution with ?? = 1 and ?? = 3.
Find ????(????>1/3)'

print(1-pbeta(1/3,1,3))

'6.
Plot the pdf of beta distribution with 
(i) ?? = 4 and ?? = 1, 
(i) ?? = 3 and ?? = 1, 
(i) ?? = 2 and ?? = 1, 
(i) ?? = 1 and ?? = 1,'

bayesAB::plotBeta(4,1)
bayesAB::plotBeta(3,1)
bayesAB::plotBeta(2,1)
bayesAB::plotBeta(1,1)

'7.Apply fitdistrplus package to 
fit beta distribution for your data (take suitable data)
'
library(fitdistrplus)
#HERE MY DATA IS X WITH VALUES OF MICROBES IN A ONE METER SALT BELT
x <- c(0.0955104277250779, 0.0782381918284555, 0.109683584625186, 
       0.10115721657354, 0.102377369846524, 0.0691699604743083,
       0.0940254652301665, 0.078494747777906, 0.0824474231569216,
       0.087598944591029, 0.114703196347032, 0.0966910484151863,
       0.0995783979120659, 0.090916800949877, 0.111081696404935,
       0.101724137931034, 0.0880689367194321, 0.106699751861042,
       0.0772264084644516, 0.109373466383118, 0.0732657833203429,
       0.139175257731959, 0.0902068720589541, 0.0900240916465903,
       0.103787190855189, 0.0846888376687521, 0.0901782178217822,
       0.0943396226415094, 0.0778525161933234, 0.057852491291837,
       0.0902365266227454, 0.0940445474171976, 0.0834914611005693,
       0.0886513916653852)

fit <- fitdist(x, "beta")
plot(fit, las = 1) # Checking fit











