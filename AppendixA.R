#Appendix A A sample session
help.start()
x<- rnorm(50)
y<- rnorm(x)
#rnornm(n, mean=0, sd=1) Will i ever need to change these parameters, or am i only ever going to enter "n"

plot(x,y)
ls()
rm(x,y)

x<-1:20
w<-1+sqrt(x)/2
dummy<-data.frame(x=x, y=x+rnorm(x)*w)
dummy
#could you explain what i need to know/where i can learn more about data frames, matrices and lists? 
#am i creating my own data frame in most cases or just referencing another one that exists?
fm <-lm(y~x, data=dummy)
summary(fm)


fm1<-lm(y~x, data=dummy, weight=1/w^2)
summary(fm1)

attach(dummy)

lrf<-lowess(x,y)

plot(x,y)

lines(x, lrf$y)
abline(0,1,lty=3)
abline(coef(fm))
abline(coef(fm1),col="red")
detach()

plot(fitted(fm), resid(fm),xlab="Fitted values", ylab="Residuals", main="Residuals vs Fitted")

qqnorm(resid(fm), main= "Residuals rankit Plot")
rm(fm,fm1, lrf, x, dummy)

library(datasets)
filepath<- system.file("data", "morley.tab", package="datasets")
filepath
file.show(filepath)
mm<- read.table(filepath)
mm
mm$Expt<-factor(mm$Expt)
mm$Run<-factor(mm$Run)
attach(mm)
plot(Expt, Speed, main="Speed of Light Data", xlab="Experiment No.", ylab="Speed")
#Expt= Experiment number from the morley.tab data set, plotted on the x axis and hence listed first (plot(x,y...))
#Speed= speed data from morley.tab plotted on the y axis.
#main in this case refers to the Main title of the graph
#xlab means x label, ylab is y label
fm<- aov(Speed~Run+Expt, data=mm)
summary(fm)
fm0<-update(fm,.~.-Run)
anova(fm0,fm)

detach()
rm(fm,fm0)
#what exactly does detach do? do you often use it?

x<- seq(-pi, pi, len=50)
y<-x
f<- outer(x,y,function(x,y) cos(y)/(1+x^2))
#"f is a square matrix, with rows and columns indezed by x and y respectively, of values of the function cos(y)/(1+x^2)
#what does this mean?

oldpar<-par(no.readonly=TRUE)
par(pty="s")
contour(x,y,f)
#ctrl enter the above script...i really do not know how i created this plot, or how to change it. please explain
#countor is the creator of the contour plot
contour(x,y,f,nlevels=15, add=TRUE)
