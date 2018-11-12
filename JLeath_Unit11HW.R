# Jeff Leath
# MSDS 6371 Stats Unit11 HW

# Question 2

#load packages
library(readr)
library(ggplot2)

# read in data
autism <- read_csv("Autism2.csv")
  cols(
    Child = col_integer(),
    Before = col_integer(),
    After = col_integer()
  )

#check data
str(autism)

# view data
View(autism)

#Linear Regression
model1 <- lm(Prev~Year, data = autism)

#Basic plot
plot(Prev ~ Year, data = autism)
abline(model1)

#Scatter Plot with CI
ggplot(autism, aes(x = Year, y = Prev)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")
newx=autism$Year
newx=sort(newx)
prd_c=predict(model1, newdata = data.frame(Year=newx),
              interval=c("confidence"),type=c("response"),
              level=.95)
prd_c

#summary of model
summary(model1)

#i.	A scatterplot with the following included on the graph: 
#   regression line, confidence intervals of the regression line,
#   and prediction intervals of the regression line.

p2a <- ggplot(autism,aes(Year,Prev)) +
  geom_smooth(method='lm',formula=autism$Prev~autism$Year)
p2a

# Scatterplot of the residuals

resp<-ggplot(model1, aes(.fitted, .resid))+
  geom_point(aes(color = .resid)) +
  scale_color_gradient2(low = "blue",
                        mid = "white",
                        high = "red") +
  guides(color = FALSE)
resp<-resp+
  stat_smooth(method="lm",se=F)+
  geom_hline(yintercept=0,
             col="red",
             linetype="dashed")
resp<-resp+
  xlab("Fitted values")+
  ylab("Residuals")
resp<-resp+theme_classic()
resp+geom_segment(aes(y=0,x=.fitted,
                      xend=.fitted,
                      yend=.resid,
                      alpha = 2*abs(.resid)))+guides(alpha=FALSE)
## Equation
summary(model1)

##Histogram of Residuals

extraVal<-fortify(model1)
#fortify helps a lot, worth a google, its a ggplot2 function
ghist<-ggplot(model1,aes(x=.resid))+
  geom_histogram(binwidth=.052,
                 fill='gray22',
                 color='ghostwhite')+
  theme_classic()
ghist+stat_function(fun=dnorm,color="forestgreen",
                    args=list(mean=mean(extraVal$.resid),
                              sd=sd(extraVal$.resid)),size=1)

## QQ Plots

diagPlot<-function(model1){
p1<-ggplot(model1, aes(.fitted, .resid))+geom_point()
p1<-p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
p1<-p1+xlab("Fitted values")+ylab("Residuals")
p1<-p1+ggtitle("Residual vs Fitted Plot")+theme_bw()

p2<-ggplot(model1, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)
p2<-p2+geom_abline(aes(qqline(.stdresid)))+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
p2<-p2+ggtitle("Normal Q-Q")+theme_bw()

p3<-ggplot(model1, aes(.fitted, sqrt(abs(.stdresid))))+geom_point(na.rm=TRUE)
p3<-p3+stat_smooth(method="loess", na.rm = TRUE)+xlab("Fitted Value")
p3<-p3+ylab(expression(sqrt("|Standardized residuals|")))
p3<-p3+ggtitle("Scale-Location")+theme_bw()

p4<-ggplot(model1, aes(seq_along(.cooksd), .cooksd))+geom_bar(stat="identity", position="identity")
p4<-p4+xlab("Obs. Number")+ylab("Cook's distance")
p4<-p4+ggtitle("Cook's distance")+theme_bw()

p5<-ggplot(model1, aes(.hat, .stdresid))+geom_point(aes(size=.cooksd), na.rm=TRUE)
p5<-p5+stat_smooth(method="loess", na.rm=TRUE)
p5<-p5+xlab("Leverage")+ylab("Standardized Residuals")
p5<-p5+ggtitle("Residual vs Leverage Plot")
p5<-p5+scale_size_continuous("Cook's Distance", range=c(1,5))
p5<-p5+theme_bw()+theme(legend.position="bottom")

p6<-ggplot(model1, aes(.hat, .cooksd))+geom_point(na.rm=TRUE)+stat_smooth(method="loess", na.rm=TRUE)
p6<-p6+xlab("Leverage hii")+ylab("Cook's Distance")
p6<-p6+ggtitle("Cook's dist vs Leverage hii/(1-hii)")
p6<-p6+geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed")
p6<-p6+theme_bw()

return(list(rvfPlot=p1, qqPlot=p2, sclLocPlot=p3, cdPlot=p4, rvlevPlot=p5, cvlPlot=p6))
}


