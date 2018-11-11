# Jeff Leath
# MSDS 6371 Stats Unit11 HW

# Question 2

#load packages
library(readr)
library(ggplot2)

# read in data
autism <- read_csv("Autism.csv")
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
autismlm <- lm(After~Before, data = autism)

newx=autism$After

newx=sort(newx)

prd_c=predict(autismlm, newdata = data.frame(Before=newx),
              interval=c("confidence"),type=c("response"),
              level=.95)
prd_c

#summary of model
summary(autismlm)


# Residual Plot, sourced from a peer.
model1=lm(After~Before, data = autism)
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
resp+geom_segment(aes(y=0,x=.fitted,xend=.fitted, yend=.resid,alpha = 2*abs(.resid)))+guides(alpha=FALSE)

##Histogram of Residuals

extraVal<-fortify(model1)
#fortify helps a lot, worth a google, its a ggplot2 fiunction
ghist<-ggplot(model1,aes(x=.resid))+
  geom_histogram(binwidth=.052,
                 fill='gray22',
                 color='ghostwhite')+
  theme_classic()
ghist+stat_function(fun=dnorm,color="forestgreen",args=list(mean=mean(extraVal$.resid),sd=sd(extraVal$.resid)),size=1)



