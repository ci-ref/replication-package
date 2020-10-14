library(lmerTest)
library(MuMIn)
library(car)
library(ggplot2)
library(beanplot)
library(effsize)

data$time_before_ci<-ifelse(data$CI_adopted==0,data$time,0)
data$time_after_ci<-ifelse(data$CI_adopted==1,data$time,0)

filterData<- subset(data, data$Ref.Breadth<quantile(data$Ref.Breadth, 0.97))
model <- lm(Ref.Breadth~ time_before_ci +time_after_ci 
           + log(TotalDev)  + log(AgeAtCI)+ log(nbrReleases)+log(TotalComm)
           ,data = filterData)
summary(model)
anova(model)
vif(model)
ggplot(data, aes(x=time, y= Ref.Breadth,group=time)) +geom_boxplot()#+ coord_cartesian(ylim = c(0,20))
ggplot(filterData, aes(x=time, y= Ref.Breadth,group=time)) +geom_boxplot()#+ coord_cartesian(ylim = c(0,20))