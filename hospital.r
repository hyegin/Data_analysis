# Data_analysis
Some fun data analysis stuff
summary(HospFull)
sd(HospFull$Length)
sd(HospFull$Infect)
sd(HospFull$Culture)
sd(HospFull$Bed)

#Histograms
par(mfrow=c(2,2))
hist(HospFull$Length,main = "Distribution of Length",xlab = "Lengths in days")
hist(HospFull$Infect,main = "Distribution of Infect",xlab = "estimated percentage of getting infection")
hist(HospFull$Culture,main = "Distribution of Culture",xlab = "Ratio of cultures performed to patients, times 100")
hist(HospFull$Bed,main = "Distribution of Bed",xlab = "average number of beds")

#Boxplots
par(mfrow=c(2,2))
boxplot(HospFull$Length,main = "Distribution of Length", xlab = "Lengths in days", horizontal = TRUE)
boxplot(HospFull$Infect,main = "Distribution of Infect",xlab = "estimated percentage of getting infection", horizontal = TRUE)
boxplot(HospFull$Culture,main = "Distribution of Culture",xlab = "Ratio of cultures performed to patients, times 100", horizontal = TRUE)
boxplot(HospFull$Bed,main = "Distribution of Bed",xlab = "average number of beds", horizontal = TRUE)

#bar plots for categorical
par(mfrow=c(2,2))
med.table=table(HospFull$MedSchool)
barplot(med.table,main = "Is the hospital associated with medical school?")
region.table=table(HospFull$Region)
barplot(region.table, main = "Geographical Region")
two.way = table(HospFull$MedSchool, HospFull$Region)
barplot(two.way, beside = TRUE, legend.text =  rownames(two.way), main = "MedSchool vs Region")

#scatterplots
par(mfrow=c(2,1))
plot(HospFull$Infect,HospFull$Length, main = "Infect vs Lenth", xlab = "Infection" ,ylab = "Length in days")
plot(HospFull$Culture,HospFull$Length, main = "Culture vs Lenth", xlab = "Culture" ,ylab = "Length in days")
plot(HospFull$Bed,HospFull$Length, main = "Bed vs Lenth", xlab = "number of beds" ,ylab = "Length in days")

#Mosaic
two.way = table(HospFull$MedSchool,HospFull$Region)
mosaicplot(two.way, main = "Medschool vs Region")

#fit the model

#fit with infect
infect.model=lm(Length~Infect, data=HospFull)
infect.model
infect.model$coefficients
the.betas = round(infect.model$coefficients,4)
xy.1 = HospFull[which(HospFull$Infect == 4), ]
ei.1 = xy.1$Length - (the.betas[1] + the.betas[2]*xy.1$Infect)
ei.1

#fit with culture
culture.model=lm(Length~Culture,data=HospFull)
culture.model
culture.model$coefficients
the.betas.2 = round(culture.model$coefficients,4)
xy.2 = HospFull[which(HospFull$Culture == 13), ]
ei.2 = xy.2$Length - (the.betas.2[1] + the.betas.2[2]*xy.2$Culture)
ei.2

#fit with bed
bed.model=lm(Length~Bed, data = HospFull)
bed.model
the.betas.3 = round(bed.model$coefficients,4)
xy.3 = HospFull[which(HospFull$Bed == 100), ]
ei.3 = xy.3$Length - (the.betas.3[1] + the.betas.3[2]*xy.3$Bed)
ei.3

#fit with med school
med.model=lm(Length~MedSchool, data = HospFull)
med.model

#fit with region
region.model=lm(Length~Region, data=HospFull)
region.model

#fit with everything
the.model= lm(Length~Infect+Culture+Bed+MedSchool+Region, data=HospFull)
the.model


#ggplot for stuff
#for med school
ggplot(HospFull,aes(y = Length, x = Infect,colour=MedSchool,shape=MedSchool)) + geom_point() + geom_smooth(method="lm",fill = NA)
ggplot(HospFull,aes(y = Length, x = Culture,colour=MedSchool,shape=MedSchool)) + geom_point() + geom_smooth(method="lm",fill = NA)
ggplot(HospFull,aes(y = Length, x = Bed,colour=MedSchool,shape=MedSchool)) + geom_point() + geom_smooth(method="lm",fill = NA)
#For region
ggplot(HospFull,aes(y = Length, x = Infect,colour=Region,shape=Region)) + geom_point() + geom_smooth(method="lm",fill = NA)
ggplot(HospFull,aes(y = Length, x = Culture,colour=Region,shape=Region)) + geom_point() + geom_smooth(method="lm",fill = NA)
ggplot(HospFull,aes(y = Length, x = Bed,colour=Region,shape=Region)) + geom_point() + geom_smooth(method="lm",fill = NA)

#Getting rid of outlier

#get rid of outlier for full model.
cutoff = 0.20
CD = cooks.distance(the.model)
CD[CD > cutoff]
n = length(the.model$residuals) #Counts the number of ei values (which should be n)
p = length(the.model$coefficients) # Counts the number of betas
alpha = 0.01 # You may change this to whatever you like
t.cutoff = qt(1- alpha/2, n-p)
t.cutoff
qt(1-alpha/(2*n), n-p)
ei.s = the.model$residuals/sqrt(sum(the.model$residuals^2)/(length(the.model$residuals) - length(the.model$coefficients)))
outliers = which(abs(ei.s) > t.cutoff)
outliers

library(MASS) #Load the required library if you have not already.
SR = stdres(the.model)
cutoff= 3
hist(SR,main = "Standardized residuals")
SR[abs(SR) > cutoff] # Show all above the poistive cutoff or below the -cutoff 
outliers = which(abs(SR)>t.cutoff)
outliers
outliers = which(SR > cutoff |  SR < -cutoff)
new.data = HospFull[-outliers,]
new.model = lm(Length~Infect+Culture+Bed+MedSchool+Region,data = new.data)

#infect model without outlier
cutoff = 0.20
CD.1 = cooks.distance(infect.model)
CD.1[CD.1 > cutoff]
n.1 = length(infect.model$residuals) #Counts the number of ei values (which should be n)
p.1 = length(infect.model$coefficients) # Counts the number of betas
alpha = 0.01 # You may change this to whatever you like
t.cutoff.1 = qt(1- alpha/2, n.1-p.1)
t.cutoff.1
qt(1-alpha/(2*n.1), n.1-p.1)
ei.s.1 = infect.model$residuals/sqrt(sum(infect.model$residuals^2)/(length(infect.model$residuals) - length(infect.model$coefficients)))
outliers.1 = which(abs(ei.s.1) > t.cutoff)
outliers.1

SR.1 = stdres(infect.model)
cutoff= 3
hist(SR.1,main = "Standardized residuals")
SR.1[abs(SR.1) > cutoff] # Show all above the poistive cutoff or below the -cutoff 
outliers.1 = which(abs(SR.1)>t.cutoff.1)
outliers.1
outliers.1 = which(SR.1 > cutoff |  SR.1 < -cutoff)
new.data.1 = HospFull[-outliers.1,]
new.model.1 = lm(Length~Infect,data = new.data.1)
new.model.1

#culture model without outlier
cutoff = 0.20
CD.2 = cooks.distance(culture.model)
CD.2[CD.2 > cutoff]
n.2 = length(culture.model$residuals) #Counts the number of ei values (which should be n)
p.2 = length(culture.model$coefficients) # Counts the number of betas
alpha = 0.01 # You may change this to whatever you like
t.cutoff.2 = qt(1- alpha/2, n.2-p.2)
t.cutoff.2
qt(1-alpha/(2*n.2), n.2-p.2)
ei.s.2 = culture.model$residuals/sqrt(sum(culture.model$residuals^2)/(length(culture.model$residuals) - length(culture.model$coefficients)))
outliers.2 = which(abs(ei.s.2) > t.cutoff)
outliers.2

SR.2 = stdres(culture.model)
cutoff= 3
hist(SR.2,main = "Standardized residuals")
SR.2[abs(SR.2) > cutoff] # Show all above the poistive cutoff or below the -cutoff 
outliers.2 = which(abs(SR.2)>t.cutoff.2)
outliers.2
outliers.2 = which(SR.2 > cutoff |  SR.1 < -cutoff)
new.data.2 = HospFull[-outliers.2,]
new.model.2 = lm(Length~Culture,data = new.data.2)
new.model.2

#bed model without outlier
new.model.3 = lm(Length~Bed ,data = new.data)
new.model.3

#med model without outlier
new.model.4 = lm(Length~MedSchool, data = new.data)
new.model.4

#region model without outlier
new.model.5 = lm(Length~Region, data = new.data)
new.model.5
#CI
alpha = 0.05
the.CIs = confint(new.model,level = 1-alpha)
the.CIs

the.CIs.1 = confint(new.model.1, level = 1-alpha)
the.CIs.1

the.CIs.2 = confint(new.model.2, level = 1-alpha)
the.CIs.2

#QQplot and errors vs fitted values plots
qqnorm(the.model$residuals)
qqline(the.model$residuals)
ei = the.model$residuals
yhat = the.model$fitted.values
qplot(yhat, ei, data = HospFull) +  ggtitle("Errors vs. Fitted Values") + xlab("Fitted Values") + 
  ylab("Errors") + geom_hline(yintercept = 0,col = "purple")

#qq plot for x variable infect
qqnorm(infect.model$residuals)
qqline(infect.model$residuals)
ei.1 = infect.model$residuals
yhat.1 = infect.model$fitted.values
qplot(yhat.1, ei.1, data = HospFull) +  ggtitle("Errors vs. Fitted Values") + xlab("Fitted Values") + 
  ylab("Errors") + geom_hline(yintercept = 0,col = "purple")

#qq plot for x variable culture
qqnorm(culture.model$residuals)
qqline(culture.model$residuals)
ei.2 = culture.model$residuals
yhat.2 = culture.model$fitted.values
qplot(yhat.2, ei.2, data = HospFull) +  ggtitle("Errors vs. Fitted Values") + xlab("Fitted Values") + 
  ylab("Errors") + geom_hline(yintercept = 0,col = "purple")

#qq plot for x variable bed
qqnorm(bed.model$residuals)
qqline(bed.model$residuals)
ei.3 = bed.model$residuals
yhat.3 = bed.model$fitted.values
qplot(yhat.3, ei.3, data = HospFull) +  ggtitle("Errors vs. Fitted Values") + xlab("Fitted Values") + 
  ylab("Errors") + geom_hline(yintercept = 0,col = "purple")

#qq plot for x variable medschool
qqnorm(med.model$residuals)
qqline(med.model$residuals)
ei.4 = med.model$residuals
yhat.4 = med.model$fitted.values
qplot(yhat.4, ei.4, data = HospFull) +  ggtitle("Errors vs. Fitted Values") + xlab("Fitted Values") + 
  ylab("Errors") + geom_hline(yintercept = 0,col = "purple")

#qq plot for x variable region
qqnorm(region.model$residuals)
qqline(region.model$residuals)
ei.5 = region.model$residuals
yhat.5 = region.model$fitted.values
qplot(yhat.5, ei.5, data = HospFull) +  ggtitle("Errors vs. Fitted Values") + xlab("Fitted Values") + 
  ylab("Errors") + geom_hline(yintercept = 0,col = "purple")

#Testing normality (Shpiro Wilks)
ei = the.model$residuals
the.SWtest = shapiro.test(ei)
the.SWtest

ei.full=new.model$residuals
the.SWtest.full=shapiro.test(ei.full)
the.SWtest.full

ei.1=new.model.1$residuals
the.SWtest.1 = shapiro.test(ei.1)
the.SWtest.1

ei.2=new.model.2$residuals
the.SWtest.2 = shapiro.test(ei.2)
the.SWtest.2

ei.3 = new.model.3$residuals
the.SWtest.3 = shapiro.test(ei.3)
the.SWtest.3

ei.4 = new.model.4$residuals
the.SWtest.4 = shapiro.test(ei.4)
the.SWtest.4

ei.5 = new.model.5$residuals
the.SWtest.5 = shapiro.test(ei.5)
the.SWtest.5

#hypothesis tests
summary(new.model)$coefficients

#new qq plot
qqnorm(new.model$residuals)
qqline(new.model$residuals)
ei.new = new.model$residuals
yhat.new = new.model$fitted.values
qplot(yhat.new, ei.new, data = new.data) +  ggtitle("Errors vs. Fitted Values") + xlab("Fitted Values") + 
  ylab("Errors") + geom_hline(yintercept = 0,col = "purple")

#model criteria
install.packages("MPV")
library(MPV)
All.Criteria = function(the.model){
  p = length(the.model$coefficients)
  n = length(the.model$residuals)
  the.BIC = BIC(the.model)
  the.LL = logLik(the.model)
  the.AIC = AIC(the.model)
  the.PRESS = PRESS(the.model)
  the.R2adj = summary(the.model)$adj.r.squared
  the.results = c(the.LL,p,n,the.AIC,the.BIC,the.PRESS,the.R2adj)
  names(the.results) = c("LL","p","n","AIC","BIC","PRESS","R2adj")
  return(the.results)
}
All.Criteria(the.model)
All.Models = c("Y~X1","Y~X2","Y~X3","Y~X4","Y~X5","Y~X1+X2+X3+X4+X5")
all.model.crit = t(sapply(All.Models,function(M){
  current.model = lm(M,data = HospFull)
  All.Criteria(current.model)
}))
round(all.model.crit,4)

#Forward and backward step wise regression
full.model = new.model
empty.model = lm(Length ~ 1,data = new.data)
library(MASS)
forward.model.AIC = stepAIC(empty.model, scope = list(lower = empty.model, upper= full.model), k = 2,direction = "forward")

backward.model.AIC = stepAIC(full.model, scope = list(lower = empty.model, upper= full.model), k = 2,direction = "backward",trace = TRUE)

FB.model.AIC = stepAIC(empty.model, scope = list(lower = empty.model, upper= full.model), k = 2,direction = "both",trace = FALSE)
