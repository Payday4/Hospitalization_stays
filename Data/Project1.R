library(ggplot2)
library(dplyr)
library(corrplot)
library(GGally)
library(car)
library(caret)

hospitalData = read.csv(file.choose())

#check row exist
head(hospitalData)

#missing vals?
colSums(is.na(hospitalData))

summary(hospitalData)

hospitalData$Med.Sc.Aff <- as.factor(hospitalData$Med.Sc.Aff)
hospitalData$Region <- as.factor(hospitalData$Region)
hospitalData = subset(hospitalData, select = -c(ID) )
par(mfrow = c(3,3))

plotVars = c('Lgth.of.Sty', 'Age', 'Inf.Risk', 'R.Cul.Rat', 'R.CX.ray.Rat', 'N.Beds', 'Avg.Pat', 'Avg.Nur', 
             'Pct.Ser.Fac')

for (var in plotVars) {
  hist(hospitalData[[var]], main=paste("Histogram of", var), xlab=var, col="black")
}

par(mfrow=c(1,1))

boxplot(hospitalData[plotVars], las=2, col="red", main="Boxplot")

corrplot(cor(hospitalData[plotVars]), method="number", main="Correlation Matrix")

ggpairs(hospitalData, columns = c('Lgth.of.Sty', 'Age', 'Inf.Risk', 'R.Cul.Rat', 'R.CX.ray.Rat', 'N.Beds', 'Avg.Pat', 'Avg.Nur', 
                                  'Pct.Ser.Fac'))

ggplot(hospitalData, aes(x=Inf.Risk, y=R.Cul.Rat, color=Lgth.of.Sty)) +
  geom_point() +
  labs(title="Infection Risk vs Routine Chest Ratio",
       x="Infection Risk", y="Routine Chest Ratio") +
  scale_color_gradient(low="orange", high="red") +
  theme_minimal()

ggplot(hospitalData, aes(x=Region, y=R.CX.ray.Rat, color=Lgth.of.Sty)) +
  geom_point(size=8) +
  labs(title="Inf",
       x="Region", y="Infection Risk") +
  scale_color_gradient(low="#21295C", high="#DCEDFF") +
  theme_minimal()

simpleMLR = lm(Lgth.of.Sty~Age+Inf.Risk+R.Cul.Rat+R.CX.ray.Rat+N.Beds+Avg.Pat+Avg.Nur+
                 Pct.Ser.Fac+Region+Med.Sc.Aff, 
               data=hospitalData)
simpleMLRInter = lm(Lgth.of.Sty~Age+Inf.Risk+R.Cul.Rat+R.CX.ray.Rat+N.Beds+Avg.Pat+Avg.Nur+
                 Pct.Ser.Fac+Region+Med.Sc.Aff+Inf.Risk*R.CX.ray.Rat+Inf.Risk*R.Cul.Rat, 
               data=hospitalData)
summary(simpleMLR)
summary(simpleMLRInter)
vif(simpleMLR)
plot(simpleMLR)

set.seed(1234)
fitControl<-trainControl(method="repeatedcv",number=10,repeats=1) 
glmnet.fit<-train(Lgth.of.Sty~Age+Inf.Risk+R.Cul.Rat+R.CX.ray.Rat+N.Beds+Avg.Pat+Avg.Nur+
                    Pct.Ser.Fac+Region+Med.Sc.Aff+Inf.Risk*R.CX.ray.Rat+Inf.Risk*R.Cul.Rat,
                  data=hospitalData,
                  method="glmnet",
                  trControl=fitControl,
                  tuneGrid=expand.grid(data.frame(alpha=1,lambda=seq(0,0.05,0.001)))
)
opt.pen<-glmnet.fit$finalModel$lambdaOpt 
coef(glmnet.fit$finalModel,opt.pen)
glmnet.fit


##KNN
set.seed(123)
splitPerc = .7
trainIndices = sample(1:dim(hospitalData)[1],round(splitPerc * dim(hospitalData)[1]))
train = hospitalData[trainIndices,]
test = hospitalData[-trainIndices,]
#fitControl<-trainControl(method="repeatedcv",number=10,repeats=1) 
knn.fit<-train(Lgth.of.Sty~.,
                  data=hospitalData,
                  method="knn",
                  trControl=fitControl,
                  tuneGrid=expand.grid(k=c(1:30)))
knn.fit
knn.fit$results
knn.fit$finalModel$k 


#NB
#fitControl<-trainControl(method="repeatedcv",number=10,repeats=1)
rf.fit<-train(Lgth.of.Sty~.,
               data=hospitalData,
               method="rf",
               trControl=fitControl
)
rf.fit