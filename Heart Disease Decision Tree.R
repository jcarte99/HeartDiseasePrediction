library(caret)
library(pROC)
library(rpart)
library(rpart.plot)

df <-read.csv("heart.csv",na.strings=c("NA",""))
summary(df)
str(df)
df$HeartDisease<- factor(df$HeartDisease)

set.seed(101)
trainingdata<- createDataPartition(df$HeartDisease,
                                   p=0.7,
                                   list=FALSE,
                                   times=1)
df.training<- df[trainingdata,]
df.val<- df[-trainingdata,]

dtmodel<- train(HeartDisease~.,
                data=df.training,
                method="rpart",
                na.action = na.pass)
dtmodel
prp(dtmodel$finalModel,type=2,extra=106)

prediction <- predict(dtmodel,newdata=df.val,na.action = na.pass)
confusionMatrix(prediction,df.val$HeartDisease)

tree.probabilities <- predict(dtmodel,newdata=df.val,type='prob',na.action=na.pass)

tree.ROC <- roc(predictor=tree.probabilities$`1`,
                response=df.val$HeartDisease,
                levels=levels(df.val$HeartDisease))
plot(tree.ROC)

tree.ROC$auc
