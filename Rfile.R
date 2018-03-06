set.seed(123456789)
#importing data
df <- read.csv('D:/dir/raw_data.csv', h = T, stringsAsFactors = T)
names(df) <- c('gravel','sand','pass200','Gs','LL','PL','PI','Class','Rank','MDD','OMC')
#data_details
str(df)
summary(df)
#exluding factor variables
df_num <- df[,-c(8,9)]
#package installation
install.packages("elmNN")
library(elmNN)

install.packages("corrgram")
#corrgram draw
corrgram::corrgram(df_num)
#missing value detection

df_num <- df_num[!is.na(df_num),]

index <- sample(x = 1:nrow(df_num), round(0.6*nrow(df_num)))
#train data splitting
traindf <- df_num[index,]
testdf <- df_num[-index,]
#model fitting
model1 <- elmtrain(MDD~gravel + sand + pass200 + Gs + LL + PL ,data = traindf, nhid = 2, actfun = "purelin")
fitted1 <- predict(model1, testdf[,-c(8,9)])
#rmse
sqrt(sum((fitted1 -na.omit( testdf['MDD']))^2))
a<-sum((fitted1 - testdf['MDD'])^2)
b<-sum((model1 - testdf['MDD'])^2)
Y<-testdf['MDD']
summary(fitted1)

print.elmNN(model1)

#model fitting
model2 <- elmtrain(MDD~gravel + sand + pass200 + Gs + LL + PL ,data = traindf, nhid = 4, actfun = "purelin")
fitted2<-predict(model2, testdf[,-c(10,11)])
#rmse
sqrt(sum((fitted2 - na.omit(testdf['MDD']))^2))

print.elmNN(model2)
#model fitting
model3 <- elmtrain(MDD~gravel + sand + pass200 + Gs + LL + PL ,data = traindf, nhid = 6, actfun = "purelin")
fitted3<-predict(model3, testdf[,-c(10,11)])
#rmse
sqrt(sum((fitted3 - na.omit(testdf['MDD']))^2))

print.elmNN(model3)
#model fitting
model4 <- elmtrain(MDD~gravel + sand + pass200 + Gs + LL + PL ,data = traindf, nhid = 8, actfun = "purelin")
fitted4<-predict(model4, testdf[,-c(10,11)])
#rmse
sqrt(sum((fitted4 - na.omit(testdf['MDD']))^2))

print.elmNN(model4)
#model fitting
model5 <- elmtrain(OMC~gravel + sand + pass200 + Gs + LL + PL ,data = traindf, nhid = 2, actfun = "purelin")
fitted5<-predict(model5, testdf[,-c(10,11)])
#rmse
sqrt(sum((fitted5 - na.omit(testdf['OMC']))^2))

print.elmNN(model5)
#model fitting
model6 <- elmtrain(OMC~gravel + sand + pass200 + Gs + LL + PL ,data = traindf, nhid = 4, actfun = "purelin")
fitted6<-predict(model6, testdf[,-c(10,11)])
#rmse
sqrt(sum((fitted6 - na.omit(testdf['OMC']))^2))

print.elmNN(model6)
#model fitting
model7 <- elmtrain(OMC~gravel + sand + pass200 + Gs + LL + PL ,data = traindf, nhid = 6, actfun = "purelin")
fitted7<-predict(model7, testdf[,-c(10,11)])
#rmse
sqrt(sum((fitted7 - na.omit(testdf['OMC']))^2))

print.elmNN(model7)

#model fitting
model8 <- elmtrain(OMC~gravel + sand + pass200 + Gs + LL + PL ,data = traindf, nhid = 8, actfun = "purelin")
fitted8<-predict(model8, testdf[,-c(10,11)])
#rmse
sqrt(sum((fitted8 - na.omit(testdf['OMC']))^2))

print.elmNN(model8)




