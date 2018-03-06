set.seed(123456789)
#importing data
df <- read.csv('D:/dir/Raw_data.csv', h = T, stringsAsFactors = T)
names(df) <- c('gravel','sand','pass200','Gs','LL','PL','PI','Class','Rank','MDD','OMC')
#data_details
str(df)
summary(df)
edit(df)
#exluding factor variables
df_num <- df[,-c(8,9)]
#package installation
install.packages("e1071")
library(e1071)

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
model1 <- svm(MDD~gravel + sand + pass200 + Gs + LL + PL ,data = traindf, scale = T)
fitted1 <- predict(model1, testdf[,-c(8,9)])
#rmse
sqrt(mean((fitted1 - na.omit(testdf['MDD']))^2))
a<-sum((fitted1 - na.omit(testdf['MDD']))^2)
b<-
model2<- svm(OMC~gravel + sand + pass200 + Gs + LL + PL ,data = traindf, scale = T)
fitted2<-predict(model2 , testdf[,-c(10,11)])
sqrt(mean((fitted2 - na.omit(testdf['OMC']))^2))







