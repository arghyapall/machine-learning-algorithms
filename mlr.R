set.seed(123456789)
#importing data
df <- read.csv('D:/dir/raw_data.csv', h = T, stringsAsFactors = T)
names(df) <- c('gravel','sand','pass200','Gs','LL','PL','PI','Class','Rank','MDD','OMC')
#data_details
str(df)
summary(df)
edit(df)
#exluding factor variables
df_num <- df[,-c(8,9)]
df_num
edit(df_num)
#missing value detection

df_num <- df_num[!is.na(df_num),]

index <- sample(x = 1:nrow(df_num), round(0.6*nrow(df_num)))
index
#train data splitting
traindf <- na.omit(df_num[index,])
testdf <- na.omit(df_num[-index,])
testdf
#model fitting
model1 <- lm(MDD~gravel + sand + pass200 + Gs + LL + PL ,data = traindf)
fitted1 <- predict(model1, testdf[,-c(8,9)])
#rmse
sqrt(mean((fitted1 -na.omit( testdf['MDD']))^2))
model2 <- lm(OMC~gravel + sand + pass200 + Gs + LL + PL ,data = traindf)
fitted2 <- predict(model2, testdf[,-c(8,9)])
sqrt(sum((fitted2 -na.omit(testdf['OMC']))^2))
diff<-na.omit((fitted2-testdf['OMC'])^2)
sqrt(mean(diff))