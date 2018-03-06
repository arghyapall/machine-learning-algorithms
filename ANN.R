set.seed(123456789)
#importing data
df <- read.csv('D:/dir/Raw_data.csv', h = T, stringsAsFactors = T)
names(df) <- c('gravel','sand','pass200','Gs','LL','PL','PI','Class','Rank','MDD','OMC')
#data_details
str(df)
summary(df)
#exluding factor variables
df_num <- df[,-c(8,9)]
edit(df_num)
#package installation
install.packages("neuralnet")
library(neuralnet)

df_num <- df_num[!is.na(df_num),]

index <- sample(x = 1:nrow(df_num), round(0.6*nrow(df_num)))
#train data splitting
traindf <- df_num[index,]
testdf <- df_num[-index,]
edit(testdf)
head(testdf[,-c(7,8,9)])
edit(traindf)
attach(c(traindf,testdf))
#model fitting
model1 <- neuralnet(OMC~gravel + sand + pass200 + Gs + LL + PL ,data = traindf, hidden = 4, learningrate = 0.01, linear.output = TRUE)
fitted1 <- compute(model1, na.omit(testdf[,-c(7,8,9)]))
sqrt(mean((na.omit(fitted1$net.result - testdf$MDD))^2))
plot(model1)
diff<-na.omit((fitted1 - testdf['OMC'])^2)
s<-sqrt(mean(diff))
### Save the plot and pase it int Excel
model2 <- neuralnet(OMC~gravel + sand + pass200 + Gs + LL + PL ,data = traindf, hidden = 4, learningrate = 0.01, linear.output = TRUE)
fitted2 <- compute(model2,testdf[,-c(7,8,9)]))
sqrt(sum((fitted1 - na.omit(testdf))^2))
plot(model2)
model3 <- neuralnet(OMC~gravel + sand + pass200 + Gs + LL + PL ,data = traindf, hidden = 2, learningrate = 0.01, linear.output = TRUE)
model4 <- neuralnet(OMC~gravel + sand + pass200 + Gs + LL + PL ,data = traindf, hidden = 4, learningrate = 0.01, linear.output = TRUE)
