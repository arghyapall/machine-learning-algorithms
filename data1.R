data1<-read.csv("D:/dir/Raw_Data.csv",header=T)
data1
edit(data1)
data1[,-c()]<-apply(data1[,-c(8,9)],2,function(x){
    x=(x-mean(x))/sd(x)
})
data1[,-c(8,9)]
edit(data1[,-c(8,9)])
data2<-data1[,-c(8,9)]
data2
install.packages("elmNN")