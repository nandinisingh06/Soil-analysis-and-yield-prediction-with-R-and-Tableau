soildata = read.csv("D:\\VIT\\sem6\\DV\\jcomp\\soilanalysis.csv")
soildata
View(soildata)
plot(pH~Aval.N,soildata,col="red",lwd=2)
with(soildata,text(pH~Aval.N,soildata,labels=name,pos=4,cex=1))


soildata1 = soildata[-1] #removes the name column for further calculations
soildata1

#normalise the dataset.
m <- apply(soildata1,2,mean)
s <- apply(soildata1,2,sd)
soildata1 <- scale(soildata1,m,s)
soildata1
print(soildata1,digits = 3)


#elbow curve
wssplot <- function(data,nc=15,seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers=i)$withinss)}
  plot(1:nc,wss,type="b",xlab="Number of clusters",
       ylab="Within Groups Sum of Squares",col="blue",lwd=2)}
wssplot(soildata1,nc=19,seed=1234)


#kmeans
testing_kmeans <- kmeans(soildata1,3)
testing_kmeans


library("scatterplot3d")
scatterplot3d(soildata1[,2:4],color = testing_kmeans$cluster,lwd=3)
scatterplot3d(soildata1[,2:4],color = "red",lwd=3)


distance <- dist(soildata1)#similarity matrix
print(distance, digits = 3)

#cluster dendogram
#complete
hc.c <- hclust(distance)
plot(hc.c,labels=soildata$name,lwd=2,col="blue")

hc.a <- hclust(distance,method = "average")#center-linkage
plot(hc.a,labels=soildata$name,lwd=2)

#cluster membership
member.c <- cutree(hc.c,3)#cutting the dendrogram tree into several groups by specifying the
#desired number of cluster k(s)
member.a <- cutree(hc.a,3)
#plot(member.c,member.a)
table(member.c, member.a)


#cluster means
aggregate(soildata1,list(member.c),mean)

library(cluster)
#silhoute plot
plot(silhouette(cutree(hc.c,3),distance,color="blue"))



##TIME SERIES

library(ggfortify)  #for plotting time series data
library(tseries)
library(forecast)
data=read.csv("D:\\VIT\\sem6\\DV\\jcomp\\district_rainfall.csv")
View(data)
data$annual<-rowSums(data[-1])


g <- ggplot(data, aes(annual,STATE))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Bar Chart", 
       subtitle="RAINFALL STATS FOR EACH STATE", 
       caption="Source: Frequency of Manufacturers from 'mpg' dataset") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

data_1 = data[2:13] #removes the name column for further calculations
data_1
data_1$id = rownames(data_1)
data_1
library(reshape2)    
mm = melt(data_1, id='id')
library(ggplot2)
ggplot(mm)+geom_line(aes(x=variable, y=value, group=id, color=id))


