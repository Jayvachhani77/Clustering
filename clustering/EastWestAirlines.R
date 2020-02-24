airlines<-read.csv(choose.files())
standardised_airlines<-scale(airlines[-1])
distance<-dist(standardised_airlines,method = "euclidean")

#hierarchical models
model<-hclust(distance,method = "single")
model<-hclust(distance,method = "complete")
model<-hclust(distance,method = "centroid")
model<-hclust(distance,method = "average")
str(model)
plot(model,hang = -1)
rect.hclust(model, k=6, border="red")
group<-cutree(model,k=6)
members<-as.matrix(group)
hier_airlines<-data.frame(members,airlines)
#cannot figure out any meaningfull information as the data is very huge




#################################using K-means method#########################
#install.packages("plyr")
library(plyr)
KMmethod<-kmeans(standardised_airlines,4)      #building a model
str(KMmethod)
KMmethod$cluster                               #which id belongs to which cluster
KMmethod$centers                               #centroids
install.packages("animation")
library(animation)
kmeans.ani(standardised_airlines,4)
twss <- NULL
for (i in 2:12){
  twss <- c(twss,kmeans(standardised_airlines,i)$tot.withinss)
  
}
plot(2:12, twss,type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

category<-as.matrix(KMmethod$cluster)
K_mean_airlines<-data.frame(category,airlines)
aggregate(airlines[,-c(1)],by=list(K_mean_airlines$category),mean)
KMmethod$size

#we can segregate people based on their loyalty
#we can say that people of group 2 are the most frequent flyers with this airline followed by group 3,group1 and group 4 respectively