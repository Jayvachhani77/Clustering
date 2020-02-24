CrimeData<-read.csv(choose.files())

Crime_clust<-scale(CrimeData[,2:5])   #Standardising the data
distance<-dist(Crime_clust,method = "euclidean")   #finding the euclidian distance between points
model<-hclust(distance,method = "complete")    #building the model
str(model)
plot(model,hang = -1)                          #Ploting the dendrogram
rect.hclust(model,k=4,border = "green")        #plotting 4 clusters in dendrogram with green lines
clusters<-cutree(model,k=4)                    #cutting it into 4 clusters
clusters
members<-as.matrix(clusters)                   #crearted a matrix column of data belonging to which group
members
clustered<-data.frame(members,CrimeData)       #created clustered data frame
aggregate(clustered[-2],by=list(clustered$members),mean)    #aggregated the cities based on average.

#so we can give saftey rating to the cities from 1 to 4 where 1)Not safe at all 2)Dangerous 3)safe 4)safest
