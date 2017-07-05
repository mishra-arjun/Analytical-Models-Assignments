###===============================Question 5==========================================
##Assignment 2

#Getting the data: Iris dataset is already built into R
#We don't want to modify the original dataset
iris_data = iris
View(iris_data)

#There are 4 variables that describe the flower and one variable that gives the type
#of the flower

#Investigating the data
str(iris_data)
summary(iris_data)

#The data is divided into equal number of flowers
#For clustering, normalization is recommended.
#When the algorithm will calculate the distance between the clusters, they will be
#on a similar scale. Thus the effect of a particular variable won't be more
#pronounced than the others.

#Normalizing the variables
iris_data$Sepal.Length = iris_data$Sepal.Length/(max(iris_data$Sepal.Length) - min(iris_data$Sepal.Length))
iris_data$Sepal.Width = iris_data$Sepal.Width/(max(iris_data$Sepal.Width) - min(iris_data$Sepal.Width))
iris_data$Petal.Length = iris_data$Petal.Length/(max(iris_data$Petal.Length) - min(iris_data$Petal.Length))
iris_data$Petal.Width = iris_data$Petal.Width/(max(iris_data$Petal.Width) - min(iris_data$Petal.Width))
summary(iris_data)


#Plotting the variables with each other
#There will be 4C2 plots.

#These plots do not use the variable of flower type.
#These will just show us the distribution of the two variables.
p1 = plot(iris_data$Sepal.Length,iris_data$Sepal.Width)
p2 = plot(iris_data$Sepal.Length,iris_data$Petal.Length)
p3 = plot(iris_data$Sepal.Length,iris_data$Petal.Width)
p4 = plot(iris_data$Sepal.Width,iris_data$Petal.Length)
p5 = plot(iris_data$Sepal.Width,iris_data$Petal.Width)
p6 = plot(iris_data$Petal.Width,iris_data$Petal.Length)

#Another way to do this
library(GGally)
ggpairs(iris[,1:4])

#With ggplot, we are going to see which of the plots are actually good at separating
#out the different species the best. For this plot, we are using the category
#variable but we will not base our analysis on these plots!!

require("ggplot2")

gg_p1 = ggplot(iris_data, aes(Petal.Length,  Petal.Width, color = Species)) + geom_point()
gg_p2 = ggplot(iris_data, aes(Petal.Length,  Sepal.Width, color = Species)) + geom_point()
gg_p3 = ggplot(iris_data, aes(Petal.Length,  Sepal.Length, color = Species)) + geom_point()
gg_p4 = ggplot(iris_data, aes(Petal.Width,  Sepal.Width, color = Species)) + geom_point()
gg_p5 = ggplot(iris_data, aes(Petal.Width,  Sepal.Length, color = Species)) + geom_point()
gg_p6 = ggplot(iris_data, aes(Sepal.Length,  Sepal.Width, color = Species)) + geom_point()

#From the 2-D plots it is clear that based on the Sepal attributes, the flower species
#are randomly distributed. (gg_p6)

#Finding correlation among the variables

cor(iris_data[1:4])

#In a basic sense, the more the variables are correlated to each other, the stronger
#their clustering power should be as together they will help in forming groups.

#The variable Sepal.Width is poorly correlated with every other variable

#Making the first cluster with all the variables and then trying after taking out
#variable Sepal.Width

set.seed(150)

#To check how many clusters to use, we can plot the SSE and see where we get a
#significant bend in the graph. We have to trade off between the number of clusters
#and the SSE. (SSE will be 0 when number of clusters = No. of points)

sse <- (nrow(iris_data)-1)*sum(apply(iris_data[1:4],2,var))
#withinss is an attribute of the kmeans algo. It gives the SSE within the clusters
for (i in 2:7) sse[i] <- sum(kmeans(iris_data[1:4], centers=i)$withinss)

#Plotting the SSE
plot(1:7, sse, type="b", xlab="Clusters",
     ylab="SSE")

#According to the plot, the bend comes at k = 2 and k = 3. k = 2, however, has 
#high SSE whereas after k = 3 the SSE remains relatively constant.

#We can also use the hierarchical clustering algorithm to determine the number of
#clusters using the dendrogram.

distances = dist(iris_data[1:4], method = "euclidean")

Hierar = hclust(distances, method = "complete")

plot(Hierar)

#Looking at the dendrogram, the best clusters will be found when we cut at k = 2
#Or k =3. As all the three branches at k = 3 also have significant length, we will
#choose k = 3.

#We can also use the NbClust library to help us determine this
install.packages("NbClust")
library(NbClust)

optim <- NbClust(iris_data[1:4], min.nc=2, max.nc=8, method="kmeans")
table(optim$Best.n[1,])

barplot(table(optim$Best.n[1,]), 
          xlab="Numer of Clusters", ylab="Number of Criteria")

#The graphs and table created from the NbClust library also suggests 3 clusters

allcluster = kmeans(iris_data[,1:4], centers = 3)
table(allcluster$cluster, iris_data$Species)
#Accuracy of 89%

#Taking out the Sepal.Width from the model

Three_cluster = kmeans(iris_data[,c(1,3,4)], centers = 3)
table(Three_cluster$cluster, iris_data$Species)

#Accuracy of 90%
#Improvement by taking out variable Sepal.Width

#Trying the clustering with only 2 variables. Might result in improvement as removing
#one variable resulted in improvement.
#The next low correlated variable is Sepal.Length
#Excluding that variable
petalclusters = kmeans(iris_data[,3:4], centers = 3)
table(petalclusters$cluster, iris_data$Species)

#Accuracy improved to 96%!!

#Checking if we get a similar result using combination of other variables

sepalclusters = kmeans(iris_data[,c(1:2)], centers = 3)
table(sepalclusters$cluster, iris$Species)

lengthclusters = kmeans(iris_data[,c(1,3)], centers = 3)
table(lengthclusters$cluster, iris$Species)

widthclusters = kmeans(iris_data[,c(2,4)], centers = 3)
table(widthclusters$cluster, iris$Species)

#The Petal cluster is the best in terms of the accuracy

#Optimizing the cluster parameters
#The k-means algorithm chooses cluster centers at random when it initiates.
#We can configure it to try out more than just one center and then try to run
#the algorithm and see where the best result is obtained

#As the accuracy is already very high, we have to give it a significant number
#of trials to result in improvement.

petalclusters = kmeans(iris_data[,3:4], centers = 3, nstart = 50)
table(petalclusters$cluster, iris_data$Species)

#The accuracy is 96%

#The centers of the clusters:
petalclusters$centers

#No improvement after adding nstart

#Adding the cluster numbers of the data entries into the dataset
Model_cluster = as.factor(petalclusters$cluster)

iris_data = cbind(iris_data, Model_cluster)

#Using the categorical variable already present in the data to plot it with the
#clusters we made. Colors are according to the actual species and shape according
#to our clusters
ggplot(iris_data, aes(Petal.Length, Petal.Width, color = Species, shape=Model_cluster)) + geom_point()

