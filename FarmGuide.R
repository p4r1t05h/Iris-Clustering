rm(list=ls())

#Setting Working Diarectory
setwd("D:\\Data Science\\FarmGuide")

#Loading Data set
iris<-read.csv("iris.data.csv", header = F)

head(iris)
str(iris)

#Changing Column Names

colnames(iris)<-c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width", "Category")

head(iris)
str(iris)

#As we can see the variables as in proper data type so there is no need for transformation.

#================Univariate Analysis===========================

#----------Statistical Summary for SEPAL LENGTH---------------

sepal.length<-summary(iris$Sepal_Length)
sepal.length

#-------------Visualization for SEPAL LENGTH--------------------

##Histogram

sepal.length.hist<-hist(iris$Sepal_Length, main = "Histogram of Sepal Length", xlab = "Sepal Length")

##Box Plot

sepal.length.boxplot<-boxplot(iris$Sepal_Length, names = "Boxplot of Sepal Length", ylab = "Sepal Length")

##Plot Density Graph

sepal.length.density<-plot(density(iris$Sepal_Length), main = "Sepal Length")

#----------Statistical Summary for SEPAL WIDTH---------------

sepal.width<-summary(iris$Sepal_Width)
sepal.width

#------------Visualization for SEPAL WIDTH-----------------------

##Histogram

sepal.width.hist<-hist(iris$Sepal_Width, main = "Histogram of Sepal Width", xlab = "Sepal Width")

##Box Plot

sepal.width.boxplot<-boxplot(iris$Sepal_Width, names = "Boxplot of Sepal Width", ylab = "Sepal Width")

##Plot Density Graph

sepal.width.density<-plot(density(iris$Sepal_Width), main = "Sepal Width")


#----------Statistical Summary for PETAL LENGTH---------------

petal.length<-summary(iris$Petal_Length)
petal.length

#-------------Visualization for PETAL LENGTH--------------------

##Histogram

petal.length.hist<-hist(iris$Petal_Length, main = "Histogram of Petal Length", xlab = "Petal Length")

##Box Plot

petal.length.boxplot<-boxplot(iris$Petal_Length, names = "Boxplot of Petal Length", ylab = "Petal Length")

##Plot Density Graph

petal.length.density<-plot(density(iris$Petal_Length), main = "Petal Length")


#----------Statistical Summary for PETAL WIDTH---------------

petal.width<-summary(iris$Petal_Width)
petal.width

#-------------Visualization for PETAL WIDTH------------------

##Histogram

petal.width.hist<-hist(iris$Petal_Width, main = "Histogram of Petal Width", xlab = "Petal Width")

##Box Plot

petal.width.boxplot<-boxplot(iris$Petal_Width, names = "Boxplot of Petal Width", ylab = "Petal Width")

##Plot Density Graph

petal.width.density<-plot(density(iris$Petal_Width), main = "Petal Width")


#===========Bivariate Analysis of X (Sepal Length) with Y (Category)

#Numerical Analysis

sepal.length.5numSummary<-tapply(iris$Sepal_Length, iris$Category, summary)
sepal.length.5numSummary

#Visualization

sepal.length.category.boxplot<-boxplot(iris$Sepal_Length~iris$Category)

#===========Bivariate Analysis of X (Sepal Width) with Y (Category)

#Numerical Analysis

sepal.width.5numSummary<-tapply(iris$Sepal_Width, iris$Category, summary)
sepal.width.5numSummary

#Visualization

sepal.width.category.boxplot<-boxplot(iris$Sepal_Width~iris$Category)

#===========Bivariate Analysis of X (Petal Length) with Y (Category)

#Numerical Analysis

petal.length.5numSummary<-tapply(iris$Petal_Length, iris$Category, summary)
petal.length.5numSummary

#Visualization

petal.length.category.boxplot<-boxplot(iris$Petal_Length~iris$Category)

#===========Bivariate Analysis of X (Petal Width) with Y (Category)

#Numerical Analysis

petal.width.5numSummary<-tapply(iris$Petal_Width, iris$Category, summary)
petal.width.5numSummary

#Visualization

petal.width.category.boxplot<-boxplot(iris$Petal_Width~iris$Category)

##========Using Unsupervised Machine Learnings to Create Clusters======

#-----------------DBSCAN---------------------------

#Loading Library for DSBSCAN
library(fpc)

#Removing target variable
iris1<-iris[,-5]

head(iris1)
summary(dist(iris1))

#Histogram for only Predicting Variables
hist(dist(iris1))

ds<-dbscan(iris1, eps = 0.42, MinPts = 5, showplot = 1)
length(ds$cluster)


table(ds$cluster, iris$Category)

plot(ds,iris1)


#---------------Hierarchichal Clustering-------------------------

par(mfrow=c(1,3))
dist(iris1)

#Heirarchichal clustering with complete linkage
hc.complete = hclust(dist(iris1), method = "complete")

#Visualizing the Complete Linkage Heirarchical Cluster in Tree Form
plot(hc.complete, main = "Complete Linkage Heirarchical Clustering", xlab = "", sub = "", cex=.9)

#Heirarchichal clustering with average linkage
hc.average = hclust(dist(iris1), method = "average")

#Visualizing the Average Linkage Heirarchical Cluster in Tree Form
plot(hc.average, main = "Average Linkage Heirarchical Clustering", xlab = "", sub = "", cex=.9)

#Heirarchichal clustering with single linkage
hc.single = hclust(dist(iris1), method = "single")

#Visualizing the Single Linkage Heirarchical Cluster in Tree Form
plot(hc.single, main = "single Linkage Heirarchical Clustering", xlab = "", sub = "", cex=.9)


#----------------K-means cllustering with k=3----------------------------

km.out=kmeans(iris1,centers = 3, nstart = 20) 
km.out$cluster


#Visualizing the Cluster
plot(iris1, col=(km.out$cluster+2), main = "K-Means Clustering Results with K=3",pch=20, cex=2)

points(km.out$centers,col=1:2,pch=3,cex=3,lwd=3)

#Total within-cluster sim of squares
km.out$tot.withinss

km.out$betweenss

km.out$totss

km.out$tot.withinss/km.out$totss
