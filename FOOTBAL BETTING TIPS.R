setwd("C:/Users/croon/Desktop/PROJECT")
stat=read.csv("understat.csv")
dim(stat)
data=stat[,1:11]
dim(data)
attach(data)

############################################################
#LIBRARIES####
##############

library(datasets)
library(dplyr)
library(MASS)
library(cluster)
library(fpc) #model based clust
library(dbscan) #model based clust
library(devtools)
library(factoextra)
library(psych)
library(ggplot2)
library(factoextra)
############################################################

########### DATA EXPLORATION AND VISUALIZATION#############
pairs(data)
#analysis of goals
#plot(missed, pts) #negative correlation between goals missed and points
#plot(scored, pts) #as we can see there is a positive correlation between points and goals scored
ggplot(data, aes(missed,pts, colour=position))+
  geom_point()+
  facet_wrap(~X)
#
ggplot(data, aes(scored,pts, col=position))+
  geom_point()+
  facet_wrap(~X)
# total goals scored
hist(scored,labels=TRUE,main="scored goals in total",xlab="Goals",ylab="Frequency",col="green")
hist(missed,
     labels=TRUE,main="missed goals in total",xlab="Missed Goals",ylab="Frequency",col="red")
#goals in every league (problema con l'asse delle ordinate)
ggplot(data,aes(scored, fill=X,col=X))+
  geom_histogram()+
  facet_wrap(~X)
###########
#plot(position, pts)
ggplot(data,aes(position, pts, col=X))+
  geom_point()+
  facet_wrap(~X.1)+
  ggtitle("position of the teams and relatived points")
##########
#teams who played more tend to have more points
range(matches)
ggplot(data, aes(matches, pts, col=X))+
  geom_point()+
  ggtitle("number of matches and points")
#######
ggplot(data, aes(wins,position, col=X))+
  geom_point()+
  facet_wrap(~X.1)+
  ggtitle("wins and position")

ggplot(data, aes(loses, position, col=X))+
  geom_point()+
  facet_wrap(~X.1)+
  ggtitle("lost matches and consequent position")
########
ggplot(data, aes(wins,scored, col=X))+
  geom_point()+
  facet_wrap(~X.1)+
  ggtitle("do the goals help?")
ggplot(data, aes(draws, scored, col=X))+
  geom_point()+
  facet_wrap(~X.1)+
  ggtitle("do the goals help?")

ggplot(data, aes(draws, missed, col=X))+
  geom_point()+
  facet_wrap(~X.1)+
  ggtitle("do the goals help?")

######################################################
########################## PCA #######################
######################################################

#library(psych)
#help(pairs.panels)

cond=(data$X.1==2018)
data2=data[cond,]
dim(data2)
attach(data2)
str(data2)
fix(data2)
  data3=data2[,-c(1,2,4)]
dim(data3)
str(data3)
#pairs.panels(data3,gap=0, density=TRUE, pch=21)

###########################################################
pca=prcomp(data3, center=TRUE,scale.=TRUE)
names(pca)
summary(pca) #first principal component explains alone 62% of the variance.
#the second captures 19%.
#by the time we reach pc4 the other do not explain much. they do not play a very important role
print(pca) #the values are going to be between -1 e 1
#pc1 increases as position drawns loses and missed increase. this is right. 
#pc2
pca$sdev
pca$center #center is the mean of the data
pca$scale
pca$rotation
## pca$dev are the sqrt of eigenvalues
## pca$dev is the rotation matrix Phi
## pca$scale simply report if the data has been scaled
## pca$x are the principal values (Z in our notation)
## SUPER IMPORTANT when p>n automatically all the eigenvector
## larger than n are not considered!!!
dim(pca$x)
#plot(pca)
biplot(pca, scale=0)
#plot(pca, type = "l")
screeplot(pca, type = "l", npcs = 8, main = "Screeplot of  PCs")
abline(h = 1, col="blue", lty=5, lwd=4)
#legend("topright", legend=c("Eigenvalue = 1"),
# col=c("red"), lty=5, cex=0.6)

pairs.panels(pca$x,gap=0) #there is an error as pc7 and pc8 have corr different from 0
#multicollinearity problem 
pca$sdev
varexplained=pca$sdev^2
varexplained.per=round(varexplained/sum(varexplained)*100,1)
#to obtain the proportion of variance explained by each principal component 
barplot(varexplained.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

pve=varexplained/sum(varexplained)
pve
plot(pve, xlab="principal component", ylab="proportion of variance", ylim=c(0,1), type="b")
abline(h=0.125,lty=2, col="blue", lwd=3)
plot(cumsum(pve), xlab="principal component", ylab="cumulative proportion of variance", ylim=c(0,1), type="b")
abline(h=0.95,lty=2,col="blue",lwd=3)
#
cumpro <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
plot(cumpro[0:8], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 3, col="blue", lty=5)
abline(h = 0.95, col="blue", lty=5)
#legend("topleft", legend=c("Cut-off @ PC3"),
#  col=c("blue"), lty=5, cex=0.6)

### I compute the cumulative sum of the proportion of
### variability explained by the principal componets
######################################################################
#how many principal components?
hm <- max(which( cumsum(pca$sdev^2)/sum(pca$sdev^2)<0.95))
hm

install.packages("factoextra")
library("factoextra")
#data(data3)
#head(data3)
res.pca <- prcomp(data3, scale = TRUE)
fviz_eig(res.pca)
#fviz_pca_ind(res.pca,
# col.ind = "cos2", # Color by the quality of representation
# gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#repel = TRUE     # Avoid text overlapping
#)
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#fviz_pca_biplot(res.pca, repel = TRUE,
# col.var = "#2E9FDF", # Variables color
#col.ind = "#696969"  # Individuals color

###############################################################
###############################################################
###############################################################
#PREPROCESSING FOR CLUSTERING##################################
###############################################################


#recall "data2" as the dataset with only year==2018
dim(data2)
attach(data2)
z <- dplyr::select(data2,-c(1,2,4)) #I don't need the ear anymore
#and also i dropped the two character variables
fix(z)
sd.data=scale(z) #standardized data


###########################
#HIERARCHICAL CLUSTERING###
###########################

data.dist=dist(sd.data)#euclidean distance
hc.complete= hclust(data.dist)
hc.average= hclust(data.dist,method = "average")
hc.single= hclust(data.dist,method = "single")


plot(hc.complete, labels=data2$team, cex=0.6, main= "complete linkage",xlab="",sublab="" ,ylab="")
#
#plot(hc.average, labels=data2$team, cex=0.6, main= "average linkage",xlab="",sublab="" ,ylab="")
#
#plot(hc.single, labels=data2$team, cex=0.6, main= "single linkage",xlab="" ,sublab="",ylab="")

#cluster membership - I suppose 2 clusters
member.c =cutree(hc.complete,2)
member.a =cutree(hc.average,2)
table(member.c,member.a)

#cluster mean
aggregate(sd.data,list(member.c),mean)
#we get average values for the 6 clusters for each variable
#those values are standardized and helps us in
#characterizing the 6 clusters

#if we do not see too much variation amog the averages
#means that the variable is not playing a big role in
#in deciding cluster membership

#example: teams in cluster 6 has really low scors instead of 
#teams in cluster 1 has really high score (same for wins)

aggregate(data2[,-c(1,2,4)],list(member.c),mean)#is an aggregation with original values

#screeplot
#i have to calculate within group sum of squares
wss=(nrow(sd.data)*sum(apply(sd.data,2,var)))
for(i in 2:20) wss[i] <- sum(kmeans(sd.data,centers=i)$withniss)
plot(1:20,wss,type = "b",xlab="Numb of clusters",ylab="within group SS",col="red")

#after to have seen the scree plot I understood that the optimal number of clusters
#is 2 
member.c2 =cutree(hc.complete,2)
member.a2 =cutree(hc.average,2)
table(member.c2,member.a2)
aggregate(sd.data,list(member.c2),mean)

plot(hc.complete, labels=data2$team, cex=0.6, main= "complete linkage",xlab="",sublab="" ,ylab="")
groups = cutree(hc.complete, k=2)
rect.hclust(hc.complete, k=2, border="violet")
clusplot(sd.data, groups, main='2D representation of the Cluster', color=TRUE, shade=TRUE,labels=2, lines=0)
si2 <- silhouette(groups, data.dist)
si2
sil_indx <- mean(si2[,"sil_width"])
sil_indx

plot(si2, col = c("green", "purple"))# good result

#AVERAGE LINKAGE 
plot(hc.average, labels=data2$team, cex=0.6, main= "average linkage",xlab="",sublab="" ,ylab="")
groups2 = cutree(hc.average, k=2)
rect.hclust(hc.average, k=2, border="green")
clusplot(sd.data, groups2, main='2D representation of the Cluster', color=TRUE, shade=TRUE,labels=2, lines=0)
si3 <- silhouette(groups, data.dist)
si3
sil3_indx <- mean(si2[,"sil_width"])
sil3_indx
plot(si3, col = c("green", "purple"))# good result

###########################
#K-MEANS CLUSTERING########
###########################

kc <- kmeans(sd.data,2)
kc #first cluster has 50 teams and the second has 64 teams
#it returns to cluster means and cluster membership
#the variability between the components of the clusters is really high 
#and I decided to increase the number of clusters
data.dist=dist(sd.data)
table(kc$cluster,member.c)
str(data2)
plot(position~wins,sd.data,col=kc$cluster)
sil2=silhouette(kc$cluster,data.dist)
sil2
sil_indx3 <- mean(sil2[,"sil_width"])
sil_indx3
plot(sil2, col = c("green", "purple"))# good result
clusplot(sd.data, kc$cluster, main='2D representation of the Cluster solution',labels = 2, color=TRUE, shade=TRUE,lines=0)

###########################

kc2 <- kmeans(sd.data,4) #much better result
kc2
table(kc2$cluster,member.c)
str(data2)
plot(position~wins,sd.data,col=kc2$cluster)
sil4=silhouette(kc2$cluster,data.dist)
sil4
sil_indx4 <- mean(sil2[,"sil_width"])
sil_indx4
plot(sil4, col = c("green", "purple"))# good result

clusplot(sd.data, kc2$cluster, main='2D representation of the Cluster solution',labels = 2, color=TRUE, shade=TRUE,lines=0)


#### AN ELBOW CRITERION FOR THE NUMBER OF CLUSTERS 
## We looks at the percentage of variance explained as a function
## of the number of clusters:
## The first clusters should  add much
## information (explain a lot of variance), but at some point the marginal 
## gain will drop, giving an angle in the graph.


wssplot <- function(sd.data, nc=25, seed=1234){
  wss <-  (nrow(sd.data)-1)*sum(apply(sd.data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(sd.data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares",col="red")}

wssplot(sd.data, nc=25) #4 seems to be a good number of clusters


library(cluster)
clusplot(sd.data, kc2$cluster, main='2D representation of the Cluster solution',labels = 2, color=TRUE, shade=TRUE,lines=0)
?clusplot

#In order to evaluate the clustering performance we build a confusion matrix:

table(data2[,1],kc2$cluster)
X=table(data2[,4],kc2$cluster)


###############################
#MODEL BASED CLUSTERS##########
###############################

#if we want to escape the "rounded" shape of the clusters and deal with
#do clustering with any shape we use model based clustering

#one of the parameter is ips: radius of a circle from a point
str(data2)
str(z) #no factors here

#optimal eps (is one of the parameters as we have seen in the theory)
#we need to find the optimal level befor finding the density based clustering
#eps provides the maximum distance 
kNNdistplot(sd.data,k=2)
abline(h=1.67,lty=2)

#density based clustering with fpc and dbscan
set.seed(123)
f <- fpc::dbscan(sd.data,eps = 1.67,MinPts = 4)
f #7 observations as noise
d <- dbscan::dbscan(sd.data,1.67,4)
d

#cluster visualization
install.packages("digest")
library(digest)
fviz_cluster(f,sd.data,geom = "point", main = "Model based clustering")
#doing it with d doesn't change

?fviz_cluster
