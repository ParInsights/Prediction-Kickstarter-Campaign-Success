data_path=file.choose()
data=read.csv(data_path)
str(data)
View(data)
sum(!complete.cases(data))
summary(data)
#look at null values
is.na(data) # returns TRUE of x is missing
data[!complete.cases(data),]

##ovewrview of data
install.packages("plyr")
install.packages("dplyr")
library("plyr")
library("dplyr")

colnames(data)

final.data$category <- as.factor(final.data$category)
final.data$main_category <- as.factor(final.data$main_category)
final.data$location <- as.factor(final.data$location)
final.data$status <- as.factor(final.data$status)

final.data <- final.data[complete.cases(final.data),]



  
  



install.packages("ggplot2")
library(ggplot2)


# What is the average of the pledged?
  
mean(final.data$usd_pledged)

#2, Distribution of backers

ggplot(final.data=final.data, aes(final.data$backers)) + 
  geom_histogram(breaks=seq(0, 100, by = 2), 
                 col="red", 
                 fill="blue", 
                 alpha = .2) + 
  labs(title="Histogram for Backers", x="Backers", y="Count")



#Scaling fp final.data 

final.data<-final.data.frame(scale(fp,center = T,scale = T))


#3 3. Is duration normaly distributed?

qqnorm(final.data$duration)
qqline(final.data$duration)



ggplot(final.data, aes(sample = duration, colour = factor(duration))) +
  stat_qq() +
  stat_qq_line()

install.packages("devtools")
library("devtools")
devtools::install_github("aloy/qqplotr")



str(final.data)
###Cluster analysis- Kickstarter Word type ################

##clean fp_wording for fp_wording related to wording 
fp_wording<-final.data[,c(-1:-12,-16:-20)]
View(fp_wording)
str(fp_wording)

fp_wording_clean<-fp_wording[,c(-3)]


fp_wording_clean<-data.frame(scale(fp_wording_clean,center = T,scale = T))


#Plot (elbow method) to decide optimal number of clusters 
set.seed(25)
optim.cluster<-function(k){
  return(kmeans(fp_wording_clean,k,nstart = 10)$tot.withinss)
}
k_values<-1:14
oc_values<-purrr::map_dbl(k_values,optim.cluster)
plot(x=k_values, y=oc_values,type="b",frame=F,xlab = "Number of clusters K",ylab="Total within-clusters (sum squared)")


#### Option 1: K means_Clustering ####
k_mean<-kmeans(fp_wording_clean, centers = 3, nstart = 10)
k_mean
k_mean$centers


######Option 2: :K_means using Hartingan-Wong Method####

##HW algorithm to find centroid 
km_output1<-kmeans(fp_wording_clean,centers=3,nstart = 10,iter.max = 100,algorithm = "Hartigan-Wong")
km_output1
km_output1$centers



#visualize the result
install.packages("cluster")
library(cluster)
install.packages("rattle")
library("rattle")
install.packages("ggplot2")
library("ggplot2")

clusplot(fp_wording_clean,km_output1$cluster,color=FALSE,shade=T,labels = 2,lines =0 )


clusplot(fp_wording_clean,km_output1$cluster,color=TRUE,shade=T,labels = 1,lines =0 )




km_df1<-data.frame(status=fp_wording$status,cluster1=km_output1$cluster)

ggplot(km_df1)+geom_polygon(aes(x=status,y=cluster1,group=status,fill=as.factor(cluster1)),color="red")+
  coord_fixed(1.3)+
  guides(fill=F)+
  theme_bw()





##### 2nd Algorithm: HAC######
hac_output<-hclust(dist(train3,method = "euclidean"),method = "average") #average
hac_output2<-hclust(dist(train3,method = "euclidean"),method = "complete") #complete 


#plot HAC 
plot(hac_output) ##average 
plot(hac_output2) ## complete 


#output desirable number of clusters after modeling

#average linkage
hac_cut<-cutree(hac_output,4)
hac_df1<-fp_wording.frame(author=fp_wording$author,cluster=hac_cut)
avg_Clus_plot<-clusplot(fp_wording,hac_cut,color=TRUE,shade=T,labels = 2,lines =0 )

#compelte linkage
hac_cut<-cutree(hac_output2,4)
hac_df1<-fp_wording.frame(author=fp_wording$author,cluster=hac_cut)
clusplot(fp_wording,hac_cut,color=TRUE,shade=T,labels = 2,lines =0 )




####3rd Algorithm: EM#####
install.packages("mclust")
library(mclust)

#visualize clusters
clPairs(fp_wording [1:30],fp_wording$author) ##disputed


fit<-Mclust(fp_wording)
summary(fit)

#1. BIC (The Bayesian information criterion (BIC) 
## is used my mclust with is a test used to assess the fit of a model)
plot(fit,what= "BIC")
#2. classification
plot(fit, what = "classification")
length(fit$classification)
