# Chargement des packages

library(dplyr)
library(FactoMineR)
library(tidyr)
library(rgl)
library(factoextra)
library(cluster)
library(cowplot)
library(stats)
library(ggplot2)
library(ggfortify)

#Q1/

# On choisit de conserver les variables crabs, FL, RW, CL, CW, BD .

(data(crabs, package = "MASS"))
crabs<-select(crabs,FL,RW,CL,CW,BD)


head(crabs)

pca_crabs<-PCA(crabs,scale.unit = FALSE,graph = F) 
pca_crabs$eig

hist(pca_crabs[["ind"]][["coord"]][,1],col = "darkolivegreen1",main ="Histogramme des données projetées \n sur la première composante principale",xlab = "Première composante principale",ylab="Fréquence")


hist(pca_crabs[["ind"]][["coord"]][,2],col = "darkolivegreen1",main ="Histogramme des données projetées \n sur la deuxième composante principale",xlab = "Deuxième composante principale",ylab="Fréquence")


plot(pca_crabs[["ind"]][["coord"]][,1],pca_crabs[["ind"]][["coord"]][,2],main ="Données projetées sur deux premières composantes principales",xlab = "Première composante principale",ylab="Deuxième composante principale")


coll <- read.table("seeds_dataset.txt", col.names = c('area','perimeter','compactness','lengthOfKernel','widthOfKernel','asymmetryCoefficient','lengthOfKernelGroove','seedType'))
head(coll)

coll <- drop_na(coll)

data_seeds <-select(coll,area,perimeter,compactness,lengthOfKernel,widthOfKernel,asymmetryCoefficient,lengthOfKernelGroove) 

hist(data_seeds$area,col = "darksalmon",main ="Histogramme de la variable Area",xlab = "Area",ylab="Fréquence") 

hist(data_seeds$perimeter,col = "darksalmon",main ="Histogramme de la variable Perimeter",xlab = "Perimeter",ylab="Fréquence")

hist(data_seeds$compactness,col = "darkseagreen",main ="Histogramme de la variable Compactness",xlab = "Compactness",ylab="Fréquence") 

hist(data_seeds$lengthOfKernel,col = "darkseagreen1",main ="Histogramme de la variable lengthOfKernel",xlab = "lengthOfKernel",ylab="Fréquence") 

hist(data_seeds$widthOfKernel,col = "coral1",main ="Histogramme de la variable widthOfKernel",xlab = "widthOfKernel",ylab="Fréquence") 

hist(data_seeds$lengthOfKernelGroove,col = "coral4",main ="Histogramme de la variable lengthOfKernelGroove",xlab = "lengthOfKernelGroove",ylab="Fréquence") 


plot(data_seeds)

pca_seeds<-PCA(data_seeds,scale.unit = FALSE,graph = F) 

pca_seeds$eig 

hist(pca_seeds[["ind"]][["coord"]][,1],col = "lightblue",main ="Histogramme des données projetées \n sur la première composante principale",xlab = "Première composante principale",ylab="Fréquence")

hist(pca_seeds[["ind"]][["coord"]][,2],col = "lightblue",main ="Histogramme des données projetées \n sur la deuxième composante principale",xlab = "Deuxième composante principale",ylab="Fréquence")

plot(pca_seeds[["ind"]][["coord"]][,1],pca_seeds[["ind"]][["coord"]][,2],main ="Données projetées sur deux premières composantes principales",xlab = "Première composante principale",ylab="Deuxième composante principale")

fviz_nbclust(data_seeds, kmeans, method = "wss") 

kmeans_res<-kmeans(data_seeds, centers = 3, nstart = 100) 
iteration<-kmeans_res$iter 

print(iteration)
plot(data_seeds,col=kmeans_res$cluster)

fviz_cluster(kmeans_res,data_seeds, palette = c("#2E9FDF", "#00AFBB", "#E7B800"), geom = "point", ellipse.type = "convex",  ggtheme = theme_bw(),main =" Algortihme des K-Means pour 3 centroides")

seedtrain_scaled<-scale(data_seeds)

fviz_nbclust(seedtrain_scaled, kmeans, method = "wss")

kmeans_res_s<-kmeans(seedtrain_scaled, centers = 3, nstart = 100)

iteration<-kmeans_res_s$iter 

plot(data_seeds,col=kmeans_res_s$cluster)

fviz_cluster(kmeans_res_s,data_seeds, palette = c("#2E9FDF", "#00AFBB", "#E7B800"), geom = "point", ellipse.type = "convex",  ggtheme = theme_bw(),main ="Algorithme des K-Means pour 3 Centroides")


pre_iteration_one <- kmeans(seedtrain_scaled, centers = 3, nstart = 1 ,iter.max = 1)
iteration_one <- fviz_cluster(pre_iteration_one,data_seeds, palette = c("#2E9FDF", "#00AFBB", "#E7B800"), geom = "point", ellipse.type = "convex",  ggtheme = theme_bw(),main ="Première itération")

pre_iteration_second <- kmeans(seedtrain_scaled, centers = pre_iteration_one$center, nstart = 1 ,iter.max = 1)
iteration_two <- fviz_cluster(pre_iteration_second,data_seeds, palette = c("#2E9FDF", "#00AFBB", "#E7B800"), geom = "point", ellipse.type = "convex",  ggtheme = theme_bw(),main ="Deuxième itération")

pre_iteration_three<-kmeans(seedtrain_scaled, centers = pre_iteration_second$center, nstart = 1 ,iter.max = 1)
iteration_three<-fviz_cluster(pre_iteration_three,data_seeds, palette = c("#2E9FDF", "#00AFBB", "#E7B800"), geom = "point", ellipse.type = "convex",  ggtheme = theme_bw(),main ="Troisième itération")

pre_iteration_four<-kmeans(seedtrain_scaled, centers = pre_iteration_three$center, nstart = 1 ,iter.max = 1)
iteration_four<-fviz_cluster(pre_iteration_three,data_seeds, palette = c("#2E9FDF", "#00AFBB", "#E7B800"), geom = "point", ellipse.type = "convex",  ggtheme = theme_bw(),main ="Quatrième itération")

pre_iteration_five<-kmeans(seedtrain_scaled, centers = pre_iteration_four$center, nstart = 1 ,iter.max = 1)
iteration_five<-fviz_cluster(pre_iteration_four,data_seeds, palette = c("#2E9FDF", "#00AFBB", "#E7B800"), geom = "point", ellipse.type = "convex",  ggtheme = theme_bw(),main ="Cinquième itération")

plot_grid(iteration_one, iteration_two,iteration_three,iteration_four,iteration_five) 

d<-dist(seedtrain_scaled)
hierarchical<-hclust(d,method='complete')
plot(hierarchical)
rect.hclust(hierarchical, k=3, border=2:5) 

hierarchical_clusters_complete<-cutree(hierarchical,k=3)
plot(data_seeds,col=hierarchical_clusters_complete)

fviz_cluster(object = list(data=data_seeds,cluster=hc_clusters_c), palette = c("#2E9FDF", "#00AFBB", "#E7B800"), geom = "point", ellipse.type = "convex",  ggtheme = theme_bw(),main ="Complete Linkage")

hierarchical__simple<-hclust(d,method='single')
plot(hierarchical__simple)
rect.hclust(hierarchical__simple, k=3, border=2:5) 

hierarchical__simple_linkage<-cutree(hierarchical__simple,k=3) 
plot(data_seeds,col=hierarchical__simple_linkage)

hierarchical_average<-hclust(d,method='average')
plot(hierarchical_average)
rect.hclust(hierarchical_average, k=3, border=2:5)

hierarchical__group_average<-cutree(hierarchical_average,k=3)
plot(data_seeds,col=hierarchical__group_average)
fviz_cluster(object = list(data=data_seeds,cluster=hierarchical__group_average), palette = c("#2E9FDF", "#00AFBB", "#E7B800"), geom = "point", ellipse.type = "convex",  ggtheme = theme_bw(),main ="Group Average")

hierarchical__w<-hclust(d,method='ward.D2')
plot(hierarchical__w)

hierarchical__ward<-cutree(hierarchical__w,k=3)
plot(data_seeds,col=hierarchical__ward)
fviz_cluster(object = list(data=data_seeds,cluster=hierarchical__ward), palette = c("#2E9FDF", "#00AFBB", "#E7B800"), geom = "point", ellipse.type = "convex",  ggtheme = theme_bw(),main ="Ward")
