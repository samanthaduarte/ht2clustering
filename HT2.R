# Clustering
# Eric Mendoza, Samantha Duarte

setwd("D:/Desktop/Clases/Minería de Datos")
library(cluster)
library(fpc)
library(mclust)
library(NbClust)
library(factoextra)

#Leer del archivo
datos<-read.csv("tmdb-movies.csv",stringsAsFactors = FALSE)
datos_orig<-read.csv("tmdb-movies.csv",stringsAsFactors = FALSE)
summary(datos)

# PREPROCESAMIENTO
# Obtener solamente las variables que se utilizaran para el clustering
datos = datos[c("popularity", "budget", "revenue", "vote_average", "release_year","budget_adj","revenue_adj")]

datos <- na.omit(datos) # Se eliminan las casillas donde puedan haber datos faltantes
datos <- scale(datos) # Se estandarizan las variables para realizar el clustering
datos_orig <- na.omit(datos_orig) # Se eliminan las casillas donde puedan haber datos faltantes
datos_orig <- scale(datos_orig) # Se estandarizan las variables para realizar el clustering

# NUMERO DE CLUSTERS
# K MEANS
# Determine number of clusters
wss <- (nrow(datos)-1)*sum(apply(datos,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(datos,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

# K-Means Cluster Analysis
fit <- kmeans(datos, 3) 
# get cluster means
aggregate(datos,by=list(fit$cluster),FUN=mean)
datos_orig <- data.frame(datos_orig, fit$cluster) 

plotcluster(datos, fit$cluster)
plot(silhouette(fit$cluster, daisy(datos)), border=NA)

#Clustering jerÃ¡rquico
datos <- movies[c("popularity", "budget", "revenue", "vote_average", "release_year","budget_adj","revenue_adj")]
hc<-hclust(dist(datos)) 
plot(hc) 
rect.hclust(hc,k=3) 
groups<-cutree(hc,k=3) 
movies$gruposHC<-groups
# Metodo de la silueta para CJ
silch<-silhouette(groups,dist(datos))
mean(silch[,3])

#Fuzzy C-Means
datos <- movies[c("popularity", "budget", "revenue", "vote_average", "release_year","budget_adj","revenue_adj")]
fcm<-cmeans(datos,3)
movies$FCGrupos<-fcm$cluster
datos<-cbind(datos,fcm$membership)

#Metodo de la silueta para fuzzy cmeans
silfcm<-silhouette(fcm$cluster,dist(datos))
mean(silfcm[,3]) #0.54, no es la mejor particiÃ³n pero no estÃ¡ mal

#Paquete para saber el mejor número de clusters
datos <- movies[c("popularity", "budget", "revenue", "vote_average", "release_year","budget_adj","revenue_adj")]
nb <- NbClust(datos, method = "complete")

clusters <- aggregate(datos_orig[3], list(datos_orig$fit.cluster), mean)
barplot(clusters$popularity, main="Popularidad")


clusters <- aggregate(datos_orig[4], list(datos_orig$fit.cluster), mean)
barplot(clusters$budget, main="Presupuesto")

clusters <- aggregate(datos_orig[5], list(datos_orig$fit.cluster), mean)
barplot(clusters$revenue, main="Ganancias")
