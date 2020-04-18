library(factoextra)

cluster <-read.csv("C:/Users/user/Documents/GitHub/Thesis/EPU_Sentiment_S&P500_1985_2018.csv", header=T, sep=",")

clusterdata = cluster[,c(3,6)]

data = scale(clusterdata)

kmeans.cluster <- kmeans(data, centers=3, nstart = 25) 

kmeans.cluster$cluster

cluster1 = kmeans.cluster$cluster

#Plot
library(ggplot2)
fviz_cluster(kmeans.cluster,           # 分群結果
             data = data,              # 資料
             geom = c("point","text"), # 點和標籤(point & label)
             frame.type = "norm")      # 框架型態

fviz_cluster(kmeans.cluster, data = data,
             palette = c("#00AFBB","#2E9FDF", "#E7B800"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot"
)

fviz_cluster(kmeans.cluster, data = data,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())




# Determining the optimal number of clusters

library("cluster")
library("factoextra")
library("magrittr")

  # Compute
library("NbClust")
data <- clusterdata %>%
  scale() %>%
  NbClust(distance = "euclidean",
          min.nc = 2, max.nc = 10, 
          method = "complete", index ="all") 
  # Visualize
fviz_nbclust(data, ggtheme = theme_minimal())


  #Gap statistic method
library("factoextra")
fviz_nbclust(data, kmeans, method = "gap_stat")

  # Elbow Method 
fviz_nbclust(data, 
             FUNcluster = kmeans,# K-Means
             method = "wss",     # total within sum of square
             k.max = 12          # max number of clusters to consider
) +
  
  labs(title="Elbow Method for K-Means") +
  
  geom_vline(xintercept = 3,        # 在 X=3的地方
             linetype = 2)          # 畫一條垂直虛線

