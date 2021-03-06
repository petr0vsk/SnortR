################################################################################################################ 
# этот скрипт демонстрирует простейшие приемы первичной обработки трафика, содержащего tcp-сканирование.
# трафик предварительно должен быть конвертирован в csv/txt файл скриптом pcap_to_txt.sh
# кластерный анализ для анализа потоков
# 
############################################################################################################
rm(list=ls())
######################################### function plot_timeseries #################################################
#
# функция fn.return.cluster.flow получает в качестве аргументов имя дата-фрейма 
# с колонками: [номер потока <--> номер кластера], иходный дата-фрейм с полным трафиком по потокам
# и номер кластера траффик которого необходимо выделить.
# возвращает дата-фрейм с трафиком (tcp-потоками), входящим в конкретный кластер (по умолчанию кластер № 1)
####################################################################################################################
fn.return.cluster.flow <- function(data.fr, data.set, nm.clust=1){
  dataset_iptcp.clust.traffic <- data.frame()
  # получим индексы с потоками заданного кластера
  data.fr %>%
    filter(num.clust==nm.clust) -> cl.index  
  # вытащим из общего трафика - трафик заданного кластера
  data.set %>%
    filter(tcp.stream %in% cl.index$num.tcp.stream) -> iptcp.clust.traffic  
  return (iptcp.clust.traffic)
}
#######################################################################################################################
library(jsonlite)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(cluster)
library(factoextra)
library(VIM)
##----------------------------------------------------------------------------------##
# read pcap
setwd('/home/petr0vsk/WorkR/SnortR/TrafficExamples')
#read dataset
#dataset <- read.table("HackChallenge_Cmas2011.txt", quote="\"", header = T, sep=",") %>%
#  na.omit()
dataset <- read.table("scan_traffic.txt", quote="\"", header = T, sep=",") %>%
  na.omit()



# удалим лишнее - канальный уровень ------------------------------------------------------------
dataset_iptcp <- select(dataset, frame.number, frame.time, frame.protocols, ip.id, ip.flags, ip.src, ip.dst, contains("tcp."),-tcp.flags.ns, -tcp.flags.cwr, -tcp.flags.ecn ,X_ws.expert.message) 
# # гистограмма распределения длины пакетов tcp
ggplot(dataset_iptcp, aes(x=tcp.len)) +
  geom_histogram(binwidth=60, alpha=0.5, fill="red")
#---------- проведем анализ по общей длине пакетов tcp для каждого tcp-потока--------------------------
## проссумируем общую длину поля данных пакета tcp
# для каждого tcp_streams
sum_tcp_len <- dataset_iptcp %>%
  group_by(tcp.stream) %>%
  summarise(sum_tcp.len = sum(tcp.len))# %>%
# посмотрим что получилось
#aggr(sum_tcp_len)
summary(sum_tcp_len)
# нормализуем параметры
sum_tcp_len_norm <- scale(sum_tcp_len)
# K-MEANS -----
# определим оптимальное число кластеров для tcp_streams
fviz_nbclust(sum_tcp_len_norm, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
df.sum.tcp.len <- kmeans(sum_tcp_len_norm, 4, nstart = 25)
# визуализируем картинку и видим четкое разделение 
fviz_cluster(df.sum.tcp.len, data = sum_tcp_len_norm,  geom = "point", 
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_linedraw())
# выделим из трафика потоки, принадлежащие  разным кластерам
sum_tcp_len.clust <- data.frame(sum_tcp_len$tcp.stream, df.sum.tcp.len$cluster) %>%
  rename(num.tcp.stream = sum_tcp_len.tcp.stream , num.clust = df.sum.tcp.len.cluster )

traff.cluster.1  <- fn.return.cluster.flow(sum_tcp_len.clust, dataset_iptcp, 1)
traff.cluster.2  <- fn.return.cluster.flow(sum_tcp_len.clust, dataset_iptcp, 2)
traff.cluster.3  <- fn.return.cluster.flow(sum_tcp_len.clust, dataset_iptcp, 3)
traff.cluster.4  <- fn.return.cluster.flow(sum_tcp_len.clust, dataset_iptcp, 4)

##----------------------------------------------------------------------------------------
## проссумируем количетсво пакетов в каждом tcp_stream
count_tcp_stream <- dataset_iptcp %>% 
  group_by(tcp.stream) %>%
  count()
# посмотрим что получилось
#aggr(sum_tcp_len)
summary(count_tcp_stream)
# нормализуем параметры
count_tcp_stream_norm <- scale(count_tcp_stream)
# K-MEANS -----
# определим оптимальное число кластеров для tcp_streams
fviz_nbclust(count_tcp_stream_norm, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
df.count.tcp.stream <- kmeans(count_tcp_stream_norm, 4, nstart = 25)
# визуализируем картинку и видим четкое разделение 
fviz_cluster(df.count.tcp.stream, data = sum_count_tcp_stream,  geom = "point", 
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_linedraw())
# выделим из трафика потоки, принадлежащие  разным кластерам
count_tcp_stream.clust <- data.frame(count_tcp_stream$tcp.stream, df.count.tcp.stream$cluster) %>%
  rename(num.tcp.stream = count_tcp_stream.tcp.stream , num.clust = df.count.tcp.stream.cluster )

traff.cluster.11  <- fn.return.cluster.flow(count_tcp_stream.clust, dataset_iptcp, 1)
traff.cluster.22  <- fn.return.cluster.flow(count_tcp_stream.clust, dataset_iptcp, 2)
traff.cluster.33  <- fn.return.cluster.flow(count_tcp_stream.clust, dataset_iptcp, 3)
traff.cluster.44  <- fn.return.cluster.flow(count_tcp_stream.clust, dataset_iptcp, 4)



############################################################################################
## попробуем обработать набор, где содержатся и суммарная длина поля пакета tcp 
## и количество пакетов в потоке
## make frame with tcp_len and packet-count
#############################################################################################
df_count.and.len.tcp <- full_join(count_tcp_stream, sum_tcp_len, by="tcp.stream") 
df_count.and.len.tcp <- df_count.and.len.tcp[,2:3]
summary(df_count.and.len.tcp)
# нормализуем параметры
df_count.and.len.tcp_norm <- scale(df_count.and.len.tcp)

# K-MEANS -----
# определим оптимальное число кластеров
fviz_nbclust(df_count.and.len.tcp_norm, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
fviz_nbclust(df_count.and.len.tcp_norm, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")
fviz_nbclust(df_count.and.len.tcp_norm, kmeans, method = "gap_stat") +
  labs(subtitle = "Gap statistic method")
# получаем 3 - кластера
# попробуем разбить на 3  кластера
df.res2 <- kmeans(df_count.and.len.tcp_norm, 3, nstart = 25)
# визуализируем картинку и видим четкое разделение 
fviz_cluster(df.res2, data = df_count.and.len.tcp_norm,  geom = "point", 
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_linedraw())
####################################################
# попробуем иерархическую кластеризацию
res.hc <- hclust(dist(df_count.and.len.tcp_norm),  method = "ward.D2")
fviz_dend(res.hc, cex = 0.5, k = 4, palette = "jco") 
############################################################
# PAM метод ---
# определим количество кластеров
fviz_nbclust(df_count.and.len.tcp_norm, pam, method = "silhouette")+
  theme_classic()
# 3 -clusters
pam.res2 <-pam(df_count.and.len.tcp_norm, 2)
fviz_cluster(pam.res2, geom = "point", 
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_linedraw())

# CLARA method ---
fviz_nbclust(df_count.and.len.tcp_norm, clara, method = "silhouette")+
  theme_classic()
clara.res <- clara(df_count.and.len.tcp_norm, 2, samples = 50, pamLike = TRUE)

fviz_cluster(clara.res,
             geom = "point", 
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_linedraw())

# DENDROGRAMS method  ---
# make dissimilatarty matrix
res.dist <- dist(df_count.and.len.tcp_norm, method = "euclidean")
# let`s see 6 rows and columns of the distance matrix:`
as.matrix(res.dist)[1:6, 1:6]
# make hierar cluster`s`
res.hc <- hclust(d = res.dist, method = "ward.D2")
fviz_dend(res.hc, cex = 0.5)
# Compute cophentic distance
res.coph <- cophenetic(res.hc)
# Correlation between cophenetic distance and
# the original distance
cor(res.dist, res.coph)
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          rect_fill = TRUE)
# graph view

require("igraph")
fviz_dend(res.hc, k = 4, # Cut in four groups
          k_colors = "jco",
          type = "phylogenic", repel = TRUE,
          phylo_layout = "layout.gem")

# Growing Neural Gas
require("gmum.r")  
gng <- GNG(df_count.and.len.tcp_norm, max.nodes=20)

predict(gng, rep(1,ncol(df_count.and.len.tcp_norm)))
meanError(gng)

# Plot with first 2 coordinates as position
plot(gng, vertex.color="cluster")















