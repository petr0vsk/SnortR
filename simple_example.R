################################################################################################################ 
#этот скрипт демонстрирует простейшие приемы первичной обработки трафика, содержащего tcp-сканирование.
# трафик предварительно должен быть конвертирован в csv/txt файл скриптом pcap_to_txt.sh
# атакующий хост - 192.168.30.68
# хост мишень - 192.168.30.53
############################################################################################################


rm(list=ls())
library(jsonlite)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
##---------------------- функции -------------------------

################################################################
# функция получает список со всеми протоколами, конкретного tcp-потока и возвращает
#  протоколы начиная от tcp и выше
##################################################################
get_proto <- function(x){
  x <- str_split(x, ",")
  temp <- x[[1]][1]
 
  for (i in 2:length(x[[1]]) )
  { # ищем самую длинную цепочку со списком протоколов
    if( nchar(x[[1]][i]) > nchar(temp)) temp <-  (x[[1]][i])
     }
  # возвращаем откусив все протоколы. которые ниже tcp-udp
  return(str_sub(temp,19, nchar(temp)))
}
########################################################################
##----------------------------------------------------------------------------------##
# read pcap
setwd('WorkR/SnortR/TrafficExamples')
#read dataset
dataset <- read.table("scan_tcp-1.txt", quote="\"", header = T, sep=",")
#dataset <- read.table("CTF_1.txt", quote="\"", header = T, sep=",") %>%
na.omit(dataset)
# удалим лишнее - канальный уровень
dataset_iptcp <- select(dataset, frame.number, frame.time, frame.protocols, ip.id, ip.flags, ip.src, ip.dst, contains("tcp."),-tcp.flags.ns, -tcp.flags.cwr, -tcp.flags.ecn ,X_ws.expert.message) 
# # гистограмма распределения длины пакетов tcp
ggplot(dataset_iptcp, aes(x=tcp.len)) +
  geom_histogram(binwidth=60, alpha=0.5, fill="red")



# график распределения плотности длины пакетов tcp
ggplot(dataset_iptcp, aes(x=tcp.len)) +
  geom_density(alpha=0.5, fill="steelblue")
# скомбинируем два графика
ggplot(dataset_iptcp, aes(x = tcp.len, y =..density..)) +
  geom_histogram(binwidth=40, alpha=0.7, fill="red", size=.1) + 
  geom_density(alpha=0.25, fill="steelblue") 

# визуализируем распределение  длины поля данных tcp по времени (номеру сетевого фрейма)
ggplot(data=dataset_iptcp, mapping=aes(y=tcp.len,  x=frame.number, size=tcp.len, color=frame.protocols)) + 
  geom_point(alpha=0.35)+
  scale_size_area() +
  ggtitle("Распределение длины поля даных пакета") +
  xlab("Номер сетевого пакета") +
  ylab("Длина поля данных в потоке (Kb)")
 
#
ggplot(dataset_iptcp, aes(y=tcp.len,  x=frame.number)) +
  stat_bin2d(position = "identity")

ggplot(dataset_iptcp, aes(y=tcp.len,  x=frame.number)) +
  stat_bin2d(position = "identity") + 
  facet_grid(tcp.len~frame.number)
#
ggplot(dataset_iptcp, aes(y=tcp.len,  x=frame.number)) +
  stat_binhex(position = "identity")
#

# Признаки сканирования:
# 1. Короткая tcp_stream, прежде всего по количеству пакетов в сессии и наличию флага RST
# 2. Существенное увеличение пакетов с флагом RST, отправляемых в ответ на устанавливаемое соединение (порт закрыт)
# 3. Увеличение попыток открытия серверных и особенно клиентских портов за короткий промежуток времени.
# 4. Может быть имеет смысл составить (для одного хоста)карту портов и проверять с какими портами была попытка установления
# соединения. При сканировании количество попытко должно стремится к 65535.

# 1. Посчитаем длину tcp_stream, исследуем параметры, выведем статику, проведем кластеризацию.
# 

summary(dataset_iptcp$ip.src)
summary(dataset_iptcp$ip.dst)
summary(dataset_iptcp$tcp.len)
summary(dataset_iptcp$tcp.srcport)
summary(dataset_iptcp$tcp.dstport)


quantile(dataset_iptcp$tcp.len)
hist(dataset_iptcp$tcp.len)
plot(density(dataset_iptcp$tcp.len))
table(dataset_iptcp$tcp.len)
barplot(table(dataset_iptcp$tcp.len))
pie(table(dataset_iptcp$tcp.len))

# коррелируется ли длина пакета с портом назначения?
cor(dataset_iptcp$tcp.len, dataset_iptcp$tcp.dstport)

boxplot(tcp.len~frame.number, data = dataset_iptcp)

#проссумируем общую длину поля данных пакета tcp
# для каждого tcp_streams

sum_tcp_len <- dataset_iptcp %>%
  group_by(tcp.stream) %>%
  summarise(sum_tcp.len = sum(tcp.len)) %>%
  arrange(sum_tcp.len)
# проссумируем количетсво пакетов в каждом tcp_stream
count_tcp_stream <- dataset_iptcp %>% 
  group_by(tcp.stream) %>%
  count()
# будет использовать в качестве названия класса протоколы от tcp и выше ##########################
class_tcp_stream <- dataset_iptcp %>%
  group_by(tcp.stream)%>%
    summarise(protocols = toString(frame.protocols) ) %>%
    as.data.frame()
for (i in 1:length(class_tcp_stream$protocols))
 { # оставим в столбце протокол проткол tcp и выше
   class_tcp_stream$protocols[i] <- get_proto(class_tcp_stream$protocols[[i]])
}

#объеденим полученный результат в один фрейм
count_len_tcp_stream <- left_join(sum_tcp_len, count_tcp_stream, by="tcp.stream")
count_len_tcp_stream <- left_join(count_len_tcp_stream, class_tcp_stream, by="tcp.stream") %>%
  arrange(tcp.stream) 
  

# есть ли корреляция между количеством пакетов и суммарной длинной поля данных в каждом потоке?
cor(sum_tcp_len$sum_tcp.len, count_tcp_stream$n)
#
ggplot(data = count_len_tcp_stream, mapping = aes(x=n, y=sum_tcp.len, color=protocols, size=sum_tcp.len)) + 
  geom_point() +
  geom_point(alpha=0.35)+
  scale_size_area() +
  expand_limits(y=0) + 
  #geom_density() +
  geom_smooth(aes(group = 1), method = "lm", size = 0.3,  se = F, linetype =2) +
  #geom_density() +
  #facet_grid(. ~ protocols) +
  ggtitle("Корреляция между количеством пакетов и суммарной длиной поля данных в каждом потоке") +
  xlab("Количество пакетов в потоке") +
  ylab("Суммарная длина поля данных в потоке (Kb)")
 
 # распределение плотности пакетов по суммарной длине 
  
  ggplot(count_len_tcp_stream, aes(n))+
    geom_histogram(aes( y = ..density.. ), binwidth =  1) 
  
  ggplot(count_len_tcp_stream, aes(sum_tcp.len))+
    geom_histogram(aes( y = ..density.. ), binwidth =  1000)
  
# проверим группировку пакетов в пространстве кол-во пакетов --- общая длина пакетов tcp в одном tcp_stream

 
clstr <- count_len_tcp_stream[,c(2,3)]
#<- select(count_len_tcp_stream, sum_tcp.len, n)
kmeans.result <- kmeans(clstr, 3)
kmeans.result
plot(clstr, col = kmeans.result$cluster)
#plot cluster centers
plot(kmeans.result$centers, col = 1:3, pch = 8, cex = 2)
layout(matrix(c(1,2),1,2))
plot(pamk.result$pamobject)
layout(matrix(1))


# The k-Medoids Clustering
library(fpc)
pamk.result <- pamk(clstr)
pamk.result$nc
table(pamk.result$pamobject$clustering, count_len_tcp_stream$protocols)









