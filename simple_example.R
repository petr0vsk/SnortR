
# этот скрипт демонстрирует простейшие приемы первичной обработки трафика.
# трафик предварительно должен быть конвертирован в csv/txt файл скриптом pcap_to_txt.sh
rm(list=ls())
library(jsonlite)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
# read pcap
setwd('/WorkR/SnortR/TrafficExamples/')
# find txt-files with formatting traffic
file_list <- str_subset((list.files()), "txt")
#convert files from pcap to table
for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.table(file, quote="\"", header = T, sep=",")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.table(file, quote="\"", header = T, sep=",")
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}
# extract from traffic tcp stream`s
tcp_streams <- filter(dataset, !is.na(tcp.stream))
# look at data
str(dataset)
summary(dataset)
# вывод суммарное количество пакетов/ip-адрес источника
dest <- count(dataset, ip.dst)
ggplot(dest) + 
  geom_bar(aes(x=reorder(ip.dst, n), y=n), stat="identity") + 
  coord_flip() +
  labs(title="Count of packets by destination address")
# вывод суммарное количество пакетов/ip-адрес источника
src <- count(dataset, ip.src)
ggplot(src) + 
  geom_bar(aes(x=reorder(ip.src, n), y=n), stat="identity") + 
  coord_flip() +
  labs(title="Count of packets by source address")
# гистограмма распределения длины пакетов канального уровня
ggplot(dataset, aes(x=frame.len)) +
  geom_histogram(alpha=0.5, fill="steelblue")
# график распределения плотности длины пакетов канального уровня
ggplot(dataset, aes(x=frame.len)) +
  geom_density(alpha=0.5, fill="steelblue")
# unique IP adress
unique_ip <- unique(c(dest, src))


