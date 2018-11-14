
rm(list=ls())
library(zoo)
library(ggplot2)
library(forecast)
library(lubridate)
library(jsonlite)
library(tidyr)
library(dplyr)
library(stringr)
######################################### function make_timeseries ##############################################################
# функция make_timeseries получает в качестве аргументов имя набора данных и поля этого набора
# данных, значение которого будет суммироваться на интервале в одну секунду
# функция возвращает регулярный временной ряд класса zoo()
# ОГРАНИЧЕНИЕ: шаг временной шкалы = 1 секунда
##########################################################################################

make_timeseries <- function(dataset_name, col_name) {
  col_name <- deparse(substitute(col_name))
  str(col_name)
   # переведем время в строку для суммирования
  dataset_name$frame.time  <- as.character.Date(strptime(dataset_name$frame.time, '%b %d, %Y %H:%M:%S'))
  # просуммируем значение поля набора данных внутри окна в одну секунду
  dataset_sum <- group_by(dataset_name, col_name)
  
  #dataset_sum <- dataset_name %>%
  #  group_by(frame.time) %>%
  #  summarise(sum_len = sum(col_name) ) 
  # переведем время из строкового типа к временному типу POSIX для манипулирования как с временным рядом
  dataset_sum$frame.time <- as_datetime(dataset_sum$frame.time)
  # создадим временную серию класса zoo, при этом временная шкала неравномерна (иррегулярна)
  irreg.times <- zoo(dataset_sum$sum_len, order.by = dataset_sum$frame.time)  
  # теперь вычисляем шкалу секунд без пропусков от начальной до конечной секунды выборки 
  sec_interval <- zoo(, seq(start(irreg.times), end(irreg.times), "sec"))
  # и наконец объеденим две шкалы, приводя выборку к регулярной. пропуски значений поля данных по времени выборки заполняем нулями
  reg.times.agg <- merge(irreg.times, sec_interval, fill = 0)
  # вернем ее из функции для дальнейшей обработки
  return(reg.times.agg)
  
}
#######################################################################################################################################
##----------------------------------------------------------------------------------##
# read pcap
setwd('/root/WorkR/SnortR/SnortR/TrafficExamples')
#read dataset
#dataset <- read.table("scan_tcp-1.txt", quote="\"", header = T, sep=",")
dataset <- read.table("CTF_1.txt", quote="\"", header = T, sep=",") %>%
na.omit(dataset)

timeseries_zoo <- make_timeseries(dataset, tcp.len)
autoplot.zoo(timeseries_zoo) + ggtitle("Распределение по времени длины пакетов tcp") + geom_line(alpha = 0.15) + geom_point() + geom_smooth(method = "loess", se = FALSE) + labs(x = "Время", y = "Суммарна длина пакетов/секунду") 


#####################################################################################################3

# просуммируем длину пакета tcp с интервалом в одну секунду
dataset$frame.time  <- as.character.Date(strptime(dataset$frame.time, '%b %d, %Y %H:%M:%S'))
dataset2 <- dataset %>%
  group_by(frame.time) %>%
  summarise(sum_tcp.len = sum(tcp.len))
dataset2$frame.time <- as_datetime(dataset2$frame.time)
#########################################################################
# суммируем длину пакетов протокола TCP внутри окна в одну секунду
#############################################################################
# создадим временную серию класса zoo
irreg.times <- zoo(dataset2$sum_tcp.len, order.by = dataset2$frame.time)  
# теперь вычисляем шкалу секунд без пропусков от начальной до конечной секунды выборки 
sec_interval <- zoo(, seq(start(irreg.times), end(irreg.times), "sec"))
# и наконец объеденим две шкалы, приводя выборку к регулярной. пропуски по времени выборки
# заполняем нулями
reg.times.agg <- merge(irreg.times, sec_interval, fill = 0)
autoplot.zoo(reg.times.agg) + ggtitle("Распределение по времени длины пакетов tcp") + geom_line(alpha = 0.15) + geom_point() + geom_smooth(method = "loess", se = FALSE) + labs(x = "Время", y = "Суммарна длина пакетов/секунду") 





