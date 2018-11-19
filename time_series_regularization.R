
rm(list=ls())
library(zoo)
library(ggplot2)
library(forecast)
library(lubridate)
library(jsonlite)
library(tidyr)
library(dplyr)
library(stringr)
library(cowplot)
######################################### function make_timeseries ##############################################################
# функция make_timeseries получает в качестве аргументов имя набора данных и поля этого набора
# данных, значение которого будет суммироваться на интервале в одну секунду
# функция возвращает ДВА АРГУМЕНТА регулярный временной ряд класса zoo() и поле по которому производилось суммирование
# ОГРАНИЧЕНИЕ: шаг временной шкалы = 1 секунда
#####################################################################################################################################

make_timeseries <- function(dataset_name, col_name, plt=FALSE) {
  # распарсим имя таблицы фрейма для передачи в качестве аргумента
  col_name <- deparse(substitute(col_name))
  # переведем время в строку для суммирования
  dataset_name$frame.time  <- as.character.Date(strptime(dataset_name$frame.time, '%b %d, %Y %H:%M:%S'))
  # просуммируем значение поля набора данных внутри окна в одну секунду
  dataset_sum <- dataset_name %>%
    group_by(frame.time) %>%
    summarise(sum_len = sum(!!rlang::sym(col_name)) ) 
  # переведем время из строкового типа к временному типу POSIX для манипулирования как с временным рядом
  dataset_sum$frame.time <- as_datetime(dataset_sum$frame.time)
  # создадим временную серию класса zoo, при этом временная шкала неравномерна (иррегулярна)
  irreg.times <- zoo(dataset_sum$sum_len, order.by = dataset_sum$frame.time)  
  # теперь вычисляем шкалу секунд без пропусков от начальной до конечной секунды выборки 
  sec_interval <- zoo(, seq(start(irreg.times), end(irreg.times), "sec"))
  # и наконец объеденим две шкалы, приводя выборку к регулярной. пропуски значений поля данных по времени выборки заполняем нулями
  reg.times.agg <- merge(irreg.times, sec_interval, fill = 0)
  # если ни чего не укзали в третьем аргументе, то по умолчанию выводим график
  if(missing(plt)) {
    plt=FALSE
   } 
  if (plt) { 
    print("TRUE")
    plot(reg.times.agg)
    autoplot.zoo(reg.times.agg) + ggtitle( "Распределение по времени длины пакетов " ) + geom_line(alpha = 0.15) + geom_point() + geom_smooth(method = "loess", se = FALSE) + labs(x = "Время", y = "Суммарна длина пакетов/секунду") 
    #autoplot.zoo(reg.times.agg ) + ggtitle( str_c("Распределение по времени длины пакетов ", as.character(col_name)) ) + geom_line(alpha = 0.15) + geom_point() + geom_smooth(method = "loess", se = FALSE) + labs(x = "Время", y = "Суммарна длина пакетов/секунду") 
  } else {
    print("For plotting use  make_timeseries(dataset_name, col_name, plt=TRUE)")
  }
  # вернем  временную серию из функции для дальнейшей обработки
  result.list <- list("timeseries_zoo" = reg.times.agg,  "col_name" = col_name)
  return(result.list)
  
}
##############################################################################################################
######################################### function plot_timeseries ##############################################################
#
# функция plot_timeseries получает в качестве аргументов имя временной серии  и поле по которому производися анализ
# 
####################################################################################################################
plot_timeseries <- function(timeseries) {
  
  autoplot.zoo(timeseries$timeseries_zoo) + ggtitle(  str_c("Распределение по времени длины пакетов ", 
      timeseries$col_name) ) + geom_line(alpha = 0.15) + geom_point() + geom_smooth(method = "loess", se = FALSE) + labs(x = "Время", y = "len packet/sec") 

}
##############################################################################
##----------------------------------------------------------------------------------##
# read pcap
setwd('/root/WorkR/SnortR/SnortR/TrafficExamples')
#read dataset
#dataset <- read.table("scan_tcp-1.txt", quote="\"", header = T, sep=",")
dataset <- read.table("CTF_1.txt", quote="\"", header = T, sep=",") %>%
na.omit(dataset)
#-------------------------------------------
timeseries.zoo.farme.len <- make_timeseries(dataset, frame.len)
plot.frame.len <- plot_timeseries(timeseries.zoo.farme.len)
#
timeseries.zoo.tcp.len <- make_timeseries(dataset, tcp.len)
plot.tcp.len <- plot_timeseries(timeseries.zoo.tcp.len)
#
timeseries.zoo.hdr.len <- make_timeseries(dataset, tcp.hdr_len)
plot.tcp.hdr.len <- plot_timeseries(timeseries.zoo.hdr.len)
#
timeseries.zoo.flags.reset <- make_timeseries(dataset, tcp.flags.reset)
plot.tcp.flags.reset <-  plot_timeseries(timeseries.zoo.flags.reset)
#
timeseries.zoo.flags.ack <- make_timeseries(dataset, tcp.flags.ack)
plot.tcp.flags.ack <- plot_timeseries(timeseries.zoo.flags.ack)
#
timeseries.zoo.flags.syn <- make_timeseries(dataset, tcp.flags.syn)
plot.tcp.flags.syn <- plot_timeseries(timeseries.zoo.flags.syn)
#
#plot_grid(plot.frame.len , plot.frame.len, plot.tcp.hdr.len , plot.tcp.flags.reset,
#          timeseries.zoo.flags.ack, timeseries.zoo.flags.syn, nrow = 6, align = "v")

plot_grid(plot.frame.len,       plot.tcp.flags.syn,
          plot.tcp.len,         plot.tcp.flags.ack,
          plot.tcp.hdr.len,     plot.tcp.flags.reset, 
          ncol = 2)
#https://habr.com/post/339090/






