########################################################################################
# Переведем иррегулярную временную серию к регулярному виду
# для этого агрегируем данные (например о длине пакета) внутри интервала в одну 
# секунду, после чего добавим во временную шкалу пропущенные секунды установив для них
# значение_переменной = 0
##########################################################################################

rm(list=ls())
library(zoo)
library(ggplot2)
library(forecast)
##----------------------------------------------------------------------------------##
# read pcap
setwd('WorkR/SnortR/SnortR/TrafficExamples')
#read dataset
#dataset <- read.table("scan_tcp-1.txt", quote="\"", header = T, sep=",")
dataset <- read.table("CTF_1.txt", quote="\"", header = T, sep=",") %>%
na.omit(dataset)
#-# tcp_streams <- tcp_streams[c(1:10), ] # временно укоротим выборку для скорости макетирования
time <- as.character.Date(dataset$frame.time)
# преобразуем timestamp к виду пригодному для обработки
time.t <- strptime(time, '%b %d, %Y %H:%M:%S')
# создадим временную серию класса zoo
irreg.times <- zoo(dataset$frame.len, order.by = time.t )
# суммируем длину пакетов канального уровня внутри окна в одну секунду
irreg.times.agg  <- aggregate(irreg.times, as.POSIXct(trunc(time(irreg.times), "sec")), sum)
# теперь вычисляем шкалу секунд без пропусков от начальной до конечной секунды выборки 
sec_interval <- zoo(, seq(start(irreg.times), end(irreg.times), "sec"))
# и наконец объеденим две шкалы, приводя выборку к регулярной. пропуски по времени выборки
# заполняем нулями
reg.times.agg <- merge(irreg.times.agg, sec_interval, fill = 0)
autoplot.zoo(reg.times.agg) + ggtitle("Распределение по времени длины фреймов канального уровня") + geom_line(alpha = 0.15) + geom_point() + geom_smooth(method = "loess", se = FALSE) + labs(x = "Время", y = "Суммарна длина пакетов/секунду")
#########################################################################
# суммируем длину пакетов протокола TCP внутри окна в одну секунду
#############################################################################
time <- as.character.Date(dataset$frame.time)
# преобразуем timestamp к виду пригодному для обработки
time.t <- strptime(time, '%b %d, %Y %H:%M:%S')
# создадим временную серию класса zoo
irreg.times <- zoo(dataset$tcp.len, order.by = time.t )
# суммируем длину пакетов TCP внутри окна в одну секунду
irreg.times.agg  <- aggregate(irreg.times, as.POSIXct(trunc(time(irreg.times), "sec")), sum)
# теперь вычисляем шкалу секунд без пропусков от начальной до конечной секунды выборки 
sec_interval <- zoo(, seq(start(irreg.times), end(irreg.times), "sec"))
# и наконец объеденим две шкалы, приводя выборку к регулярной. пропуски по времени выборки
# заполняем нулями
reg.times.agg <- merge(irreg.times.agg, sec_interval, fill = 0)
autoplot.zoo(reg.times.agg) + ggtitle("Распределение по времени длины пакета TCP IP") + geom_area(alpha = 0.15) + geom_point() + geom_smooth(method = "loess", se = FALSE) + labs(x = "Время", y = "Суммарна длина пакетов/секунду")

########################################################################################
# функция вывода графика количественного параметра трафика (длины пакета соответствующего уровня модели TCP IP, кол-ва пакетов 
# с определенным флагом и т.п.). В качестве параметров принимает следующие значения:
# field - количественный параметр трафика
# interval - интервал на котором проводится суммирование (по умолчанию секунды)
#######################################################################################
plot_time_series <- function(field, interval) {
  # создадим временную серию класса zoo
  irreg.times <- zoo(field, order.by = time.t )
  # суммируем длину пакетов TCP внутри временного окна 
  irreg.times.agg  <- aggregate(irreg.times, as.POSIXct(trunc(time(irreg.times), "sec")), sum)
  # теперь вычисляем шкалу времени без пропусков от начальной до конечной точки выборки
  sec_interval <- zoo(, seq(start(irreg.times), end(irreg.times), "sec"))
  # и наконец объеденим две шкалы, приводя выборку к регулярной. пропуски по времени выборки
  # заполняем нулями
  eg.times.agg <- merge(irreg.times.agg, sec_interval, fill = 0)
  autoplot.zoo(reg.times.agg) + ggtitle("Распределение по времени") + geom_area(alpha = 0.15) + geom_point() + geom_smooth(method = "loess", se = FALSE) + labs(x = "Время", y = "Суммарна длина пакетов/секунду")
}



