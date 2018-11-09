########################################################################################
# Переведем иррегулярную временную серию к регулярному виду
# для этого агрегируем данные (например о длине пакета) внутри интервала в одну 
# секунду, после чего добавим во временную шкалу пропущенные секунды установив для них
# значение_переменной = 0
##########################################################################################

library(zoo)
library(ggplot2)
library(forecast)
#-# tcp_streams <- tcp_streams[c(1:10), ] # временно укоротим выборку для скорости макетирования
time <- as.character.Date(tcp_streams$frame.time)
# преобразуем timestamp к виду пригодному для обработки
time.t <- strptime(time, '%b %d, %Y %H:%M:%S')
# создадим временную серию класса zoo
irreg.times <- zoo(tcp_streams$frame.len, order.by = time.t )
# суммируем длину пакетов внутри окна в одну секунду
irreg.times.agg  <- aggregate(irreg.times, as.POSIXct(trunc(time(irreg.times), "sec")), sum)
# теперь вычисляем шкалу секунд без пропусков от начальной до конечной секунды выборки 
sec_interval <- zoo(, seq(start(irreg.times), end(irreg.times), "sec"))
# и наконец объеденим две шкалы, приводя выборку к регулярной. пропуски по времени выборки
# заполняем нулями
reg.times.agg <- merge(irreg.times.agg, sec_interval, fill = 0)
autoplot.zoo(reg.times.agg) + ggtitle("Общая длина пакетов канального уровня") + geom_area(alpha = 0.15) + geom_point() + geom_smooth(method = "loess", se = FALSE) + labs(x = "Время", y = "Суммарна длина пакетов/секунду")
