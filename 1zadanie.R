#Пестрякова Елизавета, Д-Х123, вариант 9
#ЗАДАНИЕ:для региона 73 Ульяновская область рассчитайте урожайность 
#пшеницы в 2003 году, взяв для рассчета средние суммы активных температур 
#за предыдущие 9 лет, с метеостанций на расстоянии от 70 до 210 км

#Установка рабочей директории
setwd("D:/TYSON/MatMod");getwd()
#Устанавливаем пакеты
library (tidyverse)
library(rnoaa)
library(lubridate)

#Скачивание списка метеостанций
#station_data=ghcnd_stations()
#write.csv(station_data,file = "station_data.csv")
station_data = read.csv("station_data.csv")

#формирование спаиска метеостанций
ulyanovsk=data_frame(id="ulyanovsk",latitude = 54.32824,
                     longitude = 48.38657)
#Задаём параметры для выбора метеостанций
ulyanovsk_around=meteo_nearby_stations(lat_lon_df = ulyanovsk, 
                                       station_data = station_data, 
                                       limit=20,
                                       var = c("PRCP", "TAVG"), 
                                       year_min = 1994, year_max = 2003)
#Получаем идентификатор метеостанции Ульяновска
ulyanovsk_id=ulyanovsk_around[["ulyanovsk"]][["id"]][1]
summary(ulyanovsk_id)
#Таблица всех метеостанций
ulyanovsk_table=ulyanovsk_around[[1]]
summary(ulyanovsk_table)
#Фильтруем станции, оставляя на расстоянии 70-210 км
ulyanovsk_table=filter(ulyanovsk_table,distance>=70&distance<=210)
ulyanovsk_stations=ulyanovsk_table
#Смотрим список необходимых станций
str(ulyanovsk_stations)
ulyanovsk_stations$id

#Скачивание погодных данных для выбранных метеостанций
#all_ulyanovsk_data=meteo_tidy_ghcnd(stationid = ulyanovsk_id)
#Смотрим, что скачали
summary(all_ulyanovsk_data)

#Объект в который скачиваем все данные метеостанций
all_ulyanovsk_meteodata=data.frame()
#Цикл, в котором собираем данные нужных метеостанций в одну таблицу
station_names=ulyanovsk_stations$id
station_names=station_names [1:16]

for(sname in station_names){
  one_meteo=meteo_tidy_ghcnd(stationid = sname,
                             date_min = "1994-01-01",
                             date_max = "2003-12-31")
  station_vars=names(one_meteo)
  if(!("tavg" %in% station_vars)){
    if(!("tmax" %in% station_vars)){
      next()
    }
    one_meteo=one_meteo%>%mutate(tavg=(tmax+tmin)/2)
  }
  one_meteo=one_meteo %>% select(id,date,tavg)
  
  one_meteo = one_meteo%>%mutate(tavg=tavg/10)
  all_ulyanovsk_meteodata = rbind(all_ulyanovsk_meteodata,one_meteo)
}
#Записываем полученные результаты
write.csv(all_ulyanovsk_meteodata,"all_ulyanovsk_meteodata.csv")
#Скачиваем данные
#all_ulyanovsk_meteodata=read.csv("all_ulyanovsk_meteodata.csv")
#Смотрим, что скачали
str(all_ulyanovsk_meteodata)
#Добавим даты
all_ulyanovsk_meteodata=all_ulyanovsk_meteodata %>% mutate(year=year(date),
                                                           month=month(date),
                                                           day=day(date))

#Заменим NA и tavg < 5 на 0
all_ulyanovsk_meteodata [is.na(all_ulyanovsk_meteodata$tavg),"tavg"]=0
all_ulyanovsk_meteodata [all_ulyanovsk_meteodata$tavg<5, "tavg"]=0

#Группировка по метеостанциям, годам и месяцам
group_meteodata=all_ulyanovsk_meteodata %>% group_by(id,year,month)
#Ищем сумму температур по полученным группам
sumT_group_meteodata=group_meteodata %>% summarise(tsum=sum(tavg))

#Группировка данных по месяцам
groups_months=sumT_group_meteodata %>% group_by(month)
groups_months

#Считаем среднее по месяуам для всех метеостанций и всех лет
sumT_months=groups_months %>% summarise(St=mean(tsum))
sumT_months

#Подготовка к расчёту урожая по формуле
#Константы для расчёта урожайности
afi = c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00)
bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
di = c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
#Коэффициент для экспозиции склона (все поля идеально ровные)
y = 1.0
#Коэффициент использования ФАР посевом
Kf = 300
#Калорийность урожая культуры
Qj = 1600
#Коэффициент "сумма частей основной и побочной продукции"
Lj = 2.2
#Коэффициент "стандартная влажность культуры"
Ej = 25 

#Считаем Fi по месяцам
sumT_months= sumT_months %>% mutate(Fi=afi+bfi*y*St)

#Считаем Yj 
sumT_months= sumT_months %>% mutate(Yj=((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))

#Считаем урожай как сумму по месяцам
Yield=sum(sumT_months$Yj)
Yield

#Урожайность = 17.6 ц/га
#Если умножать на 10^6, то получаем значение в мг/га, 
#поэтому у нас получилось значение в ц/га