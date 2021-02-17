#Пестрякова Елизавета - для региона 73 Ульяновская область
#ЗАДАНИЕ:рассчитайте урожайность пшеницы в 2003 году, взяв для рассчета средние суммы активных температур за предыдущие 9 лет, с метеостанций на расстоянии от 70 до 210 км
#Установка рабочей директории
setwd("D:/TYSON/MatMod");getwd()
#Устанавливаем пакеты
library (tidyverse)
library(rnoaa)
station_data=ghcnd_stations()
write.csv(station_data,"station_data.csv")
station_data = read.csv("station_data.csv")
ulyanovsk=data_frame(id="ulyanovsk",latitude = 54.32824,  longitude = 48.38657)
ulyanovsk_around=meteo_nearby_stations(lat_lon_df = ulyanovsk, station_data = station_data, limit = 20, var = c("PRCP", "TAVG"), year_min = 1994, year_max = 2003)
ulyanovsk_id=ulyanovsk_around[["ulyanovsk"]][["id"]][1]
summary(ulyanovsk_id)
ulyanovsk_table=ulyanovsk_around[[1]]
summary(ulyanovsk_table)
ulyanovsk_stations=ulyanovsk_table[ulyanovsk_table$ulyanovsk.distance > 70 & ulyanovsk_table$ulyanovsk.distance < 210]
str(ulyanovsk_stations)
ulyanovsk_stations$ulyanovsk.id
