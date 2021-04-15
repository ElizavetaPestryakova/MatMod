#Пестрякова Елизавета Д-Х 123, вариант 9
#модель множественной линейной регрессии дневных потоков паров воды 
#за период 2013 года по данным измерений методом турбулентной пульсации

#подключение пакетов
library(tidyverse)
library(rnoaa)
library(lubridate)
library(stringr)

eddypro = read.csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))

#приводим таблицу в удобный вид
#удаляем пустую первую строку
eddypro = eddypro[-1, ]
#возвратим столбцы таблицы в виде векторов для проверки
glimpse(eddypro)
#удаляем  пустой столбец "roll"
eddypro = select(eddypro, -(roll))
#заменим символы в названии стобцов на имена
names(eddypro) = names(eddypro) %>%
  str_replace_all("[!]","_exclam_") %>%
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_slash_") %>%
  str_replace_all("[%]", "__pecent_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]","_")

#фильтруем данные, оставляя только дневное время
eddypro = filter(eddypro, daytime)

#преобразуем в факторы (factor) столбы типа char (символ)
eddypro = eddypro %>% mutate_if(is.character, as.factor)

#где содержатся численные значения
eddypro_numeric=eddypro[,sapply(eddypro,is.numeric)]
str(eddypro_numeric)

#корреляционный анализ
cor_eddy=cor(eddypro_numeric)
str(cor_eddy)
#считаем сколько NA в каждой переменной
na_cor_eddy=eddypro_numeric %>% summarise_all(~sum(is.na(.x)))
navect=na_cor_eddy[1,]%>% as.integer()
#смотрим у каких переменных NA > 30
names(eddypro_numeric)[navect>30]
#исключим все NA
eddypro_numeric=na.exclude(eddypro_numeric)
#считаем коэф корреляции
cor_eddy=cor(eddypro_numeric)
cor_eddy=data.frame(cor_eddy)
#коэф детерминации для нашей зависимой переменной
cor_vars=cor_eddy$h2o_flux^2
names(cor_vars)=names(cor_eddy)
#выбрем только значимые коэффициенты, в которых коэф детерминации более 0,16
cor_vars=cor_vars[cor_vars>0.16]
# узнаем имена значимых переменных
names(cor_vars)%>% na.exclude()

#Создадим непересекающиеся выборки
row_numbers = 1:length(eddypro_numeric$h2o_flux)
#Обучающая
teach = sample(row_numbers, floor(length(eddypro_numeric$h2o_flux)*.7))
#Тестирующая
test = row_numbers[-teach]
#Обучающая
teaching_edd = eddypro_numeric[teach,]
#Тестирующая
testing_edd = eddypro_numeric[test,]


#модель 1
mod1=lm(h2o_flux ~ rand_err_Tau + rand_err_H + qc_LE + rand_err_LE + co2_flux + rand_err_co2_flux
        + qc_h2o_flux + rand_err_h2o_flux + h2o_time_lag + sonic_temperature + air_density
        + air_molar_volume + es + RH + VPD + u. + TKE + un_H + un_LE + un_co2_flux + un_h2o_flux
        + h2o_var + w.co2_cov + w.h2o_cov + flowrate, data = teaching_edd)
#Коэффициенты
coef(mod1)
#остатки
resid(mod1)
#доверительный интервал
confint(mod1)
#P-значения по модели
summary(mod1)
#коэф детерминации = 0.9997
#Дисперсионный анализ
anova(mod1)
#Графиик на нормальной веротяностной бумаге
plot(mod1,2)
#данные распределены нормально
#график наблюдаемых значений от предсказанных значений
plot(mod1$fitted.values, teaching_edd$h2o_flux)
# Добавим линию у=х
abline(a=0, b=1, col="red")
#все точки лежат на прямой,
#модель хорошо оценивает данные дневных потоков h2o
#график зависимости остатков от наблюдаемых значений
plot(teaching_edd$h2o_flux,mod1$residuals)
#Для поиска коэффициентов для линии зададим модель, связывающую остатки и h2o
mo1=lm(mod1$residuals~teaching_edd$h2o_flux)
abline(a=mo1$coefficients[1],b=mo1$coefficients[2],col="red")


#модель 2
mod2=lm(h2o_flux ~ (rand_err_Tau + rand_err_H + qc_LE + rand_err_LE + co2_flux + rand_err_co2_flux
        + qc_h2o_flux + rand_err_h2o_flux + h2o_time_lag + sonic_temperature + air_density
        + air_molar_volume + es + RH + VPD + u. + TKE + un_H + un_LE + un_co2_flux + un_h2o_flux + h2o_var
        + w.co2_cov + w.h2o_cov + flowrate)^2, data = teaching_edd)
#Коэффициенты
coef(mod2)
#остатки
resid(mod2)
#доверительный интервал
confint(mod2)
#P-значения по модели
summary(mod2)
#коэф детерминации = 1
#Дисперсионный анализ
anova(mod2)
#Графиик на нормальной веротяностной бумаге
plot(mod2,2)
#данные распределены нормально
#график наблюдаемых значений от предсказанных значений
plot(mod2$fitted.values, teaching_edd$h2o_flux)
# Добавим линию у=х
abline(a=0, b=1, col="blue")
#все точки лежат на прямой,
#модель хорошо оценивает данные дневных потоков h2o
#график зависимости остатков от наблюдаемых значений
plot(teaching_edd$h2o_flux,mod2$residuals)
#Для поиска коэффициентов для линии зададим модель, связывающую остатки и h2o
mo2=lm(mod2$residuals~teaching_edd$h2o_flux)
abline(a=mo2$coefficients[1],b=mo2$coefficients[2],col="blue")


#модель 3
#убираем независимые переменные: co2_flux, rand_err_co2_flux, un_LE, un_h2o_flux, w.h2o_cov
#так как 
mod3=lm(h2o_flux ~ (rand_err_Tau + rand_err_H + qc_LE + rand_err_LE
                    + qc_h2o_flux + rand_err_h2o_flux + h2o_time_lag + sonic_temperature + air_density
                     + es + RH + VPD + u. + TKE + un_H + un_co2_flux + h2o_var
                    + w.co2_cov + flowrate)^2, data = teaching_edd)
#Коэффициенты
coef(mod3)
#остатки
resid(mod3)
#доверительный интервал
confint(mod3)
#P-значения по модели
summary(mod3)
#коэф детерминации = 0.9853
#Дисперсионный анализ
anova(mod3)
#Графиик на нормальной веротяностной бумаге
plot(mod3,2)
#данные распределены нормально
#график наблюдаемых значений от предсказанных значений
plot(mod3$fitted.values, teaching_edd$h2o_flux)
# Добавим линию у=х
abline(a=0, b=1, col="green")
#все точки лежат на прямой,
#модель хорошо оценивает данные дневных потоков h2o
#график зависимости остатков от наблюдаемых значений
plot(teaching_edd$h2o_flux,mod2$residuals)
#Для поиска коэффициентов для линии зададим модель, связывающую остатки и h2o
mo3=lm(mod3$residuals~teaching_edd$h2o_flux)
abline(a=mo3$coefficients[1],b=mo3$coefficients[2],col="green")


#модель 4
#убираем незначимые переменные по anova
mod4=lm(h2o_flux ~ (rand_err_Tau + rand_err_H + qc_LE + rand_err_LE
                    + qc_h2o_flux + rand_err_h2o_flux + h2o_time_lag + sonic_temperature + air_density
                    + es + RH + VPD + u. + TKE + un_H + un_co2_flux + h2o_var
                    + w.co2_cov + flowrate)^2 - air_density - rand_err_Tau:rand_err_H
        - rand_err_Tau:rand_err_h2o_flux - rand_err_H:un_co2_flux - rand_err_H:w.co2_cov
        - rand_err_H:flowrate - qc_LE:qc_h2o_flux - qc_LE:air_density - qc_LE:RH - qc_LE:VPD - qc_LE:un_H
        - qc_LE:flowrate - rand_err_LE:air_density - rand_err_LE:TKE - rand_err_LE:h2o_var
        - rand_err_LE:w.co2_cov - rand_err_h2o_flux:air_density - rand_err_h2o_flux:es - rand_err_h2o_flux:u.
        - rand_err_h2o_flux:TKE - h2o_time_lag:es - h2o_time_lag:VPD - h2o_time_lag:un_H - h2o_time_lag:h2o_var
        - h2o_time_lag:flowrate - air_density:es - air_density:RH - air_density:un_H - air_density:un_co2_flux
        - air_density:h2o_var - air_density:flowrate - es:u. - es:TKE - RH:VPD - RH:TKE - RH:h2o_var - RH:w.co2_cov
        - RH:flowrate - u.:TKE - un_H:w.co2_cov - un_co2_flux:h2o_var - h2o_var:flowrate, data = teaching_edd)
#Коэффициенты
coef(mod4)
#остатки
resid(mod4)
#доверительный интервал
confint(mod4)
#P-значения по модели
summary(mod4)
#коэф детерминации = 0.9841
#Дисперсионный анализ
anova(mod4)
#Графиик на нормальной веротяностной бумаге
plot(mod4,2)
#данные распределены нормально
#график наблюдаемых значений от предсказанных значений
plot(mod4$fitted.values, teaching_edd$h2o_flux)
# Добавим линию у=х
abline(a=0, b=1, col="yellow")
#все точки лежат на прямой,
#модель хорошо оценивает данные дневных потоков h2o
#график зависимости остатков от наблюдаемых значений
plot(teaching_edd$h2o_flux,mod4$residuals)
#Для поиска коэффициентов для линии зададим модель, связывающую остатки и h2o
mo4=lm(mod4$residuals~teaching_edd$h2o_flux)
abline(a=mo4$coefficients[1],b=mo4$coefficients[2],col="yellow")


#модель 5
#убираем незначимые переменные по anova
mod5=lm(h2o_flux ~ (rand_err_Tau + rand_err_H + qc_LE + rand_err_LE
                    + qc_h2o_flux + rand_err_h2o_flux + h2o_time_lag + sonic_temperature + air_density
                    + es + RH + VPD + u. + TKE + un_H + un_co2_flux + h2o_var
                    + w.co2_cov + flowrate)^2 - air_density - rand_err_Tau:rand_err_H
        - rand_err_Tau:rand_err_h2o_flux - rand_err_H:un_co2_flux - rand_err_H:w.co2_cov
        - rand_err_H:flowrate - qc_LE:qc_h2o_flux - qc_LE:air_density - qc_LE:RH - qc_LE:VPD - qc_LE:un_H
        - qc_LE:flowrate - rand_err_LE:air_density - rand_err_LE:TKE - rand_err_LE:h2o_var
        - rand_err_LE:w.co2_cov - rand_err_h2o_flux:air_density - rand_err_h2o_flux:es - rand_err_h2o_flux:u.
        - rand_err_h2o_flux:TKE - h2o_time_lag:es - h2o_time_lag:VPD - h2o_time_lag:un_H - h2o_time_lag:h2o_var
        - h2o_time_lag:flowrate - air_density:es - air_density:RH - air_density:un_H - air_density:un_co2_flux
        - air_density:h2o_var - air_density:flowrate - es:u. - es:TKE - RH:VPD - RH:TKE - RH:h2o_var - RH:w.co2_cov
        - RH:flowrate - u.:TKE - un_H:w.co2_cov - un_co2_flux:h2o_var - h2o_var:flowrate - sonic_temperature - h2o_var
        - rand_err_Tau:sonic_temperature - rand_err_Tau:air_density - rand_err_Tau:VPD - rand_err_Tau:h2o_var
        - rand_err_H:rand_err_LE - rand_err_H:sonic_temperature - rand_err_H:RH - qc_LE:h2o_time_lag  - qc_h2o_flux:air_density
        - qc_h2o_flux:RH - qc_h2o_flux:VPD - rand_err_h2o_flux:h2o_time_lag - rand_err_h2o_flux:RH - rand_err_h2o_flux:flowrate
        - h2o_time_lag:RH - sonic_temperature:air_density - sonic_temperature:RH - sonic_temperature:u. - sonic_temperature:TKE
        - sonic_temperature:un_H - sonic_temperature:un_co2_flux - sonic_temperature:flowrate - es:un_H - es:h2o_var  - es:flowrate
        - RH:un_co2_flux - VPD:u. - VPD:TKE - VPD:un_H - VPD:w.co2_cov - VPD:flowrate - un_H:un_co2_flux
        - h2o_var:w.co2_cov, data = teaching_edd)
#Коэффициенты
coef(mod5)
#остатки
resid(mod5)
#доверительный интервал
confint(mod5)
#P-значения по модели
summary(mod5)
#коэф детерминации = 0.9822
#Дисперсионный анализ
anova(mod5)
#Графиик на нормальной веротяностной бумаге
plot(mod5,2)
#данные распределены нормально
#график наблюдаемых значений от предсказанных значений
plot(mod5$fitted.values, teaching_edd$h2o_flux)
# Добавим линию у=х
abline(a=0, b=1, col="purple")
#все точки лежат на прямой,
#модель хорошо оценивает данные дневных потоков h2o
#график зависимости остатков от наблюдаемых значений
plot(teaching_edd$h2o_flux,mod5$residuals)
#Для поиска коэффициентов для линии зададим модель, связывающую остатки и h2o
mo5=lm(mod5$residuals~teaching_edd$h2o_flux)
abline(a=mo5$coefficients[1],b=mo5$coefficients[2],col="purple")


#модель 6
#убираем незначимые переменные по anova
mod6=lm(h2o_flux ~ (rand_err_Tau + rand_err_H + qc_LE + rand_err_LE
                    + qc_h2o_flux + rand_err_h2o_flux + h2o_time_lag + sonic_temperature + air_density
                    + es + RH + VPD + u. + TKE + un_H + un_co2_flux + h2o_var
                    + w.co2_cov + flowrate)^2 - air_density - rand_err_Tau:rand_err_H
        - rand_err_Tau:rand_err_h2o_flux - rand_err_H:un_co2_flux - rand_err_H:w.co2_cov
        - rand_err_H:flowrate - qc_LE:qc_h2o_flux - qc_LE:air_density - qc_LE:RH - qc_LE:VPD - qc_LE:un_H
        - qc_LE:flowrate - rand_err_LE:air_density - rand_err_LE:TKE - rand_err_LE:h2o_var
        - rand_err_LE:w.co2_cov - rand_err_h2o_flux:air_density - rand_err_h2o_flux:es - rand_err_h2o_flux:u.
        - rand_err_h2o_flux:TKE - h2o_time_lag:es - h2o_time_lag:VPD - h2o_time_lag:un_H - h2o_time_lag:h2o_var
        - h2o_time_lag:flowrate - air_density:es - air_density:RH - air_density:un_H - air_density:un_co2_flux
        - air_density:h2o_var - air_density:flowrate - es:u. - es:TKE - RH:VPD - RH:TKE - RH:h2o_var - RH:w.co2_cov
        - RH:flowrate - u.:TKE - un_H:w.co2_cov - un_co2_flux:h2o_var - h2o_var:flowrate - sonic_temperature - h2o_var
        - rand_err_Tau:sonic_temperature - rand_err_Tau:air_density - rand_err_Tau:VPD - rand_err_Tau:h2o_var
        - rand_err_H:rand_err_LE - rand_err_H:sonic_temperature - rand_err_H:RH - qc_LE:h2o_time_lag  - qc_h2o_flux:air_density
        - qc_h2o_flux:RH - qc_h2o_flux:VPD - rand_err_h2o_flux:h2o_time_lag - rand_err_h2o_flux:RH - rand_err_h2o_flux:flowrate
        - h2o_time_lag:RH - sonic_temperature:air_density - sonic_temperature:RH - sonic_temperature:u. - sonic_temperature:TKE
        - sonic_temperature:un_H - sonic_temperature:un_co2_flux - sonic_temperature:flowrate - es:un_H - es:h2o_var  - es:flowrate
        - RH:un_co2_flux - VPD:u. - VPD:TKE - VPD:un_H - VPD:w.co2_cov - VPD:flowrate - un_H:un_co2_flux
        - h2o_var:w.co2_cov - es - rand_err_Tau:es  - rand_err_H:rand_err_h2o_flux - rand_err_H:h2o_var - qc_LE:es - rand_err_LE:VPD
        - rand_err_LE:flowrate - qc_h2o_flux:h2o_time_lag - qc_h2o_flux:un_H - qc_h2o_flux:flowrate - rand_err_h2o_flux:sonic_temperature
        - rand_err_h2o_flux:VPD - h2o_time_lag:sonic_temperature - sonic_temperature:w.co2_cov - air_density:u. - air_density:TKE
        - es:VPD - RH:un_H - VPD:un_co2_flux - VPD:h2o_var - u.:flowrate - un_H:flowrate, data = teaching_edd)
#Коэффициенты
coef(mod6)
#остатки
resid(mod6)
#доверительный интервал
confint(mod6)
#P-значения по модели
summary(mod6)
#коэф детерминации = 0.9815
#Дисперсионный анализ
anova(mod6)
#Графиик на нормальной веротяностной бумаге
plot(mod6,2)
#данные распределены нормально
#график наблюдаемых значений от предсказанных значений
plot(mod6$fitted.values, teaching_edd$h2o_flux)
# Добавим линию у=х
abline(a=0, b=1, col="pink")
#все точки лежат на прямой,
#модель хорошо оценивает данные дневных потоков h2o
#график зависимости остатков от наблюдаемых значений
plot(teaching_edd$h2o_flux,mod6$residuals)
#Для поиска коэффициентов для линии зададим модель, связывающую остатки и h2o
mo6=lm(mod6$residuals~teaching_edd$h2o_flux)
abline(a=mo6$coefficients[1],b=mo6$coefficients[2],col="pink")


#модель 7
#убираем незначимые переменные по anova
mod7=lm(h2o_flux ~ (rand_err_Tau + rand_err_H + qc_LE + rand_err_LE
                    + qc_h2o_flux + rand_err_h2o_flux + h2o_time_lag + sonic_temperature + air_density
                    + es + RH + VPD + u. + TKE + un_H + un_co2_flux + h2o_var
                    + w.co2_cov + flowrate)^2 - air_density - rand_err_Tau:rand_err_H
        - rand_err_Tau:rand_err_h2o_flux - rand_err_H:un_co2_flux - rand_err_H:w.co2_cov
        - rand_err_H:flowrate - qc_LE:qc_h2o_flux - qc_LE:air_density - qc_LE:RH - qc_LE:VPD - qc_LE:un_H
        - qc_LE:flowrate - rand_err_LE:air_density - rand_err_LE:TKE - rand_err_LE:h2o_var
        - rand_err_LE:w.co2_cov - rand_err_h2o_flux:air_density - rand_err_h2o_flux:es - rand_err_h2o_flux:u.
        - rand_err_h2o_flux:TKE - h2o_time_lag:es - h2o_time_lag:VPD - h2o_time_lag:un_H - h2o_time_lag:h2o_var
        - h2o_time_lag:flowrate - air_density:es - air_density:RH - air_density:un_H - air_density:un_co2_flux
        - air_density:h2o_var - air_density:flowrate - es:u. - es:TKE - RH:VPD - RH:TKE - RH:h2o_var - RH:w.co2_cov
        - RH:flowrate - u.:TKE - un_H:w.co2_cov - un_co2_flux:h2o_var - h2o_var:flowrate - sonic_temperature - h2o_var
        - rand_err_Tau:sonic_temperature - rand_err_Tau:air_density - rand_err_Tau:VPD - rand_err_Tau:h2o_var
        - rand_err_H:rand_err_LE - rand_err_H:sonic_temperature - rand_err_H:RH - qc_LE:h2o_time_lag  - qc_h2o_flux:air_density
        - qc_h2o_flux:RH - qc_h2o_flux:VPD - rand_err_h2o_flux:h2o_time_lag - rand_err_h2o_flux:RH - rand_err_h2o_flux:flowrate
        - h2o_time_lag:RH - sonic_temperature:air_density - sonic_temperature:RH - sonic_temperature:u. - sonic_temperature:TKE
        - sonic_temperature:un_H - sonic_temperature:un_co2_flux - sonic_temperature:flowrate - es:un_H - es:h2o_var  - es:flowrate
        - RH:un_co2_flux - VPD:u. - VPD:TKE - VPD:un_H - VPD:w.co2_cov - VPD:flowrate - un_H:un_co2_flux
        - h2o_var:w.co2_cov - es - rand_err_Tau:es  - rand_err_H:rand_err_h2o_flux - rand_err_H:h2o_var - qc_LE:es - rand_err_LE:VPD
        - rand_err_LE:flowrate - qc_h2o_flux:h2o_time_lag - qc_h2o_flux:un_H - qc_h2o_flux:flowrate - rand_err_h2o_flux:sonic_temperature
        - rand_err_h2o_flux:VPD - h2o_time_lag:sonic_temperature - sonic_temperature:w.co2_cov - air_density:u. - air_density:TKE
        - es:VPD - RH:un_H - VPD:un_co2_flux - VPD:h2o_var - u.:flowrate - un_H:flowrate - rand_err_Tau:un_H - rand_err_Tau:flowrate
        - rand_err_H:es - qc_LE:sonic_temperature - qc_LE:h2o_var - h2o_time_lag:w.co2_cov - es:RH - u.:h2o_var - un_co2_flux:flowrate, data = teaching_edd)
#Коэффициенты
coef(mod7)
#остатки
resid(mod7)
#доверительный интервал
confint(mod7)
#P-значения по модели
summary(mod7)
#коэф детерминации = 0.981
#Дисперсионный анализ
anova(mod7)
#Графиик на нормальной веротяностной бумаге
plot(mod7,2)
#данные распределены нормально
#график наблюдаемых значений от предсказанных значений
plot(mod7$fitted.values, teaching_edd$h2o_flux)
# Добавим линию у=х
abline(a=0, b=1, col="gray")
#все точки лежат на прямой,
#модель хорошо оценивает данные дневных потоков h2o
#график зависимости остатков от наблюдаемых значений
plot(teaching_edd$h2o_flux,mod7$residuals)
#Для поиска коэффициентов для линии зададим модель, связывающую остатки и h2o
mo7=lm(mod7$residuals~teaching_edd$h2o_flux)
abline(a=mo7$coefficients[1],b=mo7$coefficients[2],col="gray")


#модель 8
#убираем незначимые переменные по anova
mod8=lm(h2o_flux ~ (rand_err_Tau + rand_err_H + qc_LE + rand_err_LE
                    + qc_h2o_flux + rand_err_h2o_flux + h2o_time_lag + sonic_temperature + air_density
                    + es + RH + VPD + u. + TKE + un_H + un_co2_flux + h2o_var
                    + w.co2_cov + flowrate)^2 - air_density - rand_err_Tau:rand_err_H
        - rand_err_Tau:rand_err_h2o_flux - rand_err_H:un_co2_flux - rand_err_H:w.co2_cov
        - rand_err_H:flowrate - qc_LE:qc_h2o_flux - qc_LE:air_density - qc_LE:RH - qc_LE:VPD - qc_LE:un_H
        - qc_LE:flowrate - rand_err_LE:air_density - rand_err_LE:TKE - rand_err_LE:h2o_var
        - rand_err_LE:w.co2_cov - rand_err_h2o_flux:air_density - rand_err_h2o_flux:es - rand_err_h2o_flux:u.
        - rand_err_h2o_flux:TKE - h2o_time_lag:es - h2o_time_lag:VPD - h2o_time_lag:un_H - h2o_time_lag:h2o_var
        - h2o_time_lag:flowrate - air_density:es - air_density:RH - air_density:un_H - air_density:un_co2_flux
        - air_density:h2o_var - air_density:flowrate - es:u. - es:TKE - RH:VPD - RH:TKE - RH:h2o_var - RH:w.co2_cov
        - RH:flowrate - u.:TKE - un_H:w.co2_cov - un_co2_flux:h2o_var - h2o_var:flowrate - sonic_temperature - h2o_var
        - rand_err_Tau:sonic_temperature - rand_err_Tau:air_density - rand_err_Tau:VPD - rand_err_Tau:h2o_var
        - rand_err_H:rand_err_LE - rand_err_H:sonic_temperature - rand_err_H:RH - qc_LE:h2o_time_lag  - qc_h2o_flux:air_density
        - qc_h2o_flux:RH - qc_h2o_flux:VPD - rand_err_h2o_flux:h2o_time_lag - rand_err_h2o_flux:RH - rand_err_h2o_flux:flowrate
        - h2o_time_lag:RH - sonic_temperature:air_density - sonic_temperature:RH - sonic_temperature:u. - sonic_temperature:TKE
        - sonic_temperature:un_H - sonic_temperature:un_co2_flux - sonic_temperature:flowrate - es:un_H - es:h2o_var  - es:flowrate
        - RH:un_co2_flux - VPD:u. - VPD:TKE - VPD:un_H - VPD:w.co2_cov - VPD:flowrate - un_H:un_co2_flux
        - h2o_var:w.co2_cov - es - rand_err_Tau:es  - rand_err_H:rand_err_h2o_flux - rand_err_H:h2o_var - qc_LE:es - rand_err_LE:VPD
        - rand_err_LE:flowrate - qc_h2o_flux:h2o_time_lag - qc_h2o_flux:un_H - qc_h2o_flux:flowrate - rand_err_h2o_flux:sonic_temperature
        - rand_err_h2o_flux:VPD - h2o_time_lag:sonic_temperature - sonic_temperature:w.co2_cov - air_density:u. - air_density:TKE
        - es:VPD - RH:un_H - VPD:un_co2_flux - VPD:h2o_var - u.:flowrate - un_H:flowrate - rand_err_Tau:un_H - rand_err_Tau:flowrate
        - rand_err_H:es - qc_LE:sonic_temperature - qc_LE:h2o_var - h2o_time_lag:w.co2_cov - es:RH - u.:h2o_var - un_co2_flux:flowrate
        - qc_h2o_flux:sonic_temperature - es:un_co2_flux - TKE:flowrate - w.co2_cov:flowrate, data = teaching_edd)
#Коэффициенты
coef(mod8)
#остатки
resid(mod8)
#доверительный интервал
confint(mod8)
#P-значения по модели
summary(mod8)
#коэф детерминации = 0.9808
#Дисперсионный анализ
anova(mod8)
#Графиик на нормальной веротяностной бумаге
plot(mod8,2)
#данные распределены нормально
#график наблюдаемых значений от предсказанных значений
plot(mod8$fitted.values, teaching_edd$h2o_flux)
# Добавим линию у=х
abline(a=0, b=1, col="black")
#все точки лежат на прямой,
#модель хорошо оценивает данные дневных потоков h2o
#график зависимости остатков от наблюдаемых значений
plot(teaching_edd$h2o_flux,mod8$residuals)
#Для поиска коэффициентов для линии зададим модель, связывающую остатки и h2o
mo8=lm(mod8$residuals~teaching_edd$h2o_flux)
abline(a=mo8$coefficients[1],b=mo8$coefficients[2],col="black")


#модель 9
#убираем незначимые переменные по anova
mod9=lm(h2o_flux ~ (rand_err_Tau + rand_err_H + qc_LE + rand_err_LE
                    + qc_h2o_flux + rand_err_h2o_flux + h2o_time_lag + sonic_temperature + air_density
                    + es + RH + VPD + u. + TKE + un_H + un_co2_flux + h2o_var
                    + w.co2_cov + flowrate)^2 - air_density - rand_err_Tau:rand_err_H
        - rand_err_Tau:rand_err_h2o_flux - rand_err_H:un_co2_flux - rand_err_H:w.co2_cov
        - rand_err_H:flowrate - qc_LE:qc_h2o_flux - qc_LE:air_density - qc_LE:RH - qc_LE:VPD - qc_LE:un_H
        - qc_LE:flowrate - rand_err_LE:air_density - rand_err_LE:TKE - rand_err_LE:h2o_var
        - rand_err_LE:w.co2_cov - rand_err_h2o_flux:air_density - rand_err_h2o_flux:es - rand_err_h2o_flux:u.
        - rand_err_h2o_flux:TKE - h2o_time_lag:es - h2o_time_lag:VPD - h2o_time_lag:un_H - h2o_time_lag:h2o_var
        - h2o_time_lag:flowrate - air_density:es - air_density:RH - air_density:un_H - air_density:un_co2_flux
        - air_density:h2o_var - air_density:flowrate - es:u. - es:TKE - RH:VPD - RH:TKE - RH:h2o_var - RH:w.co2_cov
        - RH:flowrate - u.:TKE - un_H:w.co2_cov - un_co2_flux:h2o_var - h2o_var:flowrate - sonic_temperature - h2o_var
        - rand_err_Tau:sonic_temperature - rand_err_Tau:air_density - rand_err_Tau:VPD - rand_err_Tau:h2o_var
        - rand_err_H:rand_err_LE - rand_err_H:sonic_temperature - rand_err_H:RH - qc_LE:h2o_time_lag  - qc_h2o_flux:air_density
        - qc_h2o_flux:RH - qc_h2o_flux:VPD - rand_err_h2o_flux:h2o_time_lag - rand_err_h2o_flux:RH - rand_err_h2o_flux:flowrate
        - h2o_time_lag:RH - sonic_temperature:air_density - sonic_temperature:RH - sonic_temperature:u. - sonic_temperature:TKE
        - sonic_temperature:un_H - sonic_temperature:un_co2_flux - sonic_temperature:flowrate - es:un_H - es:h2o_var  - es:flowrate
        - RH:un_co2_flux - VPD:u. - VPD:TKE - VPD:un_H - VPD:w.co2_cov - VPD:flowrate - un_H:un_co2_flux
        - h2o_var:w.co2_cov - es - rand_err_Tau:es  - rand_err_H:rand_err_h2o_flux - rand_err_H:h2o_var - qc_LE:es - rand_err_LE:VPD
        - rand_err_LE:flowrate - qc_h2o_flux:h2o_time_lag - qc_h2o_flux:un_H - qc_h2o_flux:flowrate - rand_err_h2o_flux:sonic_temperature
        - rand_err_h2o_flux:VPD - h2o_time_lag:sonic_temperature - sonic_temperature:w.co2_cov - air_density:u. - air_density:TKE
        - es:VPD - RH:un_H - VPD:un_co2_flux - VPD:h2o_var - u.:flowrate - un_H:flowrate - rand_err_Tau:un_H - rand_err_Tau:flowrate
        - rand_err_H:es - qc_LE:sonic_temperature - qc_LE:h2o_var - h2o_time_lag:w.co2_cov - es:RH - u.:h2o_var - un_co2_flux:flowrate
        - qc_h2o_flux:sonic_temperature - es:un_co2_flux - TKE:flowrate - w.co2_cov:flowrate - qc_h2o_flux:es - h2o_time_lag:air_density
        - es:w.co2_cov, data = teaching_edd)
#Коэффициенты
coef(mod9)
#остатки
resid(mod9)
#доверительный интервал
confint(mod9)
#P-значения по модели
summary(mod9)
#коэф детерминации = 0.9807
#Дисперсионный анализ
anova(mod9)
#Графиик на нормальной веротяностной бумаге
plot(mod9,2)
#данные распределены нормально
#график наблюдаемых значений от предсказанных значений
plot(mod9$fitted.values, teaching_edd$h2o_flux)
# Добавим линию у=х
abline(a=0, b=1, col="brown")
#все точки лежат на прямой,
#модель хорошо оценивает данные дневных потоков h2o
#график зависимости остатков от наблюдаемых значений
plot(teaching_edd$h2o_flux,mod9$residuals)
#Для поиска коэффициентов для линии зададим модель, связывающую остатки и h2o
mo10=lm(mod9$residuals~teaching_edd$h2o_flux)
abline(a=mo9$coefficients[1],b=mo9$coefficients[2],col="brown")


#модель 10
#убираем незначимые(**) переменные по anova 
mod10=lm(h2o_flux ~ (rand_err_Tau + rand_err_H + qc_LE + rand_err_LE
                    + qc_h2o_flux + rand_err_h2o_flux + h2o_time_lag + sonic_temperature + air_density
                    + es + RH + VPD + u. + TKE + un_H + un_co2_flux + h2o_var
                    + w.co2_cov + flowrate)^2 - air_density - rand_err_Tau:rand_err_H
        - rand_err_Tau:rand_err_h2o_flux - rand_err_H:un_co2_flux - rand_err_H:w.co2_cov
        - rand_err_H:flowrate - qc_LE:qc_h2o_flux - qc_LE:air_density - qc_LE:RH - qc_LE:VPD - qc_LE:un_H
        - qc_LE:flowrate - rand_err_LE:air_density - rand_err_LE:TKE - rand_err_LE:h2o_var
        - rand_err_LE:w.co2_cov - rand_err_h2o_flux:air_density - rand_err_h2o_flux:es - rand_err_h2o_flux:u.
        - rand_err_h2o_flux:TKE - h2o_time_lag:es - h2o_time_lag:VPD - h2o_time_lag:un_H - h2o_time_lag:h2o_var
        - h2o_time_lag:flowrate - air_density:es - air_density:RH - air_density:un_H - air_density:un_co2_flux
        - air_density:h2o_var - air_density:flowrate - es:u. - es:TKE - RH:VPD - RH:TKE - RH:h2o_var - RH:w.co2_cov
        - RH:flowrate - u.:TKE - un_H:w.co2_cov - un_co2_flux:h2o_var - h2o_var:flowrate - sonic_temperature - h2o_var
        - rand_err_Tau:sonic_temperature - rand_err_Tau:air_density - rand_err_Tau:VPD - rand_err_Tau:h2o_var
        - rand_err_H:rand_err_LE - rand_err_H:sonic_temperature - rand_err_H:RH - qc_LE:h2o_time_lag  - qc_h2o_flux:air_density
        - qc_h2o_flux:RH - qc_h2o_flux:VPD - rand_err_h2o_flux:h2o_time_lag - rand_err_h2o_flux:RH - rand_err_h2o_flux:flowrate
        - h2o_time_lag:RH - sonic_temperature:air_density - sonic_temperature:RH - sonic_temperature:u. - sonic_temperature:TKE
        - sonic_temperature:un_H - sonic_temperature:un_co2_flux - sonic_temperature:flowrate - es:un_H - es:h2o_var  - es:flowrate
        - RH:un_co2_flux - VPD:u. - VPD:TKE - VPD:un_H - VPD:w.co2_cov - VPD:flowrate - un_H:un_co2_flux
        - h2o_var:w.co2_cov - es - rand_err_Tau:es  - rand_err_H:rand_err_h2o_flux - rand_err_H:h2o_var - qc_LE:es - rand_err_LE:VPD
        - rand_err_LE:flowrate - qc_h2o_flux:h2o_time_lag - qc_h2o_flux:un_H - qc_h2o_flux:flowrate - rand_err_h2o_flux:sonic_temperature
        - rand_err_h2o_flux:VPD - h2o_time_lag:sonic_temperature - sonic_temperature:w.co2_cov - air_density:u. - air_density:TKE
        - es:VPD - RH:un_H - VPD:un_co2_flux - VPD:h2o_var - u.:flowrate - un_H:flowrate - rand_err_Tau:un_H - rand_err_Tau:flowrate
        - rand_err_H:es - qc_LE:sonic_temperature - qc_LE:h2o_var - h2o_time_lag:w.co2_cov - es:RH - u.:h2o_var - un_co2_flux:flowrate
        - qc_h2o_flux:sonic_temperature - es:un_co2_flux - TKE:flowrate - w.co2_cov:flowrate - qc_h2o_flux:es - h2o_time_lag:air_density
        - es:w.co2_cov - rand_err_H:qc_LE - qc_h2o_flux:h2o_var - RH:u. - u.:w.co2_cov - TKE:h2o_var, data = teaching_edd)
#Коэффициенты
coef(mod10)
#остатки
resid(mod10)
#доверительный интервал
confint(mod10)
#P-значения по модели
summary(mod10)
#коэф детерминации = 0.9803
#Дисперсионный анализ
anova(mod10)
#Графиик на нормальной веротяностной бумаге
plot(mod10,2)
#данные распределены нормально
#график наблюдаемых значений от предсказанных значений
plot(mod10$fitted.values, teaching_edd$h2o_flux)
# Добавим линию у=х
abline(a=0, b=1, col="red")
#все точки лежат на прямой,
#модель хорошо оценивает данные дневных потоков h2o
#график зависимости остатков от наблюдаемых значений
plot(teaching_edd$h2o_flux,mod10$residuals)
#Для поиска коэффициентов для линии зададим модель, связывающую остатки и h2o
mo10=lm(mod10$residuals~teaching_edd$h2o_flux)
abline(a=mo10$coefficients[1],b=mo10$coefficients[2],col="red")

#при выбросе незначимых переменных коэффициент детерминации меняется незначительно
#в модели10 остались только значимые переменные

#лучше всего данные описывает модель2