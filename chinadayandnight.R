load("C:/Users/Lu000012/Documents/files/GLOBAL_LUR/china_hourly.r")
load("C:/Users/Lu000012/Documents/files/GLOBAL_LUR/China2017hourly.r")

dfstno2 = data.frame(stNO2)

time1 = strptime( names(df1), "H%Y.%m.%d.%H.%M.%S")
df1=df1[,-2003]
time = time1[-2003]
dftime = cbind(time, data.frame(t(df1)))

library(lubridate)
library(dplyr)
df1 = dftime%>%mutate(hours  = as.numeric(hour(time)))
chinaday = df1%>% filter(hours>=7 &hours <= 21)
chinanight  = df1%>% filter(hours<7 | hours>21)

CN_mean_day = colMeans(chinaday[,-c(1, 1581)], na.rm = T, dims = 1)
CN_mean_night = colMeans(chinanight[,-c(1,1581)], na.rm = T, dims = 1)

Chinaday  = cbind(data.frame(stNO2@sp),value = CN_mean_day,   country = "CN")
Chinanight = cbind(data.frame(stNO2@sp),value = CN_mean_night,  country = "CN")

qnight = read.csv("C:/Users/Lu000012/Documents/files/GLOBAL_LUR/q_night.csv")
qday = read.csv("C:/Users/Lu000012/Documents/files/GLOBAL_LUR/q_day.csv")

wdfnight =  with(qnight, data.frame(latitude, longitude,  value,country))
wdfday  =  with(qday, data.frame(latitude, longitude,  value,country))

world_day  = rbind(wdfday, Chinaday)
world_night  = rbind(wdfnight, Chinanight)
#write.csv(world_day, file= "world_day.csv")
#write.csv(world_night, file= "world_night.csv")
world_day = read.csv("C:/Users/Lu000012/Documents/files/GLOBAL_LUR/world_day.csv")
world_night = read.csv("C:/Users/Lu000012/Documents/files/GLOBAL_LUR/world_night.csv")
testoaq = read.csv("C:/Users/Lu000012/Documents/files/GLOBAL_LUR/openaqmorethan1000_ch_locations.csv") # ID with locaitons
load("C:/Users/Lu000012/Documents/files/GLOBAL_LUR/merged_day_night.Rdata")
merged  = join_by_id(IDfile= testoaq, coordinatefile=world_day, file2merge = merged, varname="value", newname = "day_value")
merged  = join_by_id(IDfile= testoaq, coordinatefile=world_night, file2merge = merged , varname="value", newname = "night_value")


merged  = within(merged, {night_value[country%in% countrywithppm]= night_value[country%in% countrywithppm]*1000*1.91})
merged  = within(merged, {day_value[country%in% countrywithppm]= day_value[country%in% countrywithppm]*1000*1.91})

summary(merged)
save(merged, file = "merged.Rdata")
getwd()
plot(with(merged,day_value-night_value))
nrow(Chinanight)
