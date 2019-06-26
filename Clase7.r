
library(tidyverse) #Es una opcion para leer, organizar, transformar y graficar

# Serie Brent: Los datos tienen fecha de operacion y precios por barril
df_brent<-read_csv(file = 'https://raw.githubusercontent.com/datasets/oil-prices/master/data/brent-daily.csv', col_names = TRUE,cols(
  Date = col_date(format = ""),
  Price = col_double()
))

str(df_brent)

summary(df_brent)

options(repr.plot.width=10, repr.plot.height=6)
with(df_brent,plot(x = Date,y = Price,type = 'l'))
grid()

#Serie WTI

df_wti<-read_csv(file = 'https://raw.githubusercontent.com/datasets/oil-prices/master/data/wti-daily.csv', col_names = TRUE,cols(
  Date = col_date(format = ""),
  Price = col_double()
))

str(df_wti)

summary(df_wti)

options(repr.plot.width=10, repr.plot.height=6)
with(df_brent,plot(x = Date,y = Price,type = 'l'))
grid()

#Dado que las series tienen diferentes fechas se debe homologar los datos con una funcion
homologar_fechas<-function(df, fecha_inicial, fecha_final){
    df<-df%>%filter(Date>=fecha_inicial)%>%filter(Date<=fecha_final)
    return(df)
}

df_brent<-homologar_fechas(df = df_brent,fecha_inicial = '2007-01-01',fecha_final = '2018-12-31')
df_wti<-homologar_fechas(df = df_wti,fecha_inicial = '2007-01-01',fecha_final = '2018-12-31')

plot(df_brent,type = 'l')
lines(df_wti,col='red')
grid()

p1<-with(df_brent,hist(Price,breaks = 30, col = rgb(0,0,1,1/4),probability = TRUE))
p2<-with(df_wti,hist(Price,breaks = 30, add = T,col = rgb(1,0,0,1/4),probability = TRUE))

lines(density(df_brent$Price),col = 'blue',lwd=3)
lines(density(df_wti$Price),col = 'red',lwd=3)

#Crear un dataframe con los datos

length(df_brent$Price)
length(df_wti$Price)

# Left join de los datos
df<-merge(df_brent,df_wti,all.x = TRUE,by = 'Date')

# Imputacion de valores faltantes con el promedio de la serie
df$Price.y[!complete.cases(df$Price.y)]<-mean(df$Price.y,na.rm = TRUE)

colnames(df)<-c('Date','precios_brent','precios_wti')

head(df)

df_mensuales<-df%>%
    mutate(ano_mes =paste0( str_sub(Date,start = 1,end = 7),'-01'))%>% #Crea una fecha estandar para indicar el ano_mes
    arrange(ano_mes)%>%                                                #ordena el dataframe por ano_mes
    group_by(ano_mes)%>%                                               #Agrupa los datos por ano_mes
    summarize(prom_brent = mean(precios_brent),                        #Resume los datos aplicando promedio
              prom_wti = mean(precios_wti))                            #



str(df_mensuales)   # Veamos la estructura final de dataframe ... se observan problemas con el campo ano_mes
df_mensuales$ano_mes <- as.Date(df_mensuales$ano_mes,format = '%Y-%m-%d')
str(df_mensuales)   # Veamos como queda corregido ... ok!!

with(df_mensuales,plot(x = ano_mes,y = prom_brent, type='l'))
lines(x = df_mensuales$ano_mes,df_mensuales$prom_wti,col = 'red',type = 'l')
grid()

with(df_mensuales,plot(x = prom_brent,y = prom_wti, type='p'))


summary(df_mensuales[,2:3])
