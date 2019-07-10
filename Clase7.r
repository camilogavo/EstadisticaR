
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
with(df_wti,plot(x = Date,y = Price,type = 'l'))
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

oro<-read_csv(file = 'https://raw.githubusercontent.com/datasets/gold-prices/master/data/monthly.csv', 
              col_names = TRUE)

colnames(oro)<-gsub(colnames(oro),pattern = ' ', replacement = '_') #Se reemplazan los espacios en blanco para evitar problemas con la lectura de datos

#Estructura del dataset
str(oro)

oro$Date<-paste0(oro$Date,'-01')

oro$Date<-as.Date(oro$Date)

oro%>%str() #equivalente a str(oro)

oro%>%head(3) #equivalente a head(oro, n = 3)
oro%>%tail(3) #equivalente a tail(oro, n = 3)

oro[!complete.cases(oro),] #filtrar las observaciones con datos NA
oro[!complete.cases(oro),]%>%nrow #Contar casos con datos NA
sapply(oro, function(x) sum(is.na(x)))

summary(oro) #Resumen de los datos

sp500<-read_csv(file = 'https://raw.githubusercontent.com/datasets/s-and-p-500/master/data/data.csv',
               col_names= TRUE)

colnames(sp500)<-gsub(colnames(sp500),pattern = ' ', replacement = '_') #Se reemplazan los espacios en blanco para evitar problemas con la lectura de datos

sp500%>%str() #equivalente a str(oro)

sp500%>%head(3) #equivalente a head(sp500, n = 3)
sp500%>%tail(3) #equivalente a tail(sp500, n = 3)

sp500[!complete.cases(sp500),] #filtrar las observaciones con datos NA
sp500[!complete.cases(sp500),]%>%nrow #Contar casos con datos NA

sapply(sp500, function(x) sum(is.na(x)))

# Completar los NA con el promedio
sp500$Dividend[is.na(sp500$Dividend)]<-mean(sp500$Dividend,na.rm = TRUE)
sp500$Earnings[is.na(sp500$Earnings)]<-mean(sp500$Earnings,na.rm = TRUE)
sp500$Real_Dividend[is.na(sp500$Real_Dividend)]<-mean(sp500$Real_Dividend,na.rm = TRUE)
sp500$Real_Earnings[is.na(sp500$Real_Earnings)]<-mean(sp500$Real_Earnings,na.rm = TRUE)
sp500$PE10[is.na(sp500$PE10)]<-mean(sp500$PE10,na.rm = TRUE)


sp500[!complete.cases(sp500),]%>%nrow #Contar casos con datos NA

homologar_fechas<-function(df,fecha_inicial, fecha_final){
    df<-df%>%
            filter(Date>=as.Date(fecha_inicial))%>%
            filter(Date<=as.Date(fecha_final))
    return(df)
}

oro<-homologar_fechas(oro,'1950-01-01','2017-12-01')
sp500<-homologar_fechas(sp500,'1950-01-01','2017-12-01')

plot(density(oro$Price)) # Distribucion empirica
hist(oro$Price,main = '') #Histograma usando la funcion de r-base

df<-left_join(oro, sp500, by= 'Date') # union de las tablas por el campo Date
                                      # dado que el oro es la fecha más reciente se toma como tabla principal para evitar NA

#seleccion de variables
df_analisis<-df%>%select(Price, SP500, Consumer_Price_Index)

# Matriz de correlacion
cor(df_analisis)

df_anual<-df%>%                                                  # Retomar el df luego de la union de las tablas
            select(Date, Price, SP500, Consumer_Price_Index)%>%  # Selecciona las variables de interes (incluye la fecha)
            mutate(ano=as.numeric(format(Date,'%Y')))%>%        # Crea la columna ano
            group_by(ano)%>%                                     # Antes de agregar datos se debe indicar los campos de agrupacion
            summarize(Price_anual_avg = mean(Price),
                      SP500_anual_avg = mean(SP500),
                      IPC_anual_avg = mean(Consumer_Price_Index))

df_anual%>%head
df_anual%>%tail

options(repr.plot.width=10, repr.plot.height=6)
with(df_anual,plot(x = ano, y = Price_anual_avg, type = 'l', lwd = 2, col = 'black', ylab = '', xlab = 'Año', ylim=c(0,2500)))
with(df_anual,lines(x = ano, y = SP500_anual_avg, col = 'darkblue',lty = 2, lwd = 2))
with(df_anual,lines(x = ano, y = IPC_anual_avg, col = 'darkred',lty = 3, lwd = 1.5))
grid()
legend(x = 1950, y = 2550,legend = c('Precio Oro', 'S&P500','Consumer Price Index'),lty = c(1,2,3),col = c('black','darkblue','darkred'), lwd=c(2,2,3), bg = 'white')
