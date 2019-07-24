library(tidyverse)  # Cargar las librerias

documentos<-list.files(path = './Precio_Bolsa/',
           pattern = '.csv',
           full.names = TRUE)


archivo1<-read_csv(file = documentos[1],col_names = TRUE,cols(
  Fecha = col_date(format = ""),
  H0 = col_double(),
  H1 = col_double(),
  H2 = col_double(),
  H3 = col_double(),
  H4 = col_double(),
  H5 = col_double(),
  H6 = col_double(),
  H7 = col_double(),
  H8 = col_double(),
  H9 = col_double(),
  H10 = col_double(),
  H11 = col_double(),
  H12 = col_double(),
  H13 = col_double(),
  H14 = col_double(),
  H15 = col_double(),
  H16 = col_double(),
  H17 = col_double(),
  H18 = col_double(),
  H19 = col_double(),
  H20 = col_double(),
  H21 = col_double(),
  H22 = col_double(),
  H23 = col_double()
))


df<-data.frame()

for(i in seq(length(documentos))){
  #print(i)
  temp<-read_csv(documentos[i],col_names = TRUE,cols(
    Fecha = col_date(format = ""),
    H0 = col_double(),
    H1 = col_double(),
    H2 = col_double(),
    H3 = col_double(),
    H4 = col_double(),
    H5 = col_double(),
    H6 = col_double(),
    H7 = col_double(),
    H8 = col_double(),
    H9 = col_double(),
    H10 = col_double(),
    H11 = col_double(),
    H12 = col_double(),
    H13 = col_double(),
    H14 = col_double(),
    H15 = col_double(),
    H16 = col_double(),
    H17 = col_double(),
    H18 = col_double(),
    H19 = col_double(),
    H20 = col_double(),
    H21 = col_double(),
    H22 = col_double(),
    H23 = col_double()
  ))
  df<-rbind(df,temp)
}


str(df)
dim(df)


paste("Tengo los siguientes datos faltantes", sum(is.na(df)))

df$promedio<-rowMeans(x = df[,2:25])

head(df$promedio)

with(df, plot(x = Fecha,
              y = promedio,
              type ='l',
              main = 'Precios promedio dia ($/kWh)',
              ylab = 'Precios Promedio'))
grid()
# 
# legend(x = 100,
#        y = 1800,legend = c('Precios'),lty = c(1))
legend(x = 200, y = 1800,legend = c('Precio'),lty = c(1),col = c('black'), lwd=c(2), bg = 'white')


