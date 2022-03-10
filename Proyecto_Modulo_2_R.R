###############################################################################
########################## LEER UN CONJUNTO DE DATOS CSV # ####################
############################## Y CONECTARNOS A MYSQL ##########################
###############################################################################


library(gitcreds)
install.packages("gitcreds")

gitcreds_set() # Para ingresar el token

gitcreds_get() # Sirve para verificar que se ingresó el token

#Funcion saber directorio
getwd()
#Funcion para fijar la ubicacion del directorio de trabajo
# setwd("C:/Users/STOCK-LAP310/Documents/Documents/00.-CURSO_MASTER/00_0_BEDU/000_Proyecto_M02")

# df <- read.csv(url_github, header = TRUE)
# dir()

library(DBI)
library(RMySQL)


database <- dbConnect(MySQL(), user="root", host="localhost",password="Soportec@0865", dbname="defun_2020")

# Exploramos la Base de Datos y todas sus tablas

dbListTables(database)

# Exploramos la tabla y sus campos

dbListFields(database,'bdd_def_norm')

# Agregamos la consulta completa a un Data Frame

df <- dbGetQuery(database, statement = "select * from bdd_def_norm  ;")


# Explorar de forma general el dataframe

class(df)# Saber tipo de objeto 

names(df)# Nombre de los encabezados

head(df)

str(df)# estructura de un DF

View(df)



# Revisar el tipo (class) de nuestras variables

lapply(df, class)

# Agregamos otra consulta para contar las muertes por localidad

df.count <- dbGetQuery(database, statement = "select count(*) as conteo,loc_ocurr from bdd_def_norm group by loc_ocurr order by 1 desc;")

lapply(df.count, class) # Nos da el tipo de dato de cada clase o campo

mean(df.count$conteo) # Media de el numero de Muertes por estado



hist(df.count$conteo) # Histograma del conteo general

str(df.count) # Structura del DFrame

library(dplyr) # Libreria dplyr

df.mas <- df.count %>% filter( conteo >1000 )

write.csv(df.tij, "mi_df.csv")

plot(df.mas$conteo)

quantile(df.mas$conteo, 0.25) # cuantil del 25

quantile(df.mas$conteo, c(0.25, 0.50, 0.75))# todos los cuartiles

quantile(df.mas$conteo,seq(0.1,0.9, by = 0.1))# Deciles


max(df.mas$conteo)

summary(df.tij$conteo)

var(df.mas$conteo)   # Varianza
sqrt(var(df.mas$conteo))

sd(df.mas$conteo)  #desviacion standar
(sd(df.mas$conteo))**2
###############################################################################
####################### ANALISIS EXPLOTATORIO DE DATOS ########################
############################## Y GRAFICAS #####################################
###############################################################################
summary(df)

#### HISTOGRAMAS
library(ggplot2)


# Graficas de puntos con  numero de muertes mayo a 1000

ggplot(df.mas, aes(x = conteo, y = loc_ocurr )) + # 
  geom_point()



df.sex <- dbGetQuery(database, statement = "select count(*) as conteo,loc_ocurr, causa_def, sexo 
                       from bdd_def_norm 
                       where loc_ocurr like 'Ju_rez%'
                     group by loc_ocurr,causa_def,sexo 
                     having count(*) > 100
                     order by 1 desc;")



# Graficos seccionado por sexo

p <- ggplot(df.sex, aes(x = conteo, y = causa_def )) + # , colour = mpg no funciono
  geom_point() +
  theme_grey() + # temas
  facet_wrap("sexo") + 
  xlab('Num de Muertes') +
  ylab("Causa de Muerte") + 
  ggtitle("Grafica de Muertes de Cd. Juarez")
# install.packages("plotly")
library(plotly)
ggplotly(p)

# Boxplot  de muertes dividido por sexo y numero de muertes de Tijuana

head(df)
str(df)
summary(df)

df.gpotij <- dbGetQuery(database, statement = " select count(*) as conteo,grupo,sexo 
                                                from bdd_def_norm 
                                                where loc_ocurr like '%Tijuan%'
                                                and sexo <> 'No especificado'
                                                group by grupo,loc_ocurr,sexo 
                                                having count(*) > 50
                                                ;")


#### Boxplots
head(df.gpotij)

ggplot(df.gpotij,
       aes(x=conteo, y=sexo, fill = sexo))+
  geom_boxplot()+
  ggtitle("Boxplots") +
  xlab("No. Muertes") +
  ylab("Sexo")


