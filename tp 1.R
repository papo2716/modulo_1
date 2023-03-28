
library(tidyverse)



b_empleados <- read.csv("https://cdn.produccion.gob.ar/cdn-cep/datos-por-departamento/salarios/w_mean_depto_tot_emp_clae2.csv")
b_clae <- read.csv("https://cdn.produccion.gob.ar/cdn-cep/datos-por-departamento/diccionario_clae2.csv")
##Filtrar año 2022

b_filtrada2022 <- b_empleados %>% 
  filter(fecha == "2022-11-01" & w_mean != -99) 


summary(b_filtrada2022$w_mean)
quantile(x=b_filtrada2022$w_mean, probs=c(0.05, 0.5, 0.9))

###deptos con media mayor a tercer cuartil 

b_primerdecil <- b_filtrada2022 %>% 
  select(codigo_departamento_indec, w_mean, clae2) %>% 
  group_by(codigo_departamento_indec) %>% 
  filter(w_mean >= 363391.4) %>%
  summarise(media = mean(w_mean, na.rm = TRUE)) %>% 
  arrange(media)

class(b_empleados$codigo_departamento_indec)
class(b_primerdecil$codigo_departamento_indec)

as.numeric(b_primerdecil$codigo_departamento_indec) 




unique(b_primerdecil$codigo_departamento_indec)

##Promedio Salarial segun codigo de act####
prom <- b_filtrada2022 %>% 
  group_by(clae2) %>% 
  summarise(media = mean(w_mean, na.rm = TRUE))  


##DFSegunda consigna####
menores5 <- prom %>% 
  select(clae2, media) %>% 
  group_by(media) %>%
  slice_min(order_by = media) %>% 
  head(menores5, n = 5)

?barplot

library(ggplot2)
v_1 <- c(menores5$clae2) 


##Grafico segunda consigna####
png(filename = "peoressalarios.png", width = 800, height = 600)
barplot(menores5$media, main = "Ramas con peores salarios promedio 2022",        
        names.arg = v_1,
        col = blues9,
        ylab = "Salario promedio en pesos",
        xlab = "Ramas de actividad")
dev.off()



ggsave(grafbarras, filename = "Graficoramaspeoressalarios.png")


summary(b_filtrada$w_mean)         

unique(b_empleados$fecha)


b_clae <- read.csv("https://cdn.produccion.gob.ar/cdn-cep/datos-por-departamento/diccionario_clae2.csv")
b_unida <- left_join(b_empleados, b_clae)

##DF Tercera consigna####

b_4sect <- b_unida %>% 
  select(fecha, clae2, letra, letra_desc, w_mean) %>% 
  filter(letra == "M" | letra == "F" | letra == "C" | letra == "P" & w_mean != -99) %>% 
  na.omit(b_4sect) %>% 
  group_by(fecha, letra) %>% 
  summarise(media = mean(w_mean, na.rm = TRUE))  



##deptos <- read.csv("TP 1 y 2/departamentos.csv")
unique(b_empleados$codigo_departamento_indec)


install.packages("sf")
library(sf)

deptosJson <- st_read("C:/Users/Pablo/OneDrive/Diplomatura/Modulo 1/TP 1 y 2/departamentos_arg.geojson")
head(deptosJson)

deptosJson$codigo_departamento_indec = as.numeric(as.character(deptosJson$codigo_departamento_indec)) 
class(deptosJson$codigo_departamento_indec)

## DF Primera consigna####
b_deptosunidos <- deptosJson %>% 
  select(codigo_departamento_indec, geometry)
b_municmedia <- left_join(b_primerdecil, b_deptosunidos)


class(b_deptosunidos$codigo_departamento_indec)


library(ggplot2)
library(dplyr)

año_ref <- 2022



##Mapa ####
###https://rstudio-pubs-static.s3.amazonaws.com/703367_c46cbf1f9e564234b98591dd47201b9c.html
###https://www.prestevez.com/es/post/tutorial-mapas-tidyverse/

library(RColorBrewer)
paleta <- rev(brewer.pal(n = 7,name = "Reds"))

mapa <- ggplot(b_municmedia) + 
  geom_sf(aes(fill = b_municmedia$media, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Promedio Salarial") +
  labs(title = "Departamentos con Mayores promedios salariales", subtitle = "Salarios del primer decil") +
  theme_minimal(base_size = 10)
mapa

ggsave(mapa, filename = "mapaPrimerDecil2022.png", width = 8,height=8,dpi = 200)



##Serie de tiempo####

b_4sect$fecha = as.date((b_4sect$fecha))
?as.date
b_4sect$fecha=as.Date(b_4sect$fecha)


class(b_4sect$fecha)
b_4sectdes <- b_4sect %>% 
  filter(fecha != "2014-06-01",fecha != "2014-12-01",fecha != "2015-06-01",fecha != "2015-12-01",fecha != "2016-06-01",fecha != "2016-12-01",fecha != "2017-06-01", fecha !="2017-12-01",fecha != "2018-06-01", fecha !="2018-12-01",fecha != "2019-06-01",fecha != "2019-12-01", fecha !="2020-06-01", fecha !="2020-12-01", fecha !="2021-06-01", fecha !="2021-12-01",fecha != "2022-06-01",fecha != "2022-12-01") %>% 
  mutate(sectnom = case_when(letra == "C" ~ "INDUSTRIA MANUFACTURERA",
                             letra == "F" ~ "CONSTRUCCION",
                             letra == "M" ~ "SERVICIOS PROFESIONALES, CIENTÍFICOS Y TÉCNICOS",
                             letra == "P" ~ "ENSEÑANZA"))


ggplot(b_4sectdes, aes(x = fecha, y = media, col = b_4sectdes$sectnom)) +
  geom_line ()


?geom_line

?ggplot

library(readxl)

b_caba <- read_excel("TP 1 y 2/variaciones_salarios_02_23.xlsx")
