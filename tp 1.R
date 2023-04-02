
library(tidyverse)

##Carga de datos####

b_salariosdeptos <- read.csv("https://cdn.produccion.gob.ar/cdn-cep/datos-por-departamento/salarios/w_mean_depto_tot_emp_clae2.csv")
b_clae <- read.csv("https://cdn.produccion.gob.ar/cdn-cep/datos-por-departamento/diccionario_clae2.csv")
deptosJson <- st_read("C:/Users/Pablo/OneDrive/Diplomatura/Modulo 1/TP 1 y 2/departamentos_arg.geojson")


##Filtrar ultimos datos disponibles###

b_filtrada2022 <- b_salariosdeptos %>% 
  filter(fecha == "2022-11-01" & w_mean != -99) 


summary(b_filtrada2022$w_mean)
decil <- quantile(x=b_filtrada2022$w_mean, probs=c(0.9))



###Seleccionar noveno decil de media de ingresos por departamento####

b_decil9 <- b_filtrada2022 %>% 
  select(codigo_departamento_indec, w_mean, clae2) %>% 
  group_by(codigo_departamento_indec) %>% 
  filter(w_mean >= decil) %>%
  summarise(media = mean(w_mean, na.rm = TRUE)) %>% 
  arrange(codigo_departamento_indec)

class(b_salariosdeptos$codigo_departamento_indec)
class(b_decil9$codigo_departamento_indec)

b_decil9$codigo_departamento_indec = as.numeric(as.character(b_decil9$codigo_departamento_indec)) 
summary(b_decil9$media)
unique(b_decil9$codigo_departamento_indec)


##Promedio Salarial segun codigo de act####
promxact <- b_filtrada2022 %>% 
  group_by(clae2) %>% 
  summarise(media = mean(w_mean, na.rm = TRUE)) 


library(ggplot2)
library(dplyr)

año_ref <- 2022



##Mapa deptos noveno decil de ingresos####
library(sf)

head(deptosJson)

deptosJson$codigo_departamento_indec = as.numeric(as.character(deptosJson$codigo_departamento_indec)) 
class(deptosJson$codigo_departamento_indec)

b_deptosunidos <- deptosJson %>% 
  select(codigo_departamento_indec, geometry)
b_deptosunidos$codigo_departamento_indec = as.numeric(as.character(b_deptosunidos$codigo_departamento_indec)) 


b_deptomedia <- left_join(b_decil9, b_deptosunidos)


class(b_deptomedia$codigo_departamento_indec)



library(RColorBrewer)
paleta <- rev(brewer.pal(n = 7,name = "Reds"))

mapa <- ggplot(b_deptomedia) + 
  geom_sf(aes(fill = b_deptomedia$media, geometry = geometry)) +
    scale_fill_gradientn(colours = rev(paleta), name = "Promedio Salarial") +
  labs(title = "Departamentos con Mayores promedios salariales", subtitle = "Salarios del primer decil") +
  theme_minimal(base_size = 10)

mapa

ggsave(mapa, filename = "mapaPrimerDecil2022.png", width = 8,height=8,dpi = 200)



##DFSegunda consigna####

letramedia <- left_join(b_filtrada2022, b_clae) %>% 
  group_by(letra) %>% 
  summarise(media = mean(w_mean, na.rm = TRUE)) 

menores5 <- letramedia %>% 
  select(letra, media) %>% 
  group_by(media) %>%
  slice_min(order_by = media) %>% 
  head(menores5, n = 5)

?barplot

library(ggplot2)
v_1 <- c(menores5$letra) 
?barplot

##Grafico segunda consigna####

barplot(menores5$media, main = "Ramas con peores salarios promedio 2022",        
        names.arg = v_1,
        col = blues9,
        ylab = "Salario promedio en pesos",
        xlab = "Ramas de actividad")


barras <- barplot(menores5$media, main = "Ramas con peores salarios promedio 2022",        
        names.arg = v_1,
        col = blues9,
        ylab = "Salario promedio en pesos",
        xlab = "Ramas de actividad")

barras

ggsave(filename = "Graficoramaspeoressalarios.png", plot = barras, width = 10, height = 8)

?ggsave

summary(b_filtrada$w_mean)         
unique(b_empleados$fecha)


b_gral <- left_join(b_salariosdeptos, b_clae)

##DF Tercera consigna####

b_4sect <- b_gral %>% 
  select(fecha, clae2, letra, letra_desc, w_mean) %>% 
  filter(letra == "M" | letra == "F" | letra == "C" | letra == "P" & w_mean != -99) %>% 
  na.omit(b_4sect) %>% 
  group_by(fecha, letra) %>% 
  summarise(media = mean(w_mean, na.rm = TRUE))  


##Serie de tiempo####


class(b_4sect$fecha)

b_4sect <- b_4sect %>% 
  filter(fecha == "2014-11-01" | fecha == "2015-11-01" | fecha == "2016-11-01" | fecha == "2017-11-01" | fecha == "2018-11-01" | fecha == "2017-11-01" | fecha == "2018-11-01" | fecha =="2019-11-01" | fecha == "2020-11-01" | fecha =="2021-11-01" | fecha == "2022-11-01") %>%  
    mutate(sectnom = case_when(letra == "C" ~ "INDUSTRIA MANUFACTURERA",
                             letra == "F" ~ "CONSTRUCCION",
                             letra == "M" ~ "SERVICIOS PROFESIONALES, CIENTÍFICOS Y TÉCNICOS",
                             letra == "P" ~ "ENSEÑANZA"))


grafico <- ggplot(b_4sect, aes(x = fecha, y = media, col = b_4sect$sectnom)) +
  geom_line ()
ggsave(filename = "prueba.png", plot = grafico, width = 10, height = 8)

class(b_4sect$fecha)
?geom_line

?ggplot


##deptos <- read.csv("TP 1 y 2/departamentos.csv")
unique(b_empleados$codigo_departamento_indec)

install.packages("sf")

library(readxl)

b_caba <- read_excel("TP 1 y 2/variaciones_salarios_02_23.xlsx")
