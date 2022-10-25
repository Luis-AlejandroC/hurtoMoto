# Author: SAMdata
# Data Base Hurto de personas Medellín]
# Source: MEDATA http://medata.gov.co/dataset/hurto-persona
# Description: the dataset is gonna be cleaned up and the it will be filtered by "Comuna 7" and year range (2020->2022)

#Change language to Spanish
Sys.getlocale()
Sys.setlocale("LC_ALL", "Spanish.UTF-8")
### -------- Libraries -------

library(tidyverse)
library(visdat)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)

### ------- uploading DB -------  

#file_p <- "D:/Mis documentos/IUPB/SAMData/Seguridad_Comuna_2022-II/hurto_persona/hurto_a_persona.csv"
file_p <- read.csv2("http://medata.gov.co/sites/default/files/medata_harvest_files/hurto_a_persona.csv",
                    header = TRUE, sep = ";", dec = ",", quote = "\"",
                    na.strings=c("NaN", "Sin dato"))

# assigning to a var the address of the DB

#df_p <- read.csv2(file = file_p, header = T)
df_p <-file_p 

# reading and creating the first dataframe

### ------- Preliminar analysis -------

## dim

dim(df_p)

# shows how many cols (vars) and rows (records) the DB has = [1] 284316(rows)     36 (cols)

## head & tail

head(df_p)

# returns n first rows

tail(df_p)

# returns n last rows

## missing values

sum(is.na(df_p))

# is.na returns bool values for empty/missing values from df_p - sum function, sums all true values (1)
# At this case, df_p hasn´t any empty values, it must be inspected for identifying NA values
# Once missing values are identifyed, these will be assigned to NA values for being dropped

df_p[df_p == "Sin dato"] <- NA
df_p[df_p == "SIN DATO"] <- NA
df_p[df_p == "NaN"] <- NA

# With this way, any "sindato, SINDATO, NaN" is iterated and replaced by NA

sum(is.na(df_p)) # 4182185 missing values

## identfying cols with more missing values

map_dbl(df_p, .f = function(x) {sum(is.na(x))})

# returns a list from a DF according to a function
# source: https://www.youtube.com/watch?v=g_dhNS1vmhk - https://rdrr.io/github/tidyverse/purrr/man/map.html
# grupo_actor, actividad_delictiva, parentesco, ocupacion, discapacidad, grupo_especial, nivel_academico, testigo 
# caracterizacion, conducta_especial, articulo_penal, categoria_penal, color, permiso, unidad_medida

### ------- Dropping missing values  -------

df_p <- df_p %>% select(-grupo_actor, -actividad_delictiva, -parentesco, -ocupacion, 
                        -discapacidad, -grupo_especial, -nivel_academico, -testigo, 
                        -caracterizacion, -conducta_especial, -articulo_penal, 
                        -categoria_penal, -color, -permiso, -unidad_medida)

# it makes a pipe with the original df_p and df_p less all the cols that 
# select(-x1, -x2, ..., xn) will drop
# I could make the same with select, but just bringing the cols that I want to 
# put into the DS, this without the operator (-)

dim(df_p)

# 15 cols were dropped from df_p

### ------- Arrange & Filter -------

## Mutabe by year

head(df_p)

class(df_p$fecha_hecho) # it shows me the data type for "fecha_hecho"

# Year is a char, it should be mutated as a numeric 

df_PYear <- df_p %>% mutate(anio_hecho = year(fecha_hecho), mes_hecho = month(fecha_hecho),
                          dia_hecho = day(fecha_hecho)) %>% 
  relocate(anio_hecho, mes_hecho, dia_hecho, .after = fecha_hecho) %>% 
  arrange(anio_hecho, mes_hecho, dia_hecho)

# mutate: it allows me to change DS
# relocate: it was used o relocate the new cols after the col "fecha_hecho"
# arrange: it allows me to organize the cols

## filter by coidgo_comuna == 7

class(df_p$codigo_comuna) # comuna is type char, mutate to num

col.numcomuna <- c("codigo_comuna")
df_PYear[col.numcomuna] <- sapply(df_PYear[col.numcomuna], as.numeric)

# sapply: It will iterate any value of the col: "codigo_columna" and apply the function as.numeric,
# mutating the data type - any chanche will be assigned to df_PYear at the same place

class(df_PYear$codigo_comuna) # now "codigo comuna is a list"

## filter by year - final df

df_finalpersonas <- df_PYear %>%  filter(codigo_comuna == 7, 
                                 anio_hecho %in% c(2020, 2021, 2022))


### ------- Analyzing the final df  -------

dim(df_finalpersonas)

## Checking missing values

sum(is.na(df_finalpersonas))

### ------- Cleaning and more analysis  -------

table(df_finalpersonas$modalidad)

## mode vs gender

table_modgender <- table(df_finalpersonas$sexo, df_finalpersonas$modalidad)
table_modgender

## mode per year

tbl_modyear <- table(df_finalpersonas$modalidad, df_finalpersonas$anio_hecho)
tbl_modyear

tbl_yearneigh <- table(df_finalpersonas$nombre_barrio, df_finalpersonas$anio_hecho)
tbl_yearneigh

tbls_neigh <- table(df_finalpersonas$nombre_barrio)
tbls_neigh


### ------- Plots  -------

## mode vs year
library(ggplot2)
plot_ppy <- df_finalpersonas %>% group_by(anio_hecho, modalidad) %>% 
  summarise(counter = n()) %>% 
  ggplot(aes(x = modalidad, y = counter,  fill = factor(anio_hecho))) + 
  geom_bar(stat = "identity", position = position_dodge(0.9) , color = "black") +
  # theme(axis.text.x = element_text(angle = 90) ) +
  scale_fill_manual(values = c("darkgoldenrod1", "burlywood2", "darkslateblue")) +
  geom_text(aes(label = counter), vjust = -1, position = position_dodge(1),
            color = "black", fontface = "bold", size = 3) + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.text.x = element_text(face = "bold", size = 10, angle = 90),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        legend.title = element_text(face = "bold", hjust = 0.5, size = 12),
        legend.text = element_text(size = 12),
        legend.justification = "top",
        legend.background = element_rect(size = 0.5, linetype = "solid", color = "black"),
        legend.key = element_blank()) + 
    labs(title = "Víctimas de hurto por modalidad",
         x = "Modalidad", y = "Cantidad", fill = "Año")
  
plot_ppy

## neighborhood vs year  - 
# Facultad de Mina U. Nacional, Facultad de Mina U. Nal,   Fac. Veterinaria y Zootecnia U.De.A., San German, 
# B. Cerro el Volador, Ecoparque Cerro El Volador
## theme(axis.text.x = element_text(angle = 90) ) - It allows me rotate the x labels
# barplot(tbl_modyear, horiz = TRUE) +

df_finalpersonas$nombre_barrio <- ifelse(df_finalpersonas$nombre_barrio == "Facultad de Minas U. Nal", 
                                         "Facultad de Minas U. Nacional", df_finalpersonas$nombre_barrio)


df_finalpersonas <- df_finalpersonas %>% mutate(var_aux = ifelse(nombre_barrio %in% c("Facultad de Minas U. Nacional", "Fac. Veterinaria y Zootecnia U.De.A.", "San Germán", 
                                                 "B. Cerro el Volador", "Ecoparque Cerro El Volador"), "Ciudadela Universitaria", "Comuna 7"))

## New var
# with this script var_aux is added (mutate) to df_...

table(df_finalpersonas$var_aux, df_finalpersonas$nombre_barrio)

plot_ppn <- df_finalpersonas %>% group_by(nombre_barrio, var_aux) %>% 
  summarise(countern = n()) %>% 
  ggplot(aes(x = reorder(nombre_barrio, -countern), y = countern, fill = factor(var_aux))) +
  geom_bar(stat = "identity", position = "dodge" , color = "black") +
  coord_flip() +
  geom_text(aes(label = countern), vjust = -0.5,hjust = -0.5, position = position_dodge(1),
            color = "black", fontface = "bold", size = 2.5)  +
  scale_fill_manual(values = c("firebrick1", "grey")) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.text.x = element_text(face = "bold", size = 8),
        axis.text.y = element_text(face = "bold", size = 8),
        axis.title.x = element_text(face = "bold", size = 8),
        axis.title.y = element_text(face = "bold", size = 8),
        legend.title = element_text(face = "bold", hjust = 0.5, size = 8),
        legend.text = element_text(size = 8),
        legend.justification = "center",
        legend.background = element_rect(size = 0.5, linetype = "solid", color = "black"),
        legend.key = element_blank(),
        legend.position = c(0.78, 0.93)) +
  labs(title = "Hurtos por barrio",
       x = "Modalidad", y = "Cantidad", fill = "Distribución Barrios")
  

plot_ppn

##
# ggplot(aes(x = reorder(nombre_barrio, -countern), y = countern)) - reorder() allows to order by quantity.
# sintaxis x = reorder(nombre_barrio => according to the var that should be orderedo