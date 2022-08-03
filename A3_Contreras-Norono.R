# Integrantes:
## Juan Contreras, C.I. 26.411.263
## Luis Noroño, C.I. 26.961.171

# Librerias:
library(tidyverse)
library(readxl)
library(haven)
library(data.table)
library(sjlabelled)
library(sjPlot)
library(mice)
library(survey)

getwd()

setwd("C:/Users/juana/OneDrive/Documentos/")


# Importamos la data
personas <- read_sav("C:/Users/juana/OneDrive/Documentos/encovi_personas2017_ds.sav")


# Renombramos las columnas y colocamos los identificadores como caracteres
new_names_pers <- c("id_hogar", "id_per", "parentesco", "edad", "sexo", 
                    "sit_conyu", "nivel_edu", "edu_ano_aprobado", "edu_sem_aprobado",
                    "tipo_edu", "sit_econo", "sector_eco", "cat_ocupa",
                    "trab_remun", "ing_laboral", "horas_trab", "ing_otro",
                    "ing_pension",
                    "pesop", "grp_edad", "anos_edu", "tipo_ciudad")

personas <- personas %>%
  setnames(old = colnames(.), new = new_names_pers) %>%
  mutate(across(.cols = c("id_hogar", "id_per"),
                .fns = as.character))


# Formamos los grupos de donantes
colnames(personas)

grupo_donantes<- personas %>%
  filter((sit_econo == 1 | 
          sit_econo == 2) & (ing_laboral != 99 |
                              ing_laboral != 98 |
                              ing_laboral != 0))%>%
  
  group_by(grp_edad, sexo, nivel_edu, cat_ocupa) %>% 
  summarise(avg_ing = weighted.mean(ing_laboral , pesop),
            n_imp = length(grp_edad))


# Realizamos la imputacion
ing_laboral_imput <- personas %>%
  left_join(grupo_donantes, by = c("grp_edad", "sexo", 
                             "nivel_edu", "cat_ocupa"))%>% 
  
  mutate(ing_laboral = ifelse(ing_laboral %in% c(99,98,0),
                               yes = avg_ing,
                               no = ing_laboral))


# Justificacion de variables




















