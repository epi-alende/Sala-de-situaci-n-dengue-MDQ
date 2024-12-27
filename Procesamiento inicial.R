###

#Este script depura y procesa los datos necesarios para la construcción del dashboard de la SS

###

library(tidyverse)
library(readr)
library(readxl)
library(writexl)
library(lubridate)
library(forcats)
library(janitor)
library(ggplot2)
library(highcharter)
library(RColorBrewer)

Sisa_completo <- read_delim("C:/Users/Epidemiologia01/Downloads/NOM_P06_[2024-11-25]/NOM_P06.csv", 
                            delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-2"), 
                            trim_ws = TRUE)
Sisa_completo <- Sisa_completo %>% filter(EVENTO == "Dengue" & ESTABLECIMIENTO_CARGA == "HOSPITAL INTERZONAL GENERAL DE AGUDOS DR. OSCAR ALENDE" |
                                            EVENTO == "Dengue" & ESTABLECIMIENTO_CARGA == "HOSPITAL INTERZONAL ESPECIALIZADO MATERNO INFANTIL DR. VICTORIO TETAMANTI")
write_xlsx(Sisa_completo, "baseSS.xlsx")

# Creo las variables semana epidemiológica y año epidemiológico
Sisa_completo <- Sisa_completo %>% mutate(Sepi = epiweek(FECHA_CONSULTA), Anioepi = epiyear(FECHA_CONSULTA))
#Transformo la variable SEXO a factor y recodifico los niveles
Sisa_completo <- Sisa_completo %>% mutate(SEXO = factor(case_when(
  SEXO == "F" ~ "Femenino",
  SEXO == "M" ~ "Masculino")))
# Creo la variable grupo_edad a partir de la variable "Edad Diagnóstico"
Sisa_completo <- Sisa_completo %>%
  mutate(grupo_edad = case_when(
    EDAD_DIAGNOSTICO >= 0 & EDAD_DIAGNOSTICO <= 4  ~ "0-4",
    EDAD_DIAGNOSTICO >= 5  & EDAD_DIAGNOSTICO <= 9  ~ "5-9",
    EDAD_DIAGNOSTICO >= 10 & EDAD_DIAGNOSTICO <= 14 ~ "10-14",
    EDAD_DIAGNOSTICO >= 15 & EDAD_DIAGNOSTICO <= 19 ~ "15-19",
    EDAD_DIAGNOSTICO >= 20 & EDAD_DIAGNOSTICO <= 24 ~ "20-24",
    EDAD_DIAGNOSTICO >= 25 & EDAD_DIAGNOSTICO <= 29 ~ "25-29",
    EDAD_DIAGNOSTICO >= 30 & EDAD_DIAGNOSTICO <= 34 ~ "30-34",
    EDAD_DIAGNOSTICO >= 35 & EDAD_DIAGNOSTICO <= 39 ~ "35-39",
    EDAD_DIAGNOSTICO >= 40 & EDAD_DIAGNOSTICO <= 44 ~ "40-44",
    EDAD_DIAGNOSTICO >= 45 & EDAD_DIAGNOSTICO <= 49 ~ "45-49",
    EDAD_DIAGNOSTICO >= 50 & EDAD_DIAGNOSTICO <= 54 ~ "50-54",
    EDAD_DIAGNOSTICO >= 55 & EDAD_DIAGNOSTICO <= 59 ~ "55-59",
    EDAD_DIAGNOSTICO >= 60 & EDAD_DIAGNOSTICO <= 64 ~ "60-64",
    EDAD_DIAGNOSTICO >= 65 & EDAD_DIAGNOSTICO <= 69 ~ "65-69",
    EDAD_DIAGNOSTICO >= 70 & EDAD_DIAGNOSTICO <= 74 ~ "70-74",
    EDAD_DIAGNOSTICO >= 75 & EDAD_DIAGNOSTICO <= 79 ~ "75-79",
    EDAD_DIAGNOSTICO >= 80 ~ "80+"))
# Creo una nueva variable de COMORBILIDADes a partir de la variable COMORBILIDAD

Sisa_completo <- Sisa_completo %>% mutate(Comorbilidades = case_when(COMORBILIDAD == "Abuso de drogas" |
                                                                 COMORBILIDAD == "Alcoholismo" |
                                                                 COMORBILIDAD == "Diabetes" |
                                                                 COMORBILIDAD == "Enfermedad cardíaca" |
                                                                 COMORBILIDAD == "Enfermedad endocrinológica" |
                                                                 COMORBILIDAD == "Enfermedad metabólica" |
                                                                 COMORBILIDAD == "Enfermedad neurológica crónica" |
                                                                 COMORBILIDAD == "Enfermedad oncohematológica" |
                                                                 COMORBILIDAD == "Enfermedad oncológica" |
                                                                 COMORBILIDAD == "Enfermedad respiratoria crónica" |
                                                                 COMORBILIDAD == "Enfermedad reumatológica" |
                                                                 COMORBILIDAD == "Hemoglobinopatía" |
                                                                 COMORBILIDAD == "Hipertensión" |
                                                                 COMORBILIDAD == "Infección por VIH/SIDA" |
                                                                 COMORBILIDAD == "Inmunocomprometido No VIH" |
                                                                 COMORBILIDAD == "Insuficiencia renal crónica" |
                                                                 COMORBILIDAD == "Obesidad (IMC 30-39" ~ "SI",
                                                               COMORBILIDAD == "Sin COMORBILIDADes" ~ "NO",
                                                               COMORBILIDAD == "*sin dato*" ~ "SIN DATO",
                                                               is.na(COMORBILIDAD) ~ "SIN DATO"))
# Creo la variable Forma clínica a partir de la variable Signo / Síntoma
Sisa_completo <- Sisa_completo %>% mutate("Forma clínica" = factor(case_when(
  SIGNO_SINTOMA == "*sin dato*" ~ "SIN DATO",
  SIGNO_SINTOMA == "Sin signos de alarma" ~ "SIN SIGNOS DE ALARMA",
  SIGNO_SINTOMA == "Con signos de alarma" ~ "CON SIGNOS DE ALARMA",
  grepl("grave*", SIGNO_SINTOMA) ~ "DENGUE GRAVE",
  grepl("^hematocrito|Derrame|continuo|Epistaxis|Gingivorragia|Hematemesis|plaquetopenia|Hemoptisis|Hepatomegalia|lipotimia|Letargo|Melena|Metrorragia|mucosas|persitentes$", SIGNO_SINTOMA) ~ "CON SIGNOS DE ALARMA",
  is.na(SIGNO_SINTOMA) ~ "SIN DATO",
  TRUE ~ "SIN SIGNOS DE ALARMA")))
Sisa_completo %>% count(`Forma clínica`)
# Transformo la variable a factor y organizo los niveles
Sisa_completo <- Sisa_completo %>% mutate(`Forma clínica` = factor(`Forma clínica`, levels = c("SIN DATO", "SIN SIGNOS DE ALARMA", "CON SIGNOS DE ALARMA", "DENGUE GRAVE")))
levels(Sisa_completo$`Forma clínica`)
# Debido a que existen duplicados, me quedo con una observación por ID de evento, teniendo en cuenta la forma clínica
Sisa_completo <- Sisa_completo %>%
  group_by(IDEVENTOCASO) %>%
  arrange(desc(`Forma clínica`)) %>%  # Ordena por forma clínica, priorizando grave
  slice(1) %>%                       # Selecciona la primera fila de cada grupo
  ungroup()
Sisa_completo %>% count(`Forma clínica`)
# Transformo la variable clasificación manual del caso a factor y creo una nueva variable llamada Clasificación que agrupa las categorías
Sisa_completo <- Sisa_completo %>% mutate(CLASIFICACION_MANUAL = factor(CLASIFICACION_MANUAL),
                                          RESULTADO = factor(RESULTADO))
Sisa_completo <- Sisa_completo %>% mutate("Clasificacion" = factor(case_when(
  grepl("coinfección*|confirmado*|brote*", CLASIFICACION_MANUAL) ~ "Confirmado",
  grepl("descartado*|invalidado*", CLASIFICACION_MANUAL) ~ "Descartado",
  grepl("probable*", CLASIFICACION_MANUAL) ~ "Probable",
  grepl("sospechoso*", CLASIFICACION_MANUAL) ~ "Sospechoso",
  is.na(CLASIFICACION_MANUAL) ~ "Sin dato")))
# Construyo la variable serotipo teniendo en cuenta las variables clasificación manual y RESULTADO
Sisa_completo <- Sisa_completo %>% 
  mutate(Serotipo = case_when(CLASIFICACION_MANUAL == "Caso confirmado DEN-1" | RESULTADO == "DEN-1" ~ "DEN 1",
                              CLASIFICACION_MANUAL == "Caso confirmado DEN-2" | RESULTADO == "DEN-2"~ "DEN 2",
                              CLASIFICACION_MANUAL == "Caso confirmado DEN-3" | RESULTADO == "DEN-3"~ "DEN 3",
                              TRUE ~ "Sin serotipo"))
# Transformo los valores sin dato a NA para poder cuantificarlos correctamente y reorganizo categorías en algunas variables
Sisa_completo <- Sisa_completo %>% mutate(ANTECEDENTE_EPIDEMIOLOGICO = case_when(ANTECEDENTE_EPIDEMIOLOGICO == "*sin dato*" | ANTECEDENTE_EPIDEMIOLOGICO == "Sin dato" | ANTECEDENTE_EPIDEMIOLOGICO == "NS"~ "SIN DATO",
                                                                                   ANTECEDENTE_EPIDEMIOLOGICO == "NO" | ANTECEDENTE_EPIDEMIOLOGICO == "Sin antecedente de viaje a zona afectada en los últimos 15 días." ~ "Sin antecedente epidemiológico",
                                                                                   ANTECEDENTE_EPIDEMIOLOGICO == "SI" | ANTECEDENTE_EPIDEMIOLOGICO == "Nexo con caso de Dengue confirmado" ~ "Antecedente epidemiológico no esp.",
                                                                                    .default = ANTECEDENTE_EPIDEMIOLOGICO))
Sisa_completo <- Sisa_completo %>% mutate(EMBARAZADA = case_when(EMBARAZADA == "*sin dato*" & SEXO == "F" | is.na(EMBARAZADA) & SEXO == "F" ~ "SIN DATO",
                                          EMBARAZADA == "*sin dato*" & SEXO == "M" | is.na(EMBARAZADA) & SEXO == "M" ~ "NO CORRESPONDE",
                                          .default = EMBARAZADA))
Sisa_completo <- Sisa_completo %>% mutate(PAIS_VIAJE = if_else(PAIS_VIAJE == "*sin dato*" | PAIS_VIAJE == "Argentina", "SIN DATO", PAIS_VIAJE))
Sisa_completo <- Sisa_completo %>% mutate(PROV_VIAJE = if_else(PROV_VIAJE == "*sin dato*", "SIN DATO", PROV_VIAJE))
# Creo la variable "Grupo_riesgo" a partir de la variable "Edad Diagnóstico"
Sisa_completo <- Sisa_completo %>% mutate("Grupo_riesgo" = if_else(EDAD_DIAGNOSTICO <2 | EDAD_DIAGNOSTICO >65, "SI", "NO"))

# La temporada anterior de dengue se extiende desde el 30/07/2023 (SE31) al 27/07/2024 (SE30)
# La temporada actual de dengue se extiende desde el 28/07/2024 (SE31) al 26/07/2025 (SE30)
Sisa_anterior <- subset(Sisa_completo, FECHA_CONSULTA >= "2023-07-30" & FECHA_CONSULTA < "2024-07-28")
Sisa_actual <- subset(Sisa_completo, FECHA_CONSULTA >= "2024-07-28" & FECHA_CONSULTA < "2025-07-27")
# Escribo un archivo csv de sisa_completo
write.csv(Sisa_completo, "Sisa completo.csv")

write_xlsx(Sisa_completo, "baseSS.xlsx")


### FIN ###