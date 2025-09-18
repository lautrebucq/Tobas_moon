# 0) Bibliotecas ####
pacman::p_load(tidyverse, Routliers, lme4, parameters, performance,
               patchwork, janitor, merDeriv)
rm(list = ls())

# 1) Sueño promedio vs celulares ####
## 1.1) Carga de datos y rearreglos varios ####

#data tobas

tobas_DLMO_2 <- read.csv("complete_dataset.csv")

tobas_DLMO <- read.csv("complete_dataset_2025.csv")

filas_2025 <- tobas_DLMO_2%>% filter(year == 2025)

tobas_DLMO <- bind_rows(tobas_DLMO, filas_2025)


tobas_DLMO <- tobas_DLMO %>%
  mutate(# Asignación de estaciones
    season = case_when(
      month %in% 10:12 ~ "Spring/Summer",
      month %in% 01:02 ~ "Summer",
      month %in% 03:09 ~ "Fall/Winter",
      TRUE ~ NA_character_),
    # calculo edades
    age = round(
      as.numeric(
        difftime(calendar_date, fecha_de_nacimiento,
                 unit = "days"))/365.25, 1),
    # genero columna "campaña"
    campaign = paste(season, year, sep = "_"),
    # rearreglo comunidades y sexo
    comunidad_agrupada = case_when(
      comunidad %in% c("IG", "VP") ~ "Rural",
      comunidad == "IJ" ~ "Urban"),
    sexo = case_when(sexo == "Masculino" ~ "Male",
                     sexo == "Femenino" ~ "Female")) %>%
  # quiero asignar una fecha de inicio/final a cada campaña para ordenarlas luego
  group_by(campaign) %>%
  mutate(inicio = min(calendar_date),
         final = max(calendar_date))

## 1.2) Carga de datos de celulares ####
datos_celu <- read_csv("C:/Users/laura/OneDrive/Escritorio/Laura/LITERA- Udesa/Tobas R01/Analisis data/Analisis hasta 2025/Paper/Datos_celular_full_2025.csv")

datos_celu <- datos_celu  %>%
  mutate(campaign = recode(temporada, 
                           # recodifico para que coincida con los datos
                           "Fall_23" = "Fall/Winter_2023",
                           "Fall_24" = "Fall/Winter_2024",
                           "Fall_25"= "Fall/Winter_2025",
                           "Spring_23" = "Spring/Summer_2023",
                           "Spring_24" = "Spring/Summer_2024"))
  

# voy a replicar los de Spring 2023 para que sirvan para la campaña Summer 24
datos_celu <- datos_celu  %>%rbind(datos_celu %>%
                                     filter(campaign == "Spring/Summer_2023") %>%
                                     mutate(campaign = "Summer_2024"))


# Junto con los datos de sueño usando la columna "campaign"
tobas_DLMO_celu <- tobas_DLMO %>%
  left_join(
    datos_celu %>% 
      clean_names() %>%
      select(uid, cellphone, campaign, me_prestan_cellphone,
             tv, compu_tablet, internet),
    by = c("uid", "campaign")
  ) %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))

## 1.4) Filtro de outliers  ####
### Primero filtro eventos nocturnos
# (esto es lo que hicimos en el paper longitudinal)
tobas_DLMO_celu <- tobas_DLMO_celu %>%
  filter(start_dt > dusk_dt & start_dt < dawn_dt)


### Luego filtro por duración ####
# utilizando un criterio de outliers basado en la desviación de la mediana
outliers <- outliers_mad(tobas_DLMO_celu$duration, threshold = 2.5)
outliers


# filtro
tobas_DLMO_celu<-  tobas_DLMO_celu %>%
  filter(duration > outliers[3] & duration < outliers[4])

### Finalmente, me quedo con sujetos con al menos 7 registros por campaña (casi todos)
tobas_DLMO_celu <- tobas_DLMO_celu %>%
  group_by(uid, campaign) %>%  
  filter(n_distinct(calendar_date) >= 7) %>%  
  ungroup() 


#DLMO

DLMO_final_pre <- read_csv("Interpolacion_melato_ALL.csv")

sum(is.na(DLMO_final_pre$DLMO))


DLMO_final_pre<- DLMO_final_pre%>%
  filter(!is.na(DLMO))

#CREO Columna

DLMO_final_pre <- DLMO_final_pre %>%
  mutate(season = sub(" .*", "", Condicion))

DLMO_final_pre <- DLMO_final_pre %>%
  mutate(moon = sub(".* ", "", Condicion))

DLMO_final_pre<- DLMO_final_pre%>%
  mutate(season = recode(season,
                         "Spring"=  "Spring/Summer",
                         "Fall" = "Fall/Winter" ))
                          
DLMO_final_pre<- DLMO_final_pre %>%
  mutate(campaign = paste(season, year, sep = "_"))

#uno tablas

DLMO_final <- DLMO_final_pre %>%
  left_join(tobas_DLMO_celu %>% 
              clean_names()%>%
              select(uid, sexo, age, calendar_date, comunidad_agrupada, campaign, ym, sleeponset, wakeup, year,wday, duration, midsleep, cellphone, photoperiod),
            by = c("uid", "campaign"))


# Recupero los horarios de puestas de sol
sunsets <- read_csv("C:/Users/laura/OneDrive/Escritorio/Laura/LITERA- Udesa/Tobas R01/Analisis data/Analisis hasta 2025/Paper/sunmoon.csv") %>%
  clean_names()%>%
  select(night_date, dusk_dt) %>%
  rename(calendar_date = night_date) %>%
  # calculate the time in hours from dusk_dt to midnight on that calendar_date
  mutate(sunset = as.numeric(difftime(dusk_dt, 
                                      calendar_date,
                                      units = "hours")))

DLMO_final$calendar_date<- as.Date(DLMO_final$calendar_date)

# las agrego
DLMO_final <- DLMO_final  %>%
  left_join(sunsets, by = "calendar_date")



# Promedios en creación de nueva base 
DLMO_final_prom <- DLMO_final %>%
  group_by(uid, moon) %>%
  summarise(
    comunidad_agrupada = first(comunidad_agrupada),
    campaign = first(campaign),
    sexo = first(sexo),
    cellphone = first(cellphone),
    sunset = first(sunset),
    DLMO=first(DLMO),
    age=first(age),
    photoperiod=first(photoperiod),
    
    promedio_sleeponset = mean(sleeponset, na.rm = TRUE),
    promedio_wakeup = mean(wakeup, na.rm = TRUE),
    promedio_midsleep = mean(midsleep, na.rm = TRUE),
    promedio_duracion = promedio_wakeup - promedio_sleeponset,
    .groups = "drop"
  )


# Calculo el tiempo de DLMO al sunset
DLMO_final_prom <- DLMO_final_prom  %>%
  mutate(dlmo_sunset = DLMO - sunset)

DLMO_final_prom <-DLMO_final_prom  %>%
  filter(!is.na(dlmo_sunset), dlmo_sunset> 0) 


DLMO_final_prom <- DLMO_final_prom%>%
  separate(
    col = campaign,              
    into = c("season", "year"),  
    sep = "_",                    
    remove = FALSE                
  )

#graficos_after_dusk


ggplot(DLMO_final_prom, aes(x = comunidad_agrupada, y = dlmo_sunset, fill = season)) +  
  geom_boxplot(alpha = 0.6, outlier.shape = NA, position = position_dodge(0.8)) +  
  geom_jitter(aes(color = season), position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), alpha = 0.7) + 
  labs(
    x = "comunidad_agrupada",
    y = "DLMO (h after dusk)") +
  theme_minimal()



promedios <- DLMO_final_prom %>%
  group_by(comunidad_agrupada, season) %>%
  summarise(promedio_melatonina = mean(dlmo_sunset, na.rm = TRUE))

#1) season y moon


ggplot(DLMO_final_prom, aes(x = moon, y = dlmo_sunset)) +
  geom_boxplot(aes(fill = moon), alpha = 0.6, outlier.shape = NA) +
  geom_line(
    aes(group = uid),
    color = "gray60",
    alpha = 0.7
  ) +
  geom_point(
    aes(color = moon),
    size = 2,
    alpha = 0.8
  ) +
  facet_grid(~season) +
  labs(
    x = "Moon Phase",
    y = "DLMO (min. after dusk)",
    fill = "Moon Phase",
    color = "Moon Phase"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )     


#filtro campaings no ritmicas

DLMO_final_prom_filter<-DLMO_final_prom %>% filter(campaign!="Spring/Summer_2024")

  ggplot(DLMO_final_prom_filter, aes(x = moon, y = dlmo_sunset)) +
  geom_boxplot(aes(fill = moon), alpha = 0.6, outlier.shape = NA) +
  geom_line(
    aes(group = uid),
    color = "gray60",
    alpha = 0.7
  ) +
  geom_point(
    aes(color = moon),
    size = 2,
    alpha = 0.8
  ) +
  facet_grid(~season) +
  labs(
    x = "Moon Phase",
    y = "DLMO (min. after dusk)",
    fill = "Moon Phase",
    color = "Moon Phase"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )                          


#promedios

promedios_moon <- DLMO_final_prom %>%
  group_by(moon, campaign) %>%
  summarise(promedio_melatonina = mean(dlmo_sunset, na.rm = TRUE))



#Filtro solo los ritmicos
pares_validos <- tibble(
  uid = c(4,22,25,29,29,130,138,139,141,141,171,174),
  campaign = c(  "Spring/Summer_2024", "Fall/Winter_2023", "Fall/Winter_2023", "Fall/Winter_2023", "Spring/Summer_2023", 
               "Fall/Winter_2023", "Spring/Summer_2023", "Fall/Winter_2024", "Spring/Summer_2023", "Fall/Winter_2024", 
               "Fall/Winter_2023", "Fall/Winter_2024")
)


data_filtrada <- DLMO_final_prom %>%
  semi_join(pares_validos, by = c("uid", "campaign"))


#grafico
ggplot(data_filtrada, aes(x = moon, y = dlmo_sunset)) +
  geom_boxplot(aes(fill = moon), alpha = 0.6, outlier.shape = NA) +
  geom_line(
    aes(group = uid),
    color = "gray60",
    alpha = 0.7
  ) +
  geom_point(
    aes(color = moon),
    size = 2,
    alpha = 0.8
  ) +
  labs(
    x = "Moon Phase",
    y = "DLMO (min. after dusk)",
    fill = "Moon Phase",
    color = "Moon Phase"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )                 


#modelo


DLMO <- lmer(dlmo_sunset~ sexo + poly(age,3) + moon + cellphone + photoperiod + campaign   + comunidad_agrupada+
               (1|uid),
             data = DLMO_final_prom)

parameters::model_parameters(DLMO)



DLMO_2 <- lmer(dlmo_sunset~ sexo + poly (age,3) + year+
                 season * comunidad_agrupada +
                 (1|uid),
               data = DLMO_final_prom)

parameters::model_parameters(DLMO_2)


DLMO_ritmicos <- lmer(dlmo_sunset~ sexo + poly (age,3) + photoperiod + year + moon + comunidad_agrupada + 
                 (1|uid),
               data = data_filtrada)

parameters::model_parameters(DLMO_ritmicos)

DLMO_ritmicos_pobl<- lmer(dlmo_sunset~ sexo + poly (age,3) +  photoperiod + year + moon+ cellphone + comunidad_agrupada + 
                            (1|uid),
                          data = DLMO_final_prom_filter)

parameters::model_parameters(DLMO_ritmicos_pobl)



#agrego info moon
# 1. Leer y renombrar
data_moon <- read.csv("sunmoon.csv") %>%
  rename(calendar_date = NightDate)

# 2. Unir con tus datos y filtrar NAs
datatobas_withmoon <- tobas_DLMO_celu %>% 
  left_join(data_moon, by = "calendar_date") %>%
  filter(!is.na(DayinCycle))

# 3. Resumir por grupo
datatobas_withmoon_prom <- datatobas_withmoon %>%
  group_by(DayinCycle, cellphone, comunidad_agrupada, campaign) %>%
  summarise(
    promedio_sleeponset = mean(sleeponset, na.rm = TRUE),
    sem_sleeponset = sd(sleeponset, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# 4. Ajustar cosenoidal por cellphone
ajustar_coseno <- function(df) {
  tryCatch({
    nls(promedio_sleeponset ~ A * cos(B * DayinCycle + C) + D,
        start = list(
          A = (max(df$promedio_sleeponset) - min(df$promedio_sleeponset)) / 2,
          B = 2 * pi / 29.5,        # ciclo lunar fijo
          C = 0,
          D = mean(df$promedio_sleeponset)
        ),
        data = df,
        control = list(maxiter = 500)
    )
  }, error = function(e) NULL)
}

# Crear modelos
df_yes <- filter(datatobas_withmoon_prom, cellphone == "YES")
df_no  <- filter(datatobas_withmoon_prom, cellphone == "NO")

modelo_yes <- ajustar_coseno(df_yes)
modelo_no  <- ajustar_coseno(df_no)

# 5. Generar predicciones
x_pred <- seq(min(datatobas_withmoon_prom$DayinCycle),
              max(datatobas_withmoon_prom$DayinCycle), length.out = 100)

pred_df <- bind_rows(
  data.frame(DayinCycle = x_pred,
             promedio_sleeponset = predict(modelo_yes, newdata = data.frame(DayinCycle = x_pred)),
             cellphone = "YES"),
  data.frame(DayinCycle = x_pred,
             promedio_sleeponset = predict(modelo_no, newdata = data.frame(DayinCycle = x_pred)),
             cellphone = "NO")
)

# 6. Gráfico
ggplot(datatobas_withmoon_prom, aes(x = DayinCycle, y = promedio_sleeponset, color = cellphone)) +
  geom_errorbar(aes(ymin = promedio_sleeponset - sem_sleeponset, 
                    ymax = promedio_sleeponset + sem_sleeponset),
                width = 0.2, alpha = 0.5) + 
  geom_point(alpha = 0.6) +  
  geom_vline(xintercept = 15, linetype = "dashed", color = "red", linewidth = 0.4) +  
  labs(title = "Sleep Onset vs Ciclo Lunar",
       x = "Día del Ciclo lunar", y = "Promedio Sleep Onset") +
  facet_wrap(~ campaign + comunidad_agrupada) +  
  scale_color_manual(values = c("NO" = "orange", "YES" = "blue")) +  
  theme_minimal()


#cosinor
library(assertthat)
library(cosinor)
library(glmmTMB)
library(GLMMcosinor)
require(reshape2)
require(limma)


#cosinor por sujeto

resultados_por_sujeto <- datatobas_withmoon %>%
  group_by(uid, campaign) %>%
  nest() %>%
  mutate(
    modelo = map(data, ~cosinor.lm(
      sleeponset ~ time(DayinCycle)+ age,
      period = 29.53,
      data = .x
    )),
    resumen = map(modelo, summary)
  )

#ver de a uno 
uid_campaign<-datatobas_withmoon %>%
  select(uid, campaign) %>% 
  distinct() %>% 
  arrange(campaign, uid)

resultados_por_sujeto %>%
  filter(uid == 4, campaign == "Spring/Summer_2024") %>%
  pull(resumen) %>%
  .[[1]]

#Fases para dias de muestreo_individual

uid_4 <- cosinor.lm(
  sleeponset ~ time(DayinCycle),
  period = 29.53,
  data = datatobas_withmoon %>%
    filter(campaign == "Spring/Summer_2024", uid == 4)
)

predict(uid_4)


#uno summer_2024 y spring_2023

datatobas_withmoon_unido <- datatobas_withmoon %>%
  mutate(campaign = ifelse(campaign == "Summer_2024", "Spring/Summer_2023", campaign))



#cosinor poblacional general 
#period=29.53

fit_1_SC <- cglmm(
  sleeponset ~ sexo + age + comunidad_agrupada +
    amp_acro(DayinCycle, period = 29.53) +
    (1 | uid),
  data = datatobas_withmoon_unido %>%
    filter(campaign == "Spring/Summer_2023"))

summary(fit_1_SC)

fit_2_SC <- cglmm(
  sleeponset ~ sexo + age + comunidad_agrupada +
    amp_acro(DayinCycle, period = 29.53) +
    (1 | uid),
  data = datatobas_withmoon_unido %>%
    filter(campaign == "Spring/Summer_2024"))

summary(fit_2_SC)


fit_3_SC <- cglmm(
  sleeponset ~ sexo + age + comunidad_agrupada +
    amp_acro(DayinCycle, period = 29.53) +
    (1 | uid),
  data = datatobas_withmoon_unido %>%
    filter(campaign == "Fall/Winter_2023"))

summary(fit_3_SC)


fit_4_SC <- cglmm(
  sleeponset ~ sexo + age + comunidad_agrupada +
    amp_acro(DayinCycle, period = 29.53) +
    (1 | uid),
  data = datatobas_withmoon_unido %>%
    filter(campaign == "Fall/Winter_2024"))

summary(fit_4_SC)


fit_5_SC <- cglmm(
  sleeponset ~ sexo + age + comunidad_agrupada +
    amp_acro(DayinCycle, period = 29.53) +
    (1 | uid),
  data = datatobas_withmoon_unido %>%
    filter(campaign == "Fall/Winter_2025"))

summary(fit_5_SC)

p1 <- autoplot(fit_3_SC) + ggtitle("Fall/Winter_2023") + scale_y_continuous(limits=c(22.5,24))
p2 <- autoplot(fit_1_SC) + ggtitle("Spring/Summer_2023")+ scale_y_continuous(limits=c(22.5,24))
p3 <- autoplot(fit_4_SC) + ggtitle("Fall/Winter_2024")+ scale_y_continuous(limits=c(22.5,24))
p4 <- autoplot(fit_5_SC) + ggtitle("Fall/Winter_2025")+ scale_y_continuous(limits=c(22.5,24))

(p1 | p2) / (p3 | p4)




library(ggforce)
library(tidyr)
library(broom)

p5<-polar_plot(fit_3_SC) + labs(title="Fall/Winter_2023")
p6<-polar_plot(fit_1_SC) + labs (title="Spring/Summer_2023")
p7<-polar_plot(fit_4_SC) + labs (title="Fall/Winter_2024")
p8<-polar_plot(fit_5_SC)+ labs(title="Fall/Winter_2025")

(p5 | p6) / (p7 | p8)

#fases para full y new_poblacional

new_point <- data.frame(
  DayinCycle = 10,          
  sexo = "Female",
  age = 22,
  comunidad_agrupada = "Rural"
)

y_pred <- predict(fit_3_SC, newdata = new_point, re.form = NA)
y_pred

#period=14.76

fit_1_SC_2 <- cglmm(
  sleeponset ~ sexo + age + comunidad_agrupada +
    amp_acro(DayinCycle,period = c(1, 0.5) * 29.5, n_components = 2) +
    (1 | uid),
  data = datatobas_withmoon_unido %>%
    filter(campaign == "Spring/Summer_2023"))

summary(fit_1_SC_2)


fit_2_SC_2 <- cglmm(
  sleeponset ~ sexo + age + comunidad_agrupada +
    amp_acro(DayinCycle, period = c(1, 0.5) * 29.5, n_components = 2) +
    (1 | uid),
  data = datatobas_withmoon_unido %>%
    filter(campaign == "Spring/Summer_2024"))

summary(fit_2_SC_2)


fit_3_SC_2 <- cglmm(
  sleeponset ~ sexo + age + comunidad_agrupada +
    amp_acro(DayinCycle, period = c(1, 0.5) * 29.5, n_components = 2) +
    (1 | uid),
  data = datatobas_withmoon_unido %>%
    filter(campaign == "Fall/Winter_2023"))

summary(fit_3_SC_2)


fit_4_SC_2 <- cglmm(
  sleeponset ~ sexo + age + comunidad_agrupada +
    amp_acro(DayinCycle, period = c(1, 0.5) * 29.5, n_components = 2) +
    (1 | uid),
  data = datatobas_withmoon_unido %>%
    filter(campaign == "Fall/Winter_2024"))

summary(fit_4_SC_2)


fit_5_SC_2 <- cglmm(
  sleeponset ~ sexo + age + comunidad_agrupada +
    amp_acro(DayinCycle, period = c(1, 0.5) * 29.5, n_components = 2) +
    (1 | uid),
  data = datatobas_withmoon_unido %>%
    filter(campaign == "Fall/Winter_2025"))

summary(fit_5_SC_2)

p2_2c <- autoplot(fit_1_SC_2) + ggtitle("Spring/Summer_2023")+ scale_y_continuous(limits=c(22.5,24))
p3_2c <- autoplot(fit_4_SC_2) + ggtitle("Fall/Winter_2024")+ scale_y_continuous(limits=c(22.5,24))
p4_2c <- autoplot(fit_5_SC_2) + ggtitle("Fall/Winter_2025")+ scale_y_continuous(limits=c(22.5,24))

 (p2_2c) / (p3_2c | p4_2c)

#cellphone

fit_1 <- cglmm(
  sleeponset ~ sexo + age + comunidad_agrupada +
    amp_acro(DayinCycle,group="cellphone", period = 29.53) +
    (1 | uid),
  data = datatobas_withmoon_unido %>%
    filter(campaign == "Spring/Summer_2023"))

summary(fit_1)


fit_2 <- cglmm(
  sleeponset ~ sexo + age + comunidad_agrupada +
    amp_acro(DayinCycle,group="cellphone", period = 29.53) +
    (1 | uid),
  data = datatobas_withmoon_unido %>%
    filter(campaign == "Spring/Summer_2024"))

summary(fit_2)


fit_3 <- cglmm(
  sleeponset ~ sexo + age + comunidad_agrupada +
    amp_acro(DayinCycle,group="cellphone", period = 29.53) +
    (1 | uid),
  data = datatobas_withmoon_unido %>%
    filter(campaign == "Fall/Winter_2024"))

summary(fit_3)


fit_5 <- cglmm(
  sleeponset ~ sexo + age + comunidad_agrupada +
    amp_acro(DayinCycle,group="cellphone", period = 29.53) +
    (1 | uid),
  data = datatobas_withmoon_unido %>%
    filter(campaign == "Fall/Winter_2025"))

summary(fit_5)


p1_cel <- autoplot(fit_1) + ggtitle("Spring/Summer_2023")+ scale_y_continuous(limits=c(22.5,24))
p3_cel <- autoplot(fit_3) +ggtitle("Fall/Winter_2024") + scale_y_continuous(limits=c(22.5,24))
p5_cel <- autoplot(fit_5) +ggtitle("Fall/Winter_2025") + scale_y_continuous(limits=c(22.5,24))

(p1_cel | p3_cel) / (p5_cel)

