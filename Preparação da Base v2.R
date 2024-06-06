#Preparação de Dados

#library
library("tidyverse") 
library("lubridate")
library("writexl") 
library("ggplot2")
library("patchwork")
library("openxlsx")
library("readxl")

#lendo as bases

dados <- read.csv("Qualidade_do_ar_-_Dados_horarios.csv",
                  encoding = "UTF-8")

areaverde <- read_excel("Dados area verde v3.xlsx", col_types = c("text","date","numeric")) %>%
  mutate(data = paste0(year(data),"-",month(data)))

dados[c('data','hora')] <- stringr::str_split_fixed(dados$data, ' ', 2)

### Tratamento da Base
base <- dados %>%
  mutate(data = as.Date(data),
         ano_mes = paste0(year(data),"-",month(data))) %>%
  select(ano_mes, "estacao" = estação, "umidade" = ur, "pressao" = pres,
         "temperatura" = temp, "vento" = vel_vento, "solar" = rs,"MP10" = pm10) %>%
  filter(temperatura != 0,
         umidade > 1) 

(plot_box_MP10 <- ggplot(base,aes(y = MP10)) +
    geom_boxplot(outlier.color = "red",
                 outlier.fill = NULL,
                 outlier.size = 0.5,
                 outlier.alpha = 0.5) +
    labs(y = "PM10 (µg/m3)"))

(plot_box_umidade <- ggplot(base,aes(y = umidade)) +
    geom_boxplot(outlier.color = "red",
                 outlier.fill = NULL,
                 outlier.size = 0.5,
                 outlier.alpha = 0.5) +
    labs(y = "Relative Humidity (%)"))

(plot_box_pressao <- ggplot(base,aes(y = pressao)) +
    geom_boxplot(outlier.color = "red",
                 outlier.fill = NULL,
                 outlier.size = 0.5,
                 outlier.alpha = 0.5) +
    labs(y = "Atmospheric Pressure (mbar)"))

(plot_box_temp <- ggplot(base,aes(y = temperatura)) +
    geom_boxplot(outlier.color = "red",
                 outlier.fill = NULL,
                 outlier.size = 0.5,
                 outlier.alpha = 0.5) +
    labs(y = "Temperature (°C)"))

(plot_box_vento <- ggplot(base,aes(y = vento)) +
    geom_boxplot(outlier.color = "red",
                 outlier.fill = NULL,
                 outlier.size = 0.5,
                 outlier.alpha = 0.5) +
    labs(y = "Wind Speed (km/h)"))


(plot_box_solar <- ggplot(base,aes(y = solar)) +
    geom_boxplot(outlier.color = "red",
                 outlier.fill = NULL,
                 outlier.size = 0.5,
                 outlier.alpha = 0.5) +
    labs(y = "Solar Radiation (w/m2)"))

(plot_box <- (plot_box_MP10|plot_box_vento)/
    (plot_box_solar|plot_box_umidade)/
    (plot_box_pressao|plot_box_temp))

# Retirar Outliers

base2 <- base %>%
  group_by(estacao) %>%
  filter(!MP10 %in% boxplot.stats(MP10)$out,
         !vento %in% boxplot.stats(vento)$out,
         !umidade %in% boxplot.stats(umidade)$out,
         !pressao %in% boxplot.stats(pressao)$out,
         !temperatura %in% boxplot.stats(temperatura)$out,
         !solar %in% boxplot.stats(solar)$out)


# Agrupando por mes (para tentar melhor estimar depois os dados faltantes)
agregado <- aggregate(base2[, 3:8], list(base2$ano_mes,base2$estacao), mean, na.rm = T) %>%
  rename(data = Group.1,
         estacao = Group.2)
  
agregado[sapply(agregado, is.nan)] <- NA

  
#Criando a base final combinando a tabela tratada com a de area verde
base_final <- agregado %>%
  left_join(areaverde, by = c('estacao','data')) %>%
  select('MP10', everything())

# Salvando
# write.csv(base_final, "Dados_final.csv")

