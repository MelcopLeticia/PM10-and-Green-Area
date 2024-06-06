#Árvore CART

#library
library("tidyverse") 
library("tidylog") 
library("lubridate")
library("partykit")
library("ggplot2")


#lendo a base

base_final <- read.csv("Dados_final.csv", encoding = "UTF-8") %>%
  select(-X)

# olhar as estatísticas
estatisticas_estacao <- base_final %>%
  na.omit() %>%
  pivot_longer(cols=c('MP10','umidade','solar','pressao','temperatura','vento','area'),
               names_to='variaveis',
               values_to='valores') %>%
  group_by(variaveis,estacao) %>%
  summarise_at(vars('valores'),
               list('sd' = sd, Q1=~quantile(.,probs = 0.25),
                    Q2 = median, Q3=~quantile(.,probs = 0.75))
  ) %>%
  mutate_if(is.numeric, round,1) %>%
  mutate("Median" = paste0(Q2," (",Q1,", ",Q3,")")) %>%
  pivot_wider(id_cols = 'estacao',
              names_from = 'variaveis',
              values_from = 'Median')

# write_xlsx(estatisticas_estacao,"estatisticas v2.xlsx")

# checar a correlação das variáveis
correlacao <- cor(base_final %>%
                    na.omit() %>%
                    select(-estacao,-data), method = "spearman")

correlacao <- as.data.frame(correlacao) %>%
  mutate_if(is.numeric, round,5)

# write_xlsx(correlacao,"correlacao spear.xlsx")

dados <- base_final %>%
  select('MP10', everything()) %>%
  separate(data, c('ano','mes')) %>%
  select(-ano) %>%
  mutate(mes = factor(mes),
         estacao = factor(estacao)) %>%
  drop_na('MP10') %>%
  rename('Month' = mes, 'Station' = estacao, 'Humidity' = umidade, 'Pressure' = pressao,
         'Temperature' = temperatura, 'Wind Speed' = vento, 'Solar Rad.' = solar, 'Green Area' = area)


# MODELO
set.seed(2024)

modelo1 <- ctree(MP10 ~., data = dados %>% select(-Month,-Station), control =  ctree_control(testtype = "Bonferroni"))

# png(file=paste0("Imagens/",estacoes[[i]],"_com.png"),
#     width=600, height=350)

plot(modelo1,  gp = gpar(fontsize = 8))
# dev.off()

set.seed(2024)
modelo2 <- ctree(MP10 ~., data = dados, control =  ctree_control(testtype = "Bonferroni"))

# png(file=paste0("Imagens/",estacoes[[i]],"_com.png"),
#     width=600, height=350)

plot(modelo2,  gp = gpar(fontsize = 8))
# dev.off()


estacoes <- list("AV","BG","CA","PG","CG","IR","SC","SP")

for (i in 1:8){
  
base <- dados %>%
  filter(Station == estacoes[[i]])

# Criando o modelo
set.seed(2024)

modelo <- ctree(MP10 ~., data = base, control =  ctree_control(testtype = "Bonferroni"))
#
# png(file=paste0("Imagens/",estacoes[[i]],"_com.png"),
#     width=600, height=350)
# 
# plot(modelo,  gp = gpar(fontsize = 8))
# dev.off()
# 
}


     