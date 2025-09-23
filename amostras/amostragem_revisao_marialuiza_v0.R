library(tidyverse)
library(readxl)
library(writexl)

#Primeiro, importo a base de dados para o meu ambiente
setwd("C:/Users/miguel/Desktop/amostra_refs_eleitorais")
banco <- read_excel("C:/Users/miguel/Desktop/amostra_refs_eleitorais/Banco_finalização_1105.xlsx", sheet = 2, )
view(banco)
rev_malu0 <- read_excel("C:/Users/miguel/Desktop/amostra_refs_eleitorais/Banco_finalização_1105.xlsx", sheet = 3, )

#Depois, coloco uma seed para termos sempre o mesmo resultado nas amostragens aleatórias
set.seed(3)

#Faço um dataframe para cada categoria da amostragem: os dispositivos de cada diploma base e aqueles de cada lei que os alterou
bancoalt9504 <- banco %>%
  filter(numero.diploma.alterado == "9504.0")%>%
  slice_sample(prop = 0.15)
  
bancoalt9096 <- banco %>%
  filter(numero.diploma.alterado == "9096.0")%>%
  slice_sample(prop = 0.15)

bancoalt4737 <- banco %>%
  filter(numero.diploma.alterado == "4737.0")%>%
  slice_sample(prop = 0.15)

banco9504 <- banco %>%
  filter(numero.diploma == "9504")%>%
  slice_sample(prop = 0.15)

banco9096 <- banco %>%
  filter(numero.diploma == "9096")%>%
  slice_sample(prop = 0.15)

banco4737 <- banco %>%
  filter(numero.diploma == "4737")%>%
  slice_sample(prop = 0.15)

#Então, junto os data frames todos em duas versões da amostra, uma com todas as variáveis e outra apenas com algumas.
amostra <- rbind(banco9504, banco9096, banco4737, bancoalt9504, bancoalt9096, bancoalt4737)

amostra_poucasinfos <- amostra %>%
  select(indice_provisorio, numero.diploma, numero.diploma.alterado, indexacao5, indexacao6, indexacao7, indexacao8)

#Faço uma amostra da amostra, para a Maria Luiza testar primeiro.
amostra_da_amostra <- slice_sample(amostra, prop = 0.15)

amostra_da_amostra_poucasinfos <- slice_sample(amostra_poucasinfos, prop = 0.15)

#Agora vamos extrair a subamostra da amostra principal, e selecionar as entradas referentes à Lei das Eleições
amostra9504 <- rbind(banco9504, bancoalt9504)%>%
  filter(!indice_provisorio %in% rev_malu0$indice_provisorio)%>%
  select(indice_provisorio, numero.diploma, numero.diploma.alterado, indexacao5, indexacao6, indexacao7, indexacao8)

#Então, fazemos o mesmo para a Lei dos Partidos
amostra9096 <- rbind(banco9096, bancoalt9096)%>%
  filter(!indice_provisorio %in% rev_malu0$indice_provisorio)%>%
  select(indice_provisorio, numero.diploma, numero.diploma.alterado, indexacao5, indexacao6, indexacao7, indexacao8)

#E agora para o Código Eleitoral
amostra4737 <- rbind(banco4737, bancoalt4737)%>%
  filter(!indice_provisorio %in% rev_malu0$indice_provisorio)%>%
  select(indice_provisorio, numero.diploma, numero.diploma.alterado, indexacao5, indexacao6, indexacao7, indexacao8)


#Salvo as amostras e "amostras da amostra"
write_xlsx(amostra, "C:/Users/miguel/Desktop/amostra_refs_eleitorais/amostra.xlsx")
write_xlsx(amostra4737, "C:/Users/miguel/Desktop/amostra_refs_eleitorais/amostra4737.xlsx")
write_xlsx(amostra_poucasinfos, "C:/Users/miguel/Desktop/amostra_refs_eleitorais/amostra_poucasinfos.xlsx")
write_xlsx(amostra9504, "C:/Users/miguel/Desktop/amostra_refs_eleitorais/amostra9504.xlsx")
write_xlsx(amostra9096, "C:/Users/miguel/Desktop/amostra_refs_eleitorais/amostra9096.xlsx")
write_xlsx(amostra_da_amostra, "C:/Users/miguel/Desktop/amostra_refs_eleitorais/amostra_da_amostra.xlsx")
write_xlsx(amostra_da_amostra_poucasinfos, "C:/Users/miguel/Desktop/amostra_refs_eleitorais/amostra_da_amostra_poucasinfos.xlsx")