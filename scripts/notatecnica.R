library(tidyverse)
library(stargazer)
library(readxl)
library(stringr)
library(janitor)
library(kableExtra)
library(tikzDevice)
library(tinytex)

setwd("C:/Users/miguel/Desktop/reformaseleitorais/reformaseleitorais_R/output")

banco0312 <- read_excel("C:/Users/miguel/Desktop/reformaseleitorais/reformaseleitorais_R/data/banco0312.xlsx", sheet = 3)

#Primeira tabela descritiva, por diplomas-base
tabela_diplomasbase <- banco0312 %>%
  mutate(numero.diploma.alterado = as.numeric(numero.diploma.alterado))%>%
  mutate(numero.diploma = as.numeric(numero.diploma))%>%
  mutate(numero_para_tabela = case_when(altera.diploma ==  "nao" ~ numero.diploma,
                                        altera.diploma == "sim" ~ numero.diploma.alterado))%>%
  mutate(Diploma = case_when(numero_para_tabela == 4737 ~ "L4737 (Código Eleitoral)",
                             numero_para_tabela == 9096 ~ "L9096 (Lei dos Partidos)",
                             numero_para_tabela == 9504 ~ "L9504 (Lei das Eleições)",
                             !(numero_para_tabela %in% c(4737, 9096, 9504)) ~ "Outros"))%>%
  group_by(Diploma)%>%
  summarise(Originais = sum(altera.diploma=="nao"), Alterações = sum(altera.diploma=="sim"))%>%
  mutate(Total = Originais + Alterações)%>%
  adorn_totals("row")

kbl(tabela_diplomasbase, "latex",label = "tabela1", caption = "Entradas no banco por diploma-base", booktabs = TRUE, centering = TRUE)


#Segunda tabela descritiva, por tipo de alteração
tabela_tiposdealt <- banco0312 %>%
  mutate(numero.diploma.alterado = as.numeric(numero.diploma.alterado))%>%
  filter(altera.diploma=="sim")%>%
  mutate(numero_para_tabela = numero.diploma.alterado)%>%
  mutate(Diploma = case_when(numero_para_tabela == 4737 ~ "L4737 (Código Eleitoral)",
                             numero_para_tabela == 9096 ~ "L9096 (Lei dos Partidos)",
                             numero_para_tabela == 9504 ~ "L9504 (Lei das Eleições)",
                             !(numero_para_tabela %in% c(4737, 9096, 9504)) ~ "Outros"))%>%
  group_by(Diploma)%>%
  summarise(Inclusões = sum(tipo.alteracao=="inclusao"), `Novas redações` = sum(tipo.alteracao=="nova redacao"), Revogações = sum(tipo.alteracao == "revogacao"), Renumerações = sum(tipo.alteracao == "renumeracao"))%>%
  mutate(Total = Inclusões + `Novas redações` + Revogações + Renumerações)%>%
  adorn_totals("row")

kbl(tabela_tiposdealt, "latex", label = "tabela2", caption = "Alterações por tipo", booktabs = TRUE, centering = TRUE)

#Gráfico 1
basegrafico1 <- banco0312%>%
  mutate(datas = as.Date(data.diploma, format = "%Y-%m-%d"))%>%
  group_by(datas)%>%
  filter(datas != "1965-07-15")%>%
  summarise(contagem = n())

grafico1 <- ggplot(basegrafico1, mapping = aes(x = datas,
                                   y = contagem))+
  geom_path(color = "darkgrey", linewidth=1)+
  geom_point(color = "grey2")+
  labs(x = "Data",
       y = "Número de novos dispositivos")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  guides(x = guide_axis(angle = 90))+
  theme_minimal()+
  theme(text = element_text(size = 17))
ggsave("grafico1.pdf", plot = grafico1, width = 12, height = 8)  
  

#Gráfico 2
basegrafico2 <- banco0312 %>%
  mutate(datas = as.Date(data.diploma, format = "%Y-%m-%d"))%>%
  rename(tipoalt = "tipo.alteracao")%>%
  mutate(tipoalt = case_when(tipoalt == "inclusao" ~ "Inclusão",
                             tipoalt == "revogacao" ~ "Revogação",
                             tipoalt == "renumeracao" ~ "Renumeração",
                             tipoalt == "nova redacao" ~ "Nova redação"))%>%
  mutate(ano = as.numeric(substr(as.character(datas),1,4)))%>%
  group_by(ano, tipoalt)%>%
  summarise(contagem = n())%>%
  filter(ano > 1965)


grafico2 <- ggplot(basegrafico2, mapping = aes(x = ano,
                                   y = contagem,
                                   fill = tipoalt))+
  geom_bar(position = 'stack', stat = 'identity')+
  labs(x = "Ano", 
       y = "Número de novos dispositivos", 
       fill = "Tipo de dispositivo")+
  scale_fill_brewer(palette = "BrBG")+
  scale_x_continuous(breaks = c(1988:2019))+
  guides(x = guide_axis(angle = 90))+
  theme_minimal()+
  theme(text = element_text(size = 17))
ggsave("grafico2.pdf", plot = grafico2, width = 12, height = 8)


#Gráfico 3
basegrafico3 <- banco0312%>%
  filter(altera.diploma=="sim")%>%
  mutate(inciso = case_when(substr(indexacao6, 1, 3)=="inc" & indexacao7=="NA" | 
           substr(indexacao7, 1, 3)=="inc" & indexacao8=="NA" |
           substr(indexacao7, 1, 4)=="alin" & indexacao8=="NA" |
           substr(indexacao8, 1, 5)=="alin" ~ 1,
           !(substr(indexacao6, 1, 3)=="inc" & indexacao7=="NA" | 
               substr(indexacao7, 1, 3)=="inc" & indexacao8=="NA" |
               substr(indexacao7, 1, 4)=="alin" & indexacao8=="NA" |
               substr(indexacao8, 1, 5)=="alin")~ 0))%>%
  mutate(datas = as.Date(data.diploma, format = "%Y-%m-%d"),
         ano = as.numeric(substr(as.character(datas),1,4)))%>%
  group_by(ano, inciso)%>%
  summarise(contagem = n())%>%
  mutate(index = case_when(inciso == 1 ~ "Incisos ou alíneas",
                           inciso == 0 ~ "Artigos ou parágrafos"))

grafico3 <- ggplot(basegrafico3, mapping = aes(x = ano,
                                 y = contagem,
                                 fill = index))+
  geom_bar(position = "fill", stat = "identity")+
  scale_fill_brewer(palette = "Blues")+
  labs(x = "Ano",
       y = "Percentual de alterações",
       fill = "Indexação das alterações")+
  scale_x_continuous(breaks = c(1988:2019))+
  guides(x = guide_axis(angle = 90))+
  theme_minimal()+
  theme(text = element_text(size = 17))
ggsave("grafico3.pdf", plot = grafico3, width = 12, height = 8)


#Gráfico 4
basegrafico4 <- banco0312%>%
  filter(altera.diploma=="sim")%>%
  mutate(inciso = case_when(substr(indexacao6, 1, 3)=="inc" & indexacao7=="NA" | 
                              substr(indexacao7, 1, 3)=="inc" & indexacao8=="NA" |
                              substr(indexacao7, 1, 4)=="alin" & indexacao8=="NA" |
                              substr(indexacao8, 1, 5)=="alin" ~ 1,
                            !(substr(indexacao6, 1, 3)=="inc" & indexacao7=="NA" | 
                                substr(indexacao7, 1, 3)=="inc" & indexacao8=="NA" |
                                substr(indexacao7, 1, 4)=="alin" & indexacao8=="NA" |
                                substr(indexacao8, 1, 5)=="alin")~ 0))%>%
  mutate(datas = as.Date(data.diploma, format = "%Y-%m-%d"),
         ano = as.numeric(substr(as.character(datas),1,4)))%>%
  group_by(ano, inciso, tipo.alteracao)%>%
  summarise(contagem = n())%>%
  mutate(index = case_when(inciso == 1 ~ "Incisos ou alíneas",
                           inciso == 0 ~ "Artigos ou parágrafos"))%>%
  rename(tipoalt = "tipo.alteracao")%>%
  mutate(tipoalt = case_when(tipoalt == "inclusao" ~ "Inclusão",
                             tipoalt == "revogacao" ~ "Revogação",
                             tipoalt == "renumeracao" ~ "Renumeração",
                             tipoalt == "nova redacao" ~ "Nova redação"))

grafico4 <- ggplot(basegrafico4, mapping = aes(x = ano,
                                   y = contagem,
                                   fill = tipoalt))+
  geom_bar(position = "stack", stat = "identity")+
  scale_fill_brewer(palette = "BrBG")+
  labs(x = "Ano",
       y = "Número de alterações",
       fill = "Tipo de alteração")+
  facet_wrap(~index)+
  guides(x = guide_axis(angle = 90))+
  theme_minimal()+
  theme_bw()+
  theme(text = element_text(size = 17))
ggsave("grafico4.pdf", plot = grafico4, width = 12, height = 8)


#Gráfico 5
basegrafico5 <- banco0312%>%
  filter(as.numeric(numero.diploma.alterado) == 9504)%>%
  mutate(datas = as.Date(data.diploma),
         ano = as.numeric(substr(as.character(datas), 1, 4)))%>%
  group_by(ano, livro)%>%
  summarise(contagem = n())%>%
  mutate(livro = str_wrap(livro, width = 20))

grafico5 <- ggplot(basegrafico5, mapping = aes(x = ano,
                                   y = contagem))+
  geom_point()+
  geom_line(color = "darkgrey", linetype = "dashed")+
  facet_wrap(~livro)+
  labs(x = "Ano",
       y = "Número de alterações")+
  guides(x = guide_axis(angle = 90))+
  theme_minimal()+
  theme_bw()
ggsave("grafico5.pdf", plot = grafico5, width = 8, height = 12)

#Grafico 6
basegrafico6 <- banco0312%>%
  filter(as.numeric(numero.diploma.alterado) == 9096)%>%
  mutate(datas = as.Date(data.diploma),
         ano = as.numeric(substr(as.character(datas), 1, 4)))%>%
  group_by(ano, título)%>%
  rename(titulo = "título")%>%
  summarise(contagem = n())%>%
  mutate(titulo = str_wrap(titulo, width = 20))

grafico6 <- ggplot(basegrafico6, mapping = aes(x = ano,
                                   y = contagem))+
  geom_point()+
  geom_line(color = "darkgrey", linetype = "dashed")+
  facet_wrap(~titulo)+
  labs(x = "Ano",
       y = "Número de alterações")+
  guides(x = guide_axis(angle = 90))+
  theme_minimal()+
  theme_bw()+
  theme(text = element_text(size = 17))
ggsave("grafico6.pdf", plot = grafico6, width = 12, height = 8)

#Grafico 7
basegrafico7 <- banco0312%>%
  filter(as.numeric(numero.diploma.alterado) == 9096)%>%
  mutate(datas = as.Date(data.diploma),
         ano = as.numeric(substr(as.character(datas), 1, 4)))%>%
  group_by(ano, capítulo)%>%
  rename(capitulo = "capítulo")%>%
  summarise(contagem = n())%>%
  mutate(capitulo = str_wrap(capitulo, width = 20))

grafico7 <- ggplot(basegrafico7, mapping = aes(x = ano,
                                               y = contagem))+
  geom_point()+
  geom_line(color = "darkgrey", linetype = "dashed")+
  facet_wrap(~capitulo)+
  ylim(0, 25)+
  labs(x = "Ano",
       y = "Número de alterações")+
  guides(x = guide_axis(angle = 90))+
  theme_minimal()+
  theme_bw()+
  theme(text = element_text(size = 17))
ggsave("grafico7.pdf", plot = grafico7, width = 12, height = 8)
