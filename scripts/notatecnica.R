#Primeiro chamamos os pacotes necessários, definimos o diretório para os outputs e importamos a base de dados via read_excel()
library(tidyverse)
library(stargazer)
library(readxl)
library(stringr)
library(janitor)
library(kableExtra)
library(tikzDevice)
library(tinytex)
library(writexl)

setwd("C:/Users/miguel/Desktop/pesquisa/reformaseleitorais/reformaseleitorais_R/output")

banco_reformas <- read_excel("C:/Users/miguel/Desktop/pesquisa/reformaseleitorais/reformaseleitorais_R/data/banco_reformas.xlsx")


#---------------------------
#Tabela 2, por diplomas-base
#---------------------------
tabela_diplomasbase <- banco_reformas %>%
  mutate(numero.diploma.alterado = as.numeric(numero.diploma.alterado))%>% #Transformamos em numérico as variáveis numero.diploma.alterado e numero.diploma
  mutate(numero.diploma = as.numeric(numero.diploma))%>%
  mutate(numero_para_tabela = case_when(altera.diploma ==  "nao" ~ numero.diploma,
                                        altera.diploma == "sim" ~ numero.diploma.alterado))%>% #Distinguimos as alterações de disposivos originais/autônomos
  mutate(Diploma = case_when(numero_para_tabela == 4737 ~ "L4737 (Código Eleitoral)",
                             numero_para_tabela == 9096 ~ "L9096 (Lei dos Partidos)",
                             numero_para_tabela == 9504 ~ "L9504 (Lei das Eleições)",
                             !(numero_para_tabela %in% c(4737, 9096, 9504)) ~ "Outros"))%>% #Renomeamos os diplomas-base para legibilidade na tabela
  group_by(Diploma)%>%
  summarise(Originais = sum(altera.diploma=="nao"), Alterações = sum(altera.diploma=="sim"))%>% #Separamos alterações e originais e somamos ambos os grupos, para cada diploma-base
  mutate(Total = Originais + Alterações)%>%
  adorn_totals("row") #Adicionamos uma coluna de totais

kbl(tabela_diplomasbase, "latex",label = "tabela1", caption = "Entradas no banco por diploma-base", booktabs = TRUE, centering = TRUE) #E aqui o output para latex.
write_xlsx(tabela_diplomasbase, "C:/Users/miguel/Desktop/pesquisa/reformaseleitorais/reformaseleitorais_R/output/tabela_diplomasbase.xlsx") #Salvando a planilha em Excel


#-------------------------------
#Tabela 3, por tipo de alteração
#-------------------------------
tabela_tiposdealt <- banco_reformas %>%
  mutate(numero.diploma.alterado = as.numeric(numero.diploma.alterado))%>% #Transformamos em numérica a variável numero.diploma.alterado
  filter(altera.diploma=="sim")%>% #Filtramos para ter apenas alterações, e não dispositivos originais
  mutate(numero_para_tabela = numero.diploma.alterado)%>% 
  mutate(Diploma = case_when(numero_para_tabela == 4737 ~ "L4737 (Código Eleitoral)",
                             numero_para_tabela == 9096 ~ "L9096 (Lei dos Partidos)",
                             numero_para_tabela == 9504 ~ "L9504 (Lei das Eleições)",
                             !(numero_para_tabela %in% c(4737, 9096, 9504)) ~ "Outros"))%>% #Renomeamos os diplomas para legibilidade na tabela
  group_by(Diploma)%>%
  summarise(Inclusões = sum(tipo.alteracao=="inclusao"), 
            `Novas redações` = sum(tipo.alteracao=="nova redacao"), 
            Revogações = sum(tipo.alteracao == "revogacao"), 
            Renumerações = sum(tipo.alteracao == "renumeracao"))%>% #Somamos as alterações por tipo de modificação realizada
  mutate(Total = Inclusões + `Novas redações` + Revogações + Renumerações)%>%
  adorn_totals("row") #Adicionamos uma coluna de totais

kbl(tabela_tiposdealt, "latex", label = "tabela2", caption = "Alterações por tipo", booktabs = TRUE, centering = TRUE) #Exportando para latex
write_xlsx(tabela_tiposdealt, "C:/Users/miguel/Desktop/pesquisa/reformaseleitorais/reformaseleitorais_R/output/tabela_tiposdealt.xlsx") #Salvando a planilha em Excel


#---------
#Figura 1
#---------
basegrafico1 <- banco_reformas %>%
  mutate(datas = as.Date(data.diploma, format = "%Y-%m-%d"))%>% #Transformamos em data a variável data.diploma
  rename(tipoalt = "tipo.alteracao")%>%
  mutate(tipoalt = case_when(tipoalt == "inclusao" ~ "Inclusão",
                             tipoalt == "revogacao" ~ "Revogação",
                             tipoalt == "renumeracao" ~ "Renumeração",
                             tipoalt == "nova redacao" ~ "Nova redação"))%>% #Renomeamos os tipos de alteração para legibilidade no gráfico
  mutate(ano = as.numeric(substr(as.character(datas),1,4)))%>% #Extraímos o ano da data dos diplomas
  group_by(ano, tipoalt)%>%
  summarise(contagem = n())%>% #Criamos uma contagem de alterações por tipo, para cada ano
  filter(ano > 1965) #Excluímos o ano de sanção do Código Eleitoral

write_xlsx(basegrafico1, "C:/Users/miguel/Desktop/pesquisa/reformaseleitorais/reformaseleitorais_R/output/basegrafico1.xlsx") #Salvando a planilha em Excel

#Abaixo geramos o gráfico de barras correspondente, em que as cores (fill) são dadas pelo tipo de alteração
grafico1 <- ggplot(basegrafico1, mapping = aes(x = ano,
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

ggsave("grafico1.pdf", plot = grafico1, width = 12, height = 8) #salvando o gráfico em pdf
ggsave("grafico1.png", plot = grafico1, width = 12, height = 8) #E em png
ggsave("grafico1.svg", plot = grafico1, width = 12, height = 8) #E svg


#---------
#Figura 2
#---------
basegrafico2 <- banco_reformas%>%
  filter(altera.diploma=="sim")%>% #Filtramos para ter apenas as alterações
  mutate(inciso = case_when(substr(indexacao7, 1, 3)=="inc" & indexacao8=="NA" | 
                              substr(indexacao8, 1, 3)=="inc" & indexacao9=="NA" |
                              substr(indexacao8, 1, 4)=="alin" & indexacao9=="NA" |
                              substr(indexacao9, 1, 5)=="alin" ~ 1,
                            !(substr(indexacao7, 1, 3)=="inc" & indexacao8=="NA" | 
                                substr(indexacao8, 1, 3)=="inc" & indexacao9=="NA" |
                                substr(indexacao8, 1, 4)=="alin" & indexacao9=="NA" |
                                substr(indexacao9, 1, 5)=="alin")~ 0))%>% #Criamos uma variável dicotômica para distinguir alterações em incisos e alíneas daquelas em artigos ou parágrafos
  mutate(datas = as.Date(data.diploma, format = "%Y-%m-%d"),
         ano = as.numeric(substr(as.character(datas),1,4)))%>% #Extraímos o ano de data.diploma
  group_by(ano, inciso)%>%
  summarise(contagem = n())%>% #Criamos uma contagem para alterações em incisos ou alíneas, de um lado e, de outro, em artigos ou parágrafos
  mutate(index = case_when(inciso == 1 ~ "Incisos ou alíneas",
                           inciso == 0 ~ "Artigos ou parágrafos")) #Criamos uma variável para melhor legibilidade no gráfico

write_xlsx(basegrafico2, "C:/Users/miguel/Desktop/pesquisa/reformaseleitorais/reformaseleitorais_R/output/basegrafico2.xlsx") #Salvando a planilha em Excel

#Abaixo geramos o gráfico correspondente. As cores (fill) são dadas pelo nível de alteração ("Incisos ou alíneas" VS "Artigos ou parágrafos")
grafico2 <- ggplot(basegrafico2, mapping = aes(x = ano,
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

ggsave("grafico2.pdf", plot = grafico2, width = 12, height = 8) #Salvando o gráfico em pdf
ggsave("grafico2.png", plot = grafico2, width = 12, height = 8) #E em png
ggsave("grafico2.svg", plot = grafico2, width = 12, height = 8) #E svg

#---------
#Figura 3
#---------
basegrafico3 <- banco_reformas%>%
  filter(altera.diploma=="sim")%>% #Filtramos para ter apenas as alterações
  mutate(inciso = case_when(substr(indexacao7, 1, 3)=="inc" & indexacao8=="NA" | 
                              substr(indexacao8, 1, 3)=="inc" & indexacao9=="NA" |
                              substr(indexacao8, 1, 4)=="alin" & indexacao9=="NA" |
                              substr(indexacao9, 1, 5)=="alin" ~ 1,
                            !(substr(indexacao7, 1, 3)=="inc" & indexacao8=="NA" | 
                                substr(indexacao8, 1, 3)=="inc" & indexacao9=="NA" |
                                substr(indexacao8, 1, 4)=="alin" & indexacao9=="NA" |
                                substr(indexacao9, 1, 5)=="alin")~ 0))%>% #Criamos novamente uma variável dicotômica para distinguir alterações em incisos e alíneas daquelas em artigos ou parágrafos
  mutate(datas = as.Date(data.diploma, format = "%Y-%m-%d"),
         ano = as.numeric(substr(as.character(datas),1,4)))%>% #Extraímos o ano de data.diploma
  group_by(ano, inciso, tipo.alteracao)%>%
  summarise(contagem = n())%>% #Criamos uma contagem para alterações em incisos ou alíneas, de um lado e, de outro, em artigos ou parágrafos
  mutate(index = case_when(inciso == 1 ~ "Incisos ou alíneas",
                           inciso == 0 ~ "Artigos ou parágrafos"))%>% #Criamos uma variável para melhor legibilidade no gráfico
  rename(tipoalt = "tipo.alteracao")%>%
  mutate(tipoalt = case_when(tipoalt == "inclusao" ~ "Inclusão",
                             tipoalt == "revogacao" ~ "Revogação",
                             tipoalt == "renumeracao" ~ "Renumeração",
                             tipoalt == "nova redacao" ~ "Nova redação")) #Melhoramos também a legibilidade dos tipos de alteração

write_xlsx(basegrafico3, "C:/Users/miguel/Desktop/pesquisa/reformaseleitorais/reformaseleitorais_R/output/basegrafico3.xlsx") #Salvando a planilha em Excel

#Abaixo geramos o gráfico correspondente. As cores (fill) são dadas pelo tipo de alteração, enquanto as facetas (facet_wrap) se dividem pelo nível das modificações - entre incisos ou alíneas e artigos ou parágrafos
grafico3 <- ggplot(basegrafico3, mapping = aes(x = ano,
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

ggsave("grafico3.pdf", plot = grafico3, width = 12, height = 8) #Salvando gráfico em pdf
ggsave("grafico3.png", plot = grafico3, width = 12, height = 8) #E em png
ggsave("grafico3.svg", plot = grafico3, width = 12, height = 8) #E svg

#---------
#Figura 4
#---------
basegrafico4 <- banco_reformas%>%
  filter(as.numeric(numero.diploma.alterado) == 9504)%>% #Filtramos para ter apenas os dispositivos que alteraram a Lei das Eleições
  mutate(datas = as.Date(data.diploma),
         ano = as.numeric(substr(as.character(datas), 1, 4)))%>% #Extraímos o ano de data.diploma
  group_by(ano, parte)%>% #Agrupamos por ano e por divisão temática (parte)
  summarise(contagem = n()) #Criamos uma contagem para alterações por divisão temática e ano

write_xlsx(basegrafico4, "C:/Users/miguel/Desktop/pesquisa/reformaseleitorais/reformaseleitorais_R/output/basegrafico4.xlsx") #Salvando planilha em Excel

#Abaixo geramos o gráfico correspondente. As janelas (facet_wrap) são dadas pelas divisões temáticas
grafico4 <- ggplot(basegrafico4, mapping = aes(x = ano,
                                   y = contagem))+
  geom_point()+
  geom_line(color = "darkgrey", linetype = "dashed")+
  facet_wrap(~parte, labeller = label_wrap_gen(width=15))+
  labs(x = "Ano",
       y = "Número de alterações")+
  guides(x = guide_axis(angle = 90))+
  theme_minimal()+
  theme_bw()

ggsave("grafico4.pdf", plot = grafico4, width = 8, height = 12) #Gráfico em pdf
ggsave("grafico4.png", plot = grafico4, width = 8, height = 12) #E em png
ggsave("grafico4.svg", plot = grafico4, width = 8, height = 12) #E svg

#---------
#Figura 5
#---------
basegrafico5 <- banco_reformas%>%
  filter(as.numeric(numero.diploma.alterado) == 9096)%>% #Agora filtramos para ter apenas as alterações na Lei dos Partidos
  mutate(datas = as.Date(data.diploma),
         ano = as.numeric(substr(as.character(datas), 1, 4)))%>%
  group_by(ano, titulo)%>% #Agrupamos por título (uma divisão temática) e ano
  summarise(contagem = n())%>%
  mutate(titulo = str_wrap(titulo, width = 20))

write_xlsx(basegrafico5, "C:/Users/miguel/Desktop/pesquisa/reformaseleitorais/reformaseleitorais_R/output/basegrafico5.xlsx") #Salvando a planilha em Excel

#Abaixo geramos o gráfico correspondente. As janelas (facet_wrap) são dadas pelo título de cada segmento da lei
grafico5 <- ggplot(basegrafico5, mapping = aes(x = ano,
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

ggsave("grafico5.pdf", plot = grafico5, width = 12, height = 8) #Gráfico em pdf
ggsave("grafico5.png", plot = grafico5, width = 12, height = 8) #E em png
ggsave("grafico5.svg", plot = grafico5, width = 12, height = 8) #E svg

#---------
#Figura 6
#---------
basegrafico6 <- banco_reformas%>%
  filter(as.numeric(numero.diploma.alterado) == 9096)%>% #Filtramos novamente para ter apenas as alterações à Lei dos Partidos
  mutate(datas = as.Date(data.diploma),
         ano = as.numeric(substr(as.character(datas), 1, 4)))%>%
  group_by(ano, capitulo)%>% #Agrupamos por ano e capítulo, uma subdivisão menor do que os títulos
  summarise(contagem = n())%>% 
  mutate(capitulo = str_wrap(capitulo, width = 20))

write_xlsx(basegrafico6, "C:/Users/miguel/Desktop/pesquisa/reformaseleitorais/reformaseleitorais_R/output/basegrafico6.xlsx") #Salvando a planilha em Excel

#Abaixo geramos o gráfico correspondente. As janelas (facet_wrap) são dadas pelos capítulos da lei
grafico6 <- ggplot(basegrafico6, mapping = aes(x = ano,
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

ggsave("grafico6.pdf", plot = grafico6, width = 12, height = 8) #Gráfico em pdf
ggsave("grafico6.png", plot = grafico6, width = 12, height = 8) #E em png
ggsave("grafico6.svg", plot = grafico6, width = 12, height = 8) #E svg
