#Case A3Data - Cientista de Dados - Isac Carvalho - 04/2022


#Bibliotecas usadas

install.packages("tidyverse")
install.packages("data.table")
install.packages("readxl")
install.packages("lubridate")
install.packages("chron")


library(data.table)
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
library(chron)

#Carregando conjunto de dados

#Base Aeronave
aeronave = read.csv("aeronave.csv", header=TRUE, sep=";", encoding = "UTF-8")

#Base Ocorrencia
ocorrencia = read.csv("ocorrencia.csv", header=TRUE, sep=";", encoding = "UTF-8")

#Base tipo de ocorrencia
ocorrencia_tipo = read.csv("ocorrencia_tipo.csv", header=TRUE, sep=";", encoding = "UTF-8")

#Base recomendação 
recomendacao = read.csv("recomendacao.csv", header=TRUE, sep=";", encoding = "UTF-8")

#Base fator contribuinte
fator_contribuinte = read.csv("fator_contribuinte.csv", header=TRUE, sep=";", encoding = "UTF-8")

#Conheceno conjunto de dados

summary(aeronave)
class(variable.names(aeronave))
colunas_aeronave = data.table(variable.names(aeronave))
print(colunas_aeronave)

summary(ocorrencia)
class(variable.names(ocorrencia))
colunas_ocorrencia = data.table(variable.names(ocorrencia))
print(colunas_ocorrencia)

summary(ocorrencia_tipo)
class(variable.names(ocorrencia_tipo))
colunas_ocorrencia_tipo = data.table(variable.names(ocorrencia_tipo))
print(colunas_ocorrencia_tipo)

summary(recomendacao)
class(variable.names(recomendacao))
colunas_recomendacao = data.table(variable.names(recomendacao))
print(colunas_recomendacao)

summary(fator_contribuinte)
class(variable.names(fator_contribuinte))
colunas_fator_contribuinte = data.table(variable.names(fator_contribuinte))
print(colunas_fator_contribuinte)


###Etapa Data Mining###

#renomeando colunas

aeronave = rename(aeronave, codigo_ocorrencia2 = X.U.FEFF.codigo_ocorrencia2)
ocorrencia = rename(ocorrencia, codigo_ocorrencia = X.U.FEFF.codigo_ocorrencia)
ocorrencia_tipo = rename(ocorrencia_tipo, codigo_ocorrencia1 = X.U.FEFF.codigo_ocorrencia1)
recomendacao = rename(recomendacao, codigo_ocorrencia4 = X.U.FEFF.codigo_ocorrencia4)
fator_contribuinte = rename(fator_contribuinte, codigo_ocorrencia3 = X.U.FEFF.codigo_ocorrencia3)


#padronizando dados vazios

is.na(aeronave[1:23]) = aeronave[1:23] == "*"
is.na(aeronave[1:23]) = aeronave[1:23] == "**"
is.na(aeronave[1:23]) = aeronave[1:23] == "***"
is.na(aeronave[1:23]) = aeronave[1:23] == "****"
is.na(aeronave[1:23]) = aeronave[1:23] == "****"
is.na(aeronave[1:23]) = aeronave[1:23] == "*****"
is.na(aeronave[1:23]) = aeronave[1:23] == "NULL"

aeronave = aeronave %>% 
  mutate_all(replace_na, "")

is.na(ocorrencia[1:22]) = ocorrencia[1:22] == "*"
is.na(ocorrencia[1:22]) = ocorrencia[1:22] == "**"
is.na(ocorrencia[1:22]) = ocorrencia[1:22] == "***"
is.na(ocorrencia[1:22]) = ocorrencia[1:22] == "****"
is.na(ocorrencia[1:22]) = ocorrencia[1:22] == "*****"
is.na(ocorrencia[1:22]) = ocorrencia[1:22] == "******"
is.na(ocorrencia[1:22]) = ocorrencia[1:22] == "*******"
is.na(ocorrencia[1:22]) = ocorrencia[1:22] == "********"
is.na(ocorrencia[1:22]) = ocorrencia[1:22] == "*********"
is.na(ocorrencia[1:22]) = ocorrencia[1:22] == "****_***"
is.na(ocorrencia[1:22]) = ocorrencia[1:22] == "****_****"
is.na(ocorrencia[1:22]) = ocorrencia[1:22] == "###!"
is.na(ocorrencia[1:22]) = ocorrencia[1:22] == "####"
is.na(ocorrencia[1:22]) = ocorrencia[1:22] == "NULL"

ocorrencia = ocorrencia %>% 
  mutate_all(replace_na, "")

ocorrencia_tipo = ocorrencia_tipo %>% 
  mutate_all(replace_na, "") #na tabela ocorrencia_tipo não foi nenhum caractere diferente

is.na(recomendacao[1:9]) = recomendacao[1:9] == "*"
is.na(recomendacao[1:9]) = recomendacao[1:9] == "**"
is.na(recomendacao[1:9]) = recomendacao[1:9] == "***"
is.na(recomendacao[1:9]) = recomendacao[1:9] == "NULL"

recomendacao = recomendacao %>% 
  mutate_all(replace_na, "")

is.na(fator_contribuinte[1:5]) = fator_contribuinte[1:5] == "*"
is.na(fator_contribuinte[1:5]) = fator_contribuinte[1:5] == "**"
is.na(fator_contribuinte[1:5]) = fator_contribuinte[1:5] == "***"

fator_contribuinte = fator_contribuinte %>% 
  mutate_all(replace_na, "")

#verificando se a coluna codigo_ocorrencia possui registros únicos e se as demais colunas são idênticas. 
analise_ocorrencias = ocorrencia %>%
  group_by(codigo_ocorrencia,codigo_ocorrencia1,codigo_ocorrencia2,codigo_ocorrencia3,codigo_ocorrencia4) %>%
  summarise(quantidade = n())

print(analise_ocorrencias)


#como suspeitava a coluna 'codigo_ocorrencia' é idêntica as outras colunas numeradas de 1 a 4 e possui registros únicos.
#Sendo assim, não é necessário manter as demais colunas e nas outras tabelas podemos retirar a numeração também.

#retirando colunas desnecessárias
ocorrencia = ocorrencia %>%
  select(codigo_ocorrencia,ocorrencia_classificacao:ocorrencia_saida_pista)

str(ocorrencia)

#renomeando colunas das demais tabelas para o mesmo nome

aeronave = rename(aeronave, codigo_ocorrencia = codigo_ocorrencia2)
ocorrencia_tipo = rename(ocorrencia_tipo, codigo_ocorrencia = codigo_ocorrencia1)
recomendacao = rename(recomendacao, codigo_ocorrencia = codigo_ocorrencia4)
fator_contribuinte = rename(fator_contribuinte, codigo_ocorrencia = codigo_ocorrencia3)

#manipulando classes

aeronave = aeronave %>%
  mutate(codigo_ocorrencia = as.numeric(codigo_ocorrencia)) %>%
  mutate(aeronave_pmd = as.numeric(aeronave_pmd)) %>%
  mutate(aeronave_pmd_categoria = as.numeric(aeronave_pmd_categoria)) %>%
  mutate(aeronave_assentos = as.numeric(aeronave_assentos)) %>%
  mutate(aeronave_ano_fabricacao = as.numeric(aeronave_ano_fabricacao)) %>%
  mutate(aeronave_fatalidades_total = as.numeric(aeronave_fatalidades_total))
str(aeronave)

ocorrencia = ocorrencia %>%
  mutate(codigo_ocorrencia = as.numeric(codigo_ocorrencia)) %>%
  mutate(ocorrencia_latitude = as.numeric(ocorrencia_latitude)) %>%
  mutate(ocorrencia_longitude = as.numeric(ocorrencia_longitude)) %>%
  mutate(ocorrencia_dia = dmy(ocorrencia_dia)) %>%
  mutate(ocorrencia_hora = chron(times = ocorrencia_hora)) %>%
  mutate(divulgacao_dia_publicacao = as.Date(strptime(divulgacao_dia_publicacao,format = "%Y-%m-%d"))) %>%
  mutate(total_recomendacoes = as.numeric(total_recomendacoes)) %>%
  mutate(total_aeronaves_envolvidas = as.numeric(total_aeronaves_envolvidas))
str(ocorrencia)

ocorrencia_tipo = ocorrencia_tipo %>%
  mutate(codigo_ocorrencia = as.numeric(codigo_ocorrencia))
str(ocorrencia_tipo)

recomendacao = recomendacao %>%
  mutate(codigo_ocorrencia = as.numeric(codigo_ocorrencia)) %>%
  mutate(recomendacao_dia_assinatura = as.Date(strptime(recomendacao_dia_assinatura,format = "%Y-%m-%d"))) %>%
  mutate(recomendacao_dia_encaminhamento = as.Date(strptime(recomendacao_dia_encaminhamento,format = "%Y-%m-%d"))) %>%
  mutate(recomendacao_dia_feedback = as.Date(strptime(recomendacao_dia_feedback,format = "%Y-%m-%d")))
str(recomendacao)

fator_contribuinte = fator_contribuinte %>%
  mutate(codigo_ocorrencia = as.numeric(codigo_ocorrencia))
str(fator_contribuinte)


#para trazer uma visao maior por periodo vamos trabalhar com mes/semana mes/semana dia

ocorrencia = ocorrencia %>%
  mutate(Mes = month(ocorrencia_dia)) %>%
  mutate(Dia_mes = day(ocorrencia_dia)) %>%
  mutate(Semana_mes = ceiling(day(ocorrencia_dia)/7)) %>%
  mutate(Dia_Semana = wday(ocorrencia_dia)) %>%
  mutate(Hora = as.numeric(str_sub(ocorrencia_hora, start = 1, end = 2)))

#Exportar dados tratados para ser usado no Power BI

write.table(ocorrencia, file= "C:/Users/isacc/OneDrive/�rea de Trabalho/Jaike/Meu novo emprego/case_A3Data/ocorrencia_R.csv", sep=';', dec=',', row.names=FALSE)
write.table(ocorrencia_tipo, file= "C:/Users/isacc/OneDrive/�rea de Trabalho/Jaike/Meu novo emprego/case_A3Data/ocorrencia_tipo_R.csv", sep=';', dec=',', row.names=FALSE)
write.table(aeronave, file= "C:/Users/isacc/OneDrive/�rea de Trabalho/Jaike/Meu novo emprego/case_A3Data/aeronave_R.csv", sep=';', dec=',', row.names=FALSE)
write.table(recomendacao, file= "C:/Users/isacc/OneDrive/�rea de Trabalho/Jaike/Meu novo emprego/case_A3Data/recomendacao_R.csv", sep=';', dec=',', row.names=FALSE)
write.table(fator_contribuinte, file= "C:/Users/isacc/OneDrive/�rea de Trabalho/Jaike/Meu novo emprego/case_A3Data/fator_contribuinte_R.csv", sep=';', dec=',', row.names=FALSE)
