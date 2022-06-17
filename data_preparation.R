#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#
#####   TCC - Preparação dos dados    #####
#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#

# Bibliotecas
library(tidyverse)
library(data.table)
library(VIM)


# K-means
path <- "/Users/felipebarreto/Desktop/TCC/Dados"

#------------------------------------------------------------#
# Maneira 1 de ter uma lista com vários arquivos de uma pasta
#------------------------------------------------------------#
lista1 <- list()
for (i in list.files(path)){
  lista1[[i]] <- read.csv2(paste0(path,'/',i))
}

# Verificando o tanho de cada arquivo da lista
for (i in 1:length(lista1)){
  paste(i,',', dim(lista1[[i]]) %>% data.frame()) %>% cat('\n')
}

df1 <- rbindlist(
  lista1, 
  idcol = NULL,
  fill = T #preenche se tiver colunas diferentes
)

#------------------------------------------------------------#
# Maneira 2 de ter uma lista com vários arquivos de uma pasta
#------------------------------------------------------------#
lista2 <- list.files(
  path,
  full.names = T # serve para pegar o caminho completo
) %>% sapply(read.csv2, simplify = T)


df2 <- rbindlist(
  lista2, 
  idcol = NULL,
  fill = T #preenche se tiver colunas diferentes
)

#------------------------------------------------------------#
# Ajuste de encoding
#------------------------------------------------------------#
df1 %>% str()

for (i in names(df1)){
  if(class(df1[[i]]) == 'character'){
    Encoding(df1[[i]]) <- 'latin1'
  }
}

df1 %>% str()

#------------------------------------------------------------#
# Retirando os espaços vazios no início/fim dos caracteres
#------------------------------------------------------------#
for (i in names(df1)){
  df1[[i]] <- trimws(df1[[i]])
}

#------------------------------------------------------------#
# Ajuste de datas para o padrão yyy-mm-dd
#------------------------------------------------------------#

#valores nulos
for (i in names(df1)){
  paste0(i,': ', sum(is.na(df1[[i]]))) %>% cat('\n')
}


df1$data <- case_when(
  substr(df1$data_inversa,3,3) == '-' ~ as.Date(df1$data_inversa, format = '%d-%m-%Y'),
  substr(df1$data_inversa,3,3) == '/' ~ as.Date(df1$data_inversa, format = '%d/%m/%Y'),
  substr(df1$data_inversa,5,5) == '-' ~ as.Date(df1$data_inversa, format = '%Y-%m-%d'),
  substr(df1$data_inversa,5,5) == '/' ~ as.Date(df1$data_inversa, format = '%Y/%m/%d'),
)

#valores nulos (outra forma --> traz somente as variáveis que têm nulos)
df1 %>% aggr(plot = F)
df1 %>% aggr(plot = T)


#------------------------------------------------------------#
# Acrescentando a variável de turno do dia
#------------------------------------------------------------#

df1$turno <- substr(df1$horario, 1,2) %>% as.integer()

df1$turno <- case_when(
  df1$turno >= 6  & df1$turno < 12 ~ 'Manhã',
  df1$turno >= 12 & df1$turno < 18 ~ 'Tarde',
  df1$turno >= 18 & df1$turno < 24 ~ 'Noite',
  df1$turno >= 0  & df1$turno < 6  ~ 'Noite'
)

#------------------------------------------------------------#
# Retirando todos os ids duplicados
#------------------------------------------------------------#
df1 <- df1 %>% distinct(id, .keep_all = T)


#------------------------------------------------------------#
# selecionando as variáveis desejadas
#------------------------------------------------------------#

df1 <- df1 %>% select(
  id
  ,data
  ,turno
  #,data_inversa
  ,dia_semana
  #,horario
  ,uf
  #,br
  #,km
  ,municipio
  ,causa_acidente
  ,tipo_acidente
  ,classificacao_acidente
  ,fase_dia
  ,sentido_via
  ,condicao_metereologica
  ,tipo_pista
  ,tracado_via
  ,uso_solo
  #,ano
  ,pessoas
  ,mortos
  ,feridos_leves
  ,feridos_graves
  ,ilesos
  ,ignorados
  ,feridos
  ,veiculos
  #,latitude
  #,longitude
  #,regional
  #,delegacia
  #,uop
)

fwrite(df1, "/Users/felipebarreto/Desktop/dados_acidentes_tratados.csv", sep = ',')



  
  
  
  






