#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#
#####  TCC - Estudo prévio do dataset #####
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


df1$Data <- case_when(
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
#df1$Turno <- 
df1$Turno <- substr(df1$horario, 1,2) %>% as.integer()

df1$Turno <- case_when(
  df1$Turno >= 6  & df1$Turno < 12 ~ 'Manhã',
  df1$Turno >= 12 & df1$Turno < 18 ~ 'Tarde',
  df1$Turno >= 18 & df1$Turno < 24 ~ 'Noite',
  df1$Turno >= 0  & df1$Turno < 6  ~ 'Noite'
)







  
  
  
  
  
  
  
  






