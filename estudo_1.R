##### ESTUDO 1

library(tidyverse)

# dados
df <- read.csv("/Users/felipebarreto/Desktop/dados_acidentes_tratados.csv")
for (i in names(df)){
  if(class(df[[i]]) == 'character'){
    Encoding(df[[i]]) <- 'latin1'
  }
}

