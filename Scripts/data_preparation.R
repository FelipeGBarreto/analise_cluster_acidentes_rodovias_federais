#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#
#####   TCC - Preparação dos dados    #####
#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#

# Bibliotecas
library(tidyverse)
library(data.table)
library(fastDummies)
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

df <- rbindlist(
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


dataset2 <- rbindlist(
  lista2, 
  idcol = NULL,
  fill = T #preenche se tiver colunas diferentes
)

#------------------------------------------------------------#
# Ajuste de encoding
#------------------------------------------------------------#
df %>% str()

for (i in names(df)){
  if(class(df[[i]]) == 'character'){
    Encoding(df[[i]]) <- 'latin1'
  }
}

df %>% str()

#------------------------------------------------------------#
# Retirando os espaços vazios no início/fim dos caracteres
#------------------------------------------------------------#
for (i in names(df)){
  df[[i]] <- trimws(df[[i]])
}

#------------------------------------------------------------#
# Ajuste de datas para o padrão yyy-mm-dd
#------------------------------------------------------------#

#valores nulos
for (i in names(df)){
  paste0(i,': ', sum(is.na(df[[i]]))) %>% cat('\n')
}


df$data <- case_when(
  substr(df$data_inversa,3,3) == '-' ~ as.Date(df$data_inversa, format = '%d-%m-%Y'),
  substr(df$data_inversa,3,3) == '/' ~ as.Date(df$data_inversa, format = '%d/%m/%Y'),
  substr(df$data_inversa,5,5) == '-' ~ as.Date(df$data_inversa, format = '%Y-%m-%d'),
  substr(df$data_inversa,5,5) == '/' ~ as.Date(df$data_inversa, format = '%Y/%m/%d'),
)

#valores nulos (outra forma --> traz somente as variáveis que têm nulos)
df %>% aggr(plot = F)
#df %>% aggr(plot = T)


#------------------------------------------------------------#
# Acrescentando a variável de turno do dia
#------------------------------------------------------------#

df$turno <- substr(df$horario, 1,2) %>% as.integer()

df$turno <- case_when(
  df$turno >= 6  & df$turno < 12 ~ 'Manhã',
  df$turno >= 12 & df$turno < 18 ~ 'Tarde',
  df$turno >= 18 & df$turno < 24 ~ 'Noite',
  df$turno >= 0  & df$turno < 6  ~ 'Noite'
)

#------------------------------------------------------------#
# Retirando todos os ids duplicados
#------------------------------------------------------------#
df <- df %>% distinct(id, .keep_all = T)


#------------------------------------------------------------#
# selecionando as variáveis desejadas
#------------------------------------------------------------#

df <- df %>% select(
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

#### VERIFICAR ESSA PARTE

# Transaformando o encoding 
for(i in names(df)){
  # encoding
  if(class(df[[i]]) == 'character'){
    Encoding(df[[i]]) <- 'latin1'
  }
  
  #Retirar todos os espaços vazios (início e fim)
  df[[i]] <- trimws(df[[i]])
  
  # Lower case 
  df[[i]] <- tolower(df[[i]])
}

# transaformando a data
df$data <- as.Date(df$data, format='%Y-%m-%d')

df$turno <- case_when(
  df$turno %in% c(tolower('manhã£')) ~ 'manhã',
  df$turno %in% c(tolower('tarde')) ~ 'tarde',
  df$turno %in% c(tolower('noite')) ~ 'noite',
)

df$is_night <- case_when(
  df$turno %in% c('manhã','tarde') ~ FALSE,
  df$turno %in% c('noite') ~ TRUE
)

df$dia_semana <- case_when(
  df$dia_semana %in% c('segunda','segunda-feira') ~ 'segunda',
  df$dia_semana %in% c('terça','terça-feira') ~ 'terça',
  df$dia_semana %in% c('quarta','quarta-feira') ~ 'quarta',
  df$dia_semana %in% c('quinta','quinta-feira') ~ 'quinta',
  df$dia_semana %in% c('sexta','sexta-feira') ~ 'sexta',
  df$dia_semana %in% c('sabado','sábado') ~ 'sabado',
  df$dia_semana %in% c('domingo') ~ 'domingo'
)

df$is_weekend <- case_when(
  df$dia_semana %in% c('segunda','terça','quarta','quinta') ~ FALSE,
  df$dia_semana %in% c('sexta','sabado','domingo') ~ TRUE
)

df$tipo_pista <- case_when(
  df$tipo_pista == 'simples' ~ 'simples',
  df$tipo_pista %in% c('múltipla','multipla','dupla') ~ 'dupla/multipla'
)

df$is_single_lane <- case_when( #se é pisca única
  df$tipo_pista %in% ('simples') ~ TRUE,
  TRUE ~ FALSE
)

### Tipos de Traçado de via
## Tipo 1: reta
# 'reta'
## Tipo 2: 
# 'curva','cruzamento','rotatória','interseção de vias','desvio temporário','retorno regulamentado'
## Tipo 3: 
# 'viaduto','ponte','túneo'

df$tracado_via <- case_when(
  df$tracado_via %in% c('reta') ~ 'tipo_1',
  df$tracado_via %in% c('curva','cruzamento',
                        'rotatória','interseção de vias',
                        'desvio temporário','retorno regulamentado') ~ 'tipo_2',
  df$tracado_via %in% c('viaduto','ponte','túneo') ~ 'tipo_3'
)


# Criando dummies das variáveis de traçado de via
df <- dummy_cols(
  df, select_columns = 'tracado_via'
)

for(column in c('tracado_via_tipo_1','tracado_via_tipo_2','tracado_via_tipo_3')){
  df[[column]] = ifelse(df[[column]] %in% c(1,'1'), T, F)
}

retirar_cols <- c(
  'id'
  ,'turno'
  ,'dia_semana'
  ,'causa_acidente'
  ,'tipo_acidente'
  ,'classificacao_acidente'
  ,'fase_dia'
  ,'sentido_via'
  ,'condicao_metereologica'
  ,'tipo_pista'
  ,'tipo_pista'
  ,'uso_solo'
  ,'tracado_via'
  #,'tracado_via_tipo_2'
  #,'tracado_via_tipo_3'
  ,'tracado_via_NA'
)

dataset_var_escolhidas <- df %>% select(-retirar_cols)

fwrite(df, "/Users/felipebarreto/Desktop/dados_acidentes_tratados_completo.csv", sep = ',')
fwrite(dataset_var_escolhidas, "/Users/felipebarreto/Desktop/dados_acidentes_tratados_selecionadas.csv", sep = ',')



#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# SHARE DE VARIÁVEIS CATEGÓRICAS // FUNÇÕES ESTÃO NO OUTRO SCRIPT (instalacao_pacotes)
#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#

# Variáveis categóricas e variáveis numéricas
# Nao vou utilizar o tracado_via_tipo_3, já que é o menos expressivo e é o complemento
df %>% str()

share_var_categ <- (
  share_var('tracado_via_tipo_1') %>% rename(share_tracado_via_tipo_1 = pct_share) %>% 
    left_join( ## VI QUE ESSA VARIÁVEL NÃO FAZ DIFERENÇA NO % DE VAR EXPLICADA
      share_var('tracado_via_tipo_2') %>% rename(share_tracado_via_tipo_2 = pct_share),
      on = 'municipio'
    ) %>% 
    left_join(
      share_var('is_night') %>% rename(share_is_night = pct_share),
      on = 'municipio'
    ) %>% 
    left_join(
      share_var('is_weekend') %>% rename(share_is_weekend = pct_share),
      on = 'municipio'
    ) %>%
    left_join(
      share_var('is_single_lane') %>% rename(share_is_single_lane = pct_share),
      on = 'municipio'
    ) %>%  as.data.frame() 
)
ver(share_var_categ)

#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# AGRUPANDO - Variáveis Quntitativas (métricas)
#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
quantitative_variables <- c(
  'pessoas'
  ,'mortos'
  ,'feridos_leves'
  ,'feridos_graves'
  ,'ilesos'
  ,'ignorados'
  ,'feridos'
  ,'veiculos'
)

var_quanti <- df %>% select(
  municipio
  ,quantitative_variables
) 

for (i in quantitative_variables){
  var_quanti[[i]] <- as.numeric(var_quanti[[i]])
}

# Agrupando os dados com as quantidades totais por município
var_quanti <- var_quanti %>%  
  group_by(municipio) %>% 
  summarise(
    across(
      everything(),
      list(sum)
    ),
    freq = n()
  ) %>%
  left_join(
    share_var_categ, 'municipio'
  ) %>% 
  arrange(desc(freq))  %>% data.frame()

# Renomeando do data frame
names(var_quanti) <- c(
  'municipio',
  'pessoas',
  'mortos',
  'feridos_leves',
  'feridos_graves', 
  'ilesos',
  'ignorados',
  'feridos',
  'veiculos',
  'freq',
  'share_tracado_via_tipo_1',
  'share_tracado_via_tipo_2',
  'share_is_night',
  'share_is_weekend',
  'share_is_single_lane'
)
  
fwrite(var_quanti, "/Users/felipebarreto/Desktop/TCC/Bases geradas/var_quanti_agg_pre.csv", sep = ',')
 
  






