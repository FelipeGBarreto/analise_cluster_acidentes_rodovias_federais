#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# INSTALAÇÃO DE PACOTES 
#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#

# Bibliotecas Inportantes para a análise
pacotes <- c("tidyverse",    # para manipulação de dados
             "cluster",      # algorítimo de cluster
             "dendextend",   # compara dendogramas (melhor p/ Mod. Hierárquicos)
             "factoextra",   # Algoritmo de cluster e visualização
             "fpc",          # Algoritmo de cluster e visualização
             "gridExtra",    # Para a função grid.arrange
             "tibble",       # Transformar Uma coluna específica em index
             "kableExtra",   # Visualização no viwer, para facilitar
             "PerformanceAnalytics",   # Correlação dos dados
             "VIM",          # Verificação de valores nulos
             "plotly",       # Visualização de dados
             "data.table",
             "hrbrthemes",   # graph theme
             "fastDummies",  # Gerar variáveis dummies
             "rlang",        
             "psych"         # correlação
)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# FUNÇÕES
#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# Função para pegar o nome da variável
get_name <- function(name, env = rlang::caller_env()) {
  name_sym <- sym(name)
  eval(name_sym, env)
}

# função para melhorar a visualização
ver <- function(data, 
                n=5, 
                name_table = "Base de Dados"){
  base_name = match.call()$data
  data %>% head(n) %>% 
    kbl(
      caption = paste("Tabela:", name_table, '|', 'Base de dados:', base_name)
    ) %>%
    kable_classic(full_width = F, html_font = "") %>% 
    scroll_box(width = "100%", height = "300px")
}

# Função para calcular o share de TRUE pelo pelo total da variável
share_var <- function(var){
  share_var <- df %>% select(municipio, sym(var)) %>% 
    dummy_cols(
      select_columns = var
    ) %>% 
    select(
      municipio,
      sym(paste0(var,'_FALSE')),
      sym(paste0(var,'_TRUE'))
    ) %>% 
    group_by(municipio) %>% 
    summarise(
      pct_share = sum(get_name(paste0(var,'_TRUE'))) / n()
    )
  
  return(share_var)
}

###############################################################################

