#------------------------------------------------------------------------------#
        ## ACIDENTES NAS RODOVIAS FEDERAIS DO BRASIL - CLUSTERIZAÇÃO ##         
#------------------------------------------------------------------------------#

## FONTE: 
# https://www.gov.br/prf/pt-br/acesso-a-informacao/dados-abertos/dados-abertos-acidentes

## OBJETIVO: 
#------------------------------------------------------------------------------#
# Identificar grupos com características semelhantes com relação aos acidentes
# nas rodovias feredais no Brasil, para uma atuação política mais assertiva.

## MODELO:
#------------------------------------------------------------------------------#
# K-MEANS (Não Hierárquico - Devido ao formato dos dados)

## INICIANDO A ANÁLISE 
#------------------------------------------------------------------------------#

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
             "rlang"
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

# Conhecendo o dataset
path <- "/Users/felipebarreto/Desktop/"
path_pasta <- paste0(path,'TCC/')

df <- read.csv2(paste0(path,'dados_acidentes_tratados.csv'), sep = ',')

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

#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# FUNÇÕES
#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# Função para pegar o nome da variável
library(rlang)
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


#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# SHARE DE VARIÁVEIS CATEGÓRICAS
#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#

# Variáveis categóricas e variáveis numéricas
# Nao vou utilizar o tracado_via_tipo_3, já que é o menos expressivo e é o complemento
ver(df)
df %>% str()

share_var_categ <- (
share_var('tracado_via_tipo_1') %>% rename(share_tracado_via_tipo_1 = pct_share) %>% 
  #left_join( ## VI QUE ESSA VARIÁVEL NÃO FAZ DIFERENÇA NO % DE VAR EXPLICADA
  #  share_var('tracado_via_tipo_2') %>% rename(share_tracado_via_tipo_2 = pct_share),
  #  on = 'municipio'
  #) %>% 
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
# ANÁLISE DE CORRESPONDÊNCIA - Variáveis categóricas
#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#

# ANACOR / ACM - Análise de correspondência para as variáveis categóricas

# Variáveis categóricas
#causa_acidente --> avaliar se é possível clusterizar
#tipo_acidente  --> avaliar se é possível clusterizar
var_categ <- df %>% select(
  municipio
  ,is_night
  ,is_weekend
  ,is_single_lane
  ,tracado_via_tipo_1
  ,tracado_via_tipo_2
  ,tracado_via_tipo_3
)  %>% 
group_by(
  municipio
  ,is_night
  ,is_weekend
  ,is_single_lane
  ,tracado_via_tipo_1
  ,tracado_via_tipo_2
  ,tracado_via_tipo_3
) %>% 
  summarise(
   freq = n()
  ) %>% as.data.frame() %>% 
  arrange(municipio, desc(freq))

ver(var_categ)


#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# CLUSTERING - Variáveis Quntitativas (métricas)
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
clustering <- var_quanti %>%  
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
names(clustering) <- c(names(var_quanti),'freq',names(select(share_var_categ, -municipio)))

ver(clustering)

###############################################################################

#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# Análises descritivas / Selecionando variáveis desejadas
#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#

## Ver a correlaç˜so entre as variáveis para entender quais podem ser retiradas
correlation <- chart.Correlation(clustering[,2:ncol(clustering)], method = "pearson")

library(psych)
corPlot(clustering[,2:ncol(clustering)], cex = .6)
# comparação das variáveis com frequência
retirar <- c(veiculos,feridos,pessoas)
clustering %>% str

## IDEAL: Estudo de correlação por CLUSTER!!

clustering %>% summary


# Qual a cidade que mais aparece no mapa de acidentes?
clustering %>% filter(freq == max(clustering$freq))
clustering %>% filter(freq == max(clustering$mortos))

# Qual a cidade que mais aparece no mapa de acidentes?
clustering %>% filter(freq == min(clustering$freq))

# Colocando a label de municipio para index
df_quanti <- clustering %>% 
  select(-c(ignorados,veiculos)) %>% 
  column_to_rownames('municipio')

# Verificando se há valores vazios. Como não há, vamos seguir em frente
df_quanti %>% aggr(plot = T)
ver(df_quanti)
fwrite(df_quanti, paste0(path_pasta,'Bases geradas/variaveis_quanti_agg.csv'), sep=';')

#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# CLUSTER - TODOS OS ESTUDOS SERÃO FEITOS APÓS CLUSTERIZAR
#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#

# Padronizando a base (mesma escala)
df.quanti.pad <- scale(df_quanti) %>% data.frame()
ver(df.quanti.pad, 10, name_table = "Dados Padronizados")
fwrite(df.quanti.pad, paste0(path_pasta,'Bases geradas/variaveis_quanti_pad_agg.csv'), sep=';')

df.quanti.pad %>% summary() # média = 0 e desvio-padrão = 1


#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# Estudo da quantidade ideal de clusters --> IMPORTANTE: Contexto do Negócio
#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#

# Método Elbow (Cotovelo) --> Variabilidade dentro do grupo?
metodo_elbow <- (
  fviz_nbclust(
    df.quanti.pad, 
    kmeans, 
    method = "wss") +
  geom_vline(xintercept = 5,linetype = 2) +
  labs(title = "Número Ótimo de Clusters - K-Means")
)

# Método da Silhouette --> Quão bom foi o agrupamento com a alocação da observação
# Quanto maior for o coef. silhouette, melhor, já que favorece o agrupamento.
metodo_cotovolo <- (
  fviz_nbclust(
    df.quanti.pad,
    kmeans,
    method = "silhouette") +
  geom_vline(xintercept = 5, linetype = 2) +
  labs(title = "Número Ótimo de Clusters - K-Means")
)

# Juntando os dois plots
grid.arrange(
  metodo_elbow,
  metodo_cotovolo
)

#db <- fpc::dbscan(df.quanti.pad ,eps = 0.1, MinPts = 6)
#Visualize DBSCAN Clustering
#dbcs <- fviz_cluster(db,
 #                    data=df.quanti.pad,
  #                   stand = FALSE,
   #                  show.clust.cent = FALSE,
    #                 geom = "point",
     #                #title = "DBSCAN — CLUSTERING"
      #               )
#dbcs

# método do cotovelo --> Percentual de variância explicada com o aumento de clusters
pct_var_explicada <- data.frame(Clusters = 2:10,
                                Percentual = 0)

totalss <- kmeans(df.quanti.pad, centers = 10)$totss

for(i in 2:10){
  pct_var_explicada[i-1, "Percentual"] <- round((
    (kmeans(df.quanti.pad, centers = i)$betweenss)*100/totalss), 2)
}

pct_var_explicada %>% ver(10,"Variância Explicada por Cluster")

plot_pct_var_explicada <- ggplotly(
  ggplot(pct_var_explicada, 
         aes(x = Clusters, y = Percentual)) +
    geom_point(color = "brown") + 
    geom_line() +
    geom_vline(xintercept = 5,linetype = 3) + 
    labs(x = "Número Ótimo de Clusters",
         y = "Percentual de Variância Total Explicada",
         title = "Percentual de Variância Explicada")
)
plot_pct_var_explicada

#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# Cluster pelo Método K-Means - (ESCOLHI 8 CLUSTERS)
#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#

opc_cluster <- list() # Opção de cluster
graph <- list()       # opção de gráfico
n <- 5  # Número mínimo de clusters para analisar
for(i in 1:6){
  opc_cluster[[i]] <- kmeans(df.quanti.pad, centers = i + n-1)
  graph[[i]] <- fviz_cluster(opc_cluster[[i]],
                             geom = "point", 
                             data = df.quanti.pad) + 
    ggtitle(paste0(i, "ª Opção - k = ",i + n-1))
}

# Plotando as possibilidades predefinidas
grid.arrange(
  graph[[1]],
  graph[[2]],
  graph[[3]], 
  graph[[4]],
  graph[[5]],
  graph[[6]],
  nrow = 3)

# ESCOLHA DA OPÇÃO 3 (7 Clusters)
opcao_clusters = 3
fviz_cluster(opc_cluster[[opcao_clusters]],
             ellipse.type = "euclid", #Elipse concentração
             star.plot =TRUE, #adiciona segmentos dos centroides para itens,
             repel = TRUE,
             #ggtheme = theme_minimal(),
             geom = "point", 
             data = df.quanti.pad) + 
  ggtitle(paste("Opção - k = ",opcao_clusters + n-1, sep=""))

# Obs: O Método K-Means é sensível a outliers

# OBSERVAÇÃO: Importante entender "Dim1" e "Dim2" (PCA)

# Escolhendo 5 clusters (opção 5) para analisar --> Vamos ver os outputs
opc_cluster[[opcao_clusters]]$cluster        # Grupo por observação
opc_cluster[[opcao_clusters]]$centers        # Matriz com as médias (cetróides) de cada grupo
opc_cluster[[opcao_clusters]]$totss          # Soma dos quadrados totais
opc_cluster[[opcao_clusters]]$betweenss      # Soma dos quadrados entre grupos
opc_cluster[[opcao_clusters]]$withinss       # Soma dos quadrados dentro dos grupos
opc_cluster[[opcao_clusters]]$tot.withinss   # Soma dos quadrados dentro de todos os grupos
opc_cluster[[opcao_clusters]]$size           # Número de observasções por grupo (tamanhos)

# Percentual de Variância Explicada com 7 clusters (R2)
(opc_cluster[[opcao_clusters]]$betweenss / opc_cluster[[opcao_clusters]]$totss)

# Obs: Os dois resultados mais importantes são os tamanhos dos grupos e as médias
# Obs2: Lembrar que está padronizado!
opc_cluster[[opcao_clusters]]$size # tamanho
opc_cluster[[opcao_clusters]]$centers # médias

# Médias das variáveis de cada grupo - Médias de grupo
centers <- data.frame(
  cluster = factor(1:(opcao_clusters+n-1)), 
  opc_cluster[[opcao_clusters]]$centers
)

centers <- as.data.frame(
  t(centers %>% select(-cluster))
)

names(centers) <- paste(
  "Cluster", 
  1:(opcao_clusters+n-1)
)

centers <- rownames_to_column(.data = centers, var = 'Variavel')

centers %>% ver(n = 11, name_table = "Centróides com Dados Padronizados")
fwrite(centers, paste0(path_pasta,'Bases geradas/centroides_padronizados.csv'),sep=';', dec=',')

#centers$Symbol <- row.names(centers)
#centers <- gather(centers, "Cluster", "Mean", -Symbol)

centers <- gather(centers, "Cluster", "Mean", -Variavel)
centers %>% ver(15)

centers$Color <- centers$Mean > 0 
centers %>% ver(n=50)

# Análise de médias com o dataset de variáveis quantitativas padronizado
avg.graph.1 <- ggplotly(
ggplot(centers, 
       aes(x = Variavel, y = Mean, fill = Color)) +
  geom_bar(stat = "identity",
           position = "identity",
           width = .75) +
  facet_grid(Cluster ~ ., scales = 'free_y')
)
avg.graph.1

#Revendo o tamanho dos grupos
opc_cluster[[opcao_clusters]]$size # tamanho
opc_cluster[[opcao_clusters]]$centers # médias

# Obs:
# É possível verificar que os grupos 1 e, principalmente, o 3 têm altos índices
# e os grupos 2 e 4 têm baixos índices

# Obs2: 
# Grupos desequilibrados (tamanho) podem gerar outliers distantes OU grupos
# de registros muito distintos do resto dos dados (o que necessita de maior inspeção)

#....................#
### CONTINUAR AQUI ###
#....................#

# Adicionando a variável "Cluster" e UF no dataset
df.quanti.clusters <- rownames_to_column(df_quanti, 'municipio') %>% 
  mutate(
    cluster = factor(opc_cluster[[opcao_clusters]]$cluster)
  ) %>% 
  left_join(
    distinct(df[,c('municipio','uf')]),
    #df %>% select(municipio, factor('uf')) %>% distinct(),
    by = 'municipio'
  ) %>% 
  mutate(uf = factor(uf))
fwrite(df.quanti.clusters, paste0(path_pasta,'Bases geradas/variaveis_quanti_agg.csv'), sep=';')

df.quanti.pad.clusters <- rownames_to_column(df.quanti.pad, 'municipio') %>% 
  mutate(
    cluster = factor(opc_cluster[[opcao_clusters]]$cluster)
  ) %>% 
  left_join(
    distinct(df[,c('municipio','uf')]),
    #df %>% select(municipio, factor('uf')) %>% distinct(),
    by = 'municipio'
  ) %>% 
  mutate(uf = factor(uf))
fwrite(df.quanti.pad.clusters, paste0(path_pasta,'Bases geradas/variaveis_quanti_pad_agg.csv'), sep=';')


df.quanti.clusters %>% ver()
df.quanti.clusters %>% summary


## MESMO GRÁFICO, MAS COM AS MÉDIAS REAIS
df.quanti.mean  <- df.quanti.clusters %>% 
  select(-c(municipio,uf)) %>% 
  group_by(cluster) %>% 
  summarise(
    across(
      everything(),
      list(mean)
    )
  ) %>% data.frame() %>%  
  rename(
    pessoas = pessoas_1,
    mortos = mortos_1,
    feridos_leves = feridos_leves_1,
    feridos_graves = feridos_graves_1,
    ilesos = ilesos_1,
    feridos = feridos_1,
    freq = freq_1,
    share_tracado_via_tipo_1 = share_tracado_via_tipo_1_1,
    share_is_night = share_is_night_1,
    share_is_weekend = share_is_weekend_1,
    share_is_single_lane = share_is_single_lane_1
  ) %>% 
  mutate(cluster = paste('Cluster', cluster))
fwrite(df.quanti.mean, paste0(path_pasta,'Bases geradas/medias_var_cluster.csv'),sep=';',dec=',')

ver(df.quanti.mean,7)

df.quanti.mean.melt <- df.quanti.mean %>% 
  reshape2::melt(.) %>% rename(Mean = value, Variavel = variable)


opc_cluster[[opcao_clusters]]$size

avg.graph.2 <- ggplotly(
ggplot(df.quanti.mean.melt, 
       aes(x = Variavel, y = Mean, fill = Variavel)) +
  geom_bar(stat = "identity",
           position = "identity",
           width = .75) +
  facet_grid(cluster ~ ., scales = 'free_y')
)
avg.graph.2

##### share de cada variável pela média de pessoas envolvidas nos acidentes #####
# Representatividade média de cada variável
df.quanti.mean.share <- df.quanti.mean %>% 
  mutate(
    mortos = round(100 * mortos / pessoas, 2),
    feridos_leves = round(100 * feridos_leves / pessoas, 2),
    feridos_graves = round(100 * feridos_graves / pessoas, 2),
    ilesos = round(100 * ilesos / pessoas, 2),
    feridos = round(100 * feridos / pessoas, 2)
  ) %>% 
  select(cluster,ilesos,feridos,feridos_leves,feridos_graves,mortos) %>% 
  reshape2::melt(.) %>% 
  mutate(
    Cluster_Variavel = paste0(
                        substr(cluster,1,1),
                        substr(cluster,9,9),
                        '_',
                        variable
                      ),
    'Mean' = value
  ) %>% arrange(variable,cluster)
  

#https://r-graph-gallery.com/304-highlight-a-group-in-lollipop.html
colors_labels = case_when(
      df.quanti.mean.share$Cluster_Variavel %like% c("mortos") ~ 'red',
      df.quanti.mean.share$Cluster_Variavel %like% c("ilesos") ~ 'blue',
      df.quanti.mean.share$Cluster_Variavel %like% c("feridos_leves") ~ '#d1a5a5',
      df.quanti.mean.share$Cluster_Variavel %like% c("feridos_graves") ~ '#cc7f7f',
      df.quanti.mean.share$Cluster_Variavel %like% c("feridos") ~ '#7d7979'
    )

ggplotly(
df.quanti.mean.share  %>% 
  mutate( #ordenar o eixo Y
    Cluster_Variavel = fct_reorder(Cluster_Variavel, 
    desc(as.integer((variable))))
    ) %>% 
  ggplot(aes(x=Mean, y=Cluster_Variavel, col = cluster)) +
    geom_segment(
      aes(x=0, xend=Mean, y=Cluster_Variavel, yend=Cluster_Variavel), 
      #color=ifelse(df.quanti.mean.share$Cluster_Variavel %like% c("mortos"), "red", "#9BB0B2"), 
      color = colors_labels,
      size=ifelse(df.quanti.mean.share$Cluster_Variavel %like% c("mortos"), 1.1, 0.7)
    ) +
    geom_point(
      #color=ifelse(df.quanti.mean.share$Cluster_Variavel %like% c("mortos"), "red", "#9BB0B2"), 
      color = colors_labels,
      size=ifelse(df.quanti.mean.share$Cluster_Variavel %like% c("mortos"), 4, 2)
    ) +
    geom_text(data=df.quanti.mean.share, aes( x=Mean, y=Cluster_Variavel, label=Mean),                 , 
            color=colors_labels, 
            size=3 , angle=0, fontface="bold"
            ) + 
    theme_ipsum() +
    #coord_flip() +
    theme(
      legend.position="none"
    ) +
    xlab("Média") +
    ylab("Clusters e Variáveis Quantitativas") +
    ggtitle("Share de Médias com a quantidade de envolvidos nos acidentes (%)")
) 

#### OBSERVAÇÃO: o avg.graph.1 não é interessante para a interpretação!!

# Plotando os centros dos grupos em dimensões duas a duas
# ------------------------------------------------------------------------------
# Se a análise se pautasse em apenas duas variáveis, poderíamos fazer assim:
df.quanti.pad$cluster <- factor(opc_cluster[[opcao_clusters]]$cluster)

ggplotly(
  ggplot(data = df.quanti.pad,
         aes(x = mortos, y = pessoas, 
             color = cluster, shape = cluster)) +
    geom_point(alpha = 0.7)
)

opc_cluster[[opcao_clusters]]$centers


# Análise dos clusters
# ------------------------------------------------------------------------------
fwrite(df.quanti.clusters, "/Users/felipebarreto/Desktop/TCC/Dados/Tratados/clusters_7.csv", sep = ';')

# Agora, vou nas variáveis categóricas para entender os agrupamentos!

# ------------------------------------------------------------------------------
                               ## FIM ##
# ------------------------------------------------------------------------------

# Tabulação personalizada
library(gtsummary)
df.quanti.pad %>% 
  tbl_cross(row = mortos,
            col = ilesos,
            percent = "row") %>% 
  bold_labels()
