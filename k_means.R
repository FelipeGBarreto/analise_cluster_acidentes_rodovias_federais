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
             "hrbrthemes"    # graph theme
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
path <- "/Users/felipebarreto/Desktop/dados_acidentes_tratados.csv"

df <- read.csv2(path, sep = ',')

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

df %>% select(tipo_pista) %>% distinct()

df$tipo_pista <- case_when(
  df$tipo_pista == 'simples' ~ 'simples',
  df$tipo_pista %in% c('múltipla','multipla','dupla') ~ 'dupla/multipla'
)

df$is_single_lane <- case_when( #se é pisca única
  df$tipo_pista %in% ('simples') ~ TRUE,
  TRUE ~ FALSE
)

x <- 1:50
case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  TRUE ~ as.character(x)
)
df %>% head() %>% View()

df %>% select(tracado_via) %>% distinct()

df$tracado_via <- case_when(
  df$tracado_via %in% c('reta') ~ 'reta',
  df$tracado_via %in% c('curva','cruzamento') ~ 'curva/cruzamento',
  df$tracado_via %in% c('rotatória',
                        'interseção de vias',
                        'desvio temporário',
                        'retorno regulamentado') ~ 'rotatória/interseção/desvio/retorno',
  df$tracado_via %in% c('viaduto','ponte','túneo') ~ 'viduto/ponte/túneo'
)

# função para melhorar a visualização
ver <- function(df, n=5, name_table = "Base de Dados"){
  df %>% head(n) %>% 
    kbl(caption = paste("Tabela:",name_table)) %>%
    kable_classic(full_width = F, html_font = "")%>% 
    scroll_box(width = "100%", height = "300px")
}


# Variáveis categóricas e variáveis numéricas
df %>% dim()
df %>% ver(10)
df %>% str()

#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# ANÁLISE DE CORRESPONDÊNCIA - Variáveis categóricas
#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#

# ANACOR / ACM - Análise de correspondência para as variáveis categóricas

# Variáveis categóricas
var_categ <- df %>% select(
  id
  ,data
  ,turno
  ,is_night
  ,dia_semana
  ,is_weekend
  ,uf
  ,municipio
  ,causa_acidente
  ,tipo_acidente
  ,classificacao_acidente
  ,fase_dia
  ,sentido_via
  ,condicao_metereologica
  ,tipo_pista
  ,is_single_lane
  ,tracado_via
  ,uso_solo
)

var_categ_agg <- var_categ %>% 
  group_by(municipio
           #,turno
           ,is_night
           ,is_weekend
           #,dia_semana
           #,causa_acidente ## AVALIAR SE É POSSÍVEL CLUSTERIZAR
           #,tipo_acidente  ## AVALIAR SE É POSSÍVEL CLUSTERIZAR
           #,tipo_pista
           ,is_single_lane
           ,tracado_via
           ) %>% 
  summarise(
   freq = n()
  ) %>% arrange(municipio, desc(freq))
  
#var_categ %>% select(tracado_via) %>% distinct()


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
  ) %>% arrange(desc(freq))  %>% data.frame()

names(clustering) <- c(names(var_quanti),'freq')


#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# Análises descritivas
#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#

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

# Há alta correlação entre os dados, o que faz sentido
chart.Correlation(df_quanti, method = "pearson")

# Verificando se há valores vazios. Como não há, vamos seguir em frente
df_quanti %>% aggr(plot = T)

#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# Cluster
#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#

df_quanti %>% arrange(desc(freq)) %>% ver(10, "Variáveis Selecionadas (Top 10)")

# Padronizando a base (mesma escala)
df.quanti.pad <- scale(df_quanti) %>% data.frame()
df.quanti.pad %>% ver(10, name_table = "Dados Padronizados")

df.quanti.pad %>% summary() # média = 0 e desvio-padrão = 1


#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# Estudo da quantidade ideal de clusters --> IMPORTANTE: Contexto do Negócio
#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#

# Verificando as possíveis quantidade de clusters
grid.arrange(
# Método Elbow (Cotovelo) --> Variabilidade dentro do grupo?
## 
  fviz_nbclust(
    df.quanti.pad, 
    kmeans, 
    method = "wss") +
  geom_vline(xintercept = 5,linetype = 2) +
  labs(title = "Número Ótimo de Clusters - K-Means"),
  
  # Método da Silhouette --> Quão bom foi o agrupamento com a alocação da observação
  # Quanto maior for o coef. silhouette, melhor, já que favorece o agrupamento.
  fviz_nbclust(
    df.quanti.pad,
    kmeans,
    method = "silhouette") +
  geom_vline(xintercept = 5, linetype = 2) +
    labs(title = "Número Ótimo de Clusters - K-Means")
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

ggplotly(
  ggplot(pct_var_explicada, 
         aes(x = Clusters, y = Percentual)) +
    geom_point(color = "brown") + 
    geom_line() +
    geom_vline(xintercept = 5,linetype = 3) + 
    labs(x = "Número Ótimo de Clusters",
         y = "Percentual de Variância Total Explicada",
         title = "Percentual de Variância Explicada")
)

#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# Cluster pelo Método K-Means
#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#

opc_cluster <- list() # Opção de cluster
graph <- list()       # opção de gráfico

for(i in 1:4){
  opc_cluster[[i]] <- kmeans(df.quanti.pad, centers = i + 2)
  graph[[i]] <- fviz_cluster(opc_cluster[[i]],
                             geom = "point", 
                             data = df.quanti.pad) + 
    ggtitle(paste0(i, "ª Opção - k = ",i+2))
}

# Plotando as possibilidades predefinidas
grid.arrange(
  graph[[1]],
  graph[[2]],
  graph[[3]], 
  graph[[4]],
  nrow = 2)

# ESCOLHA DA OPÇÃO 3 (5 Clusters)
opcao_clusters = 3
fviz_cluster(opc_cluster[[opcao_clusters]],
             ellipse.type = "euclid", #Elipse concentração
             star.plot =TRUE, #adiciona segmentos dos centroides para itens,
             repel = TRUE,
             #ggtheme = theme_minimal(),
             geom = "point", 
             data = df.quanti.pad) + 
  ggtitle(paste("Opção - k = ",opcao_clusters, sep=""))

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

# Percentual de Variância Explicada com 5 clusters (R2)
(opc_cluster[[opcao_clusters]]$betweenss / opc_cluster[[opcao_clusters]]$totss)

# Obs: Os dois resultados mais importantes são os tamanhos dos grupos e as médias
# Obs2: Lembrar que está padronizado!
opc_cluster[[opcao_clusters]]$size # tamanho
opc_cluster[[opcao_clusters]]$centers # médias

# Médias das variáveis de cada grupo - Médias de grupo
centers <- data.frame(
  cluster = factor(1:(opcao_clusters+2)), 
  opc_cluster[[opcao_clusters]]$centers
)

centers <- as.data.frame(
  t(centers %>% select(-cluster))
)

names(centers) <- paste(
  "Cluster", 
  1:(opcao_clusters+2)
)

centers <- rownames_to_column(.data = centers, var = 'Variavel')

centers %>% ver(n = 7, name_table = "Centróides com Dados Padronizados")

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
    freq = freq_1
  ) %>% 
  mutate(cluster = paste('Cluster', cluster))

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
fwrite(df.quanti.clusters, "/Users/felipebarreto/Desktop/TCC/Dados/Tratados/clusters_5.csv", sep = ';')

# Agora, vou nas variáveis categóricas para entender os agrupamentos!

# ------------------------------------------------------------------------------
                               ## FIM ##
# ------------------------------------------------------------------------------

# Tabulação personalizada
library(gtsummary)
df.quanti.pad %>% 
  tbl_cross(row = mortos,
            col = fase_dia,
            percent = "row") %>% 
  bold_labels()

