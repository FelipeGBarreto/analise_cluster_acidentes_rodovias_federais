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
             "data.table")

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

df['turno'][df['turno'] %in% c('ManhÃ£', 'Manhã£')] <- 'Manhã'


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
  ,dia_semana
  ,uf
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
)

var_categ$dia_semana <- case_when(
  var_categ$dia_semana == 'segunda-feira' ~ 'segunda',
  var_categ$dia_semana == 'terça-feira' ~ 'terça',
  var_categ$dia_semana == 'quarta-feira' ~ 'quarta',
  var_categ$dia_semana == 'quinta-feira' ~ 'quinta',
  var_categ$dia_semana == 'sexta-feira' ~ 'sexta'
)

var_categ$tipo_pista <- case_when(
  var_categ$tipo_pista == 'simples' ~ 'simples',
  var_categ$tipo_pista %in% c('multipla','dupla') ~ 'dupla/multipla'
)

var_categ_agg <- var_categ %>% 
  group_by(municipio
           ,turno
           ,dia_semana
           #,causa_acidente ## AVALIAR SE É POSSÍVEL CLUSTERIZAR
           #,tipo_acidente  ## AVALIAR SE É POSSÍVEL CLUSTERIZAR
           ,tipo_pista
           ,tracado_via) %>% 
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
  ) %>% data.frame()

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
centers <- data.frame(cluster = factor(1:(opcao_clusters+2)), opc_cluster[[opcao_clusters]]$centers)
centers <- as.data.frame(t(centers %>% select(-cluster)))
names(centers) <- paste("Cluster", 1:(opcao_clusters+2))
centers <- rownames_to_column(.data = centers, var = 'Variavel')

centers %>% ver(n = 7, name_table = "Centróides com Dados Padronizados")

#centers$Symbol <- row.names(centers)
#centers <- gather(centers, "Cluster", "Mean", -Symbol)

centers <- gather(centers, "Cluster", "Mean", -Variavel)
centers %>% ver(15)

centers$Color <- centers$Mean > 0 

# Análise de médias com o dataset de variáveis quantitativas padronizado
ggplot(centers, 
       aes(x = Variavel, y = Mean, fill = Color)) +
  geom_bar(stat = "identity",
           position = "identity",
           width = .75) +
  facet_grid(Cluster ~ ., scales = 'free_y')

#Revendo o tamanho dos grupos
opc_cluster[[opcao_clusters]]$size # tamanho
opc_cluster[[opcao_clusters]]$centers # médias

# Obs:
# É possível verificar que os grupos 1 e, principalmente, o 3 têm altos índices
# e os grupos 2 e 4 têm baixos índices

# Obs2: 
# Grupos desequilibrados (tamanho) podem gerar outliers distantes OU grupos
# de registros muito distintos do resto dos dados (o que necessita de maior inspeção)

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
  mutate(uf = factor('uf'))

df.quanti.clusters %>% summary
df.quanti.clusters %>% ver



df.quanti.mean <- df.quanti.clusters %>% select(-c(municipio,uf)) %>% 
  group_by(cluster) %>% summarise(
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
  mutate(cluster = paste('Cluster', cluster)) %>% 
  reshape2::melt(.) %>% rename(Mean = value, Variavel = variable)


opc_cluster[[opcao_clusters]]$size

ggplotly(
ggplot(df.quanti.mean, 
       aes(x = Variavel, y = Mean, fill = Variavel)) +
  geom_bar(stat = "identity",
           position = "identity",
           width = .75) +
  facet_grid(cluster ~ ., scales = 'free_y')
)

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

