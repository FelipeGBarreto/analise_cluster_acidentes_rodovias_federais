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
#acidentes$Qtd <- 1
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

df['turno'][df['turno'] == 'ManhÃ£'] <- 'Manhã'


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

# Qaul a cidade que mais aparece no mapa de acidentes?
clustering %>% filter(freq == max(clustering$freq))

# Qaul a cidade que mais aparece no mapa de acidentes?
clustering %>% filter(freq == min(clustering$freq))

clustering %>% filter(mortos == 706)
clustering %>% filter(municipio == 'SAOPAULO')

# Colocando a label de municipio para index
clustering <- dataset %>% column_to_rownames('municipio')
clustering %>% ver(10, "Acumulado 2007-2014")

# Há alta correlação entre os dados, o que faz sentido
chart.Correlation(dataset, method = "pearson")
dataset <- dataset %>% select(c(Qtd, pessoas, mortos, veiculos))
dataset %>% ver(10, "Variáveis Selecionadas (Top 10)")

# Padronizando a base (mesma escala)
dataset.pad <- scale(dataset) %>% data.frame()
dataset.pad %>% ver(10, name_table = "Dados Padronizados")
dataset.pad %>% summary() # média = 0 e desvio-padrão = 1

# Verificando se há valores vazios. Como não há, vamos seguir em frente
dataset %>% aggr(plot = T)

for(i in 1:ncol(dataset)){
  print(dataset[is.na(dataset[[i]]), ])
}

# ------------------------------------------------------------------------------
# Começando a clusterização --> IMPORTANTE: Contexto do Negócio
# ------------------------------------------------------------------------------

# Verificando as possíveis quantidade de clusters
grid.arrange(
  # Método Elbow (Cotovelo) --> Variabilidade dentro do grupo?
  fviz_nbclust(dataset.pad, kmeans, method = "wss") +
    geom_vline(xintercept = 4, linetype = 2) +
    labs(title = "Número Ótimo de Clusters - K-Means"),
  
  # Método da Silhouette --> Quão bom foi o agrupamento com a alocação da observação
  # Quanto maior for o coef. silhouette, melhor, já que favorece o agrupamento.
  fviz_nbclust(dataset.pad, kmeans, method = "silhouette") +
    geom_vline(xintercept = 4, linetype = 2) +
    labs(title = "Número Ótimo de Clusters - K-Means")
)


#db <- fpc::dbscan(dataset.pad ,eps = 0.1, MinPts = 6)
#Visualize DBSCAN Clustering
#dbcs <- fviz_cluster(db,
 #                    data=dataset.pad,
  #                   stand = FALSE,
   #                  show.clust.cent = FALSE,
    #                 geom = "point",
     #                #title = "DBSCAN — CLUSTERING"
      #               )
#dbcs

# método do cotovelo --> Percentual de variância explicada com o aumento de clusters
pct_var_explicada <- data.frame(Clusters = 2:10,
                                Percentual = 0)
totalss <- kmeans(dataset.pad, centers = 10)$totss
for(i in 2:10){
  pct_var_explicada[i-1, "Percentual"] <- round((
    (kmeans(dataset.pad, centers = i)$betweenss)*100/totalss), 2)
}
pct_var_explicada %>% ver(10,"Variância Explicada por Cluster")

ggplotly(
  ggplot(pct_var_explicada, 
         aes(x = Clusters, y = Percentual)) +
    geom_point(color = "brown") + 
    geom_line() +
    labs(x = "Número Ótimo de Clusters",
         y = "Percentual de Variância Total Explicada",
         title = "Percentual de Variância Explicada")
)

# Cluster pelo Método K-Means
# ------------------------------------------------------------------------------
opc_cluster <- list() # Opção de cluster
graph <- list()       # opção de gráfico

for(i in 1:4){
  opc_cluster[[i]] <- kmeans(dataset.pad, centers = i + 2)
  graph[[i]] <- fviz_cluster(opc_cluster[[i]],
                             geom = "point", 
                             data = dataset.pad) + 
    ggtitle(paste(i, "ª Opção - k = ",i+2, sep=""))
}

# Plotando as possibilidades predefinidas
grid.arrange(graph[[1]], graph[[2]],graph[[3]], graph[[4]], nrow = 2)

# ESCOLHA DA OPÇÃO 2 (4 Clusters)
fviz_cluster(opc_cluster[[2]],
             ellipse.type = "euclid", #Elipse concentração
             star.plot =TRUE, #adiciona segmentos dos centroides para itens,
             repel = TRUE,
             #ggtheme = theme_minimal(),
             geom = "point", 
             data = dataset.pad) + 
  ggtitle(paste("Opção - k = ",4, sep=""))

# Obs: O Método K-Means é sensível a outliers

# OBSERVAÇÃO: Importante entender "Dim1" e "Dim2" (PCA)

# Escolhendo 4 clusters (opção 2) para analisar --> Vamos ver os outputs
opc_cluster[[2]]$cluster        # Grupo por observação
opc_cluster[[2]]$centers        # Matriz com as médias (cetróides) de cada grupo
opc_cluster[[2]]$totss          # Soma dos quadrados totais
opc_cluster[[2]]$betweenss      # Soma dos quadrados entre grupos
opc_cluster[[2]]$withinss       # Soma dos quadrados dentro dos grupos
opc_cluster[[2]]$tot.withinss   # Soma dos quadrados dentro de todos os grupos
opc_cluster[[2]]$size           # Número de observasções por grupo (tamanhos)

# Percentual de Variância Explicada com 4 clusters (R2)
(opc_cluster[[2]]$betweenss / opc_cluster[[2]]$totss)

# Obs: Os dois resultados mais importantes são os tamanhos dos grupos e as médias
# Obs2: Lembrar que está padronizado!
opc_cluster[[2]]$size # tamanho
opc_cluster[[2]]$centers # médias

# Médias das variáveis de cada grupo - Médias de grupo
centers <- data.frame(cluster = factor(1:4), opc_cluster[[2]]$centers)
centers %>% ver(name_table = "Centróides com Dados Padronizados")

centers <- as.data.frame(t(centers %>% select(-cluster)))
names(centers) <- paste("Cluster", 1:4)
centers$Symbol <- row.names(centers)
centers <- gather(centers, "Cluster", "Mean", -Symbol)
centers$Color = centers$Mean > 0 
ggplot(centers, aes(x = Symbol, y = Mean, fill = Color)) +
  geom_bar(stat = "identity", position = "identity", width = .75) +
  facet_grid(Cluster ~ ., scales = 'free_y')

# Obs:
# É possível verificar que os grupos 1 e, principalmente, o 3 têm altos índices
# e os grupos 2 e 4 têm baixos índices

# Obs2: 
# Grupos desequilibrados (tamanho) podem gerar outliers distantes OU grupos
# de registros muito distintos do resto dos dados (o que necessita de maior inspeção)

# Adicionando a variável "Cluster" no dataset
dataset$cluster <- factor(opc_cluster[[2]]$cluster)
dataset.pad$cluster <- factor(opc_cluster[[2]]$cluster)
dataset %>% ver()
dataset.pad %>% ver()
# Plotando os centros dos grupos em dimensões duas a duas
# ------------------------------------------------------------------------------
# Se a análise se pautasse em apenas duas variáveis, poderíamos fazer assim:
ggplotly(
  ggplot(data = dataset.pad,
         aes(x = mortos, y = veiculos, 
             color = cluster, shape = cluster)) +
    geom_point(alpha = 0.3)
)

opc_cluster[[2]]$centers

# ------------------------------------------------------------------------------
# Classificando a base com os clusters
# ------------------------------------------------------------------------------
acidentes$municipio <- str_replace_all(acidentes$municipio, fixed(" "), "")

cluster.acidentes <- acidentes %>% 
  left_join(rownames_to_column(dataset, var = "municipio")[, c("municipio","cluster")],
            var = "municipio", by = ("municipio"))

cluster.acidentes %>% ver()

# Análise dos clusters
# ------------------------------------------------------------------------------
c1 <- dataset.pad %>% filter(cluster == 1) %>% select(-cluster)
summary(c1)
opc_cluster[[2]]$centers

dataset %>% group_by(cluster) %>% 
  summarise("N_Municípios" = n(),
            "Total_Acidentes" = mean(Qtd) %>% round(1),
            "Pessoas" = mean(pessoas) %>% round(1),
            "Mortos" = mean(mortos) %>% round(1), 
            "Veículos" = mean(veiculos) %>% round(1)) 

anos_estudados <- 14 # 2007 até 2020
acidentes_ano <- dataset %>% select(-cluster) / anos_estudados
acidentes_ano$cluster <- factor(opc_cluster[[2]]$cluster)

pct_mean <- acidentes_ano %>% group_by(cluster) %>% 
  summarise("N_Municípios" = n(),
            "Total_Acidentes" = mean(Qtd) %>% round(1),
            "Pessoas" = mean(pessoas) %>% round(1),
            "Mortos" = mean(mortos) %>% round(1), 
            "Veículos" = mean(veiculos) %>% round(1))  %>% 
  column_to_rownames("cluster")

# Cada cidade do cluster teve em média por ano:
pct_mean %>% ver(name_table = "Clusters - Médias por Ano")

pct <- list()
for(i in colnames(pct_mean)){
  pct[[i]] <-  pct_mean[[i]] / sum(pct_mean[[i]])
}

pct <- round(data.frame(
  "N_Municipios" = pct[[1]],
  "Mortos" = pct[[2]],
  "Feridos_Graves" = pct[[3]],
  "Ignorados" = pct[[4]],
  "Veiculos" = pct[[5]]
)*100, 2)
pct %>% rownames_to_column(var = "Cluster") %>% ver()

dataset %>% filter(cluster == 3) %>% arrange(desc(mortos))

# Agora, vou nas variáveis categóricas para entender os agrupamentos!

# ------------------------------------------------------------------------------
                               ## FIM ##
# ------------------------------------------------------------------------------

# Tabulação personalizada
acidentes %>% 
  tbl_cross(row = mortos,
            col = fase_dia,
            percent = "row") %>% 
  bold_labels()


#Grafo

acidentes = acidentes %>% filter(condicao_metereologica!="Ignorado")

adj_autor_temas=table(acidentes$classificacao_acidente,acidentes$condicao_metereologica)
net_autores_temas <- graph_from_incidence_matrix(adj_autor_temas)
net2=asNetwork(net_autores_temas)

df2=data.frame(table(acidentes$series))
df3=data.frame(acidentes$Profile.Name)

net2 %v% "Series" = acidentes$classificacao_acidente

n<-ggnetwork(net2)

df3=data.frame(table(cbind(acidentes$classificacao_acidente,acidentes$condicao_metereologica)))

n_merge=merge(n,df3, by.x=names(n)[5],by.y=names(df3)[1])

ggplotly(
  ggplot(n_merge, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(color = "grey50") +
    geom_nodes(aes(color = vertex.names, size=Freq)) +
    geom_nodetext(aes(color = Series, label = vertex.names),
                  fontface = "bold",size=3) +
    theme_blank()
)



dados_teste=head(acidentes,500)

m <- leaflet() %>%
  addTiles() %>%  
  addMarkers(lng=dados_teste$longitude_numero, lat=dados_teste$latitude_numero, 
             popup=dados_teste$municipio)

m
