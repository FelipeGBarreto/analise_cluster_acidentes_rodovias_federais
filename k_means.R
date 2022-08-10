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

#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# BASE DE DADOS
#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#

# Conhecendo o dataset
path <- "/Users/felipebarreto/Desktop/"
path_pasta <- paste0(path,'TCC/')

df_completo <- read.csv2(paste0(path,'dados_acidentes_tratados_completo.csv'), sep = ',')
df <- read.csv2(paste0(path,'dados_acidentes_tratados_selecionadas.csv'), sep = ',')
var_quanti <- read.csv2(paste0(path,'TCC/Bases geradas/var_quanti_agg.csv'), sep = ',')

####clustering = var_quanti

ver(var_quanti)

###############################################################################

#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# Análises descritivas / Selecionando variáveis desejadas
#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#

## Ver a correlação entre as variáveis para entender quais podem ser retiradas
chart.Correlation(clustering[,2:ncol(clustering)], method = "pearson")

library(psych)
corPlot(clustering[,2:ncol(clustering)], cex = .6)
# comparação das variáveis com frequência

## IDEAL: Estudo de correlação por CLUSTER!!
clustering %>% str
clustering %>% summary


# Qual a cidade que mais aparece no mapa de acidentes?
clustering %>% filter(freq == max(clustering$freq))
clustering %>% filter(freq == max(clustering$mortos))

# Qual a cidade que mais aparece no mapa de acidentes?
clustering %>% filter(freq == min(clustering$freq))

# Colocando a label de municipio para index
retirar_cols <- c('veiculos','feridos','pessoas')

df_quanti <- clustering %>% select(-retirar_cols) %>% column_to_rownames('municipio')
corPlot(df_quanti[,2:ncol(df_quanti)], cex = .6)

# Verificando se há valores vazios. Como não há, vamos seguir em frente
df_quanti %>% aggr(plot = F)
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
  geom_vline(xintercept = 7,linetype = 2) +
  labs(title = "Número Ótimo de Clusters - K-Means")
)

# Método da Silhouette --> Quão bom foi o agrupamento com a alocação da observação
# Quanto maior for o coef. silhouette, melhor, já que favorece o agrupamento.
metodo_cotovolo <- (
  fviz_nbclust(
    df.quanti.pad,
    kmeans,
    method = "silhouette") +
  geom_vline(xintercept = 7, linetype = 2) +
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
    geom_vline(xintercept = 7,linetype = 3) + 
    labs(x = "Número Ótimo de Clusters",
         y = "Percentual de Variância Total Explicada",
         title = "Percentual de Variância Explicada")
)
plot_pct_var_explicada

#--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--.--#
# Cluster pelo Método K-Means - (ESCOLHI 7 CLUSTERS)
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
    mortos = mortos_1,
    feridos_leves = feridos_leves_1,
    feridos_graves = feridos_graves_1,
    ilesos = ilesos_1,
    ignorados = ignorados_1,
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

##### share de cada variável pela média de acidentes #####
# Representatividade média de cada variável
df.quanti.mean.share <- df.quanti.mean %>% 
  mutate(
    mortos = round(100 * mortos / freq, 2),
    feridos_leves = round(100 * feridos_leves / freq, 2),
    feridos_graves = round(100 * feridos_graves / freq, 2),
    ilesos = round(100 * ilesos / freq, 2)
  ) %>% 
  select(cluster,freq,ilesos,feridos_leves,feridos_graves,mortos) %>% 
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
df.quanti.mean.share %>% 
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
    ggtitle("Share de Médias com a frequência de acidentes (%)")
) 

#### OBSERVAÇÃO: o avg.graph.1 não é interessante para a interpretação!!

# Plotando os centros dos grupos em dimensões duas a duas
# ------------------------------------------------------------------------------
# Se a análise se pautasse em apenas duas variáveis, poderíamos fazer assim:
df.quanti.pad$cluster <- factor(opc_cluster[[opcao_clusters]]$cluster)

ggplotly(
  ggplot(data = df.quanti.pad,
         aes(x = mortos, y = freq, 
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
df.quanti.mean %>% 
  tbl_cross(row = cluster,
            col = mortos,
            percent = "row") %>% 
  bold_labels()
