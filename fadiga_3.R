############################################################################
#            Encontrar a melhor escala de identificação de fadiga          #
############################################################################

library("neuralnet")
library("hydroGOF")

set.seed(1234567890)

#Ler dataset do ficheiro csv e atribuir à variavel dataset
dataset <- read.csv("/home/paulo/SRCRTP3/Material/exercicio3n.csv")

# Resumo das estatisticas básicas do dataset
summary(dataset)

#Mostrar o head dos datasets(primeiros 6 resultados)
head(dataset)





head(normalized)

fatigueclust <- subset(dataset, select = c("Performance.KDTMean", "Performance.MAMean",
                                            "Performance.MVMean", "Performance.MVMean",
                                            "Performance.DDCMean", "Performance.DMSMean", 
                                            "Performance.AEDMean", "Performance.ADMSLMean",
                                            "Performance.Task"))

fatigueclust2 <- subset(dset, select = c("Performance.KDTMean", "Performance.MAMean",
                                           "Performance.MVMean", "Performance.MVMean",
                                           "Performance.DDCMean", "Performance.DMSMean", 
                                           "Performance.AEDMean", "Performance.ADMSLMean",
                                           "Performance.Task"))


#############################################################################
#                     Determinar número de clusters                         #
#############################################################################

#Gráfico auxiliar para apresentar número ideal de clusters
# nc -> nmr máximo de clusters, data -> dataset
wssplot <- function(data, nc=15, seed=1234){
              wss <- (nrow(data)-1)*sum(apply(data,2,var))
                for (i in 2:nc){
                  set.seed(seed)
                  wss[i] <- sum(kmeans(data, centers=i)$withinss)
                }
                plot(1:nc, wss, type="b", xlab="Number of Clusters",
                ylab="Within groups sum of squares")
          }


wssplot(fatigueclust)                          


#Estimativa de número ótimo de clusters
pamk(fatigueclust, krange = 2:15)


#Distribuição do nível de fadiga anterior pelos diferentes clusters obtidos
table(dataset$FatigueLevel, fit$cluster)


# K-Means Cluster Analysis
fit <- kmeans(fatigueclust, 3)

# get cluster means
aggregate(fatigueclust,by=list(fit$cluster),FUN=mean)
# Juntar novos valores de fadiga aos dados
mydata <- data.frame(fatigueclust, fit$cluster)
mydata


##############################################################################
#                         Plotting das soluções
##############################################################################

# ClustPot (fazer apenas com parâmetros com mais peso)
library(cluster) 
clusplot(fatigueclust, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Centroid Plot
library(fpc)
plotcluster(fatigueclust, fit$cluster)



