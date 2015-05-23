############################################################################
#            Encontrar a melhor escala de identificação de fadiga          #
############################################################################

#Ler dataset do ficheiro csv e atribuir à variavel dataset
dataset <- read.csv("/home/paulo/SRCRTP3/Material/exercicio3n.csv")

# Resumo das estatisticas básicas do dataset
summary(dataset)

#Mostrar o head dos datasets(primeiros 6 resultados)
head(dataset)


#Substituir valores de fadiga por 0 e 1 (tem de ser por esta ordem)
dataset$FatigueLevel[dataset$FatigueLevel <= 3] <- 0
dataset$FatigueLevel[dataset$FatigueLevel > 3] <- 1


#Example Data

dset <- dataset[1:100, ]

#Normalized Data
normalized = (dataset-min(dataset))/(max(dataset)-min(dataset))

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


#Gráfico auxiliar para apresentar número ideal de clusters
wssplot <- function(data, nc=15, seed=1234){
              wss <- (nrow(data)-1)*sum(apply(data,2,var))
              for (i in 2:nc){
                set.seed(seed)
                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
            plot(1:nc, wss, type="b", xlab="Number of Clusters",
            ylab="Within groups sum of squares")}



#1 standardize data

df <- scale(fatigueclust[-1])

#2 determine number of clusters
wssplot(df)    
wssplot(fatigueclust)                                                #2

library(NbClust)

set.seed(1234)

nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
nc <- NbClust(fatigueclust, min.nc=2, max.nc=15, method="kmeans")

table(nc$Best.n[1,])



barplot(table(nc$Best.n[1,]),
          xlab="Número de Clusters", ylab="Número de Critérios",
          main="Número de clusters escolhidos por 26 critérios")

set.seed(1234)

#3 K-means cluster analysis

