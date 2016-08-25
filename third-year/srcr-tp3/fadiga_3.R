############################################################################
#            Encontrar a melhor escala de identificação de fadiga          #
############################################################################

library("neuralnet")
library("hydroGOF")
library("fpc")

set.seed(1234567890)

#Ler dataset do ficheiro csv e atribuir à variavel dataset
dataset <- read.csv("/home/paulo/SRCRTP3/Material/exercicio3n.csv")

fatigueclust <- subset(dataset, select = c("Performance.KDTMean", "Performance.MAMean", "Performance.MVMean", "Performance.TBCMean",
                                           "Performance.DDCMean", "Performance.DMSMean", "Performance.AEDMean", "Performance.ADMSLMean",
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

# K-Means Cluster Analysis
fit <- kmeans(fatigueclust, 3)

#Distribuição do nível de fadiga anterior pelos diferentes clusters obtidos
table(dataset$FatigueLevel, fit$cluster)

# get cluster means
aggregate(fatigueclust,by=list(fit$cluster),FUN=mean)

# Juntar novos valores de fadiga aos dados
dataclustered <- data.frame(fatigueclust, fit$cluster)
dataclustered


##############################################################################
#                         Plotting das soluções                              #
##############################################################################

# ClustPot (fazer apenas com parâmetros com mais peso)
library(cluster) 
clusplot(fatigueclust, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Centroid Plot
library(fpc)
plotcluster(fatigueclust, fit$cluster)



##############################################################################
#                         Testes com os novos dados                          #
##############################################################################

#Dataset invertido
ndataset <- dataclustered
ndataseti <- ndataset[nrow(ndataset):1, ]

#Extrair n casos do dataset para um novo dataset que será usado para treinar a Rede Neuronal 
trainset1 <- ndataset[1:400, ] #extrair 400 casos
trainset2 <- ndataset[1:500, ] #extrair 500 casos
trainset3 <- ndataset[1:600, ] #extrair 600 casos

trainset1i <- ndataseti[1:400, ] #extrair 400 casos invertido
trainset2i <- ndataseti[1:500, ] #extrair 500 casos invertido
trainset3i <- ndataseti[1:600, ] #extrair 600 casos invertido


#Extrair as restantes entradas do dataset para um dataset que será usado para testar a Rede Neuronal
testset1 <- ndataset[401:844, ]
testset2 <- ndataset[501:844, ]
testset3 <- ndataset[601:844, ]

testset1i <- ndataseti[401:844, ] # invertido
testset2i <- ndataseti[501:844, ] # invertido
testset3i <- ndataseti[601:844, ] # invertido

#Tipos d"e algoritmos de aprendizagem a usar
alg2 <- "rprop+"
alg3 <- "rprop-"
alg4 <- "sag"
alg5 <- "slr"

#Variáveis sobre as quais a função vai incidir
variables <- c("Performance.KDTMean", "Performance.MAMean", "Performance.MVMean", "Performance.TBCMean",
               "Performance.DDCMean", "Performance.DMSMean", "Performance.AEDMean", "Performance.ADMSLMean",
               "Performance.Task")


#Variáveis mais relevantes
variablesR <- c("Performance.KDTMean", "Performance.MAMean", "Performance.DDCMean", "Performance.Task")


#Variável a ser medida
mVar <- "fit.cluster"

#Criar fórmula de treino neuronal (soma das váriaveis)
f <- as.formula(paste(mVar, paste(variables, collapse=" + "), sep=" ~ "))
fr <- as.formula(paste(mVar, paste(variablesR, collapse=" + "), sep=" ~ "))

#Disposição dos neurónios na rede neuronal
nn0 <- c(3)
nn1 <- c(10)
nn2 <- c(20, 10)
nn3 <- c(40, 20)

# Treinar a rede neuronal para usar todas as variáveis como input e produzir a variável "Fadiga" como output
performancenet1 <- neuralnet(fr, trainset1i, hidden = nn3, lifesign = "minimal", algorithm = alg2,
                             linear.output = TRUE, threshold = 0.01)

performancenet2 <- neuralnet(fr, trainset2, hidden = nn3, lifesign = "minimal", algorithm = alg3,
                             linear.output = TRUE, threshold = 0.01)

performancenet3 <- neuralnet(fr, trainset3, hidden = nn0, lifesign = "minimal", algorithm = alg4,
                             linear.output = TRUE, threshold = 0.01)

performancenet4 <- neuralnet(fr, trainset1, hidden = nn0, lifesign = "minimal", algorithm = alg5,
                             linear.output = TRUE, threshold = 0.01)


## Definir variaveis de input para teste (todas menos "FatigueLevel" que é a variável de output)
fatigue_test1 <- subset(testset1i, select = variablesR)
fatigue_test2 <- subset(testset2, select = variablesR)
fatigue_test3 <- subset(testset3, select = variablesR)
fatigue_test4 <- subset(testset1, select = variablesR)


#Testar a rede com os casos de teste
performancenet1.results <- compute(performancenet1, fatigue_test1)
performancenet2.results <- compute(performancenet2, fatigue_test2)
performancenet3.results <- compute(performancenet3, fatigue_test3)
performancenet4.results <- compute(performancenet4, fatigue_test4)


#Declarar results
results1 <- data.frame(actual = testset1i$fit.clust, prediction = performancenet1.results$net.result)
results2 <- data.frame(actual = testset2$fit.clust, prediction = performancenet2.results$net.result)
results3 <- data.frame(actual = testset3$fit.clust, prediction = performancenet3.results$net.result)
results4 <- data.frame(actual = testset1$fit.clust, prediction = performancenet4.results$net.result)

#Imprimir resultados arrendondados
results1$prediction <- round(results1$prediction)
results2$prediction <- round(results2$prediction)
results3$prediction <- round(results3$prediction)
results4$prediction <- round(results4$prediction)

results1
results2
results3
results4

#Calcular o "root-mean-square deviation" 
rmse(c(testset1i$fit.clust),c(results1$prediction))
rmse(c(testset2$fit.clust),c(results2$prediction))
rmse(c(testset3$fit.clust),c(results3$prediction))
rmse(c(testset1$fit.clust),c(results4$prediction))

#Calcular o "percentage-bias"
pbias(results1$prediction, testset1i$fit.clust)
pbias(results2$prediction, testset2$fit.clust)
pbias(results3$prediction, testset3$fit.clust)
pbias(results4$prediction, testset1$fit.clust)



##############################################################################
#                 Testes de presença ou ausência de fadiga                   #
##############################################################################


#Substituir valores de fadiga por 0 e 1 (tem de ser por esta ordem)
ndataset$fit.clust[ndataset$fit.clust <= 2] <- 0
ndataset$fit.clust[ndataset$fit.clust > 2] <- 1

ndataseti$fit.clust[ndataseti$fit.clust <= 2] <- 0
ndataseti$fit.clust[ndataseti$fit.clust > 2] <- 1

#Extrair n casos do dataset para um novo dataset que será usado para treinar a Rede Neuronal 
ntrainset <- ndataset[1:400, ] #extrair 400 casos
ntrainseti <- ndataseti[1:400, ] #extrair 400 casos

#Variáveis sobre as quais a função vai incidir (todas menos "FatigueLevel" que é a variável output)
variables <- c("Performance.KDTMean", "Performance.MAMean", "Performance.MVMean", "Performance.TBCMean",
               "Performance.DDCMean", "Performance.DMSMean", "Performance.AEDMean", "Performance.ADMSLMean",
               "Performance.Task")


#Variável a ser medida
mVar <- "fit.clust"

#Criar fórmula de treino neuronal (soma das váriaveis)
f <- as.formula(paste(mVar, paste(variables, collapse=" + "), sep=" ~ "))


#Testes
test1 <-data.frame(Performance.KDTMean=0.0035540704,
                   Performance.MAMean=0.1244468065,
                   Performance.MVMean=0.1136319403,
                   Performance.TBCMean=0.0478007137,
                   Performance.DDCMean=-0.0164877513,
                   Performance.DMSMean=0.015593045,
                   Performance.AEDMean=0.0003232315,
                   Performance.ADMSLMean=0.1478848995,
                   Performance.Task=1)

test3 <-data.frame(Performance.KDTMean=0.0003860245,
                   Performance.MAMean=0.0712769783,
                   Performance.MVMean=-0.0305269427,
                   Performance.TBCMean=0.0195957896,
                   Performance.DDCMean=0.0394563841,
                   Performance.DMSMean=-0.0079845752,
                   Performance.AEDMean=-0.008174305,
                   Performance.ADMSLMean=0.0140264185,
                   Performance.Task=1)

test6 <-data.frame(Performance.KDTMean=0.0056120995,
                   Performance.MAMean=-0.0025835316,
                   Performance.MVMean=-0.0019763726,
                   Performance.TBCMean=-0.0381802676,
                   Performance.DDCMean=0.0241317396,
                   Performance.DMSMean=-0.0027214447,
                   Performance.AEDMean=-0.0409258567,
                   Performance.ADMSLMean=0.023098876,
                   Performance.Task=2)

testx <-data.frame(Performance.KDTMean=1,
                   Performance.MAMean=-1,
                   Performance.MVMean=-1,
                   Performance.TBCMean=-1,
                   Performance.DDCMean=1,
                   Performance.DMSMean=-1,
                   Performance.AEDMean=-1,
                   Performance.ADMSLMean=1,
                   Performance.Task=2)

testy <-data.frame(Performance.KDTMean=-1,
                   Performance.MAMean=--1,
                   Performance.MVMean=--1,
                   Performance.TBCMean=--1,
                   Performance.DDCMean=-1,
                   Performance.DMSMean=--1,
                   Performance.AEDMean=--1,
                   Performance.ADMSLMean=-1,
                   Performance.Task=2)


#Disposição dos neurónios na rede neuronal
nn0 <- c(3)
nn1 <- c(40, 20)

#Algorimos
alg2 <- "rprop+"
alg5 <- "slr"

# Treinar a rede neuronal para usar todas as variáveis como input e produzir a variável "Fadiga" como output
performancenet1 <- neuralnet(f, ntrainseti, hidden = nn1, lifesign = "minimal", algorithm = alg2,
                             linear.output = TRUE, threshold = 0.01)

performancenet2 <- neuralnet(f, ntrainset, hidden = nn0, lifesign = "minimal", algorithm = alg5,
                             linear.output = TRUE, threshold = 0.01)



#Testar a rede com os casos de teste
performancenet1.results1 <- compute(performancenet1, test1)
performancenet1.results2 <- compute(performancenet1, test3)
performancenet1.results3 <- compute(performancenet1, test6)
performancenet1.results4 <- compute(performancenet1, testx)
performancenet1.results5 <- compute(performancenet1, testy)

performancenet2.results1 <- compute(performancenet2, test1)
performancenet2.results2 <- compute(performancenet2, test3)
performancenet2.results3 <- compute(performancenet2, test6)
performancenet2.results4 <- compute(performancenet2, testx)
performancenet2.results5 <- compute(performancenet2, testy)


#Declarar results
results11 <- round(performancenet1.results1$net.result)
results12 <- round(performancenet1.results2$net.result)
results13 <- round(performancenet1.results3$net.result)
results14 <- round(performancenet1.results4$net.result)
results15 <- round(performancenet1.results5$net.result)

results21 <- round(performancenet2.results1$net.result)
results22 <- round(performancenet2.results2$net.result)
results23 <- round(performancenet2.results3$net.result)
results24 <- round(performancenet2.results4$net.result)
results25 <- round(performancenet2.results5$net.result)

#Imprimir resultados arrendondados
results11
results12
results13
results14
results15

results21
results22
results23
results24
results25













