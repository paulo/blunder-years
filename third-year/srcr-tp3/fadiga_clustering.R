############################################################################
#            Encontrar a melhor escala de identificação de fadiga          #
############################################################################


#Usar biblioteca "neuralnet" para utilização de Redes Neuronais
library("neuralnet")
library("hydroGOF")

#Ler dataset do ficheiro csv e atribuir à variavel dataset
dataset <- read.csv("/home/paulo/SRCRTP3/Material/exercicio3n.csv")

#Mostrar o head do dataset(primeiros 6 resultados)
head(dataset)

#Extrair 600 casos do dataset para um novo dataset que será usado para treinar a Rede Neuronal 
trainset <- dataset[1:600, ]

#Extrair as restantes entradas do dataset para um dataset que será usado para testar a Rede Neuronal
testset <- dataset[601:844, ]

scaleset <- dataset[1:20, ]

transdata <- t(clustset)


kc <- kmeans(clustset, 3)

plot(dataset[c("FatigueLevel", "Performance.KDTMean")], col=kc$cluster)

points(kc$centers[,c("FatigueLevel", "Performance.KDTMean")], col=1:3, pch=5, cex=2)



# Ward Hierarchical Clustering
d <- dist(transdata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D") 

plot(fit) # display dendogram

groups <- cutree(fit, k=5) # cut tree into 5 clusters

# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")


clustset <- subset(dataset, select = c("Performance.KDTMean", "Performance.MAMean",
                                           "Performance.MVMean", "Performance.MVMean",
                                           "Performance.DDCMean", "Performance.DMSMean", 
                                           "Performance.AEDMean", "Performance.ADMSLMean",
                                           "Performance.Task"))



#Variaveis sobre as quais a função vai incidir (todas menos "FatigueLevel" e "Performance.Task")
variables <- c("Performance.KDTMean", "Performance.MAMean", "Performance.MVMean", "Performance.TBCMean",
               "Performance.DDCMean", "Performance.DMSMean", "Performance.AEDMean", "Performance.ADMSLMean",
               "Performance.Task")

#Variável a ser medida
mVar <- "FatigueLevel"

#Criar fórmula de treino neuronal (soma das váriaveis)
f <- as.formula(paste(mVar, paste(variables, collapse=" + "), sep=" ~ "))

#hidden -> número de neurónios por camada (é um array)
#threshold -> limite de erro
#Threshold is a numeric value specifying the threshold for the partial derivatives of the error function as stopping criteria.
# Treinar a rede neuronal para usar todas as variáveis como input e produzir a variável "Fadiga" como output
performancenet <- neuralnet(f, trainset, hidden = c(5), lifesign = "minimal", 
                            linear.output = FALSE, threshold = 0.01)
# hidden: 4, 2    thresh: 0.01    rep: 1/1    steps:      33  error: 1414.50887  time: 0.1 secs

# Desenhar a Rede Neuronal
plot(performancenet, rep = "best")

## Definir variaveis de input para teste (todas menos FatigueLevel(output) e tipo de task)
fatigue_test <- subset(testset, select = c("Performance.KDTMean", "Performance.MAMean",
                                           "Performance.MVMean", "Performance.MVMean",
                                           "Performance.DDCMean", "Performance.DMSMean", 
                                           "Performance.AEDMean", "Performance.ADMSLMean",
                                           "Performance.Task"))

#Testar a rede com os novos casos
performancenet.results <- compute(performancenet, fatigue_test)

#Declarar results
results <- data.frame(actual = testset$FatigueLevel, prediction = performancenet.results$net.result)

#Imprimir results
results

#Imprimir resultados arrendondados
results$prediction <- round(results$prediction)