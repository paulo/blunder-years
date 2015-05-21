############################################################################
#                         Análise de Regression
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

#Variaveis sobre as quais a função vai incidir (todas menos "FatigueLevel" e "Performance.Task")
variables <- c("Performance.KDTMean", "Performance.MAMean", "Performance.MVMean", "Performance.TBCMean",
               "Performance.DDCMean", "Performance.DMSMean", "Performance.AEDMean", "Performance.ADMSLMean",
               "Performance.Task")

variables2 <- c("Performance.KDTMean", "Performance.MAMean", "Performance.MVMean", "Performance.DMSMean", 
                "Performance.Task")

#Variável a ser medida
mVar <- "FatigueLevel"

#Criar fórmula de treino neuronal (soma das váriaveis)
f <- as.formula(paste(mVar, paste(variables, collapse=" + "), sep=" ~ "))

f2 <- as.formula(paste(mVar, paste(variables2, collapse=" + "), sep=" ~ "))

#hidden -> número de neurónios por camada (é um array)
#threshold -> limite de erro
#Threshold is a numeric value specifying the threshold for the partial derivatives of the error function as stopping criteria.
# Treinar a rede neuronal para usar todas as variáveis como input e produzir a variável "Fadiga" como output
performancenet <- neuralnet(f, trainset, hidden = c(4, 2), lifesign = "minimal", 
                            linear.output = FALSE, threshold = 0.1)

performancenet <- neuralnet(f2, trainset, hidden = c(2, 1), lifesign = "minimal", 
                            linear.output = FALSE, threshold = 0.1)
# hidden: 4, 2    thresh: 0.01    rep: 1/1    steps:      33  error: 1414.50887  time: 0.1 secs

# Desenhar a Rede Neuronal
plot(performancenet, rep = "best")

## Definir variaveis de input para teste (todas menos FatigueLevel(output))
fatigue_test <- subset(testset, select = c("Performance.KDTMean", "Performance.MAMean",
                                           "Performance.MVMean", "Performance.MVMean",
                                           "Performance.DDCMean", "Performance.DMSMean", 
                                           "Performance.AEDMean", "Performance.ADMSLMean",
                                           "Performance.Task"))

fatigue_test2 <- subset(testset, select = c("Performance.KDTMean", "Performance.MAMean",
                                           "Performance.MVMean", "Performance.DMSMean", 
                                           "Performance.Task"))



#Testar a rede com os novos casos
performancenet.results <- compute(performancenet, fatigue_test)

performancenet.results <- compute(performancenet, fatigue_test2)

#Declarar results
results <- data.frame(actual = testset$FatigueLevel, prediction = performancenet.results$net.result)

#Imprimir results
results

#Imprimir resultados arrendondados
results$prediction <- round(results$prediction)
results













with(dataset, cor(dataset$FatigueLevel, dataset$Performance.Task))

#Calcular o "root-mean-square deviation" 
rmse(c(testset$FatigueLevel),c(results$prediction))

# Resumo das estatisticas básicas do dataset
summary(dataset)

# Gráfico da Fadiga em função do KDTMean
plot(testset$Performance.KDTMean, testset$FatigueLevel, xlab="KDTMean", ylab="Fatigue",)

abline(lm(testset$Performance.KDTMean ~ testset$FatigueLevel))

#Correlação linear

cor(testset$Performance.KDTMean + testset$Performance.MAMean +
      testset$Performance.MVMean + testset$Performance.MVMean +
      testset$Performance.DDCMean + testset$Performance.DMSMean + 
      testset$Performance.AEDMean + testset$Performance.ADMSLMean, testset$FatigueLevel)