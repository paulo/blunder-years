##Nota: Não está a ser tida em conta a váriavel Performance.Task
#Usar biblioteca "neuralnet" para utilização de Redes Neuronais
library("neuralnet")

#Ler dataset do ficheiro csv e atribuir à variavel dataset
dataset <- read.csv("/home/paulo/SRCRTP3/Material/exercicio3.csv")

#Mostrar o head do dataset(primeiros 6 resultados)
#head(dataset)

#Extrair 800 casos do dataset para um novo dataset que será usado para treinar a Rede Neuronal 
trainset <- dataset[1:800, ]

#Extrair as restantes entradas do dataset para um dataset que será usado para testar a Rede Neuronal
testset <- dataset[801:2000, ]

#Variaveis sobre as quais a função vai incidir (todas menos "FatigueLevel" e "Performance.Task")
variables <- c("Performance.KDTMean", "Performance.MAMean", "Performance.MVMean", "Performance.TBCMean", 
            "Performance.DDCMean", "Performance.DMSMean", "Performance.AEDMean", "Performance.ADMSLMean")

#Variável a ser medida
mVar <- "FatigueLevel"

#Criar fórmula de treino neuronal (soma das váriaveis)
f <- as.formula(paste(mVar, paste(variables, collapse=" + "), sep=" ~ "))
                 
#hidden -> número de neurónios por camada (é um array)
#Threshold is a numeric value specifying the threshold for the partial derivatives of the error function as stopping criteria.
# Treinar a rede neuronal para usar todas as variáveis como input e produzir a variável "Fadiga" como output
performancenet <- neuralnet(f, trainset, hidden = c(4), lifesign = "minimal", 
                       linear.output = FALSE, threshold = 0.01)
# hidden: 4, 2    thresh: 0.01    rep: 1/1    steps:      33  error: 1414.50887	time: 0.1 secs

# Desenhar a Rede Neuronal
plot(performancenet, rep = "best")

## Definir variaveis de input para teste
fatigue_test <- subset(testset, select = c("Performance.KDTMean", "Performance.MAMean",
                                           "Performance.MVMean", "Performance.MVMean",
                                           "Performance.DDCMean", "Performance.DMSMean", 
                                           "Performance.AEDMean", "Performance.ADMSLMean"))

#Testar a rede com os novos casos
performance.results <- compute(performancenet, fatigue_test)

#Declarar results
results <- data.frame(actual = testset$FatigueLevel, prediction = performance.results$net.result)

#Imprimir results
results

#Imprimir resultados arrendondados
results$prediction <- round(results$prediction)
results$prediction

#Calcular o "root-mean-square deviation" 
rmse(c(testset$default10yr),c(results$prediction))