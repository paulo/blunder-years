########################################################################################
#                     Identificar variáveis mais relevantes                            #
########################################################################################

#Usar biblioteca "neuralnet" para utilização de Redes Neuronais
library("neuralnet")
library("hydroGOF")

set.seed(1234567890)

#Ler dataset do ficheiro csv e atribuir à variavel dataset
dataset <- read.csv("/home/paulo/SRCRTP3/Material/exercicio3n.csv")

#Dataset invertido
dataseti <- dataset[nrow(dataset):1, ]

#Extrair n casos do dataset para um novo dataset que será usado para treinar a Rede Neuronal 
trainset1 <- dataset[1:400, ] #extrair 400 casos
trainset2 <- dataset[1:500, ] #extrair 500 casos
trainset3 <- dataset[1:600, ] #extrair 600 casos

trainset1i <- dataseti[1:400, ] #extrair 400 casos invertido
trainset2i <- dataseti[1:500, ] #extrair 500 casos invertido
trainset3i <- dataseti[1:600, ] #extrair 600 casos invertido


#Extrair as restantes entradas do dataset para um dataset que será usado para testar a Rede Neuronal
testset1 <- dataset[401:844, ]
testset2 <- dataset[501:844, ]
testset3 <- dataset[601:844, ]

testset1i <- dataseti[401:844, ] # invertido
testset2i <- dataseti[501:844, ] # invertido
testset3i <- dataseti[601:844, ] # invertido

#Tipos d"e algoritmos de aprendizagem a usar
alg2 <- "rprop+"
alg3 <- "rprop-"
alg4 <- "sag"
alg5 <- "slr"

#Variáveis sobre as quais a função vai incidir (mais relevantes)
variablesR <- c("Performance.KDTMean", "Performance.MAMean", "Performance.DDCMean", "Performance.Task") #arranjar com novas variaveis descobertas

#Variáveis menos relevantes
variablesN <- c("Performance.MVMean", "Performance.TBCMean", "Performance.AEDMean", "Performance.ADMSLMean")


#Variável a ser medida
mVar <- "FatigueLevel"

#Criar fórmula de treino neuronal (soma das váriaveis)
f <- as.formula(paste(mVar, paste(variablesR, collapse=" + "), sep=" ~ "))
fn <- as.formula(paste(mVar, paste(variablesN, collapse=" + "), sep=" ~ "))

#Disposição dos neurónios na rede neuronal
nn0 <- c(3)
nn1 <- c(10)
nn2 <- c(20, 10)
nn3 <- c(40, 20)

# Treinar a rede neuronal para usar todas as variáveis como input e produzir a variável "Fadiga" como output
performancenet1 <- neuralnet(fn, trainset1i, hidden = nn3, lifesign = "minimal", algorithm = alg2,
                            linear.output = TRUE, threshold = 0.01)

performancenet2 <- neuralnet(fn, trainset2, hidden = nn3, lifesign = "minimal", algorithm = alg3,
                             linear.output = TRUE, threshold = 0.01)

performancenet3 <- neuralnet(fn, trainset3, hidden = nn0, lifesign = "minimal", algorithm = alg4,
                             linear.output = TRUE, threshold = 0.01)

performancenet4 <- neuralnet(fn, trainset1, hidden = nn0, lifesign = "minimal", algorithm = alg5,
                             linear.output = TRUE, threshold = 0.01)


## Definir variaveis de input para teste (todas menos "FatigueLevel" que é a variável de output)
fatigue_test1 <- subset(testset1i, select = variablesN)
fatigue_test2 <- subset(testset2, select = variablesN)
fatigue_test3 <- subset(testset3, select = variablesN)
fatigue_test4 <- subset(testset1, select = variablesN)


#Testar a rede com os casos de teste
performancenet1.results <- compute(performancenet1, fatigue_test1)
performancenet2.results <- compute(performancenet2, fatigue_test2)
performancenet3.results <- compute(performancenet3, fatigue_test3)
performancenet4.results <- compute(performancenet4, fatigue_test4)


#Declarar results
results1 <- data.frame(actual = testset1i$FatigueLevel, prediction = performancenet1.results$net.result)
results2 <- data.frame(actual = testset2$FatigueLevel, prediction = performancenet2.results$net.result)
results3 <- data.frame(actual = testset3$FatigueLevel, prediction = performancenet3.results$net.result)
results4 <- data.frame(actual = testset1$FatigueLevel, prediction = performancenet4.results$net.result)

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
rmse(c(testset1i$FatigueLevel),c(results1$prediction))
rmse(c(testset2$FatigueLevel),c(results2$prediction))
rmse(c(testset3$FatigueLevel),c(results3$prediction))
rmse(c(testset1$FatigueLevel),c(results4$prediction))

#Calcular o "percentage-bias"
pbias(results1$prediction, testset1i$FatigueLevel)
pbias(results2$prediction, testset2$FatigueLevel)
pbias(results3$prediction, testset3$FatigueLevel)
pbias(results4$prediction, testset1$FatigueLevel)
