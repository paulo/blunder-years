############################################################################
#           Identificar a existência ou ausência de fadiga                 #
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

#Substituir valores de fadiga por 0 e 1 (tem de ser por esta ordem)
dataset$FatigueLevel[dataset$FatigueLevel <= 3] <- 0
dataset$FatigueLevel[dataset$FatigueLevel > 3] <- 1


#Extrair n casos do dataset para um novo dataset que será usado para treinar a Rede Neuronal 
trainset <- dataset[1:600, ] #extrair 600 casos


#Extrair as restantes entradas do dataset para um dataset que será usado para testar a Rede Neuronal
testset <- dataset[601:844, ]


#Variáveis sobre as quais a função vai incidir (todas menos "FatigueLevel" que é a variável output)
variables <- c("Performance.KDTMean", "Performance.MAMean", "Performance.MVMean", "Performance.TBCMean",
               "Performance.DDCMean", "Performance.DMSMean", "Performance.AEDMean", "Performance.ADMSLMean",
               "Performance.Task")

variablesR <- c("Performance.KDTMean", "Performance.Task")


#Variável a ser medida
mVar <- "FatigueLevel"

#Criar fórmula de treino neuronal (soma das váriaveis)
f <- as.formula(paste(mVar, paste(variables, collapse=" + "), sep=" ~ "))
fr <- as.formula(paste(mVar, paste(variablesR, collapse=" + "), sep=" ~ "))


#Disposição dos neurónios na rede neuronal
nn <- c(40, 20)


# Treinar a rede neuronal para usar todas as variáveis como input e produzir a variável "Fadiga" como output
performancenet <- neuralnet(f, trainset, hidden = nn, lifesign = "minimal",
                            linear.output = TRUE, threshold = 0.01)


# Desenhar a Rede Neuronal
plot(performancenet, rep = "best")


## Definir variaveis de input para teste (todas menos "FatigueLevel" que é a variável de output)
fatigue_test <- subset(testset, select = variables)


#Testar a rede com os casos de teste
performancenet.results <- compute(performancenet, fatigue_test)


#Declarar results
results <- data.frame(actual = testset$FatigueLevel, prediction = performancenet.results$net.result)

#Imprimir results
results

#Imprimir resultados arrendondados
results$prediction <- round(results$prediction)
results


#Calcular o "root-mean-square deviation" 
rmse(c(testset$FatigueLevel),c(results$prediction))


#Normalizar dados
normalized = (dataset-min(dataset))/(max(dataset)-min(dataset))


# Talvez apenas normalizar o output após obter os resultados

plot(dataset$FatigueLevel ~ dataset$Performance.KDTMean, data = dataset)

abline(dataset)
