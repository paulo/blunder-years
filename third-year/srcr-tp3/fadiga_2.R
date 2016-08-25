############################################################################
#           Identificar a existência ou ausência de fadiga                 #
############################################################################

library("neuralnet")
library("hydroGOF")

set.seed(1234567890)

#Ler dataset do ficheiro csv e atribuir à variavel dataset
dataset <- read.csv("/home/paulo/SRCRTP3/Material/exercicio3n.csv")

#Dataset invertido
dataseti <- dataset[nrow(dataset):1, ]

#Substituir valores de fadiga por 0 e 1 (tem de ser por esta ordem)
dataset$FatigueLevel[dataset$FatigueLevel <= 3] <- 0
dataset$FatigueLevel[dataset$FatigueLevel > 3] <- 1

dataseti$FatigueLevel[dataseti$FatigueLevel <= 3] <- 0
dataseti$FatigueLevel[dataseti$FatigueLevel > 3] <- 1

#Extrair n casos do dataset para um novo dataset que será usado para treinar a Rede Neuronal 
trainset <- dataset[1:400, ] #extrair 400 casos
trainseti <- dataseti[1:400, ] #extrair 400 casos

#Variáveis sobre as quais a função vai incidir (todas menos "FatigueLevel" que é a variável output)
variables <- c("Performance.KDTMean", "Performance.MAMean", "Performance.MVMean", "Performance.TBCMean",
               "Performance.DDCMean", "Performance.DMSMean", "Performance.AEDMean", "Performance.ADMSLMean",
               "Performance.Task")


#Variável a ser medida
mVar <- "FatigueLevel"

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
performancenet1 <- neuralnet(f, trainseti, hidden = nn1, lifesign = "minimal", algorithm = alg2,
                             linear.output = TRUE, threshold = 0.01)

performancenet2 <- neuralnet(f, trainset, hidden = nn0, lifesign = "minimal", algorithm = alg5,
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



#Calcular o "root-mean-square deviation" 
rmse(c(testset$FatigueLevel),c(results$prediction))


#Calcular o "percentage-bias" (tendência média que os valores testados têm em ser maiores ou menores que os originais)
pbias(results$prediction, testset1$FatigueLevel)




#Normalizar dados
normalized = (dataset-min(dataset))/(max(dataset)-min(dataset))


# Talvez apenas normalizar o output após obter os resultados
plot(dataset$FatigueLevel ~ dataset$Performance.KDTMean, data = dataset)

abline(dataset)
