########################################################################################
#                     Indentificar os 7 níveis de fadiga                               #
########################################################################################

#Usar biblioteca "neuralnet" para utilização de Redes Neuronais
library("neuralnet")
library("hydroGOF")

set.seed(1234567890)

#Ler dataset do ficheiro csv e atribuir à variavel dataset
dataset <- read.csv("/home/paulo/SRCRTP3/Material/exercicio3n.csv")

#Dataset invertido
dataseti <- dataset[nrow(dataset):1, ]

# Resumo das estatisticas básicas do dataset
summary(dataset)

#Mostrar o head dos datasets(primeiros e últimos 6 resultados)
head(dataset)
head(dataseti)

#Extrair n casos do dataset para um novo dataset que será usado para treinar a Rede Neuronal 
poser<- dataset[1:100, ]

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
alg2 <- "rprop+" #por defeito
alg3 <- "rprop-" #funciona
alg4 <- "sag" #funciona
alg5 <- "slr"

#Variáveis sobre as quais a função vai incidir (todas menos "FatigueLevel" que é a variável output)
variables <- c("Performance.KDTMean", "Performance.MAMean", "Performance.MVMean", "Performance.TBCMean",
               "Performance.DDCMean", "Performance.DMSMean", "Performance.AEDMean", "Performance.ADMSLMean",
               "Performance.Task")

variablesR <- c("Performance.KDTMean", "Performance.MAMean", "Performance.DDCMean", "Performance.Task") #arranjar com novas variaveis descobertas



#Variável a ser medida
mVar <- "FatigueLevel"

#Criar fórmula de treino neuronal (soma das váriaveis)
f <- as.formula(paste(mVar, paste(variables, collapse=" + "), sep=" ~ "))
fr <- as.formula(paste(mVar, paste(variablesR, collapse=" + "), sep=" ~ "))


#Disposição dos neurónios na rede neuronal
nn0 <- c(3)
nn1 <- c(10)
nn2 <- c(20, 10)
nn3 <- c(40, 20)


# Treinar a rede neuronal para usar todas as variáveis como input e produzir a variável "Fadiga" como output
performancenet <- neuralnet(f, poser, hidden = nn3, lifesign = "minimal", algorithm = alg2,
                            linear.output = TRUE, threshold = 0.01)

performancenet2 <- neuralnet(f, trainset1, hidden = nn2, lifesign = "minimal", algorithm = alg2,
                            linear.output = TRUE, threshold = 0.01)

performancenet3 <- neuralnet(f, trainset1, hidden = nn3, lifesign = "minimal", algorithm = alg2,
                            linear.output = TRUE, threshold = 0.01)





# Desenhar a Rede Neuronal
plot(performancenet, rep = "best")


## Definir variaveis de input para teste (todas menos "FatigueLevel" que é a variável de output)
fatigue_test <- subset(testset3, select = variables)


#Testar a rede com os casos de teste
performancenet.results <- compute(performancenet, fatigue_test)

#Declarar results
results <- data.frame(actual = testset3$FatigueLevel, prediction = performancenet.results$net.result)

#Imprimir results
results

#Imprimir resultados arrendondados
results$prediction <- round(results$prediction)
results


#Calcular o "root-mean-square deviation" 
rmse(c(testset3$FatigueLevel),c(results$prediction))

#Calcular o "percentage-bias" (tendência média que os valores testados têm em ser maiores ou menores que os originais)
pbias(results$prediction, testset3$FatigueLevel)





################################################################################
#             Identificação da relevância das variáveis                        #
################################################################################

require(clusterGeneration)
require(nnet)

set.seed(1234567890)

#define number of variables and observations

num.vars <- 9
num.obs <- 844

#define correlation matrix for explanatory variables 
#define actual parameter values

#cov.mat <- genPositiveDefMat(num.vars,covMethod=c("unifcorrmat"))$Sigma
#rand.vars <- mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)
#parms <- runif(num.vars,-10,10)
#y <- rand.vars %*% matrix(parms) + rnorm(num.obs,sd=20)


#prep data and create neural network
y<-data.frame((y-min(y))/(max(y)-min(y)))
names(y)<-'y'
names(trainset1) <- 'tset'
rand.vars<-data.frame(rand.vars)

subtraini <- subset(testset1, select = c("Performance.KDTMean", "Performance.MAMean",
                                         "Performance.MVMean", "Performance.MVMean",
                                         "Performance.DDCMean", "Performance.DMSMean", 
                                         "Performance.AEDMean", "Performance.ADMSLMean",
                                         "Performance.Task"))  
  
subtraino <- subset(testset1, select = c("FatigueLevel"))  
  
names(subtraino)<-'y'


#talvez seja preciso normalizar fatigue level

mod1<-nnet(subtraini,subtraino,size=10,linout=T)




#create a pretty color vector for the bar plot
cols<-colorRampPalette(c('lightgreen','lightblue'))(num.vars)

#use the function on the model created above
par(mar=c(3,4,1,1),family='serif')
gar.fun('tset',mod1)






















with(dataset, cor(dataset$FatigueLevel, dataset$Performance.Task))

# Gráfico da Fadiga em função do KDTMean
plot(testset$Performance.KDTMean, testset$FatigueLevel, xlab="KDTMean", ylab="Fatigue",)

abline(lm(testset$Performance.KDTMean ~ testset$FatigueLevel))