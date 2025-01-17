#Carrega o pacote utilizado para montagem do modelo
library(nlstools)

#Carrega os dados para teste do modelo
data <- file.choose(new=FALSE)
dados <- read.csv(data, sep=";")
attach(dados)


#Primeiro � necess�rio setar os valores start para poder gerar o modelo de predi��o
#Voc� pode estimar valores iniciais para a estrutura do modelo de curva de crescimento
svTypical <- list(A=390,K=0.20,b=0.53)

#Abaixo voc� primeiro cria a fun��o de trabalho
vbTypical <- Peso ~ (A)*(1-b*exp(-K*(Idade)))^3

#Aplica o modelo n�o linear,nesse caso com os pesos aplicados
fitTypical <- nls(vbTypical,data=dados,start=svTypical, trace=TRUE,model=TRUE, control = list(warnOnly = TRUE),weights = Ponderador)

#Plota os resultados em uma curva 
fitPlot(fitTypical,xlab="Idade",ylab="Peso (kg)",main="")

#Mostra os resultados de converg�ncia e dados auxiliares
summary(fitTypical)
overview(fitTypical)
fitTypical$call

#Extrair o intervalo de confian�a para os par�metros A,b e K
confint2(fitTypical)

