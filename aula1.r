#Aula de introdução em R
x <- c(2,3,8,7,5)
mean( x)
mb$
mb<- read.table("/home/aluno/fernanda/milsa.txt", header = T)

#comando para ler arquivo txt tabulado
#comando para abrir tela e selecionar arquivo
#mb<- read.table(file.choose())
mb<- read.table(file.choose(), header = T)

#Como acessar uma coluna de forma individual
mb$salario

#separando variaveis
attach(mb)
civil
salario

#ANALISE UNIVARIADA

#ANALISE DE VARIAVEIS QUALITATIVA NOMINAL
is.factor(civil)
mb$civil <- factor(mb$civil, label = c("solteiro", "casado"),levels=1:2)
civil

mb$instrucao <-factor(mb$instrucao, label=c("fundamental", "medio", "superior"),levels=c(1,2,3), ord=T)
mb$regiao <-factor(mb$instrucao,labels=c("capital", "interior","outro"),
                   levels= c(1,2,3))
is.factor(instrucao)                       

mb$idade = mb$ano+mb$mes/12

#ANALISE DE VARIAVEIS QUALITATIVA
mb$civil

#calculando a frequencia absoluta
civil.tb <-table(mb$civil)
civil.tb

#calculando a frequencia relativa
civil.rel<-prop.table(civil.tb)
civil.rel

#Plotando um grafico de setores(pizza)
pie(civil.tb)


#ANALISE QUALITATIVA ORDINAL
mb$instrucao

#Calcular a frequencia absoluta
instrucao.tb<-table(mb$instrucao)
instrucao.tb

#Calcular a frequencia relativa
instrucao.rel<-prop.table(instrucao.tb)
instrucao.rel

#Plotando um grafico de barras

barplot(instrucao.tb)
barplot(instrucao.rel)

#ANALISE DE VARIAVEL QUANTITATIVA DISCRETA
mb$filhos

#Verificando se é um factor
is.factor(mb$filhos)
is.numeric(mb$filhos)

#Calcular a frequencia absoluta
filhos.tb = table(mb$filhos)

#Calcular a frequencia relativa
filhos.rel = prop.table(filhos.tb)
filhos.rel.per = filhos.rel*100

#Plotando um grafico
plot(filhos.tb)
plot(filhos.rel)
#plot(filhos.rel.per)

#Calcular a frequencia (relativa ou absoluta) acumulada
filhos.fac <- cumsum(filhos.tb)
plot(filhos.fac, type = "S" )

#Calculando a MODA
filhos.mo <-names(filhos.tb)[filhos.tb == max(filhos.tb)]  

#Calculando a MEDIA
filhos.me <-mean(mb$filhos,na.rm = T)

#Calculando a MEDIANA
filhos.md = median(mb$filhos, na.rm = T)

#Calculando os quantis
filhos.qt = quantile(mb$filhos, na.rm = T)
boxplot(mb$filhos)

#Calculando a VARIANCIA
a = summary(mb$filhos)
filhos.var = var(mb$filhos, na.rm = T)

#Calculando o DESVIO PADRAO
filhos.dp = sd(mb$filhos, na.rm = T)

#Calculando o COEFICIENTE VARIANCIA
filhos.cv = 100*filhos.dp/filhos.me

#ANALISE DE VARIAVEL QUANTITATIVA CONTINUA
mb$salario
is.factor(mb$salario)
is.numeric(mb$salario)

range(mb$salario)

salario.tb<-table(cut(mb$salario,seq(3.5,23.5,l=8)))
salario.rel<- prop.table(salario.tb)

hist(mb$salario)
barplot(mb$salario)
barplot(salario.rel)

mean(mb$salario)#media
median(mb$salario)#mediana
summary(mb$salario)
