#**********************************************************
#ANALISE DE CLUSTER - CONSUMO DE ALIMENTOS
#**********************************************************

#**********************************************************
#Atencao: Alterar Diretorio
setwd("~/analise-de-cluster-fia/data")

#Retirar notacao cientifica no R
options(scipen = 999)

#Instalacao de pacotes para leitura de banco de dados do Excel
install.packages("readxl")


#**********************************************************
#Instalacao de pacotes (base realizar 1 vez)
install.packages("cluster")
install.packages("factoextra")
install.packages("gridExtra")

#**********************************************************

#Leitura da Base de Consumo de Alimentos
library(readxl)
consumo <- read_excel("Consumo_Alimentos.xlsx", sheet = 'Base de Dados')

# Número de linhas
nrow(consumo)

# Número de colunas
ncol(consumo)
#**********************************************************

#**********************************************************
# Faca uma analise exploratoria da base de dados 
# (obtenha as medidas de posicao e dispersao). 
summary(consumo[,-1]) #Min, Q1, Q2, Q3 e Max
apply(consumo[,-1] , 2 , sd) #Desvio Padrao

#Considerando as variaveis carne vermelha e carne branca, qual possui a maior variabilidade?
#calculo do coeficiente de variacao
sd(consumo$carne_vermelha)/mean(consumo$carne_vermelha)*100
sd(consumo$carne_branca)/mean(consumo$carne_branca)*100

#Considerando o histograma das variaveis leite e carboidratos, as distribuicoes sao simetricas?
hist(consumo$leite, main="Leite")  
hist(consumo$carboidratos, main="Carboidratos") 


#Existe outlier nas variaveis carne vermelha e carne branca?
par(mfrow=c(1,2)) # Dividindo a janela gráfica
boxplot(consumo$carne_vermelha, col="paleturquoise", main="Carne vermelha")
boxplot(consumo$carne_branca, col="pink", main="Carne branca")


par(mfrow=c(2,2)) # 2 linhas e 2 colunas
boxplot(consumo$carne_vermelha, col="paleturquoise", main="Carne vermelha")
boxplot(consumo$carne_branca, col="chocolate1", main="Carne branca")
boxplot(consumo$ovos, col="darkgoldenrod1", main="Ovos")
boxplot(consumo$leite, col="deeppink4", main="Leite")



#**********************************************************

#Padronize as variaveis.
consumo_z<-scale(consumo[,-1])
head(consumo_z)

# DISTÂNCIAS EUCLIDIANAS
#Calcule a matriz de distancias euclidianas entre os 25 paises.
#Calculo da distancia euclidiana entre os elementos
distancia <- dist(consumo_z, method="euclidean") # Calculo das distancias euclidianas
distancia

#Faca a analise de agrupamento com as variaveis padronizadas
#usando os 2 metodos apresentados, escolha um dos metodos 
#e justifique a quantidade de grupos apos a analise do Dendrograma.

#Matriz de graficos de dimensao 1 linhas x 2 colunas

# Janela gráfica para 1 linha e 1 coluna
par(mfrow=c(1,1))

# Método do vizinho mais próximo
clust_single <- hclust(distancia, method="single") 
plot(clust_single, main="Metodo Single", hang=-1, labels = consumo$Pais) #hang=-1 para deixar todos iniciando do zero
rect.hclust(clust_single, k=3, border=1:5) # 3 grupos

# Método do vizinho mais distante
clust_complete <- hclust(distancia, method="complete") # Gerar
plot(clust_complete, main="Metodo Complete", hang=-1, labels = consumo$Pais) # Dendograma
rect.hclust(clust_complete, k=3, border=1:5) # Partir em 3 grupos

#Analise as caracteristicas de cada grupo.
#Atribui a cada pais o cluster a qual ele pertence pela variavel cluster
# cutree: marca no banco de dados que país está em cada grupo
consumo$cluster <- as.factor(cutree(clust_complete, k=3))

#Tamanho dos Clusters
# table: quantas informações temos em cada grupo
table(consumo$cluster)
# 1   2  3 
# 6  17  2 
# Grupo 1: 6 países
# Grupo 2: 17 países
# Grupo 3: 2 países


#Faz BoxPlot para cada variavel e compara por cluster
#Distribuicao das variaveis por cluster
par(mfrow=c(2,5)) #coloca os graficos lado a lado
boxplot(consumo$carne_vermelha ~ consumo$cluster, col="paleturquoise",main="carne_vermelha")
boxplot(consumo$carne_branca ~ consumo$cluster, col="darkturquoise",main="carne_branca")
boxplot(consumo$ovos ~ consumo$cluster, col="paleturquoise",main="ovos")
boxplot(consumo$leite ~ consumo$cluster, col="darkturquoise",main="leite")
boxplot(consumo$peixes ~ consumo$cluster, col="paleturquoise",main="peixes")
boxplot(consumo$cereais ~ consumo$cluster, col="darkturquoise",main="cereais")
boxplot(consumo$carboidratos ~ consumo$cluster, col="paleturquoise",main="carboidratos")
boxplot(consumo$graos ~ consumo$cluster, col="darkturquoise",main="graos")
boxplot(consumo$fruta_vegetais ~ consumo$cluster, col="paleturquoise",main="fruta_vegetais")

