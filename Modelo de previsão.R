# Criando um modelo de previsão para classificação da região que está detereminado azeite 

azeite <- read.table(file = "olive.txt",                       header = TRUE,                       sep = ",")

str(azeite)
indices <- sample(1:nrow(azeite),size = nrow(azeite),replace = FALSE)

azeite <- azeite[indices,]

head(azeite)
tail(azeite)

n <- round(nrow(azeite)*0.8) #determinando 80% dos dados 
n

treinamento <- azeite[1:n,] #80% para treinar o modelo
teste <- azeite[(n+1):nrow(azeite),] #20% para testar o modelo

summary(treinamento)
summary(teste)

norte <-treinamento[treinamento$region == "Northern Italy",] 
sardinia <-treinamento[treinamento$region == "Sardinia",] 
sul <-treinamento[treinamento$region == "Southern Italy",]

barplot(table(treinamento$region))

plot( x = treinamento$linoleic, y =treinamento$eicosenoic, pch = 16)
points (x=norte$linoleic, y = norte$eicosenoic, col ="blue",pch=16)
points(x=sul$linoleic, y =sul$eicosenoic, col= "green", pch = 16 )
points(x=sardinia$linoleic, y =sardinia$eicosenoic, col= "red", pch = 16)

abline(h=0.05)
abline(v= 10.5)

previsao <- c()   # vetor que guarda as previsões 
# arvore de decisão para prever a região em que o azeite foi produzido
for(j in 1 :114){
  if(teste$eicosenoic[j]>0.1){
    previsao[j]<- "sul"
  }else{
      if(teste$linoleic[j]>10.5){
        previsao[j]<-"sardinia"
      }else{
        previsao[j]<-"norte"
      }
    }
}
previsao
teste[22,]

mean(previsao == teste$region)
