library(caret)
library(purrr)
library(pROC)
library(dplyr)
library(reshape)
library(precrec)
library(themis)
library(DMwR2)
library(ROSE)
library(ROCR)
library(ada)


Dat<-read.delim("clipboard")
head(Dat)
Dat$Malware <- factor(Dat$Malware)
Dat$Tipo <- factor(Dat$Tipo)
Dat1 <- data.frame(Dat[,-1], row.names=Dat[,1])
head(Dat1[,5])
#------------------------------------------------------------------#
#               DATOS DE ENTRENAMIENTO Y DE PRUEBA                 #
#------------------------------------------------------------------#
table(Dat[,6])

# Distribuci?n Datos originales
# -----------------------------
a1=data.frame(Dat1[,5]);colnames(a1)<-c("Malware")
theme_update(plot.title = element_text(hjust = 0.5))
co_orig=ggplot(a1, aes(x = Malware, y = 1000, color=Malware)) + 
  geom_jitter(shape = 1)+ theme(axis.title.x=element_blank())+
  ggtitle("Datos originales")+labs(y = "")
co_orig




#------------------------------------------------------------------#
#                      FORMANDO DATASETS                           #
#------------------------------------------------------------------#
Dat2 <- Dat1
head(Dat2)
Dat3 <- Dat2[,-2]
head(Dat3)
head(Dat3)
Dat3 <- Dat3[,-1]
V1 <- Dat3$Minima.Entropía
V2 <- Dat3$Valor.de.Markov
Info <- cbind(V1,V2)
Std <- scale(Info)
Dat4 <- Dat3
head(Dat4)
Dat4$Minima.Entropía <- Std[,1]
Dat4$Valor.de.Markov <- Std[,2]
Cor <- cor(V1,V2)
print(Cor)

i7_2<- createDataPartition(Dat4$Malware, p=0.7, list=FALSE)
data.train7_2 <- Dat4[ i7_2, ]                         
data.test7_2  <- Dat4[-i7_2, ] 


i8_2<- createDataPartition(Dat4$Malware, p=0.8, list=FALSE)
data.train8_2 <- Dat4[ i8_2, ]                         
data.test8_2  <- Dat4[-i8_2, ] 








#------------------------------------------------------------------#
#                       ALGORITMO ADABOOST                         #
#------------------------------------------------------------------#

set.seed(1437)
ctrl <- trainControl(method="cv", number=10)

modelo_boost <- train(Malware ~ ., 
                     data = data.train7_2, 
                     method = "ada", tuneLength=6,
                     trControl=ctrl, metric="Accuracy")

plot(modelo_boost)
modelo_boost$bestTune
pred_boost <- predict(modelo_boost, newdata=data.test7_2,type="raw")
confusionMatrix(data= pred_boost, reference= factor(data.test7_2$Malware), positive="Si")

# Predicción correcta y error
# ---------------------------
accuracy <- mean(data.train7_2$Malware==pred_boost) ; accuracy
error <- mean(data.train7_2$Malware!=pred_boost) ; error

# Curva ROC
# -----------
pred_boos2 <- predict(modelo_boost, newdata=data.test7_2,type="prob")
roc.curve(data.test7_2$Malware, pred_boos2[,2],lty=2,lwd=1.8,col="blue" ,main="ROC curves")

# Importancia de Variables
# -------------------------
impor=varImp(modelo_boost)
impor
plot(impor)




#------------------------------------------------------------------#
#                      EL ALGORITMO BAGGING                        #
#------------------------------------------------------------------#
RNGkind(sample.kind = "Rounding")
set.seed(100)
ctrl <- trainControl(method="cv",number=10)
modelo_bag <- train(Malware ~ ., 
                    data = data.train7_2, 
                    method = "treebag",
                    trControl = ctrl, 
                    tuneLength = 5, 
                    metric="Accuracy")
modelo_bag

pred_bag <- predict(modelo_bag, newdata=data.test7_2,type="raw")
confusionMatrix(data= pred_bag, reference= as.factor(data.test7_2$Malware), positive="Si")

# Curva ROC
# -----------
pred_bag2 <- predict(modelo_bag, newdata=data.test7_2,type="prob")
roc.curve(data.test7_2$Malware, pred_bag2[,2],lty=2,lwd=1.8,col="blue" ,main="ROC curves")

# Importancia de Variables
# -------------------------
impor=varImp(modelo_bag)
impor
plot(impor)


#------------------------------------------------------------------#
#                       REGRESIÓN LOGISTICA                        #
#------------------------------------------------------------------#

set.seed(1437)
orig.ctrl = trainControl( method = "cv", number = 10 ,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)

orig_logist <- train(Malware ~ .,
                     data = data.train7_2,
                     method = "glm",
                     trControl = orig.ctrl,
                     metric = "ROC")

pred_logist_orig <- predict(orig_logist, newdata=data.test7_2,type="raw")
confusionMatrix(data= pred_logist_orig, reference= data.test7_2$Malware,positive="Si")

# Curva ROC
# -----------
pred_log2 <- predict(orig_logist, newdata=data.test7_2,type="prob")
roc.curve(data.test7_2$Malware, pred_log2[,2],lty=2,lwd=1.8,col="blue" ,main="ROC curves")


#------------------------------------------------------------------#
#                          RANDOM FOREST                           #
#------------------------------------------------------------------#

set.seed(1437)
orig.ctrl = trainControl( method = "cv", number = 10 ,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)

orig_rf <- train(Malware ~ .,
                 data = data.train7_2,
                 method = "rf",
                 trControl = orig.ctrl,
                 metric = "ROC")

pred_rf_orig <- predict(orig_rf, newdata=data.test7_2,type="raw")
confusionMatrix(data= pred_rf_orig, reference= data.test7_2$Malware,positive="Si")

# Curva ROC
# -----------
pred_rf2 <- predict(orig_rf, newdata=data.test7_2,type="prob")
roc.curve(data.test7_2$Malware, pred_rf2[,2],lty=2,lwd=1.8,col="blue" ,main="ROC curves")

