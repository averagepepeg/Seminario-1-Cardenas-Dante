library(caret)
library(purrr)
library(pROC)
library(dplyr)
library(reshape)
library(precrec)
library(themis)
library(DMwR2)
library(ROSE)

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

#  Sub-muestreo (Down)                                                                                                               #---------------------
library(caret)
set.seed(1437)
down_train <- downSample(x = Dat1[, -5],y = factor(Dat1$Malware))
table(down_train$Class)
head(down_train)

#Grafica de datos DOWN
# -------------------------
a2=data.frame(down_train[,5]);colnames(a2)<-c("Malware")
co_down=ggplot(a2, aes(x = Malware, y = 1000, color=Malware)) + 
  geom_jitter(shape = 1) + theme(axis.title.x=element_blank())+
  ggtitle("Sub-muestreo (DOWN)")+labs(y = "")
co_down

#  Sobre-muestreo (Up)                                                                                                                        #---------------------
set.seed(1437)
up_train <- upSample(x = Dat1[, -5],y = factor(Dat1$Malware))                         
table(up_train$Class) 
head(up_train)

# Grafica de datos UP
# -------------------------
a3=data.frame(up_train[,5]);colnames(a3)<-c("Malware")
co_up=ggplot(a3, aes(x = Malware, y = 1000, color=Malware)) + 
  geom_jitter(shape = 1)+ theme(axis.title.x=element_blank())+
  ggtitle("Sobre-muestreo (UP)")+labs(y = "")
co_up



#  (ROSE)  
library(ROSE)
set.seed(1437)
rose_train <- ROSE(Malware ~.,data  = Dat1)$data 
table(rose_train$Malware) 
head(rose_train)

# Distribuci?n Datos ROSE
# -------------------------
a5=data.frame(rose_train[,5]);colnames(a5)<-c("Malware")
co_ROSE=ggplot(a5, aes(x = Malware, y = 1000, color=Malware)) + 
  geom_jitter(shape = 1) + theme(axis.title.x=element_blank())+
  ggtitle("ROSE")+labs(y = "")
co_ROSE

#------------------------------------------------------------------#
#                      FORMANDO LOS MODELOS                        #
#------------------------------------------------------------------#
# Modelo original 
#--------------------
set.seed(1437)
orig.ctrl = trainControl( method = "cv", number = 10 ,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)

orig_logist <- train(Malware ~ .,
                     data = Dat1,
                     method = "glm",
                     trControl = orig.ctrl,
                     metric = "ROC")

# Modelo down 
#--------------
set.seed(1437)
down.ctrl = trainControl( method = "cv", number = 10 ,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary,
                          sampling = "down")

down_logist <- train(Malware ~ .,
                     data = Dat1,
                     method = "glm",
                     trControl = down.ctrl,
                     metric = "ROC")
# Modelo up   
#-------------------
set.seed(1437)
up.ctrl = trainControl( method = "cv", number = 10 ,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        sampling = "up")

up_logist <- train(Malware ~ .,
                   data = Dat1,
                   method = "glm",
                   trControl = up.ctrl,
                   metric = "ROC")


# Modelo rose 
#-------------
set.seed(1437)
rose.ctrl = trainControl( method = "cv", number = 10 ,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary,
                          sampling = "rose")

rose_logist <- train(Malware ~ .,
                     data = Dat1,
                     method = "glm",
                     trControl = rose.ctrl,
                     metric = "ROC")


# Medidas de precisiÃ³n
# --------------------
pred_logist_orig <- predict(orig_logist, newdata=Dat,type="raw")
confusionMatrix(data= pred_logist_orig, reference= Dat$Malware,positive="Si")

pred_logist_down <- predict(down_logist, newdata=Dat,type="raw")
confusionMatrix(data= pred_logist_down, reference= Dat$Malware,positive="Si")

pred_logist_up <- predict(up_logist, newdata=Dat,type="raw")
confusionMatrix(data= pred_logist_up, reference= Dat$Malware,positive="Si")

pred_logist_rose <- predict(rose_logist, newdata=Dat,type="raw")
confusionMatrix(data= pred_logist_rose, reference= Dat$Malware,positive="Si")



#------------------------------------------------------------------#
#           COMPARANDO EL ENTRENAMIENTO DE MODELOS                 #
#------------------------------------------------------------------#
modelos  <- list(Original  = orig_logist,
                 DOWN      = down_logist,
                 UP        = up_logist,
                 SMOTE     = smote_logist
                 )

# COMPARACI?N DE LOS MODELOS EN VALIDACI?N CRUZADA
# ------------------------------------------------
comparacion_modelos <- resamples(modelos) 
summary(comparacion_modelos)
bwplot(comparacion_modelos,metric = "ROC")
densityplot(comparacion_modelos, metric = "ROC",auto.key=TRUE)

# Curva ROC
# -----------
modelos  <- list(Original  = orig_logist,
                 DOWN      = down_logist,
                 UP        = up_logist,
                 SMOTE     = smote_logist)
pred_logis1 <- predict(orig_logist, newdata=Dat[,-1],type="prob")
roc.curve(Dat$Malware, pred_logis1[,2],lty=2,lwd=1.8,col="blue" ,main="ROC curves")

pred_logis2 <- predict(down_logist, newdata=Dat[,-1],type="prob")
roc.curve(Dat$Malware, pred_logis2[,2],lty=2,lwd=1.8,col="blue" ,main="ROC curves")

pred_logis3 <- predict(up_logist, newdata=Dat[,-1],type="prob")
roc.curve(Dat$Malware, pred_logis3[,2],lty=2,lwd=1.8,col="blue" ,main="ROC curves")

pred_logis4 <- predict(smote_logist, newdata=Dat[,-1],type="prob")
roc.curve(Dat$Malware, pred_logis4[,2],lty=2,lwd=1.8,col="blue" ,main="ROC curves")

pred_logis5 <- predict(rose_logist, newdata=Dat[,-1],type="prob")
roc.curve(Dat$Malware, pred_logis5[,2],lty=2,lwd=1.8,col="blue" ,main="ROC curves")



#------------------------------------------------------------------#
#                  FORMANDO LOS MODELOS de RF                      #
#------------------------------------------------------------------#
# Modelo original 
#--------------------
set.seed(1437)
orig.ctrl = trainControl( method = "cv", number = 10 ,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)

orig_rf <- train(Malware ~ .,
                     data = Dat1,
                     method = "rf",
                     trControl = orig.ctrl,
                     metric = "ROC")

# Modelo down 
#--------------
set.seed(1437)
down.ctrl = trainControl( method = "cv", number = 10 ,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary,
                          sampling = "down")

down_rf <- train(Malware ~ .,
                     data = Dat1,
                     method = "rf",
                     trControl = down.ctrl,
                     metric = "ROC")
# Modelo up   
#-------------------
set.seed(1437)
up.ctrl = trainControl( method = "cv", number = 10 ,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        sampling = "up")

up_rf <- train(Malware ~ .,
                   data = Dat1,
                   method = "rf",
                   trControl = up.ctrl,
                   metric = "ROC")


# Modelo rose 
#-------------
set.seed(1437)
rose.ctrl = trainControl( method = "cv", number = 10 ,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary,
                          sampling = "rose")

rose_rf <- train(Malware ~ .,
                     data = Dat1,
                     method = "rf",
                     trControl = rose.ctrl,
                     metric = "ROC")


# Medidas de precisiÃ³n
# --------------------
pred_rf_orig <- predict(orig_rf, newdata=Dat,type="raw")
confusionMatrix(data= pred_rf_orig, reference= Dat$Malware,positive="Si")

pred_rf_down <- predict(down_rf, newdata=Dat,type="raw")
confusionMatrix(data= pred_rf_down, reference= Dat$Malware,positive="Si")

pred_rf_up <- predict(up_rf, newdata=Dat,type="raw")
confusionMatrix(data= pred_rf_up, reference= Dat$Malware,positive="Si")

pred_rf_rose <- predict(rose_rf, newdata=Dat,type="raw")
confusionMatrix(data= pred_rf_rose, reference= Dat$Malware,positive="Si")




setwd("D:/")
write.table(rose_train, file = "ROSE.txt", sep = "\t", eol = "\n", dec = ".", row.names = F, col.names = TRUE)
# Guardar modelo en UCV O DISCO

saveRDS(rose_logist, "./rose_logist.rds")
# Leer el modelo final
#---------------------
super_model <- readRDS("./rose_logist.rds")
print(super_model)