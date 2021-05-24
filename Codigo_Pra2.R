library(dplyr)
library(caret)
library(nortest)
library(ranger)
library(rpart.plot)

# 2. Integraci?n y selecci?n de los datos de inter?s a analizar

## Lectura archivo
titanic_raw <- read.csv("./csv/train.csv")

#observamos la estructura de los datos 
str(titanic_raw)
summary(titanic_raw)

## Selecci?n de datos

#eliminamos columnas
titanic <- subset(titanic_raw, select= -c(PassengerId, Name, Cabin, Ticket))

#covertimos variables categoricas
titanic$Survived <- as.factor(titanic$Survived) 
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

levels(titanic$Survived)
levels(titanic$Pclass)
levels(titanic$Sex)
levels(titanic$Embarked)


# 3. Limpieza de los datos

## ¿0's o elementos vacíos?

###  0's
where.ceros <- function(x){
  which(x[!is.na(x)] == 0)
  }

titanic.ceros <- lapply(titanic[,c("Fare", "Age")], where.ceros)
titanic.ceros

### NA
where.na <- function(x){
  which(is.na(x) == TRUE)
}

titanic.na <- lapply(titanic[, c("Age", "SibSp", "Parch", "Fare")], where.na)
titanic.na


## Gestión de los casos.
### 0's
age.ceros <- unlist(titanic.ceros[1])

ceros.class <- titanic$Pclass[age.ceros]
table(ceros.class)

fareMean.byClass <- by(titanic$Fare, titanic$Pclass, mean)

titanic$Fare[age.ceros[ceros.class==1]] <- fareMean.byClass[1]
titanic$Fare[age.ceros[ceros.class==2]] <- fareMean.byClass[2]
titanic$Fare[age.ceros[ceros.class==3]] <- fareMean.byClass[3]

lapply(titanic[,c("Fare", "Age")], where.ceros) # Comprobamos que ha funcionado

### NA's
age.na <- unlist(titanic.na[1])

predicted_age <- train(
  Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + SibSp + Survived,
  data = titanic[-age.na, ],
  method = "ranger",
  trControl = trainControl(
    method = "cv", number = 10, verboseIter = TRUE),
  importance = 'impurity'
)

titanic$Age[age.na] <- predict(predicted_age, titanic[age.na,])

summary(titanic$Age[age.na])
summary(titanic$Age[-age.na])

### Embarked

embarked.na <- which(titanic$Embarked == "")
titanic$Embarked[embarked.na] <- NA

titanic[embarked.na, c("Pclass", "Fare")]

#filtramos por clase y puerta de embarque
titanic.C <- titanic[titanic$Embarked =="C" & titanic$Pclass =="1",]
titanic.Q <- titanic[titanic$Embarked =="Q" & titanic$Pclass =="1",]
titanic.S <- titanic[titanic$Embarked =="S" & titanic$Pclass =="1",]

median(titanic.C$Fare, na.rm = TRUE)
median(titanic.Q$Fare, na.rm = TRUE)
median(titanic.S$Fare, na.rm = TRUE)

## Identificación y tratamiento de valores extremos

out.Age <- boxplot(titanic[,"Age"])$out
out.SibSp <- boxplot(titanic[,"SibSp"])$out
out.Parch <- boxplot(titanic[,"Parch"])$out
out.Fare <- boxplot(titanic[,"Fare"])$out 


length(out.Age)
length(out.SibSp)
length(out.Parch)
length(out.Fare)

billetes.caros <- which(titanic$Fare > 500)
titanic_raw$Name[billetes.caros] # Hemos hecho una pequeña vuelta a este dataset tan sólo para recuperar el nombre de Cardeza


# 4. Análisis de los datos
## 4.1. Selección de los grupos de datos que se quieren analizar/comparar
titanic.primeraClase <- titanic[titanic$Pclass == 1,]
titanic.segundaClase <- titanic[titanic$Pclass == 2,]
titanic.terceraClase <- titanic[titanic$Pclass == 3,]

titanic.mujeres <- titanic[titanic$Sex == "female",]
titanic.hombres <- titanic[titanic$Sex == "male",]


titanic.supervivientes <- titanic[titanic$Survived == 1,]
titanic.fallecidos <- titanic[titanic$Survived == 0,]


## 4.2. Comprobación de la normalidad y la homogeneidad de la varianza

#normalidad
lapply(titanic[,c("Age", "SibSp", "Parch", "Fare")], shapiro.test)

#homohgeneidad de varianzas

fligner.test(Age ~ Survived, data = titanic)

boxplot(titanic.supervivientes[,"Age"])
boxplot(titanic.fallecidos[,"Age"])


## Aplicación de pruebas estadísticas para comparar los grupos de datos

### Estudio de variables significativas

modelo.Surv <- glm(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Age +  Embarked, data = titanic, family = "binomial"(link=logit))
summary(modelo.Surv)

#Vamos a comprobar si efectivamente a mayor número de hermanos menor era el rango de edad.

plot(titanic$SibSp, titanic$Age,
     xlab="Número de hermanos",
     ylab="Edad")

#Comprobamos predicciones

test <- data.frame( Pclass= c("1","1","1","1"),
                    Sex = c("female","male","female","male"),
                    SibSp = c(3,3,3,3),
                    Parch = c(1,1,1,1),
                    Fare = c(200,200,200,200),
                    Age = c(15,15,50,50),
                    Embarked = c("C","C","C","C")
                    )

predict(modelo.Surv, test, type = "response")


### Estudio de la relación de Survived con Age, Sex y Pclass

tabla.SurvPclass <- table(titanic$Survived, titanic$Pclass)
tabla.SurvSex <- table(titanic$Survived, titanic$Sex)

chisq.test(tabla.SurvPclass)
chisq.test(tabla.SurvSex)
chisq.test(x =titanic$Survived, y = titanic$Age )

### ¿La proporción de pasajeros fallecidos de Clase 3 es realmente mayor que la proporción de pasajeros fallecidos de clase 1?

(numPrimera <- length(titanic.primeraClase$Survived))
(numTercera <- length(titanic.terceraClase$Survived))

(p1 <- sum(titanic.terceraClase$Survived == 0) / numTercera)
(p2 <- sum(titanic.primeraClase$Survived == 0) / numPrimera)



#Comprobación
success <- c(p1*numTercera,p2*numPrimera)
n <- c(numTercera,numPrimera)
prop.test( success, n, alternative="greater", correct=FALSE)

#representacion de resultados y conclusiones

plot(titanic$Parch, titanic$Age,
     xlab="Miembros en la familia",
     ylab="Edad")

interaction.plot(titanic$Survived,titanic$Sex,titanic$Age,
                 xlab = "Survived", ylab = "Edad")     

supervivencia <- rpart(Survived~.,data=titanic,method="class",
                        minsplit=1,cp=.02,parms = list(split ="gini"))

rpart.plot(supervivencia,type = 3,extra = 100 ,clip.right.lab = FALSE)                 

# Exportación del dataset



str(titanic)

titanic_raw$Survived <- titanic$Survived
titanic_raw$Pclass   <- titanic$Pclass  
titanic_raw$Sex      <- titanic$Sex     
titanic_raw$Age      <- titanic$Age     
titanic_raw$SibSp    <- titanic$SibSp   
titanic_raw$Parch    <- titanic$Parch   
titanic_raw$Fare     <- titanic$Fare    
titanic_raw$Embarked <- titanic$Embarked

write.csv(titanic_raw,"./csv/Titanic_clean.csv",
          row.names=FALSE, fileEncoding = 'UTF-8')

