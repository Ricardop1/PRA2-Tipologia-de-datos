library(dplyr)
library(caret)
library(nortest)

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


# 3. Limpieza de los datos

## ¿0's o elementos vacíos?

###  0's

where.ceros <- function(x){
  which(x[!is.na(x)] == 0)
  }

titanic.ceros <- lapply(titanic[,c("Fare", "Age")], where.ceros)

### NA

where.na <- function(x){
  which(is.na(x) == TRUE)
}

titanic.na <- lapply(titanic[, c("Age", "SibSp", "Parch", "Fare")], where.na)


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

titanic$Age[age.na] <- predict(predicted_age, titanic[titanic$Age[age.na], ])

summary(titanic$Age[age.na])
summary(titanic$Age[-age.na])

### Embarked

embarked.na <- which(titanic$Embarked == "")
titanic$Embarked[embarked.na] <- NA

titanic[embarked.na, c(2, 7)]


titanic %>%
  group_by(Embarked, Pclass) %>%
  filter(Pclass == "1") %>%
  summarise(mfare = median(Fare),
            n = n())

titanic$Embarked[embarked.na] <- "C"
levels(titanic$Embarked) <- factor(titanic$Embarked)

## Identificación y tratamiento de valores extremos

detect.outliers <- function(x){
  iqr <- quantile(x)
  lower.iqr <- iqr[2]
  upper.iqr <- iqr[4]
  x.iqr <- upper.iqr - lower.iqr
  
  upper.threshold <- (x.iqr*1.5) + upper.iqr
  lower.threshold <- lower.iqr - (x.iqr*1.5)
  
  values <- x < lower.threshold | x > upper.threshold
  return(values)
}


outliers <- lapply(titanic[, c("Age", "SibSp", "Parch", "Fare")], detect.outliers)
lapply(titanic[, c("Age", "SibSp", "Parch", "Fare")], boxplot)

which(titanic$Fare > 500)
titanic_raw[c(259, 680, 738), ] # Hemos hecho una pequeña vuelta a este dataset tan sólo para recuperar el nombre de Cardeza


# 4. Análisis de los datos
## 4.1. Selección de los grupos de datos que se quieren analizar/comparar
titanic.primeraClase <- titanic[titanic$Pclass == 1,]
titanic.segundaClase <- titanic[titanic$Pclass == 3,]
titanic.terceraClase <- titanic[titanic$Pclass == 3,]

titanic.mujeres <- titanic[titanic$Sex == "female",]
titanic.hombres <- titanic[titanic$Sex == "male",]

titanic.supervivientes <- titanic[titanic$Survived == 1,]
titanic.fallecidos <- titanic[titanic$Survived == 0,]


## 4.2. Comprobación de la normalidad y la homogeneidad de la varianza


lapply(titanic[,c("Age", "SibSp", "Parch", "Fare")], ad.test)




















