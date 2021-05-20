library(dplyr)

#lectura archivo
titanic_raw <- read.csv("./csv/train.csv")

#observamos la estructura de los datos 
str(titanic_raw)
summary(titanic_raw)

#eliminamos columnas
titanic <- subset(titanic_raw, select= -c(PassengerId, Name, Cabin, Ticket))

#covertimos variables categoricas
titanic$Survived <- as.factor(titanic$Survived) 
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

