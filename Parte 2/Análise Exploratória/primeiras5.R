library(faraway)
par()
attach(diabetes)

## Gerar a variável binária que indica se tem ou não diabetes
diabetesB <- rep(0,403)
diabetesB[diabetes$glyhb > 7]=1
# por como fator (é qualitativa)
diabetesB <- factor(diabetesB)
diabetes <-  diabetes[,-1]

#plot(lm.auto2)
d <- diabet

### ANÁLISE COLESTROL ##
colestrol <- diabetes$chol
table(diabetesB)
hist(colestrol)
table(diabetesB,colestrol)
barplot(table(diabetesB,colestrol))
boxplot(table(diabetesB,colestrol))
boxplot(colestrol~diabetesB)
barplot(colestrol~diabetesB)
table(diabetesB)
valoresNormal <- rnorm(400,mean=mean(colestrol, na.rm = TRUE), sd=sd(colestrol, na.rm = TRUE)) 
hist(valoresNormal) ##PARECE NORMAL
fivenum(colestrol)
#Min: 78, Max:443, Media 179, 25%->78 75%->230



### ANÁLISE STAB-GLU ##
stab <- diabetes$stab.glu
hist(stab)
#FIVENUM()
fivenum(stab)
#Min: 48, Max:385, Media 89, 25%->81 75%->106
table(diabetesB,stab)
boxplot(stab)
boxplot(stab~diabetesB)
valoresNormal <- rnorm(400,mean=mean(stab, na.rm = TRUE), sd=sd(stab, na.rm = TRUE)) 
hist(valoresNormal) ##PARECE +- NORMAL





### ANÁLISE HDL ##

hdl <- diabetes$hdl
table(diabetesB,hdl)
#Histograma
hist(hdl)
valoresNormal <- rnorm(400,mean=mean(hdl, na.rm = TRUE), sd=sd(hdl, na.rm = TRUE)) 
hist(valoresNormal) ##PARECE +- NORMAL
fivenum(hdl)
#Min: 12, Max:120, Media 46, 25%->38 75%->59
boxplot(hdl)
boxplot(hdl~diabetesB)



### ANÁLISE location ##
loc <- diabetes$location
table(diabetesB,loc)
hist(loc)
barplot(table(diabetesB,loc),beside=T, legend.text=c("Nao ter","Ter"))




##ANALISE AGE
age <- diabetes$age
summary(age)
hist(age)
table(diabetesB,age)
barplot(table(diabetesB,age))
boxplot(age)
boxplot(table(diabetesB,age))
boxplot(diabetesB~age)
valoresNormal <- rnorm(400,mean=mean(age, na.rm = TRUE), sd=sd(age, na.rm = TRUE)) 
hist(valoresNormal) ##PARECE NORMAL
