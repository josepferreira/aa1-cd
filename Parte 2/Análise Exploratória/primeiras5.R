 

library(faraway)
par()
attach(diabetes)

## Gerar a variável binária que indica se tem ou não diabetes
diabetesB <- rep(NA,403)
diabetesB[diabetes$glyhb > 7]=1
diabetesB[diabetes$glyhb <= 7]=0
# por como fator (é qualitativa)
diabetesB <- factor(diabetesB)
diabetes <-  diabetes[,-1]

#plot(lm.auto2)
d <- diabet

### ANÁLISE COLESTROL ##
colestrol <- diabetes$chol
qqnorm(colestrol) ##Parece normal ao contrario do ze
qqline(colestrol, col = 2)
table(diabetesB)
hist(colestrol)
shapiro.test(colestrol)
table(diabetesB,colestrol)
table(diabetesB,colestrol)
boxplot(table(diabetesB,colestrol))
boxplot(colestrol~diabetesB)
barplot(colestrol~diabetesB) ##Pode ter inflcuencia maior o colestrol diabetes bitch, mediana ligeiramente a cima
table(diabetesB)
valoresNormal <- rnorm(400,mean=mean(colestrol, na.rm = TRUE), sd=sd(colestrol, na.rm = TRUE)) 
hist(valoresNormal) ##PARECE NORMAL
fivenum(colestrol)
#Min: 78, Max:443, Media 179, 25%->78 75%->230



### ANÁLISE STAB-GLU ##
stab <- diabetes$stab.glu
hist(stab) # 
#FIVENUM()
fivenum(stab)
#Min: 48, Max:385, Media 89, 25%->81 75%->106
table(diabetesB,stab)
boxplot(stab)
boxplot(stab~diabetesB) #STAB-GLU CLARAMENTE AFETA 
valoresNormal <- rnorm(400,mean=mean(stab, na.rm = TRUE), sd=sd(stab, na.rm = TRUE)) 
hist(valoresNormal) ##PARECE +- NORMAL

##ANALISE RACIO ##
racio <- diabetes$ratio
hist(racio)
summary(racio)
boxplot(racio~diabetesB) ## Parece ter algumo
valoresNormal <- rnorm(400,mean=mean(racio, na.rm = TRUE), sd=sd(racio, na.rm = TRUE)) 
hist(valoresNormal)
qqnorm(racio)
qqline(racio, col = 2) ##Dificil de analisar



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
boxplot(hdl~diabetesB) ##Mais baixo HDL menor os "diabetes"



### ANÁLISE location ##
loc <- diabetes$location
table(diabete,loc)
hist(loc)
barplot(table(diabetesB,loc),beside=T, legend.text=c("Nao ter","Ter")) ## Nao parece ter influencia nenhuma a localização





##ANALISE AGE
age <- diabetes$age
summary(age)
hist(age)
table(diabetesB,age)
barplot(table(diabetesB,age))
boxplot(age)
boxplot(age~diabetesB)
qqnorm(age)
qqline(age, col="2")
boxplot(diabetes$glyhb~diabetes$gender) ## Quanto mais velho mais colestrol
valoresNormal <- rnorm(400,mean=mean(age, na.rm = TRUE), sd=sd(age, na.rm = TRUE))  ## Dificil de analisar
hist(valoresNormal) ##PARECE NORMAL
