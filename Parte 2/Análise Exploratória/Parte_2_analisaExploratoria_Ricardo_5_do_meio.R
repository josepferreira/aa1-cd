#################### Ricardo #####################
# -> Age
# -> Gender
# -> Height
# -> Weight
# -> Frame
#
#---------------------#
# Teste à normalidade | 
#---------------------#

# Age
par(mfrow=c(2,1))
hist(nossoDiabetes$age) ## Aproxima-se da distribuição normal, contudo do lado esquedo apresenta
                        ## um crescimento bastante mais acentuado que do lado direito.
mean(nossoDiabetes$age,na.rm=TRUE)
sd(nossoDiabetes$age,na.rm=TRUE)
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$age, na.rm = TRUE), sd=sd(nossoDiabetes$age, na.rm = TRUE)) 
hist(valoresNormal) ## Observando os dois diagramas podemos afirmar que são parecidos. è possivel
                    ## notar valores ligeiramente mais elevados à esquerda da média no de cima
summary(nossoDiabetes$age)
shapiro.test(valoresNormal) ## O diagrama inferior tem um pvalor alto pelo que não se rejeita
shapiro.test(nossoDiabetes$age) ## O diagrama superior tem um pvalor muito reduzido pelo que se 
                                ## rejeita a hipótese nula (H0: dados normalmente distribuídos)
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$age)
qqline(nossoDiabetes$age, col = 2) ## Do gráfico resultante podemos ver que a variável "Age"
                                   ## não parece ser normalmente distribuida. Apesar de entre os
                                   ## 35 e os 45 anos mostrar bastante convergência, fora deste 
                                   ## intrevalo é os valores divergem bastante

# Height
par(mfrow=c(2,1))
hist(nossoDiabetes$height) ## Verifica-se algumas semelhanças à distribuição normal (maior 
                           ## probabilidade em torno do centro), contudo é possivel verificar 
                           ## que na zona central a frequência mostra-se relativamente constante
                           ## ao longo de um largo intrevalo.
mean(nossoDiabetes$height,na.rm=TRUE)
sd(nossoDiabetes$height,na.rm=TRUE)
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$height, na.rm = TRUE), sd=sd(nossoDiabetes$height, na.rm = TRUE)) 
hist(valoresNormal) ## Observando os dois diagramas pé possível verificar algumas diferenças com 
                    ## as freqências mais elevadas a mostrarem-se em torno das 67 polegadas no
                    ## diagrama inferior ao passo que no diagrama superior as frequências mais
                    ## elevadas se encontravam em torno das 63 polegadas
summary(nossoDiabetes$height)
shapiro.test(valoresNormal) ## O diagrama inferior tem um pvalor alto (30%) pelo que não se rejeita
shapiro.test(nossoDiabetes$height) ## O diagrama superior tem um pvalor muito reduzido (0.2%) pelo 
## que se rejeita a hipótese nula.
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$height)
qqline(nossoDiabetes$height, col = 2) ## Do gráfico resultante podemos ver que a variável "Height"
                                      ## não parece ser normalmente distribuida. Para alturas 
                                      ## inferiores a 62 e superiores a 72 polegadas verifica-se 
                                      ## uma tendência a divergir dos valores teóricos.


# Weight
set.seed(1)
par(mfrow=c(2,1))
hist(nossoDiabetes$weight) ## Olhando para o histograma é possível retirar bastantes parecências a
                           ## uma distribuição normal. Do lado esquerda dos 170 pounds verifica-se
                           ## uma subida mais constante da frequência relativamente ao lado direito
                           ## (os dois lados diferem ligeiramente)
mean(nossoDiabetes$weight,na.rm=TRUE)
sd(nossoDiabetes$weight,na.rm=TRUE)
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$weight, na.rm = TRUE), sd=sd(nossoDiabetes$weight, na.rm = TRUE)) 
hist(valoresNormal) ## Observando os dois diagramas é possível verificar bastantes semelhanças
                    ## entre eles nomeadamente no valor onde a frequência é mais elevada (170 pounds)
                    ## e também a nível da distribuição à esquerda e à direita deste valor.
summary(nossoDiabetes$weight)
shapiro.test(valoresNormal) ## O diagrama inferior tem um pvalor alto (80%) pelo que não se rejeita
shapiro.test(nossoDiabetes$weight) ## O diagrama superior tem um pvalor muito reduzido (<0.01%) pelo 
                                   ## que se rejeita a hipótese nula.
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$weight)
qqline(nossoDiabetes$weight, col = 2) ## Do gráfico resultante podemos ver que a variável "Weight"
                                      ## não parece ser normalmente distribuida. Para pesos 
                                      ## inferiores a 130 pounds e superiores a 220 pounds 
                                      ## verifica-se uma tendência a divergir superiormente dos 
                                      ## valores teóricos.


#---------------------#
# Teste à influência  | 
#---------------------#

### Variável Binária:
par(mfrow=c(1,1))
nossoDiabetes$diabetesB <- rep('no',403)
nossoDiabetes$diabetesB[nossoDiabetes$glyhb > 7]='yes'
nossoDiabetes$diabetesB <- as.factor(nossoDiabetes$diabetesB)

# Age
boxplot(nossoDiabetes$age~nossoDiabetes$diabetesB) ## parece afetar um pouco
# Height
boxplot(nossoDiabetes$height~nossoDiabetes$diabetesB) ## não parece afetar praticamente nada
# Weight
boxplot(nossoDiabetes$weight~nossoDiabetes$diabetesB) ## parece afetar um pouco (menos que 
                                                      ## a primeira)


### Variável glyhb:
# Gender
boxplot(nossoDiabetes$glyhb~nossoDiabetes$gender) ## não parece afetar praticamente nada

# Frame
boxplot(nossoDiabetes$glyhb~nossoDiabetes$frame) ## não parece afetar praticamente nada

# Gender
table(nossoDiabetes$diabetesB, nossoDiabetes$gender) ## Na tabela podemos verificar que 18.2% dos
26/143                                               ## homens do dataset possuem diabetes. Já nas 
34/200                                               ## mulheres a percentagem é de 17%. Tendo isto
                                                     ## conta o género parece não afetar


# Frame
table(nossoDiabetes$diabetesB, nossoDiabetes$frame) ## Na tabela podemos verificar que quanto maior
9/95                                                ## é o frame, mais incidências de diabetes 
26/158                                              ## são registadas. Parece portanto afetar
23/80                                               

<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<