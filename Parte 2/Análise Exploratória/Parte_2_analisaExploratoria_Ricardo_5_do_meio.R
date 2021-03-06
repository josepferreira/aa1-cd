#################### Ricardo #####################
# -> Age
# -> Gender
# -> Height
# -> Weight
# -> Frame
#
#---------------------#
# Teste � normalidade | 
#---------------------#

# Age
nossoDiabetes <- diabetes[,-1]
nossoDiabetes

par(mfrow=c(2,1))
hist(nossoDiabetes$age) ## Aproxima-se da distribui��o normal, contudo do lado esquedo apresenta
                        ## um crescimento bastante mais acentuado que do lado direito.
mean(nossoDiabetes$age,na.rm=TRUE)
sd(nossoDiabetes$age,na.rm=TRUE)
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$age, na.rm = TRUE), sd=sd(nossoDiabetes$age, na.rm = TRUE)) 
hist(valoresNormal) ## Observando os dois diagramas podemos afirmar que s�o parecidos. � possivel
                    ## notar valores ligeiramente mais elevados � esquerda da m�dia no de cima
summary(nossoDiabetes$age)
shapiro.test(valoresNormal) ## O diagrama inferior tem um pvalor alto pelo que n�o se rejeita
shapiro.test(nossoDiabetes$age) ## O diagrama superior tem um pvalor muito reduzido pelo que se 
                                ## rejeita a hip�tese nula (H0: dados normalmente distribu�dos)
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$age)
qqline(nossoDiabetes$age, col = 2) ## Do gr�fico resultante podemos ver que a vari�vel "Age"
                                   ## n�o parece ser normalmente distribuida. Apesar de entre os
                                   ## 35 e os 45 anos mostrar bastante converg�ncia, fora deste 
                                   ## intrevalo � os valores divergem bastante

# Height
par(mfrow=c(2,1))
hist(nossoDiabetes$height) ## Verifica-se algumas semelhan�as � distribui��o normal (maior 
                           ## probabilidade em torno do centro), contudo � possivel verificar 
                           ## que na zona central a frequ�ncia mostra-se relativamente constante
                           ## ao longo de um largo intrevalo.
mean(nossoDiabetes$height,na.rm=TRUE)
sd(nossoDiabetes$height,na.rm=TRUE)
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$height, na.rm = TRUE), sd=sd(nossoDiabetes$height, na.rm = TRUE)) 
hist(valoresNormal) ## Observando os dois diagramas p� poss�vel verificar algumas diferen�as com 
                    ## as freq�ncias mais elevadas a mostrarem-se em torno das 67 polegadas no
                    ## diagrama inferior ao passo que no diagrama superior as frequ�ncias mais
                    ## elevadas se encontravam em torno das 63 polegadas
summary(nossoDiabetes$height)
shapiro.test(valoresNormal) ## O diagrama inferior tem um pvalor alto (30%) pelo que n�o se rejeita
shapiro.test(nossoDiabetes$height) ## O diagrama superior tem um pvalor muito reduzido (0.2%) pelo 
## que se rejeita a hip�tese nula.
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$height)
qqline(nossoDiabetes$height, col = 2) ## Do gr�fico resultante podemos ver que a vari�vel "Height"
                                      ## n�o parece ser normalmente distribuida. Para alturas 
                                      ## inferiores a 62 e superiores a 72 polegadas verifica-se 
                                      ## uma tend�ncia a divergir dos valores te�ricos.


# Weight
set.seed(1)
par(mfrow=c(2,1))
hist(nossoDiabetes$weight) ## Olhando para o histograma � poss�vel retirar bastantes parec�ncias a
                           ## uma distribui��o normal. Do lado esquerda dos 170 pounds verifica-se
                           ## uma subida mais constante da frequ�ncia relativamente ao lado direito
                           ## (os dois lados diferem ligeiramente)
mean(nossoDiabetes$weight,na.rm=TRUE)
sd(nossoDiabetes$weight,na.rm=TRUE)
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$weight, na.rm = TRUE), sd=sd(nossoDiabetes$weight, na.rm = TRUE)) 
hist(valoresNormal) ## Observando os dois diagramas � poss�vel verificar bastantes semelhan�as
                    ## entre eles nomeadamente no valor onde a frequ�ncia � mais elevada (170 pounds)
                    ## e tamb�m a n�vel da distribui��o � esquerda e � direita deste valor.
summary(nossoDiabetes$weight)
shapiro.test(valoresNormal) ## O diagrama inferior tem um pvalor alto (80%) pelo que n�o se rejeita
shapiro.test(nossoDiabetes$weight) ## O diagrama superior tem um pvalor muito reduzido (<0.01%) pelo 
                                   ## que se rejeita a hip�tese nula.
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$weight)
qqline(nossoDiabetes$weight, col = 2) ## Do gr�fico resultante podemos ver que a vari�vel "Weight"
                                      ## n�o parece ser normalmente distribuida. Para pesos 
                                      ## inferiores a 130 pounds e superiores a 220 pounds 
                                      ## verifica-se uma tend�ncia a divergir superiormente dos 
                                      ## valores te�ricos.


#---------------------#
# Teste � influ�ncia  | 
#---------------------#

### Vari�vel Bin�ria:
par(mfrow=c(1,1))
nossoDiabetes$diabetesB <- rep(NA,403)
nossoDiabetes$diabetesB[nossoDiabetes$glyhb > 7]=1
nossoDiabetes$diabetesB[nossoDiabetes$glyhb <= 7]=0

nossoDiabetes$diabetesB <- as.factor(nossoDiabetes$diabetesB)

# Age
boxplot(nossoDiabetes$age~nossoDiabetes$diabetesB) ## parece afetar um pouco
# Height
boxplot(nossoDiabetes$height~nossoDiabetes$diabetesB) ## n�o parece afetar praticamente nada
# Weight
boxplot(nossoDiabetes$weight~nossoDiabetes$diabetesB) ## parece afetar um pouco (menos que 
                                                      ## a primeira)


### Vari�vel glyhb:
# Gender
boxplot(nossoDiabetes$glyhb~nossoDiabetes$gender) ## n�o parece afetar praticamente nada

# Frame
boxplot(nossoDiabetes$glyhb~nossoDiabetes$frame) ## n�o parece afetar praticamente nada

# Gender
table(nossoDiabetes$diabetesB, nossoDiabetes$gender) ## Na tabela podemos verificar que 18.2% dos
26/162 #1 em homens                                               ## homens do dataset possuem diabetes. J� nas 
34/228  #1 em mulheres                                             ## mulheres a percentagem � de 17%. Tendo isto

136/(136+194) #homens em 0                                                     ## conta o g�nero parece n�o afetar
26/(26+34) #homens em 1

# Frame
table(nossoDiabetes$diabetesB, nossoDiabetes$frame) ## Na tabela podemos verificar que quanto maior
9/102 #1 em small                                                ## � o frame, mais incid�ncias de diabetes 
26/178 #1 em medium                                              ## s�o registadas. Parece portanto afetar
23/99 #1 em large

93/(93+152+76) #small em 0
152/(93+152+76) #medium em 0

9/(9+26+23) #small em 1
26/(9+26+23) #medium em 1

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<