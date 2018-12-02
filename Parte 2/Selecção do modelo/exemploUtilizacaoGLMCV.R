
## Variar os thresholds ##
set.seed(1)
aux = glmCV(nossoDiabetes,diabetesB~stab.glu,0.5)
set.seed(1)
aux2 = glmCV(nossoDiabetes,diabetesB~stab.glu,0.75)
set.seed(1)
aux3 = glmCV(nossoDiabetes,diabetesB~stab.glu,0.9)
set.seed(1)
aux4 = glmCV(nossoDiabetes,diabetesB~stab.glu,0.3)
set.seed(1)
aux5 = glmCV(nossoDiabetes,diabetesB~stab.glu,0.15)
set.seed(1)
aux6 = glmCV(nossoDiabetes,diabetesB~stab.glu,0.10)

## Esta funcao já faz variar os thresholds consoante o delta que é passado ##
## neste caso o delta é 0.1 e é entre 0 e 1 ##
set.seed(1)
thr = graficoThreshold(0,1,0.1,glmCV,diabetesB~stab.glu,nossoDiabetes)
melhor = melhorIndice(thr)

