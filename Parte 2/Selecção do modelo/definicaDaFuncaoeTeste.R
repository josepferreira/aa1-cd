graficoThreshold <- function(inferior,superior,delta, funcao, formula, dados){
  num = ((superior - inferior) / delta) + 2
  aux = 0
  auxT = 0
  comeco = inferior
  v = rep(0,num)
  acerto = rep(0,num)
  acertoP = rep(0,num)
  acertoN = rep(0,num)
  
  for(a in 1: num){
    comeco = inferior + ((a-1)*delta)
    resultados = funcao(dados,formula,comeco)
    v[a] = comeco
    acerto[a] = resultados[1,1]
    acertoP[a] = resultados[1,2]
    acertoN[a] = resultados[1,3]
  }
  estrutura <- data.frame(v,acerto,acertoP,acertoN)
}

melhorThreshold <- function(inferior,superior,delta, fit, teste){
  num = ((superior - inferior) / delta) + 2
  acerto = 0
  valor = -1
  comeco = inferior
  
  for(a in 1: num){
    comeco = inferior + ((a-1)*delta)
    resultados = rep(NA,183)
    resultados[fit<=comeco] = 0
    resultados[fit>comeco] = 1
    condicaoT <- resultados == teste
    
    mediaAuxT = mean(teste==resultados,na.rm=TRUE) ##aqui pode-se utilizar depois
    ##algo diferente tendo em conta aquilo q disse na analise
    if(mediaAuxT >= 0.65){
      mediaAuxP = length(resultados[!is.na(condicaoT) & condicaoT & resultados==0])/length(resultados[!is.na(teste) & teste == 0])
      
      if(mediaAuxP >= 0.65){
        mediaAuxN = length(resultados[!is.na(condicaoT) & condicaoT & resultados==1])/length(resultados[!is.na(teste) & teste == 1])
        
        if(mediaAuxN >= 0.65){
          mediaAux = (2*mediaAuxT) + mediaAuxP + mediaAuxN ##aqui pode-se
          ##truncar os valores para x casas decimais
          
          if(mediaAux > acerto){
            acerto = mediaAux
            valor = comeco
          }
          
        }
        
      }
    }
    
  }
  resultados = rep(NA,183)
  resultados[fit<=valor] = 0
  resultados[fit>valor] = 1
  condicaoT <- resultados == teste
  
  acertoT = mean(teste==resultados,na.rm=TRUE) ##aqui pode-se utilizar depois
  
  acertoN = length(resultados[!is.na(condicaoT) & condicaoT & resultados==0])/length(resultados[!is.na(teste) & teste == 0])
  
  acertoP = length(resultados[!is.na(condicaoT) & condicaoT & resultados==1])/length(resultados[!is.na(teste) & teste == 1])
  
  acerto = acerto / 4
  estrutura <- data.frame(valor, acertoT, acertoN, acertoP,acerto)
}

geraKNN <- function(treinoX, treinoY, testeX, teste, inicio, fim){
  num = fim - inicio + 1
  v = rep(0,num)
  t = rep(0,num)
  fp = rep(0,num)
  fn = rep(0,num)
  for(a in 1:num){
    i = inicio + a - 1
    set.seed(1)
    resultados=knn(treinoX,testeX,treinoY,k=i)
    
    v[a] = i
    mediaAux = mean(teste==resultados,na.rm=TRUE) ##aqui pode-se utilizar depois
    ##algo diferente tendo em conta aquilo q disse na analise
    t[a] = mediaAux
    
    condicaoT <- resultados == teste
    mediaAux = length(resultados[!is.na(condicaoT) & condicaoT & resultados==0])/length(resultados[!is.na(teste) & teste == 0])
    fp[a] = mediaAux
    
    mediaAux = length(resultados[!is.na(condicaoT) & condicaoT & resultados==1])/length(resultados[!is.na(teste) & teste == 1])
    fn[a] = mediaAux
    
  }
  estrutura <- data.frame(v,t,fp,fn)
}


geraKNNCV <- function(treinoX, treinoY, inicio, fim){
  num = fim - inicio + 1
  v = rep(0,num)
  t = rep(0,num)
  fp = rep(0,num)
  fn = rep(0,num)
  for(a in 1:num){
    i = inicio + a - 1
    set.seed(1)
    resultados = knn.cv(treino,treinoY,k=a)
    
    v[a] = i
    mediaAux = mean(treinoY==resultados,na.rm=TRUE) ##aqui pode-se utilizar depois
    ##algo diferente tendo em conta aquilo q disse na analise
    t[a] = mediaAux
    
    condicaoT <- resultados == treinoY
    mediaAux = length(resultados[!is.na(condicaoT) & condicaoT & resultados==0])/length(resultados[!is.na(treinoY) & treinoY == 0])
    fp[a] = mediaAux
    
    mediaAux = length(resultados[!is.na(condicaoT) & condicaoT & resultados==1])/length(resultados[!is.na(treinoY) & treinoY == 1])
    fn[a] = mediaAux
    
  }
  estrutura <- data.frame(v,t,fp,fn)
}

melhorIndice = function(dados){
  data = dados
  indice = -1
  valor = 0
  for(i in 1:dim(data)[1]){
    valorAux = (2*data[i,2]) + data[i,3] + data[i,4]
    
    if(valorAux > valor){
      valor = valorAux
      indice = i
    }
  }
  
  resultado = data.frame(indice,valor)
  
}

glmCV = function(dados, formula, thr){
  treino = dados[complete.cases(dados),]
  
  quantos = dim(treino)[1]/10
  acerto = rep(0,quantos)
  acertoP = rep(0,quantos)
  acertoN = rep(0,quantos)
  
  for(a in 1:quantos){
    teste = sample(length(treino),10)
    dadosTeste = treino[teste, ]
    treino = treino[-teste, ]
    
    if(a == 1){
      
      glm.fit = glm(formula, data=treino, family=binomial)
      glm.probs=predict(glm.fit, newdata=dadosTeste, type="response")
      resultados = ifelse(glm.probs > thr, 1, 0)
      treinoY = dadosTeste[,1]
      
      mediaAux = mean(treinoY==resultados,na.rm=TRUE) ##aqui pode-se utilizar depois
      ##algo diferente tendo em conta aquilo q disse na analise
      acerto[a] = mediaAux
      
      condicaoT <- resultados == treinoY
      divide = length(resultados[!is.na(treinoY) & treinoY == 0])
      if(divide != 0){
        mediaAux =length(resultados[!is.na(condicaoT) & condicaoT & resultados==0])/divide
        acertoN[a] = mediaAux
      }
      divide = length(resultados[!is.na(treinoY) & treinoY == 1])
      
      if(divide!=0){
        mediaAux = length(resultados[!is.na(condicaoT) & condicaoT & resultados==1])/divide
        acertoP[a] = mediaAux
      }
      jaUsados = dadosTeste
    }
    else{
      paraTreinar =  rbind(treino,jaUsados)
      glm.fit = glm(formula, data=paraTreinar, family=binomial)
      glm.probs=predict(glm.fit, newdata=dadosTeste, type="response")
      resultados = ifelse(glm.probs > thr, 1, 0)
      treinoY = dadosTeste[,1]
      
      mediaAux = mean(treinoY==resultados,na.rm=TRUE) ##aqui pode-se utilizar depois
      ##algo diferente tendo em conta aquilo q disse na analise
      acerto[a] = mediaAux
      
      condicaoT <- resultados == treinoY
      divide = length(resultados[!is.na(treinoY) & treinoY == 0])
      if(divide != 0){
        mediaAux =length(resultados[!is.na(condicaoT) & condicaoT & resultados==0])/divide
        
        acertoN[a] = mediaAux
      }
      divide = length(resultados[!is.na(treinoY) & treinoY == 1])
      if(divide != 0){
        mediaAux = length(resultados[!is.na(condicaoT) & condicaoT & resultados==1])/divide
        
        acertoP[a] = mediaAux
      }
      jaUsados = rbind(jaUsados,dadosTeste)
    }
  }
  acertoMedio = mean(acerto)
  acertoMedioP = mean(acertoP)
  acertoMedioN = mean(acertoN)
  
  data.frame(acertoMedio,acertoMedioP,acertoMedioN)
}

normaliza = function(dados){
  data = dados
  for(i in 1:dim(data)[2]){
    if(class(data[,i])!="factor"){
      data[,i] <- (data[,i] - min(data[,i], na.rm = TRUE)) / (max(data[,i],na.rm = TRUE)-min(data[,i],na.rm = TRUE))
    }
  }
    
  resultado = data
}
