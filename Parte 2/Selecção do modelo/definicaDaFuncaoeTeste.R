graficoThreshold <- function(inferior,superior,delta, fit, teste){
  num = ((superior - inferior) / delta) + 2
  aux = 0
  auxT = 0
  comeco = inferior
  v = rep(0,num)
  t = rep(0,num)
  fp = rep(0,num)
  fn = rep(0,num)
  
  for(a in 1: num){
    comeco = inferior + ((a-1)*delta)
    resultados = rep(NA,183)
    resultados[fit<=comeco] = 0
    resultados[fit>comeco] = 1
    
    v[a] = comeco
    mediaAux = mean(teste==resultados,na.rm=TRUE) ##aqui pode-se utilizar depois
    ##algo diferente tendo em conta aquilo q disse na analise
    t[a] = mediaAux
    
    condicaoT <- resultados == teste
    mediaAux = length(resultados[!is.na(condicaoT) & condicaoT & resultados==0])/length(resultados[!is.na(teste) & teste == 0])
    fp[a] = mediaAux
    
    mediaAux = length(resultados[!is.na(condicaoT) & condicaoT & resultados==1])/length(resultados[!is.na(teste) & teste == 1])
    fn[a] = mediaAux
    
    
    if(mediaAux > aux){
      aux = mediaAux
      auxT = comeco
    }
    
  }
  estrutura <- data.frame(v,t,fp,fn)
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

normaliza = function(dados){
  data = dados
  for(i in 1:dim(data)[2]){
    if(class(data[,i])!="factor"){
      data[,i] <- (data[,i] - min(data[,i], na.rm = TRUE)) / (max(data[,i],na.rm = TRUE)-min(data[,i],na.rm = TRUE))
    }
  }
    
  resultado = data
}
