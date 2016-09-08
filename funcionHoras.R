#librerías necesarias---------------------------------------

library(dplyr)
library(stringr)



#función del mame para hacer medias horas filtradas por lo que sea!-------------------------

funcionCalculaTablaMediasHoras<-function(datos,datosExtra,criterio){
  
  require(dplyr)
  require(stringr)
  
  #creo la estructura de la base de datos que usaré
  datosLimpios<-data.frame(id=0,pregunta=I('pregunta'),hora=I('hora'),filtro=I('fitro'))
  #comienzo a llenar la estructura
  k<-1
  f<-1
  datosLimpios
  for(i in 1:nrow(datosHoras)){
    for(j in 1:length(datosHoras)){
      if(!is.na(datosHoras[i,j])){
        datosLimpios[k,1]<-i
        datosLimpios[k,2]<-names(datosHoras)[j]
        datosLimpios[k,3]<-as.character(datosHoras[i,j])
        f<-ceiling(j/2)
        datosLimpios[k,4]<-datosExtra[i,f]
        k<-k+1
      }
    }
  }
  
  eval(parse(text=paste0(
    'datosLimpios<-datosLimpios %>%
  filter(filtro %in% ',criterioDeFiltro,')'
  )))

  print(summary(as.factor(datosLimpios$filtro)))
  # neta <- readline(prompt="furula? n/s")
  # if(neta=='n') stop("sorry bro...")
  #quito un pedazo problemático dentro de las cadenas
  datosLimpios$pregunta<-str_replace_all(datosLimpios$pregunta,'_i_','_')
  datosLimpios$pregunta<-str_replace_all(datosLimpios$pregunta,'_f_','_')
  head(datosLimpios)
  datosLimpios<-datosLimpios %>%
    filter(hora>0)
  error<-1
  while(!is.na(error)){
    datosLimpios$checador<-rep(TRUE,nrow(datosLimpios))
    rownames(datosLimpios)<-1:nrow(datosLimpios)
    for(i in seq(2,nrow(datosLimpios),2)){
      datosLimpios$checador[i]<-(paste(datosLimpios$id[i],str_sub(datosLimpios[i,'pregunta'],-4))==paste(datosLimpios$id[i-1],str_sub(datosLimpios[(i-1),'pregunta'],-4)))
    }
    error<-which(datosLimpios$checador==FALSE)[1]
    if(!is.na(error)){
      datosLimpios<-rbind(
        datosLimpios[1:(error-1),],
        datosLimpios[(error-1):nrow(datosLimpios),]
      )
    }
  }
  #datosLimpios$medio<-'tele'
   #datosLimpios$medio[str_detect(datosLimpios$pregunta,'apps')]<-'app'
    datosLimpios$medio[str_detect(datosLimpios$pregunta,'radio')]<-'radio'
   #datosLimpios$medio[str_detect(datosLimpios$pregunta,'internet')]<-'internet'
   # datosLimpios$medio[str_detect(datosLimpios$pregunta,'periodico')]<-'periodico'
   # datosLimpios$medio[str_detect(datosLimpios$pregunta,'revista')]<-'revista'
  # datosLimpios$medio[str_detect(datosLimpios$pregunta,'tvi')]<-'tvi'
  # datosLimpios$medio[str_detect(datosLimpios$pregunta,'you')]<-'youtube'
  
  datosLimpios$horario<-'manana'
  datosLimpios$horario[str_detect(datosLimpios$pregunta,'_t_')]<-'tarde'
  datosLimpios$horario[str_detect(datosLimpios$pregunta,'_n_')]<-'noche'
  datosLimpios$horario[str_detect(datosLimpios$pregunta,'_z_')]<-'madrugada'
  
  
  head(datosLimpios)
  datosLimpios$hora<-as.numeric(datosLimpios$hora)
  head(datosLimpios)
  
  datosLimpios<-datosLimpios %>%
    mutate(llave=paste0(horario,'_',medio,'_',hora))
  
  for(i in 1:nrow(datosLimpios)){
    if(datosLimpios$horario[i]=='manana' & datosLimpios$hora[i]==15)datosLimpios$hora[i]<-14
    if(datosLimpios$horario[i]=='tarde' & datosLimpios$hora[i]>12)datosLimpios$hora[i]<-12
    if(datosLimpios$horario[i]=='noche' & datosLimpios$hora[i]>12)datosLimpios$hora[i]<-12
    if(datosLimpios$horario[i]=='madrugada' & datosLimpios$hora[i]>10)datosLimpios$hora[i]<-10
  }
  
  datosLimpios<-datosLimpios %>%
    mutate(llave=paste0(horario,'_',medio,'_',hora))
  
  #comenzamos la matriz loca
  datosLimpios$horario<-as.factor(datosLimpios$horario)
  parHorarios<-levels(datosLimpios$horario)
  datosLimpios$medio<-as.factor(datosLimpios$medio)
  parMedios<-levels(datosLimpios$medio)
  parHoras<-c(14,12,12,10)
  
  nombres<-NULL
  l<-1
  for(i in parHorarios){
    for(j in parMedios){
      for(k in 1:14){
        nombres[l]<-paste0(i,'_',j,'_',k)
        l<-l+1
      }
    }
  }
  (nombres)
  
  #lleno la matriz loca
  
  
  matrizloca<-as.data.frame(t(rep(0,length(nombres))))
  names(matrizloca)<-nombres
  matrizloca<-cbind(0,matrizloca)
  names(matrizloca)[1]<-'id'
  
  matrizloca
  
  
  idsUnicos<-unique(datosLimpios$id)
  matriztemporal<-matrizloca
  
  
  for(i in 1:length(idsUnicos)){
    n<-0
    matriztemporal2<-matriztemporal
    matriztemporal2[,1]<-idsUnicos[i]
    datosTemporales<-datosLimpios %>%
      filter(id==idsUnicos[i])
    for(rr in 1:(nrow(datosTemporales)/2)){
      n<-n+1
      datosSupertemporales<-datosTemporales[c((1+(rr-1)*2),(2+(rr-1)*2)),]
      for(k in 2:length(matriztemporal2)){
        if(!is.na(match(names(matriztemporal2)[k],datosSupertemporales$llave))){
          matriztemporal2[,k]<-n
        }
      }
      if(length(unique(datosSupertemporales$hora))>1){
        a<-which(matriztemporal2[1,-1]==n)[1]+1
        b<-which(matriztemporal2[1,-1]==n)[2]
        for(unos in a:b){
          matriztemporal2[1,unos]<-n
        }
      }
    }
    for(hhh in 2:length(matriztemporal2)){
      matriztemporal2[1,hhh]<-min(1,matriztemporal2[1,hhh])
    }
    matrizloca<-rbind(matrizloca,matriztemporal2)
  }
  
  
  
  
  checador<-sample(1:nrow(matrizloca),1)
  print(checador)
  print(sum(matrizloca[checador,-1]))
  print(datosLimpios[datosLimpios$id==idsUnicos[checador-1],'llave'])
  print(names(matrizloca)[which(matrizloca[checador,]==1)])
  print('\n')
  checador<-sample(1:nrow(matrizloca),1)
  print(checador)
  print(sum(matrizloca[checador,-1]))
  print(datosLimpios[datosLimpios$id==idsUnicos[checador-1],'llave'])
  print(names(matrizloca)[which(matrizloca[checador,]==1)])
  print('\n')
  checador<-sample(1:nrow(matrizloca),1)
  print(checador)
  print(sum(matrizloca[checador,-1]))
  print(datosLimpios[datosLimpios$id==idsUnicos[checador-1],'llave'])
  print(names(matrizloca)[which(matrizloca[checador,]==1)])
  
  
  return(matrizloca)
  
  
 
  
  
}


########################################################################################################
#tv-------------------------------------------------------------------
########################################################################################################


#cargo la base
datos<-readRDS('Base_Exp_a_medios_20160628.rds')
#me quedo con las variables correspondientes a lo que necesito, en este caso TV
datosHoras<-datos %>%
  select(contains('p2_h_'),
         -contains('radio'),
         -contains('periodico'),
         -contains('revista'),
         -contains('internet'),
         -contains('tvi'),
         -contains('apps'),
         -contains('you'))

#lo de abierta o de paga es como sigue
# 1 abierta
# 2 de paga
# 3 ambas

#las opciones en tv son las siguientes
# Convencional
# Smart
# Celular
# Tableta
# Computadora
# Otro

tipo<-c('1','2','3')
opciones<-c('Convencional','Smart','Celular','Tableta','Computadora')

###############################################################################
#tv abierta--------------------------------------------------------------------
###############################################################################

#checar código de la función 63-70 para que sea de TV


for(contador in 1:length(opciones)){
  eval(parse(text=paste(
    "datosExtra1<-datos %>%
  select(contains('P5_')) %>%
  mutate_each(funs(as.character))
datosExtra2<-datos %>%
  select(contains('P6a'),-contains('RRR')) %>%
  select(contains('",opciones[contador],"')) %>%
  mutate_each(funs(as.character))
datosExtra3<-datos %>%
  select(contains('P7_1'),-contains('RRR')) %>%
  select(contains('",opciones[contador],"')) %>%
  mutate_each(funs(as.character))
#pegando todo bien loco
datosExtra<-datosExtra1
for(i in 1:nrow(datosExtra1)){
  for(j in 1:length(datosExtra1)){
    datosExtra[i,j]<-paste0(datosExtra1[i,j],'_',datosExtra2[i,j],'_',datosExtra3[i,j])
  }
}"
    ,sep="")))
  
  criterioDeFiltro<-'c("1_1_1","1_1_0","1_0_1","1_NA_1","1_1_NA")'
  
  assign(paste0('mhAbierta',opciones[contador]),
         funcionCalculaTablaMediasHoras(datos=datosHoras,datosExtra = datosExtra,criterio = criterioDeFiltro))
  
}



###############################################################################
#tv paga--------------------------------------------------------------------
###############################################################################

#checar código de la función 63-70 para que sea de TV


for(contador in 1:length(opciones)){
  eval(parse(text=paste(
    "datosExtra1<-datos %>%
  select(contains('P5_')) %>%
  mutate_each(funs(as.character))
datosExtra2<-datos %>%
  select(contains('P6a'),-contains('RRR')) %>%
  select(contains('",opciones[contador],"')) %>%
  mutate_each(funs(as.character))
datosExtra3<-datos %>%
  select(contains('P7_1'),-contains('RRR')) %>%
  select(contains('",opciones[contador],"')) %>%
  mutate_each(funs(as.character))
#pegando todo bien loco
datosExtra<-datosExtra1
for(i in 1:nrow(datosExtra1)){
  for(j in 1:length(datosExtra1)){
    datosExtra[i,j]<-paste0(datosExtra1[i,j],'_',datosExtra2[i,j],'_',datosExtra3[i,j])
  }
}"
    ,sep="")))
  
  criterioDeFiltro<-'c("2_1_1","2_1_0","2_0_1","2_NA_1","2_1_NA")'
  
  assign(paste0('mhPaga',opciones[contador]),
         funcionCalculaTablaMediasHoras(datos=datosHoras,datosExtra = datosExtra,criterio = criterioDeFiltro))
  
}


###############################################################################
#tv ambas--------------------------------------------------------------------
###############################################################################

#checar código de la función 63-70 para que sea de TV


for(contador in 1:length(opciones)){
  eval(parse(text=paste(
    "datosExtra1<-datos %>%
  select(contains('P5_')) %>%
  mutate_each(funs(as.character))
datosExtra2<-datos %>%
  select(contains('P6a'),-contains('RRR')) %>%
  select(contains('",opciones[contador],"')) %>%
  mutate_each(funs(as.character))
datosExtra3<-datos %>%
  select(contains('P7_1'),-contains('RRR')) %>%
  select(contains('",opciones[contador],"')) %>%
  mutate_each(funs(as.character))
#pegando todo bien loco
datosExtra<-datosExtra1
for(i in 1:nrow(datosExtra1)){
  for(j in 1:length(datosExtra1)){
    datosExtra[i,j]<-paste0(datosExtra1[i,j],'_',datosExtra2[i,j],'_',datosExtra3[i,j])
  }
}"
    ,sep="")))
  
  criterioDeFiltro<-'c("3_1_1","3_1_0","3_0_1","3_NA_1","3_1_NA")'
  
  assign(paste0('mhAmbas',opciones[contador]),
         funcionCalculaTablaMediasHoras(datos=datosHoras,datosExtra = datosExtra,criterio = criterioDeFiltro))
  
}



########################################################################################################
#radio-------------------------------------------------------------------
########################################################################################################

#checar código de la función 63-70 para que sea de TV


#cargo la base
datos<-readRDS('Base_Exp_a_medios_20160628.rds')
#me quedo con las variables correspondientes a lo que necesito, en este caso TV
datosHoras<-datos %>%
  select(contains('p2_h_')) %>%
  select(contains('radio'),
         -contains('periodico'),
         -contains('revista'),
         -contains('internet'),
         -contains('tvi'),
         -contains('apps'),
         -contains('you'))

#las opciones en radio son las siguientes
# Convencional
# Smart
# Celular
# Tableta
# Computadora
# Otro

opciones<-c('1','2','3','4')


for(contador in 1:length(opciones)){
  eval(parse(text=paste(
    "
    datosExtra1<-datos %>%
    select(contains('P8A_'),-contains('RRR')) %>%
    mutate_each(funs(as.character))
    #pegando todo bien loco
    datosExtra<-datosExtra1
  "
    ,sep="")))
  
    criterioDeFiltro<-paste0('c("',opciones[contador],'")')
 
  assign(paste0('mhRadio',opciones[contador]),
         funcionCalculaTablaMediasHoras(datos=datosHoras,datosExtra = datosExtra,criterio = criterioDeFiltro))
}



########################################################################################################
#periódico-------------------------------------------------------------------
########################################################################################################

#checar código de la función 63-70 para que sea de TV


#cargo la base
datos<-readRDS('Base_Exp_a_medios_20160628.rds')
#me quedo con las variables correspondientes a lo que necesito, en este caso TV
datosHoras<-datos %>%
  select(contains('p2_h_')) %>%
  select(
         contains('periodico')
       )

#las opciones en radio son las siguientes
# Convencional
# Smart
# Celular
# Tableta
# Computadora
# Otro

opciones<-c('Peri_dico_impreso','Tel_fono_Celular','Tableta','Computadora')


for(contador in 1:length(opciones)){
  eval(parse(text=paste(
    "
    datosExtra1<-datos %>%
    select(contains('P9A_'),-contains('RRR')) %>%
    select(contains('",opciones[contador],"')) %>%
    mutate_each(funs(as.character))
    #pegando todo bien loco
    datosExtra<-datosExtra1
  "
    ,sep="")))
  
  criterioDeFiltro<-'c("1")'
  
  assign(paste0('mhPeriódico',opciones[contador]),
         funcionCalculaTablaMediasHoras(datos=datosHoras,datosExtra = datosExtra,criterio = criterioDeFiltro))
}


########################################################################################################
#revista-------------------------------------------------------------------
########################################################################################################

#checar código de la función 63-70 para que sea de TV


#cargo la base
datos<-readRDS('Base_Exp_a_medios_20160628.rds')
#me quedo con las variables correspondientes a lo que necesito, en este caso TV
datosHoras<-datos %>%
  select(contains('p2_h_')) %>%
  select(
    contains('revista')
  )

#las opciones en radio son las siguientes
# Convencional
# Smart
# Celular
# Tableta
# Computadora
# Otro

opciones<-c('Revista_impresa','Tel_fono_Celular','Tableta','Computadora')


for(contador in 1:length(opciones)){
  eval(parse(text=paste(
    "
    datosExtra1<-datos %>%
    select(contains('P10A_'),-contains('RRR')) %>%
    select(contains('",opciones[contador],"')) %>%
    mutate_each(funs(as.character))
    #pegando todo bien loco
    datosExtra<-datosExtra1
  "
    ,sep="")))
  
  criterioDeFiltro<-'c("1")'
  
  assign(paste0('mhRevista',opciones[contador]),
         funcionCalculaTablaMediasHoras(datos=datosHoras,datosExtra = datosExtra,criterio = criterioDeFiltro))
}

########################################################################################################
#internet-------------------------------------------------------------------
########################################################################################################

#checar código de la función 63-70 para que sea de TV


#cargo la base
datos<-readRDS('Base_Exp_a_medios_20160628.rds')
#me quedo con las variables correspondientes a lo que necesito, en este caso TV
datosHoras<-datos %>%
  select(contains('p2_h_')) %>%
  select(
    contains('internet')
  )

#las opciones en radio son las siguientes
# Convencional
# Smart
# Celular
# Tableta
# Computadora
# Otro

opciones<-c('Celular','Tableta','Computadora')


for(contador in 1:length(opciones)){
  eval(parse(text=paste(
    "
    datosExtra1<-datos %>%
    select(contains('P11B'),-contains('RRR')) %>%
    select(contains('",opciones[contador],"')) %>%
    mutate_each(funs(as.character))
    #pegando todo bien loco
    datosExtra<-datosExtra1
    "
    ,sep="")))
  
  criterioDeFiltro<-'c("1")'
  
  assign(paste0('mhInternet',opciones[contador]),
         funcionCalculaTablaMediasHoras(datos=datosHoras,datosExtra = datosExtra,criterio = criterioDeFiltro))
}




#####################################################################
#guardo todo el desmadrito-------------------------------------------
#####################################################################

rm(contador,criterioDeFiltro,
   datos,
   datosExtra,
   datosExtra1,datosExtra2,datosExtra3,
   datosHoras,funcionCalculaTablaMediasHoras,i,j,
   opciones,tipo)

save.image('mh_medioPorDispositivo.RData')
