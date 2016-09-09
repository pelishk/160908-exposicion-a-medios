library(dplyr)

#############################################################################
#cargo las funciones necesarias----------------------------------------------
#############################################################################

#función para pasar variables de una tabla a otra-------------------------
funcionTransmitir<-function(da,db,va,vb,variables){
  llave<-match(da[,va],db[,vb])
  nvariables<-match(variables,names(db))
  for(i in 1:length(nvariables)){
    da[,variables[i]]<-db[llave,nvariables[i]]
  }
  return(da)
}

#función que hace subsets con un vector---------------------------------
funcionFiltroVariables<-function(base,filtros='blablabla',filtrosExactos='blablabla',filtrosMultiples=c('blablabla','blablabla')){
  
  p0<-paste(filtrosExactos,collapse=',',sep='')
  eval(parse(text=paste(
    'baseFiltrosExactos<-base %>%
    select(',p0,')'
  )))
  
  p1<-paste('contains("',filtros,'")',collapse=',',sep='')
  eval(parse(text=paste(
    'baseFiltros<-base %>%
    select(',p1,')'
  )))
  
  p2<-paste('contains("',filtrosMultiples[seq(1,length(filtrosMultiples),2)],'")',collapse=',',sep='')
  p3<-paste('contains("',filtrosMultiples[seq(2,length(filtrosMultiples),2)],'")',collapse=',',sep='')
  
  eval(parse(text=paste(
    'baseFiltrosMultiples<-base %>%
    select(',p2,') %>%
    select(',p3,')'
  )))
  
  baseR<-baseFiltrosExactos
  
  if(nrow(baseFiltros)>0)baseR<-cbind(baseR,baseFiltros)
  if(nrow(baseFiltrosMultiples)>0)baseR<-cbind(baseR,baseFiltrosMultiples)
  
  
  return(baseR)
}


#función que crea la variable de suma de horas de acuerdo a la función anterior----

funcionMedioHorarioHoras<-function(base,nombre,k){
  for(i in 1:nrow(base)){
    base[i,nombre]<-sum(base[i,k:length(base)],na.rm=T)/2
  }
  return(base)
}

#función que saca el promedio de una variable tomando un ponderador------------

funcionMediaPonderada<-function(base,ponderador,variable){
  require(survey)
  eval(parse(text=paste(
    'diseno<-svydesign(data=base,ids=~1,weights=~',ponderador,')'
    ,sep='')))
  eval(parse(text=paste(
    'resultado<-svymean(~',variable,',diseno,na.rm=T)'
    ,sep='')))
  return(resultado)
}


#función que saca la suma de una variable tomando un ponderador------------

funcionSumaPonderada<-function(base,ponderador,variable){
  require(survey)
  
  eval(parse(text=paste(
    'diseno<-svydesign(data=base,ids=~1,weights=~',ponderador,')'
    ,sep='')))
  
  
  eval(parse(text=paste(
    'resultado<-svytotal(~',variable,',diseno,na.rm=T)'
    ,sep='')))
  return(resultado)
}


#función que junta todo-------------------------------------------


#función para promedio de medios-------------------------------------------
funcionTodo<-function(base,medioJM,medioPeli,filtro=NA,filtroCategoria,JUMP=0,operacion='media'){
  eval(parse(text=paste(
    "datosC<-funcionFiltroVariables(datosB,
    filtrosExactos=c('id','pond','",medioJM,"'),
    filtrosMultiples=c('",medioPeli,"','medias')) %>%
    filter(",medioJM,"==1)"
    ,sep='')))
  if(sum(!is.na(filtro))>0){
    mojon<-paste(filtro,collapse="','")
    eval(parse(text=paste(
      "datosC<-funcionFiltroVariables(datosB,
      filtrosExactos=c('id','pond','",mojon,"','",medioJM,"'),
      filtrosMultiples=c('",medioPeli,"','medias')) %>%
      filter(",medioJM,"==1)"
      ,sep='')))
  }
  if(sum(!is.na(filtro))>0){
    for(i in (1:length(filtro)))
      eval(parse(text=paste(
        "datosC<-datosC %>%
        filter(",filtro[i],"=='",filtroCategoria[i],"')"
        ,sep='')))
  }
  resultado<-NA
  if(nrow(datosC)>1){
    if(sum(!is.na(filtro))>0){
      JUMP<-length(filtro)
    }
    datosD<-funcionMedioHorarioHoras(datosC,'sumaHoras',(4+JUMP))
    datosD$sumaHoras[is.na(datosD$sumaHoras)]<-0
    # datosE<-datosD %>%
    #   filter(sumaHoras>0)
    if(operacion=='media')resultado<-funcionMediaPonderada(datosD,'pond','sumaHoras')
    if(operacion=='suma')resultado<-funcionSumaPonderada(datosD,'pond','sumaHoras')
  }
  return(resultado)
}



#######################################
#vectores de medios--------------------
#######################################

mediosJM<-c('GG_P1_r1','GG_P1_r2','GG_P1_r3','GG_P1_r4','GG_P1_r5',
            'GG_P1_r8','GG_P1_r9','GG_P1_r10')
mediosPeli<-c('radio','periodico','revista','tele','internet',
              'youtube','tvi','app')

###########################################################################################################
#medio por dispositivo-------------------------------------------------------------------------------------
###########################################################################################################

datos<-readRDS('tablaFinalv5.rds')
datos$id<-1:nrow(datos)

mediasHoras<-mhAbiertaConvencional
names(mediasHoras)[-1]<-paste(names(mediasHoras)[-1],'_medias')
names(mediasHoras)

#la pego en la base
datosB<-funcionTransmitir(da=datos,
                          db=mediasHoras,
                          va='id',
                          vb='id',
                          variables=names(mediasHoras)[-1])

#total
abiertaConvencional<-funcionTodo(base=datosB,
                                 medioJM=mediosJM[4],
                                 medioPeli=mediosPeli[4],
                                 operacion='suma'
)


#perfil
cont<-1
var1<-levels(datosB$perfil)
  for(i in 1:length(var1)){
    vector[cont]<-funcionTodo(base=datosB,
                              medioJM=mediosJM[4],
                              medioPeli=mediosPeli[4],
                              filtro=c('perfil'),
                              filtroCategoria=var1[i],
                              operacion='suma'
    )
    names(vector)[cont]<-paste0(var1[i])
    cont<-cont+1
    
  }

abiertaConvencionalPerfil<-vector
abiertaConvencional
abiertaConvencionalPerfil








#total por tipo de día de la semana-----------------------------

mediosJM<-c('GG_P1_r1','GG_P1_r2','GG_P1_r3','GG_P1_r4','GG_P1_r5',
            'GG_P1_r8','GG_P1_r9','GG_P1_r10')
mediosPeli<-c('radio','periodico','revista','tele','internet',
              'youtube','tvi','app')
var1<-levels(datosB$diaR2)


vector<-NULL
cont<-1
for(j in 1:length(mediosJM)){
  for(i in 1:length(var1)){
    vector[cont]<-funcionPromediosTodo(base=datosB,
                                       medioJM=mediosJM[j],
                                       medioPeli=mediosPeli[j],
                                       filtro=c('diaR2'),
                                       filtroCategoria=var1[i]
    )
    names(vector)[cont]<-paste0(mediosPeli[j],var1[i])
    cont<-cont+1
    
  }
}
vector
vectorTotalTipoDía<-vector


##############################################################
# tv abierta--------------------------------------------------
##############################################################

#cargo los datos----------------------------------------------------------
datos<-readRDS('tablaFinalv5.rds')
datos$id<-1:nrow(datos)

tail(names(datos))

#cargo la cosa de medias horas--------------------------------------------
mediasHoras<-readRDS('medias horas TV abierta.rds')
names(mediasHoras)[-1]<-paste(names(mediasHoras)[-1],'_medias')
names(mediasHoras)

#la pego en la base------------------------
datosB<-funcionTransmitir(da=datos,
                          db=mediasHoras,
                          va='id',
                          vb='id',
                          variables=names(mediasHoras)[-1])

# lo corro--------------------

mediosJM<-c('GG_P1_r4')
mediosPeli<-c('tele')

#total-----------------------------

vectorTotal<-NULL
cont<-1
for(i in 1:length(mediosJM)){
    vectorTotal[cont]<-funcionTodo(base=datosB,medioJM=mediosJM[i],medioPeli=mediosPeli[i],
                                   operacion='suma')
    names(vectorTotal)[cont]<-paste0(mediosPeli[i])
    cont<-cont+1
}

teleAbiertaTotal<-vectorTotal

#total por perfil-----------------------------

var1<-levels(datosB$perfil)
vector<-NULL
cont<-1
for(j in 1:length(mediosJM)){
  for(i in 1:length(var1)){
      vector[cont]<-funcionTodo(base=datosB,
                                            medioJM=mediosJM[j],
                                            medioPeli=mediosPeli[j],
                                            filtro=c('perfil'),
                                            filtroCategoria=var1[i],
                                operacion='suma'
      )
      names(vector)[cont]<-paste0(mediosPeli[j],var1[i])
      cont<-cont+1
    
  }
}
vector
teleAbiertaPorPerfil<-vector


##############################################################
# tv de paga--------------------------------------------------
##############################################################

#cargo los datos----------------------------------------------------------
datos<-readRDS('tablaFinalv5.rds')
datos$id<-1:nrow(datos)

tail(names(datos))

#cargo la cosa de medias horas--------------------------------------------
mediasHoras<-readRDS('medias horas TV de paga.rds')
names(mediasHoras)[-1]<-paste(names(mediasHoras)[-1],'_medias')
names(mediasHoras)

#la pego en la base------------------------
datosB<-funcionTransmitir(da=datos,
                          db=mediasHoras,
                          va='id',
                          vb='id',
                          variables=names(mediasHoras)[-1])

# lo corro--------------------

mediosJM<-c('GG_P1_r4')
mediosPeli<-c('tele')

#total-----------------------------

vectorTotal<-NULL
cont<-1
for(i in 1:length(mediosJM)){
  vectorTotal[cont]<-funcionTodo(base=datosB,medioJM=mediosJM[i],medioPeli=mediosPeli[i],
                                 operacion='suma')
  names(vectorTotal)[cont]<-paste0(mediosPeli[i])
  cont<-cont+1
}

telePagaTotal<-vectorTotal

#total por perfil-----------------------------

var1<-levels(datosB$perfil)
vector<-NULL
cont<-1
for(j in 1:length(mediosJM)){
  for(i in 1:length(var1)){
    vector[cont]<-funcionTodo(base=datosB,
                              medioJM=mediosJM[j],
                              medioPeli=mediosPeli[j],
                              filtro=c('perfil'),
                              filtroCategoria=var1[i],
                              operacion='suma'
    )
    names(vector)[cont]<-paste0(mediosPeli[j],var1[i])
    cont<-cont+1
    
  }
}
vector
telePagaPorPerfil<-vector

##############################################################
# tv ambos--------------------------------------------------
##############################################################

#cargo los datos----------------------------------------------------------
datos<-readRDS('tablaFinalv5.rds')
datos$id<-1:nrow(datos)

tail(names(datos))

#cargo la cosa de medias horas--------------------------------------------
mediasHoras<-readRDS('medias horas TV ambos.rds')
names(mediasHoras)[-1]<-paste(names(mediasHoras)[-1],'_medias')
names(mediasHoras)

#la pego en la base------------------------
datosB<-funcionTransmitir(da=datos,
                          db=mediasHoras,
                          va='id',
                          vb='id',
                          variables=names(mediasHoras)[-1])

# lo corro--------------------

mediosJM<-c('GG_P1_r4')
mediosPeli<-c('tele')

#total-----------------------------

vectorTotal<-NULL
cont<-1
for(i in 1:length(mediosJM)){
  vectorTotal[cont]<-funcionTodo(base=datosB,medioJM=mediosJM[i],medioPeli=mediosPeli[i],
                                 operacion='suma')
  names(vectorTotal)[cont]<-paste0(mediosPeli[i])
  cont<-cont+1
}

teleAmbosTotal<-vectorTotal

#total por perfil-----------------------------

var1<-levels(datosB$perfil)
vector<-NULL
cont<-1
for(j in 1:length(mediosJM)){
  for(i in 1:length(var1)){
    vector[cont]<-funcionTodo(base=datosB,
                              medioJM=mediosJM[j],
                              medioPeli=mediosPeli[j],
                              filtro=c('perfil'),
                              filtroCategoria=var1[i],
                              operacion='suma'
    )
    names(vector)[cont]<-paste0(mediosPeli[j],var1[i])
    cont<-cont+1
    
  }
}
vector
teleAmbosPorPerfil<-vector

#######################################################################
#junto los resultados en algo lindo------------------------------------
#######################################################################

resultado<-data.frame(c(teleAbiertaTotal,teleAbiertaPorPerfil))
resultado<-t(resultado)

resultado<-rbind(resultado,c(telePagaTotal,telePagaPorPerfil))
resultado<-rbind(resultado,c(teleAmbosTotal,teleAmbosPorPerfil))

rownames(resultado)<-c('abierta','paga','ambos')

resultado
write.csv(resultado,'160824 total de hroas vistas en TV abierta, paga y ambos.csv')

########################################################################
#tiempo total por medio por dispositivo---------------------------------
########################################################################



