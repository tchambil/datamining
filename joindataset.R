

####################################
# GRAFICOS Y FUNCIONES ADICIONALES
#
#Funcion para graficar horas trabajas durante la semana

#Funcion para escribir archivos

write.table(datafinal, "datawekafinal2.csv", sep=",")
#write.table(datawekafinal, "datawekafinal.csv", sep=",")
write.arff(datafinal, "datawekafinal2.arff")

####################
histhora <- function(anho)
{
  adato<-subset(data, pano==anho)
  dt<-adato[[ "p209t" ]]
  
  hist(dt, breaks=seq(from=0,to=120,by=4), 
  prob=TRUE,col="grey",
  border="blue",
  xlab="Hora semana",
  ylab="Cantidad",main="Horas trabajadas durante la semana")
}


#Graficando promedios de horas trabajadas por dia
GMedia<-function()
{ da<-promediohoras()
  barplot(da, main="Promedio de horas por dia", 
          names.arg=c("Dom", "Lun", "Mar","Mie", "Jue","Vie","Sab"),
          xlab="Dias de la semana",
          ylab="Horas" )
  par(new=TRUE)
  abline(h = 7.6, col="red", lwd=1, lty=1)
}

#Obteniendo Promedio de Horas trabajadas por dia 
promediohoras <- function()
{
 mediana<-c()#mediana de los dias de semana
 x<-1
 for (i in 37:43)
 { 
  mediana[x]<- mean((data)[[ i ]],na.rm = TRUE )
  x<-x+1
 }
 return(mediana)
}

 #Funcion para obtener empleados con ingresos mayores a 0
ingresototal<-function()
{
 
  empleado<-subset(data,!(is.na(data["ingtot"]) ))
  itotal<-subset(empleado,ingtot>0)
  return(itotal)
}
#Fin graficos
#####################
  
datacorrelation<- subset(data, select = -c(p108, p208b3,sem) )
datacore<-cor(datacorrelation)
install.packages("corrgram")
library(corrgram)
install.packages("ellipse")
library(ellipse)
install.packages("FactoMineR")
library(FactoMineR)
R = cor(datacorrelation)
round(R, 4)
   
install.packages("sjPlot")
library(sjPlot)
source("sjPlotCorr.R")
sjp.corr(R)
sjp.corr(R, type="tile")
 
sjp.corr(R, type = "tile", hideLegend = FALSE)
sjp.frq(datac$p109a)
sjp.frq(data$anho)
sjp.frq(data$p108)
write.table(datacore, "correla3.csv", sep=",")

EmpleadoEdad<-EmpleadoEdad(data)

library(ggplot2)
library(PerformanceAnalytics)
chart.Correlation(datacorrelation)

#################################

#reemplzando=p108 x edad,p208b3 x anho
#datac<- subset(data, select = c(p205a=="4711") )  



require(stringr)



head(datar)
dataweka<-datar

#######################DATA 2015##########
data2015<-read.csv("EmpleadoEdad2015.csv")
Datafinal2015<-reemplaceData(data2015)
Datafinal2015<- subset(Datafinal2015, select = -c(p108,p208b3,p201,p204a,p103) )  
names(Datafinal2015)[names(Datafinal2015) == 'edad'] <- 'p108'
names(Datafinal2015)[names(Datafinal2015) == 'anho'] <- 'p208b3'
Datafinal2015 <- subset(Datafinal2015, select=c(1,21,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,22,18,19,20))
Datafinal2015<-Datafinal2015[!(Datafinal2015$p109a=="99"),]

Datafinal2015<- subset(Datafinal2015, select = c(p107,p108,p109a,p205a, p206, p207a, p208b3, p212e,p222,sem) )  
library(foreign)
write.arff(Datafinal2015, "dataWekaTodo2015.arff")

#######################DATA TODO ##################
dataR<-read.csv("datafinaltodo.csv")

data<- subset(dataR, select = -c(p108,p208b3,p201,p204a,p103) )  
names(data)[names(data) == 'edad'] <- 'p108'
names(data)[names(data) == 'anho'] <- 'p208b3'
data <- subset(data, select=c(1,21,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,22,18,19,20))
data<-data[!(data$p109a=="99"),]

library(foreign)
write.arff(data, "dataWekaTodo.arff")

#######################################
####
#INICIO PASO 8
#Mas limpieza de datos
write.table(Datafinal, "datafinaltodo.csv", sep=",")
Datafinal<-reemplaceData(dataR)
reemplaceData <-function(data)
{ 
  
  for (i in 1:nrow(data)) 
  {
    if(!is.na(data[i, "p205a"])==TRUE)
    {  print(i)
       data[i, "p205a"]<-substring(data[i, "p205a"], 1, 1)  
       
    }
    
    
  }
  return (data) 
}

#FIN PASO 7
####
########################
#Inicio paso 6
#Agrupar en Joven (J), adulto(A), adultomayor(AM), TercerarEdad(TE) de acuerdo a la edad.
#Agrupar anhnio de inicio de trabajo 
#library(foreign)

write.arff(EmpleadoEdad, "EmpleadoEdad2015.arff")
write.table(EmpleadoEdad, "EmpleadoEdad2015.csv", sep=",")
summary(EmpleadoEdad)
EmpleadoEdad<-EmpleadoEdad(datafinal2015)

EmpleadoEdad <-function(data)
{ #install.packages("tm")
  library(tm)
  data["edad"]<-NA
  data["anho"]<-NA
  for (i in 1:nrow(data)) 
  {
     print(i)
       
       if(data[i, "p108"]<=18){
         data[i, "edad"]<-"N"         
       }
      else if (data[i, "p108"]>18 & data[i, "p108"]<=30){
         data[i, "edad"]<-"J"         
       }
      else if (data[i, "p108"]>30 & data[i, "p108"]<=45){
               data[i, "edad"]<-"A"
       }
      else if (data[i, "p108"]>45 & data[i, "p108"]<=65){
           data[i, "edad"]<-"AM"
         }
      else if (data[i, "p108"]>65){
             data[i, "edad"]<-"TE"  
           }
  
     print(i)
      #definicion ML
     
      if(data[i, "p208b3"]<=1990){
        data[i, "anho"]<-"ML"
        }
      else if (data[i, "p208b3"]>1990 & data[i, "p208b3"]<=2000){
        data[i, "anho"]<-"L"        
      }
      else if (data[i, "p208b3"]>2000 & data[i, "p208b3"]<=2010){
        data[i, "anho"]<-"C"
      }
      else if (data[i, "p208b3"]>2010){
        data[i, "anho"]<-"R"  
      }
   
  }
  return (data)
}
#Final paso 6
##############

################
#Inicio Paso 5
# Reemplezar los NA
#
#Loop Rellenar los NA en los atributos 
#p202,p203, p2041,p2042,p2043
#p2044,p2045,p2046,p2047,p2048    
#p2049, p20410 
datafinal2015<-read.csv("datafinal2015.csv")
write.table(datafinal2015, "datafinal2015.csv", sep=",")
 datafinal2015<-datafinal(datacleanfinal2015)

datafinal<-function(data){
for (i in 1:nrow(data)) {
 
  print(i) 
  for(j in 6:17)
  {  
    if(!is.na(data[i, j])==FALSE)
    {   
      data[i, j]<-2
    }
  }    
}
return(data)

}

#Final Paso 5
#########################

################
#Inicio paso 4
#
#Eliminar atributos no relevantes para el estudio
#
#
#Funcion para eliminar NA's de los que no tiene mucha relevancia en la data empleado 
#ejecutar: 
 datacleanfinal2015<-datacleanfinal(dataEtiquetado2015)
 write.table(datacleanfinal2015, "datacleanfinal2015.csv", sep=",")

datacleanfinal<-function(data){
dataemp<-cleandata(data)
dataf <- subset(dataemp, select = -c(pano, 
                                    pmes,em,p104,p105,p106,p109b,p109c,
                                    p200g,p207b,p209a,p209b,p209c,p209d,
                                    p209e,p209f,p209g,p209h,p209t,p210, 
                                    p213, p214, p215, p216,p217,p218,
                                    p219, p220,p208b1, p208b2,p209bb,
                                    p209cc,p209ee,ingtot, ingprin, ocu200) )

return (dataf)

}

cleandata<-function(data)
{ 
  #Eliminando NA'S
  data<-subset(data,!(is.na(data["p108"]) ))
  data<-subset(data,!(is.na(data["p208b3"]) ))
  data<-subset(data,!(is.na(data["p212e"]) ))
  data<-subset(data,!(is.na(data["p222"]) ))
  #subset empleados con ingresos menores a 3000 Nuevos Soles
  data<-subset(data,ingtot<=3000)
  #subset empleado con horas trabajas mayores a 15 (dias domingo)
  data<-subset(data,p209t<96)
  return(data)
  
}



#Final paso 4
#################

################
#Inicio paso 3
# Creacion de atributo Clase
# Funcion para entiquetar empledos con subempleo
# guardar en excel  
 write.table(dataEtiquetado, "dataEtiquedatasubempleado.csv", sep=",")
# ejecutar: 
  dataEtiquetado2015<-subempleado(dataempleado2015a)
#
subempleado <-function(data)
{ #install.packages("tm")
  library(tm)
  semp<-data
  semp["sem"]<-NA
  for (i in 1:nrow(semp)) 
  {
    if(!is.na(semp[i, "p209t"])==TRUE)
    {  print(i)
       if(semp[i, "p209t"]>=35 & semp[i, "ingtot"]<850)
       {
         semp[i, "sem"]<-"Subempleado"
         
       }
       else
       {
         semp[i, "sem"]<-"NoSubempleado" 
       }
    }
    
  }
  return (semp)
}
#Final paso 3
##############

###############################
#Inicio paso 2
#
# Funcion para etiquetar como empleado
# la variable  data contiene los datos
# previamente almacenados en el archivo csv
# ejecutar function: 
# dataempleado2015a<-empleado(data2015)
# 
#write.table(dataempleado2015a, "dataempleado2015.csv", sep=",")
empleado<-function(data){
tdato <- data.frame(em = c(NA), data)
 for (i in 1:nrow(tdato)) {
   print(i) 
   for(j in 15:27)
    { 
      if(!is.na(tdato[i, j])==TRUE)
      {  
        if(tdato[i, j]==1)
        {
          tdato[i, "em"]<-1
          break
        }
      }
    }    
 }
 ndato <- subset(tdato, em == 1)
return (ndato)
}
#final paso 2

###################################################
#Inicio paso 1
#Funcion para construir la data desde archivos(47) spss
#para ejecutar: 
#data2015<-readspss("2015")
#guardar en excel: 
#
#write.table(data2015, "data2015", sep=",")

readspss<-function(directory,id = 1:47, label=FALSE){
  library(foreign)
  for (cid in id) 
  { print(cid)
    newdata1 <- getdir(cid, directory,label)
    if (!exists("dataframe"))
    { 
      dataframe <- data.frame(newdata1)
    }
    else if (exists("dataframe"))
    {
      temp_dataset <-data.frame(newdata1)
      dataframe<-rbind(dataframe, temp_dataset)
      rm(temp_dataset)
    }      
  } 
  data<-dataframe
  return (data) 
}
#Funcion para obtener ubicacion de archivos
getdir <- function(id, directory,label) {
  fileStr <- paste(directory, "/", sprintf("%01d", as.numeric(id)), ".sav", 
                   sep = "")
  print(fileStr)
  Dfr <- read.spss(fileStr,use.value.labels=label)
  return(Dfr)
}
#final paso 1

