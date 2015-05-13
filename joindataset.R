#El Archivo MyData.cvs contiene toda la data completa, importada desde spss
#Lista para ser preporcesado
data<-read.csv("MyData.csv")

#El archivo dataempleado.cvs contiene data de empleados con ingresos mayores a 0 y sin NA
dataempleado<-read.csv("dataempleado.csv")

#Funcion para obtener empleados con ingresos mayores a 0
ingresototal<-function()
{
  
  empleado<-subset(data,!(is.na(data["ingtot"]) ))
  itotal<-subset(empleado,ingtot>0)
  return(itotal)
}



 

#Funcion para preprar data para Weka (algoritmo arbol binario)
dweka<-function()
{
  dataemp<-cleanempleado()
  dwek <- subset(dataemp, select = -c(pano,estrato, 
                                         X,pmes,em,p104,p105,p106,p109b,p109c,
                                         p108,p207b,p209a,p209b,p209c,p209d,
                                         p209e,p209f,p209g,p209h,p209t,p210, 
                                         p213, p214, p215, p216,p217,p218,
                                         p219, p220,p208b1, p208b2,p209bb,
                                         p209cc,p209ee,ingprin) )
 
  dataeval<- dwek
  
  #Loop Rellenar los NA en los atributos 
  #p202,p203, p2041,p2042,p2043
  #p2044,p2045,p2046,p2047,p2048    
  #p2049, p20410 
  
  for (i in 1:nrow(dataeval)) {
    print(i) 
    for(j in 6:17)
    { 
      if(!is.na(dataeval[i, j])==FALSE)
      {   
        dataeval[i, j]<-2
      }
    }    
  }
  
  View(dataeval)
  Summary(dataeva)
  return(dataeval)
  
}
#Funcion para eliminar NA's de los que no tiene mucha relevancia en la data empleado 
cleanempleado<-function()
{
  dataempleado<-subset(dataempleado,!(is.na(dataempleado["p200g"]) ))
  dataempleado<-subset(dataempleado,!(is.na(dataempleado["p208b3"]) ))
  dataempleado<-subset(dataempleado,!(is.na(dataempleado["p212e"]) ))
 return(dataempleado)
   
}
#Funcion para entiquetar empledos con subempleo
subempleado <-function()
{ #install.packages("tm")
  library(tm)
  semp <- data.frame(sem = c(NA), data)
  for (i in 1:nrow(semp)) 
    {
      if(!is.na(semp[i, "p209t"])==TRUE)
      {  print(i)
        if(semp[i, "p209t"]>=35 & semp[i, "ingtot"]<850)
        {
          semp[i, "sem"]<-1
        
        }
        else
        {
          semp[i, "sem"]<-0 
        }
      }
         
  }
  return (semp)
}

#Funcion para graficar horas trabajas durante la semana
histhora <- function(anho)
{
  adato<-subset(data, pano==anho)
  dt<-adato[[ "p209t" ]]
  
  hist(dt, breaks=seq(from=0,to=120,by=4), prob=TRUE,col="grey",
  border="blue",
  xlab="Hora semana",
  ylab="Cantidad",main="Horas trabajadas durante la semana")
  #lines(density(dt), col = "red")
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
#Funcion para etiquetar como empleado
empleado <-function(directory, id=1:47, label =FALSE)
{
 tdato<-readspss(directory, id, label)
 tdato <- data.frame(em = c(NA), tdato)
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
#Funcion para construir la data desde archivos(47) spss
readspss<-function(directory,id = 1:47, label=FALSE){
  library(foreign)
  for (cid in id) 
  {
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
  rawDfr <- read.spss(fileStr,use.value.labels=label)
  
  return(rawDfr)
}

# MyData <- read.csv("2003.csv", header=TRUE, sep=",") 