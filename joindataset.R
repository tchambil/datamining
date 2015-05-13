
data<-read.csv("MyData.csv")
dataempleado<-read.csv("dataempleado.cvs")

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
 
ingresototal<-function()
{
 
  empleado<-subset(data,!(is.na(data["ingtot"]) ))
  itotal<-subset(empleado,ingtot>0)
  return(itotal)
}
#87500
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
getdir <- function(id, directory,label) {
  fileStr <- paste(directory, "/", sprintf("%01d", as.numeric(id)), ".sav", 
                   sep = "")
  print(fileStr)
  rawDfr <- read.spss(fileStr,use.value.labels=label)
  
  return(rawDfr)
}

# MyData <- read.csv("2003.csv", header=TRUE, sep=",") 