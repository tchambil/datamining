
data<-read.csv("MyData.csv")

datanho<-subset(data, pano==2003)

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

horatrabaja<-function(directory, id=1:47, label =FALSE)
{
  ingresototal<-ingresototal(directory, id, label)
  htotal<-subset(ingresototal,p209t>35)
  return(htotal)
  
}
ingresototal<-function(directory, id=1:47, label =FALSE)
{
  empleado<-empleado(directory, id, label)
  itotal<-subset(empleado,ingtot>0)
  return(itotal)
}

empleado <-function(directory, id=1:47, label =FALSE)
{
 tdato<-readspss(directory, id, label)
 
 #d<- tdato[tdato$p2041== 1,]
 #f<-subset(d,p2041==1 | p2042==1 )
 # Agrega nueva columna
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

cleandata<- function(directory, id=1:47, label =FALSE)

  {
            data<-readspss(directory, id, label)
              
           return(data)
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
  #fileout <- paste(directory, "/", directory ,sep = "")
  #write.table(dataframe, paste(fileout, ".csv",sep=""), sep=",")
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