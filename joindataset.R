
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
    for(j in 15:27)
    { if(!is.na(tdato[i, j])==TRUE)
      {  
        if(tdato[i, j]==1)
        {
          tdato[i, "em"]<-1
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