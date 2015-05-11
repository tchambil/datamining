empleado <-function(directory, id=1:47, label =FALSE)
{
 tdato<-readspss(directory, id, label)
 
 #d<- tdato[tdato$p2041== 1,]
 #f<-subset(d,p2041==1 | p2042==1 )
 # Agrega nueva columna
 tdato <- data.frame(em = c(NA), tdato)
 for (i in 1:nrow(tdato)) {
   if(!is.na(tdato[i, "p201"]==1)  
      || !is.na(tdato[i, "p202"]==1) 
      || !is.na(tdato[i, "p203"]==1)
      || !is.na(tdato[i, "p2041"]==1)
      || !is.na(tdato[i, "p2042"]==1)  
      || !is.na(tdato[i, "p2043"]==1) 
      || !is.na(tdato[i, "p2044"]==1)
      || !is.na(tdato[i, "p2045"]==1)
      || !is.na(tdato[i, "p2046"]==1)
      || !is.na(tdato[i, "p2047"]==1)  
      || !is.na(tdato[i, "p2048"]==1)
      || !is.na(tdato[i, "p2049"]==1)
      || !is.na(tdato[i, "p20410"]==1) )
      {
     tdato[i, "em"]<-1
        
    }
   }

return (tdato)
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