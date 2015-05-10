empleado <-function(directory, id=1:47, label =FALSE)
{
 tdato<-readspss(directory, id, label)
 d<- tdato[tdato$p200g== 1958,]
  return (d)
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