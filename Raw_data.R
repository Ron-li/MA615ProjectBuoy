```{r}
library(tidyverse)
library(stringr)
library(rstanarm)

url_1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=mlrf1h"
url_2 <- ".txt.gz&dir=data/historical/stdmet/"
years <- c(1999:2018)
urls <- str_c(url_1, years, url_2, sep = "")
filenames <- str_c("mr", years, sep = "")


# Year 1999 - 2006
for(i in 1:8){
  suppressMessages(
    # Fill any missing values with NA:
    assign(filenames[i], read.table(urls[i], header = TRUE, fill = TRUE))
  )
  
}
# Year 2007 - 2018
for(i in 9:20){
  suppressMessages(
    # Fill any missing values with NA and use the same column names as year 2006
    assign(filenames[i], read.table(urls[i], header = FALSE, 
                                    fill = TRUE, col.names = colnames(mr2006))),
  )
  
}

```

```{r}
#screen out data at 13 o'clock
i<-1999
repeat { 
  assign(paste("mr",as.character(i),sep=""),get(paste("mr",as.character(i),sep=""))[which(get(paste("mr",as.character(i),sep=""))$hh == 13), ])
  i=i+1
  if(i>2018)
  {break}
}

```


```{r}
mr1999$TIDE <- NA
n <- 20
for (i in 1:n){
  file <- get(filenames[i])
  colnames(file)[1] <-"YYYY"
  if(ncol(file) == 18){
    file <- subset(file, select = -mm )
  }
  if(i == 1){
    MRC <- file
  }else{
    MRC <- rbind.data.frame(MRC, file)
  }
  
}


MRC<-MRC[c(1,2,3,13,14)]
```

```{r}
#clean up abnormal data
MRC$ATMP[which(MRC$ATMP>=100)]=NA
MRC$WTMP[which(MRC$WTMP>=100)]=NA
MRC=na.omit(MRC)
```
