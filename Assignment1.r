setwd("C:/Users/ali/Documents/Aaron/R Projects/JHK R programming/Prog Assgnt 1")

pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332){
  #container for all non-na data
  data.vec <- c()
  
  # set working directory
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  
  #get all file path list
  all.files <- as.character(list.files(directory))
  file.path <- paste(directory, all.files, sep = "")
  
  #combine all non-na data into container
  for (i in id){
    current <- read.csv(file.path[[i]], header = TRUE, sep = ",")
    data <- na.omit(current[pollutant])[[1]]  
    data.vec <- c(data.vec, data)
  }
  
  mean(data.vec)
}

complete <- function(directory, id = 1:332){
  #container for all non-na data
  data.df <- data.frame()
  
  # set working directory
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  
  #get all file path list
  all.files <- as.character(list.files(directory))
  file.path <- paste(directory, all.files, sep = "")
  
  #combine all non-na data into container
  for (i in id){
    current <- read.csv(file.path[[i]], header = TRUE, sep = ",")
    data <- na.omit(current)[[1]]
    data.df <- rbind(data.df, c(i, length(data)))
  }
  
  colnames(data.df) <- c("id", "nobs")
  data.df
}

corr <- function(directory, threshold=0){
  #container for all non-na data
  corr.vec <- c()
  
  # set working directory
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  
  #get all file path list
  all.files <- as.character(list.files(directory))
  file.path <- paste(directory, all.files, sep = "")
  
  id <- 1:length(file.path)
  #combine all non-na data into container
  for (i in id){
    current <- read.csv(file.path[[i]], header = TRUE, sep = ",")
    data <- na.omit(current)
    if(length(data[,1]) > threshold){
      correlation <- cor(data[,2], data[,3])
      corr.vec <- c(corr.vec, correlation)
    }
    
  }
  corr.vec
}


##Test
#source("pollutantmean.R")
#pollutantmean("specdata", "sulfate", 1:10)
## [1] 4.064
#pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706
#pollutantmean("specdata", "nitrate", 23)
## [1] 1.281