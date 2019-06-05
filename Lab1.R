### Restaurant data from reviewers in US cities
##
##
##
##

# Import session files as a singular dataframe

library(data.table)
library(dplyr)
library(purrr)
library(raster)
library(stringr)
library(stringi)
library(arules)
library(arulesViz)

### Get features.txt ###
setwd("~/Data Cert/CSDA1040_FT/Lab1 - Restaurants/entree")
features <- read.csv("features.txt",sep="\t",col.names=c("Code","Feature"),colClasses="character")

##Enter session information
setwd("~/Data Cert/CSDA1040_FT/Lab1 - Restaurants/entree/session")
file_list <- list.files()

for (file in file_list){
  
  # if the merged dataset does exist, append to it
  if (exists("session")){
    temp_dataset <-read.delim(file, header=FALSE, strip.white = TRUE,fill=TRUE,sep=" ")
    session<-bind(session, temp_dataset)
    rm(temp_dataset)
  }
  # if the merged dataset doesn't exist, create it
  if (!exists("session")){
    session <- read.delim(file, header=FALSE, strip.white = TRUE,fill=TRUE, sep=" ")
  }  
  
}

#pull tag codes
yz<-as.character(session$V2)
custlist={}
cust<-str_extract_all(yz,"(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+")
##########
#as.data.frame(nums)
tagCust <- as.data.frame(t(stri_list2matrix(cust)))
#######  Merge back into city dataframe
session <- cbind(session$V1,tagCust)
colnames(session)[1:3] <- c("Data","Url","Start")

##############   RESTAURANT DATA   ##################################

setwd("~/Data Cert/CSDA1040_FT/Lab1 - Restaurants/entree/data")
file_list2 <- list.files()

## Import and bind all "session" files into one
#
for (file in file_list2){
  filename <- as.character(file)
  
  # if the merged dataset does exist, append to it
  if (exists("city")){
    temp_dataset2 <-read.csv(file, header=FALSE,fill=TRUE,stringsAsFactors = FALSE,sep="\t",colClasses = "character")
    name <- rep(substr(filename,0,nchar(filename)-4),nrow(temp_dataset2))
    temp_dataset2 <- cbind(name, temp_dataset2)
    city<-bind(city, temp_dataset2)
    rm(temp_dataset2)
    
  }
  # if the merged dataset doesn't exist, create it
  if (!exists("city")){
    city <- read.csv(file, header=FALSE, fill=TRUE,stringsAsFactors = FALSE,sep="\t",colClasses = "character")
    name <- rep(substr(filename,0,nchar(filename)-4),nrow(city))
    city <- cbind(name, city)
  }
  
}

#pull tag codes
xy<-as.character(city$V3)
numslist={}
nums<-str_extract_all(xy,"\\d{3}")
##########
#as.data.frame(nums)
tagInds <- as.data.frame(t(stri_list2matrix(nums)))
#######  Merge back into city dataframe
city2 <- cbind(city$V1,city$name,city$V2,tagInds)

##Convert factor tag codes to numeric-class
indx <- sapply(tagInds, is.factor)
tagInds[indx] <- lapply(tagInds[indx], function(x) as.numeric(as.character(x)))
#######
#create 'a' matrix, which is a 4160*256 matrix where having a code is indicated by a "1" in the corresponding column
a = matrix(0, nrow = nrow(tagInds) , ncol = 256)
colnames(a) <- features[,2]
for (i in 1:nrow(tagInds)){
  dimtag<-dim(which( !is.na(tagInds[i,]), arr.ind=TRUE)) 
  for (j in 1:dimtag[1]){
    a[i,tagInds[i,j]]=1
  }
}

##  ONLY CHICAGO RESTAURANTS  ##
chicago <- dplyr::filter(city2, grepl('chicago', city2$`city$name`))


##  CREATE A DATAFRAME OF LETTERS WITH START AND END POINTS ##
sessionL <- session[,-c(1:3)]

mypattern = "^(\\d{3})([A-Z])"

sessionL[] = lapply(sessionL, gsub, pattern=mypattern,replacement = "\\2")
mypattern = "^(\\d{2})([A-Z])"
sessionL[] = lapply(sessionL, gsub, pattern=mypattern,replacement = "\\2")
mypattern = "^(\\d{1})([A-Z])"
sessionL[] = lapply(sessionL, gsub, pattern=mypattern,replacement = "\\2")

##  Cut out the unknown starting restaurants
sessionL <- cbind(session[,3],sessionL)
colnames(sessionL)[1] <- "Start"

#sessionL <- sessionL[sessionL$Start != 0,]
transL1 <- data.frame(sapply(sessionL,as.factor))
###  Apriori attempt
rules <- apriori(transL1,parameter = list(support=0.005,confidence=0.5))
plot(rules)

plotly_arules(rules)
inspectDT(rules)
rules <- rules[!is.redundant(rules)]
inspectDT(rules)
gi <- generatingItemsets(rules)
d <- which(duplicated(gi))
nondup<-rules[-d]
nondup <- nondup[!is.redundant(nondup)]
inspectDT(nondup)

bucket <- as.data.frame(t(transL1))
View(bucket)
itemFrequencyPlot(bucket,topN=10,type="absolute")
