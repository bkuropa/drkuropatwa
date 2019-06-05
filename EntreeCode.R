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




