              ## Problem Set 1 ##
 
                                            
rm(list=ls())


##############################################
##############################################

## Exercise 1

## US air pollution data

usair<-read.table("data/usair.txt",header=TRUE) 

head(usair)
dim(usair)

##############################################
##############################################

## Exercise 2

##############################################
##############################################

## Exercise 3

## Pen digit data

pendigits<-read.table("data/pendigits.txt", sep=",",head=F)

names(pendigits)<-c(paste0(rep(c("x","y"),8),rep(1:8,each=2)),"digit")

head(pendigits)
dim(pendigits)

lookup<-c("darkgreen",  "brown", "lightblue",  "magenta", "purple", 
          "blue", "red", "lightgreen", "orange", "cyan")
names(lookup)<-as.character(0:9)

digit.col<-lookup[as.character(pendigits$digit)]
#                                            # color coding
