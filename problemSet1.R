              ## Problem Set 1 ##
 
                                            
rm(list=ls())


##############################################
##############################################

## Exercise 1

## US air pollution data

usair <- read.table("data/usair.txt", header = TRUE)
# converto a dataframe
DF <- as.data.frame(usair)
head(DF)


# cancello la colonna da ignorare
DF$SO2 <- NULL
head(DF)
# "Neg.Temp" "Manuf"  "Pop"   "Wind"  "Precip"  "Days" 
#names(DF) <- paste("x",1:6,sep="")
names(DF) <- c('x1','x2','x3','x4','x5','x6')
attach(DF)

# 1)
# Media di ciascuna colonna
x_bar <- colMeans(DF)
# matrice di correlazione e covarianza
Cors <- cor(DF)
varcov <- cov(DF)

x_bar
Cors
varcov
# commentare le correlazioni
'
COMMENTI
'
# 2)

boxplot(x1)
which(x1 < -71)   #9 Miami
boxplot(x2)
which(x2 > 1500)  #11 29  Chicago(+3000) e philadelphia
boxplot(x3)
which(x3 > 1600)  #come sopra
# parentesi su x2 e x3
div = x2/x3 # fabbriche con >20 lavoratori per ogni 1k abitanti
boxplot(div) # ci sono città con tante fabbriche in rapp agli abitanti
which(div > 1.3) #5 27 31  Hartford Cleveland Providence

boxplot(x4) # ok
boxplot(x5)
which(x5 < 10)  #1 23 Phoenix Alburquerque
boxplot(x6)
which(x6<60) #stessi due

# calcolo la distanze di mahalanobis univariata

DF_centr = sweep(DF,2,x_bar) # tolgo la media ad ogni riga
names(DF_centr) <- c('m1','m2','m3','m4','m5','m6')
attach(DF_centr)
varianze = diag(varcov) # estraggo le varianze delle colonne
for (i in 1:6){
    DF_centr[,i] <- DF_centr[,i]/varianze[i]
} # calcolo le distanze per ogni osservazione per ogni colonna

# boxplot(m1) etc.. si vedono i boxplot di prima ma normalizzati

# 3)
install.packages("moments")
library(moments)
qqnorm(x1,pch=16,main="Q-Q plot for x1")
qqline(x1,col="red") #quite normal
skewness(x1)
qqnorm(x2,pch=16,main="Q-Q plot for x2")
qqline(x2,col="red") #right skew distribution
skewness(x2)
qqnorm(x3,pch=16,main="Q-Q plot for x3")
qqline(x3,col="red") #right skew distribution
skewness(x3)
qqnorm(x4,pch=16,main="Q-Q plot for x4")
qqline(x4,col="red") #normal
skewness(x4)
qqnorm(x5,pch=16,main="Q-Q plot for x5")
qqline(x5,col="red") #little left skewness
skewness(x5)
qqnorm(x6,pch=16,main="Q-Q plot for x6")
qqline(x6,col="red") #normal
skewness(x6)

# 4)
# plotto i scatterplot per ogni coppia di variabili piazzando sui grafici
# l'indice dell'istanza
n <- dim(DF)[1]
the.labels <- 1:n
pairs(DF, panel = function(x, y) text(x, y, labels = the.labels))

# le istanze che compaiono più volte come outlier multivariati
# sono la 11 e la 29, Chicago e Philadelphia, le stesse che erano
# outlier nelle analisi di x2 e x3 univariate


# 5-6)
## multivariate normality
n<-dim(DF)[1]
n
p<-dim(DF)[2]
p
X<-DF
# one<-rep(1,n)
# D<-as.matrix(X-one%*%t(bar.x))
D<-scale(DF,scale=F)

d<-rep(0,n)
for(i in 1:n) d[i]<-t(D[i,])%*%solve(varcov)%*%D[i,]
help("mahalanobis")
d
mahalanobis(X,center=colMeans(X),cov=var(X))
#                                     # the same
plot(d)

plot(qchisq(ppoints(d),df=p),sort(d),main="Chisq Q-Q plot of Mahalanobis distance",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")
abline(0,1)
text(qchisq(ppoints(d),df=p),sort(d),label=order(d),
     pos=4,cex=0.5,offset=0.3)
#outliers 1, 9, 11. L'11 era stato identificato anche in precedenza

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
