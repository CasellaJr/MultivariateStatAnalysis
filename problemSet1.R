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
which(Cors>0.5 & Cors < 0.99)
which(Cors>0.4 & Cors < 0.5)
which(Cors<0.0)

'
COMMENTI
'
# 2)

boxplot(x1)
which(x1 < -71)   #9 
DF[9,] #9 is Miami
boxplot(x2)
which(x2 > 1500)  #11 29  metto >1500 perchè ne vogliamo da testo solo 2 per variabile
DF[11,]
DF[29,] #Chicago(+3000) e philadelphia
boxplot(x3)
which(x3 > 1600)  #di nuovo 11 e 29

'
# parentesi su x2 e x3
# forse inutile perchè questi outlier non sono outlier nelle
# singole distribuzioni
div = x2/x3 # fabbriche con >20 lavoratori per ogni 1k abitanti
boxplot(div) # ci sono città con tante fabbriche in rapp agli abitanti
which(div > 1.3) #5 27 31  Hartford Cleveland Providence
'

boxplot(x4) # ok
boxplot(x5)
which(x5 < 10)  #1 23 
DF[1,] 
DF[23,] #Phoenix Alburquerque
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
plot(density(x1))

qqnorm(x2,pch=16,main="Q-Q plot for x2")
qqline(x2,col="red") #right skew distribution
skewness(x2)
plot(density(x2))

qqnorm(x3,pch=16,main="Q-Q plot for x3")
qqline(x3,col="red") #right skew distribution
skewness(x3)
plot(density(x3))

qqnorm(x4,pch=16,main="Q-Q plot for x4")
qqline(x4,col="red") #normal
skewness(x4)
plot(density(x4))

qqnorm(x5,pch=16,main="Q-Q plot for x5")
qqline(x5,col="red") #little left skewness
skewness(x5)
plot(density(x5))

qqnorm(x6,pch=16,main="Q-Q plot for x6")
qqline(x6,col="red") #normal
skewness(x6)
plot(density(x6))

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
D<-scale(DF,scale=F) # centro rispetto alla media (no dev.std.)

d<-rep(0,n)
for(i in 1:n) d[i]<-t(D[i,])%*%solve(varcov)%*%D[i,]
help("mahalanobis")
d
mahalanobis(X,center=colMeans(X),cov=var(X))
#                                     # the same
plot(density(d))

plot(qchisq(ppoints(d),df=p),sort(d),main="Chisq Q-Q plot of Mahalanobis distance",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")
abline(0,1)
text(qchisq(ppoints(d),df=p),sort(d),label=order(d),
     pos=4,cex=0.5,offset=0.3)
# outlier 1, 9, 11. Erano stati tutti identificati in
# precedenza come outlier univariati in diverse variabili

# qqplot senza gli outlier ORA FUNZIONA

DF2 <- DF[-c(1,9,11),]
no_out <- mahalanobis(DF2,center=colMeans(DF2),cov=var(DF2))
plot(qchisq(ppoints(no_out),df=p),sort(no_out),main="Chisq Q-Q plot of Mahalanobis distance",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")
abline(0,1)
text(qchisq(ppoints(no_out),df=p),sort(no_out),label=order(no_out),
     pos=4,cex=0.5,offset=0.3)


##############################################
##############################################

## Exercise 2

# 1)
# PDF derivazione autovalori, calcolo della PEV e determinazione
# di rho rchiesta

# 2) 
# PDF derivazione matematica degli autovettori

# provo a simulare e vedere se i risultati combaciano
# definisco la distribuzione una volta con corr (rho) <0 e una
# con corr >0
A <- matrix(c(1,-0.1,0,  -0.1,1,-0.1,  0,-0.1,1), 3, 3)
B <- matrix(c(1,0.1,0,  0.1,1,0.1,  0,0.1,1), 3, 3)

# calcolo gli autovalori e vettori in entrambi i casi. gli autovettori
# sono giusti e i primi due (la seconda colonna e la prima) mi danno
# le prime 2 PC
pr1 <- eigen(A)
pr2 <- eigen(B)

library(MASS)
# ora provo a simulare e ad usare il comando prcomp sui dati
# passo quindi sal population level al sample level
XA<- mvrnorm(n=10000, mu=c(1,-1,2), Sigma=A)
XB<- mvrnorm(n=10000, mu=c(1,-1,2), Sigma=B)

pca.normA<- prcomp(XA)
pca.normB<- prcomp(XB)
# le PC sono praticamente uguali agli autovettori a meno di
# qualche piccola differenza dovuta al fatto che stiamo simulando

# 3)
# PDF derivazione di Z e dei suoi parametri come combinazione
# lineare di X

# 4)

rho <- -2/3
muz <- c(2,-3)
Sigz <- matrix(c(2-2*rho, 2*rho-1, 2*rho-1, 2-2*rho), ncol=2, ,byrow = T)

plot(muz[1], muz[2], xlab="Z1", ylab="Z2", main="Correlation = -2/3", pch=16,
     xlim=c(-6,10), ylim=c(-11,5))
library(ellipse)
abline(0,0)
abline(v=0)
lines(ellipse(x=Sigz,centre=muz,level=0.95),col="red",lwd=1.5)
# 5)

# con rho=2/3 le varianze diminuiscono e la correlazione
# diventa positiva quindi l'ellisse è più piccola, l'asse maggiore
# e quello minore cambiano segno e l'asse maggiore diventa
# parallelo alla retta y=x


# codice per plottare il disegno anche se non chiesto
'
rho <- 2/3
Sigz2 <- matrix(c(2-2*rho, 2*rho-1, 2*rho-1, 2-2*rho), ncol=2, ,byrow = T)

plot(muz[1], muz[2], xlab="Z1", ylab="Z2", main="Correlation = -2/3", pch=16,
     xlim=c(-6,10), ylim=c(-11,5))

abline(0,0)
abline(v=0)
lines(ellipse(x=Sigz2,centre=muz,level=0.95),col="red",lwd=1.5)
# 5)
'

##############################################
##############################################

## Exercise 3

## Pen digit data

pendig<-read.table("data/pendigits.txt", sep=",",head=F)

names(pendig)<-c(paste0(rep(c("x","y"),8),rep(1:8,each=2)),"digit")

head(pendig)
dim(pendig)

lookup<-c("darkgreen",  "brown", "lightblue",  "magenta", "purple", 
          "blue", "red", "lightgreen", "orange", "cyan")
names(lookup)<-as.character(0:9)

digit.col<-lookup[as.character(pendig$digit)]
#                                            # color coding

'Bisogna togliere la colonna digits dal dataset altrimenti
la considera come variabile (con varianza da spiegare)
mentre è solo una label'
# 1)

pendigits <- pendig
pendigits$digit <- NULL

n<-dim(pendigits)[1];n
p<-dim(pendigits)[2];p

pendigits.pca<-prcomp(pendigits)
pendigits.Npca<-prcomp(pendigits, scale=T)
pendigits.pca

summary(pendigits.pca)
pendigits.Npca$sdev^2

plot(1:p,pendigits.Npca$sdev^2,type="b")
abline(h=1)

#Dalla cumulative proportion si nota che le prime 2 componenti presentano circa il 53% della varianza dei dati.
#Le prime 3 circa il 69%, mentre le prime 4 circa il 78%. Già dalla 5a pc si ottiene poco incremento di varianza (circa 6%), 
#quindi io terrei le prime 4. 
# se consideriamo la pca fatta sul dataset standardizzato, notiamo che solo le
# prime 5 PC hanno varianza maggiore di 1. Anche 5 può essere un buon numero di PC
# Però adesso tramite lo screeplot cerco una conferma con l'elbow method

# screeplot
screeplot(pendigits.pca, type="lines", main="Screeplot for pendigits data")
abline(v=4, col="blue", lty=2, lwd=2)
abline(v=5, col="blue", lty=2, lwd=2)
#lo screeplot non è di particolare aiuto in questo caso, in quanto non c'è un elbow evidente. 

# 2) l'esercizio 2 richiede di utilizzare le prime 3 PCs.
# normalità univariata
qqnorm(pendigits.pca$x[,1], pch=16)
qqline(pendigits.pca$x[,1], col="red") #heavy tailed

qqnorm(pendigits.pca$x[,2], pch=16)
qqline(pendigits.pca$x[,2], col="red") #heavy tailed

qqnorm(pendigits.pca$x[,3], pch=16)
qqline(pendigits.pca$x[,3], col="red") #normal

# normalità multivariata
d <- as.data.frame(pendigits.pca$x[,1:3])

D<-scale(d,scale=F) # centro rispetto alla media (no dev.std.)
Mala <- mahalanobis(D,center=colMeans(D),cov=cov(D))
   
plot(density(Mala))

plot(qchisq(ppoints(Mala),df=3),sort(Mala),main="Chisq Q-Q plot of Mahalanobis distance",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")
abline(0,1)

# 3)
outs <- which(Mala>5)

# mycols <- c("forestgreen", "gold", "dodgerblue", "green", "red", "orange", "yellow", "blue", "black")
pairs(d,lower.panel=NULL,col=lookup,pch=16)

'si riesce a fare lo scatterplot di un colore per volta?'


# 4)
boxplot(pendigits.pca$x[,1],main="PC1")
boxplot(pendigits.pca$x[,2],main="PC2")
boxplot(pendigits.pca$x[,3],main="PC3")

outs <- which(Mala>5)
outt <- pendig$digit[outs]

table(outt)
hist(outt)

install.packages("knitr")
install.packages("rmarkdown")
#
