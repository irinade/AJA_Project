# install.packages('tidyverse')
library(tidyverse)
library(ggplot2)

df1 <- read.csv("~/AJA/code/SingleVolumeCurves-paperNatCom-vf-df_corrected-indexes.csv")

#Exclusion des relevées pour lesquelles les cellules ne sont pas en phase G1: 
df <- subset(df1, df1$phase == 1)

#Exclusion des lignes pour lesquelles vitesse de croissance indisponible: 
df <- subset(df, df$advdt.av.4!= "NA")

#Suppression des colonnes non utiles ici: 
df$ID <- NULL
df$Frame <- NULL
df$ CellIdInAutoTracking<- NULL
df$sum_im.median <- NULL
df$sum_mask.median <- NULL
df$event <- NULL
df$date <- NULL
df$V1 <- NULL
df$V2 <- NULL
df$av <- NULL
df$x1.av.4 <- NULL
df$y1.av.4 <- NULL
df$x2.av.4  <- NULL
df$y2.av.4 <- NULL
df$bdvdt.av.4 <- NULL
df$median <- NULL
df$index <- NULL
df$SD1 <- NULL
df$R2.av.4 <- NULL
df$tnormg1 <- NULL
df$cond2 <- NULL

View(df)

# création d'une liste avec 1 seul fois chaque index diff de index2
list <- unique(df$index2)


TC <- list()
tc <- list() 
# Calcul du taux croissance
# Methode line (robust)
for (i in list){
  x <- filter(df, index2 == i)
  xi <- x$Volume1
  yi <- x$advdt.av.4
  coef <- coef(line(xi, yi))[2]
  TC <- c(TC, coef)}

# Methode lm (moindre carré)
for (i in list){
  x <- filter(df, index2 == i)
  xi <- x$Volume1
  yi <- x$advdt.av.4
  coef <- coef(lm(yi ~ xi))[2]
  tc <- c(tc, coef)}


#trier 
#Volumes à la naissance et cond:
Vb <- list()
condi <- list()
condi2 <- list()
dv <- subset(df1, df1$tnormi == 0)
for (i in list){
  w <- filter(dv, index2 == i)
  Vb <- c(Vb, w$Volume1)
  condi <- c(condi, w$cond)}

# renomer correctement condition cellules (pas utilisé dans rendu)
for (i in condi){
  if (i=="2"){
    condi2 <- c(condi2, "rosco")
  }
  else {condi2<-c(condi2, "ctrl")}
}


# Tableau final
data <- data.frame(index = list)
data <- mutate(data, taux_croissance_line = TC)
data <- mutate(data, taux_croissance_lm = tc)
data <- mutate(data, volume_naissance = Vb)
data <- transform(data, légende=unlist(condi2))
data <- as.data.frame(lapply(data, unlist))

View(data)   #dataframe not order



#Regression linéaire avec lm
ggplot(data,
       aes(x=as.numeric(data$volume_naissance),
           y=as.numeric(data$taux_croissance_lm)))+
  geom_point()+ geom_smooth(method="lm")+
  scale_x_continuous("Volume à la naissance (microm3)")+scale_y_continuous("Taux de croissance en phase G1 (hrs-1)", limits = c(-1,1.5))+
  ggtitle("Regression linéaire avec lm")

regression <- lm(as.numeric(data$taux_croissance_lm) ~ as.numeric(data$volume_naissance))
coef(regression)[2]   #coef de la regression linéaire
confint(regression, level = 0.95)   #interval de confiance a 95% de l'ordonné a l'origine et du coef de la regression linéaire



#Regression linéaire avec line
ggplot(data,
       aes(x=as.numeric(data$volume_naissance),
           y=as.numeric(data$taux_croissance_line),
           color = légende))+
  geom_point()+ geom_smooth(method="lm")+
  scale_x_continuous("Volume à la naissance (microm3)")+scale_y_continuous("Taux de croissance en phase G1 (hrs-1)", limits = c(-1,1.5))+
  ggtitle("Regression linéaire avec line")

regres <- lm(as.numeric(data$taux_croissance_line) ~ as.numeric(data$volume_naissance))
coef(regres)[2]
confint(regres, level = 0.95)
