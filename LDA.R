library("xlsx");
library(MASS);


# Importation des données
donnees <- read.xlsx("N:/cars.xls",sheetName="cars");

# Statistiques générales
# MoyPoids <- mean(donnees[["Weight"]]); # sd ... pour l'écart-type

# Centrer et réduire les variables
colonnesDInteret <- data.matrix(donnees[,2:8]);
colonnesDInteret <- scale(colonnesDInteret);
colonnesDInteret <- data.frame(colonnesDInteret);

# Fusion:
colonnesDInteret <- cbind(colonnesDInteret,donnees[,9]);
colnames(colonnesDInteret)[colnames(colonnesDInteret) == "donnees[, 9]"] <- "Origin";

# Visualisation
plot(colonnesDInteret[1:6], pch=20,col=c("red", "green", "blue")[as.numeric(colonnesDInteret$Origin)])


# Analyse discriminante descriptive (ADD)
resultatADD = lda(Origin~.,data=colonnesDInteret);

# Analyse discriminante prédictive (ADP)
resultatADP.pred = predict(resultatADD);

# Analayse factorielle discriminante
plot(resultatADP.pred$x[,1],resultatADP.pred$x[,2],bg=colonnesDInteret$Origin,pch=21, main="Analyse factorielle discriminante sur les données Cars", xlab="Premier axe discriminant",ylab="Second axe discriminant", cex.lab=1.5,cex.main=2);
m=matrix(rep(0,6),nrow=3,ncol=2);
for (i in 1:3){ for (j in 1:2) { m[i,j]=mean(resultatADP.pred$x[unclass(colonnesDInteret$Origin)==i,j])}};
points(m[,1],m[,2],pch=15,bg=1:3,cex=2,col=1:3);