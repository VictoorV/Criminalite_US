######################################### Exercice 1 #########################################
data(Orange)

############## Question 1 ##############

plot(Orange[1:7,2],Orange[1:7,3],col="blue",type='l',ylim=c(0,250),xlab="Âge",ylab="Circonférence")
points(Orange[8:14,2],Orange[8:14,3],col="red",type='l')
points(Orange[15:21,2],Orange[15:21,3],col="green",type='l')
points(Orange[22:28,2],Orange[22:28,3],col="purple",type='l')
points(Orange[29:35,2],Orange[29:35,3],col="black",type='l')
legend("topleft", legend = c("Arbre 1","Arbre 2","Arbre 3","Arbre 4","Arbre 5"), col = c("blue","red","green","purple","black"), lty = c(1,1,1,1,1))

plot(Orange[1:7,2],(Orange[22:28,3]-Orange[15:21,3]),col="blue",type='l',xlab="Âge",ylab="Différence de circonférence")

############## Question 2 ##############

arbre1<-lm(Orange[1:7,3]~Orange[1:7,2])
arbre2<-lm(Orange[8:14,3]~Orange[1:7,2])
arbre3<-lm(Orange[15:21,3]~Orange[1:7,2])
arbre4<-lm(Orange[22:28,3]~Orange[1:7,2])
arbre5<-lm(Orange[29:35,3]~Orange[1:7,2])

fit <- lm(circumference ~ age, data = Orange)
summary(fit)

############## Question 3 ##############

dataf<-matrix(nrow=5,ncol=2)
dataf[1,]=arbre1[["coefficients"]]
dataf[2,]=arbre2[["coefficients"]]
dataf[3,]=arbre3[["coefficients"]]
dataf[4,]=arbre4[["coefficients"]]
dataf[5,]=arbre5[["coefficients"]]

############## Question 4 ##############

modele <- lm(circumference ~ age*Tree, data = Orange)
summary(anova(modele))
anova(modele)

#On a pval très petite (plus petite que la alpha choisi) donc on peut conclure H1, les arbres sont indépendants



######################################### Exercice 2 #########################################
uscrimes<-read.csv("C:/Users/jmv/Documents/Travail/Master/r/projet_MS/uscrimes.csv",header=TRUE)

############## Question 1.1 ##############

mod01<-lm(uscrimes[,1] ~ uscrimes[,2])
mod02<-lm(uscrimes[,1] ~ uscrimes[,3])
mod03<-lm(uscrimes[,1] ~ uscrimes[,4])
mod04<-lm(uscrimes[,1] ~ uscrimes[,5])

mod1<-lm(uscrimes[,1] ~ uscrimes[,2]+0)
mod2<-lm(uscrimes[,1] ~ uscrimes[,3]+0)
mod3<-lm(uscrimes[,1] ~ uscrimes[,4]+0)
mod4<-lm(uscrimes[,1] ~ uscrimes[,5]+0)

############## Question 1.2 ##############

AIC(mod01,mod02,mod03,mod04,mod1,mod2,mod3,mod4)

# On choisit le modèle 02 d'après les valeurs de AIC (possède le plus petit AIC)

############## Question 1.3 ##############
par(mfrow=c(2,2))
plot(uscrimes[,2],uscrimes[,1],xlab="poverty",ylab="murder")
plot(uscrimes[,3],uscrimes[,1],xlab="single",ylab="murder")
poly02<-function(x){
  return(unname(mod02[["coefficients"]][1])+x*unname(mod02[["coefficients"]][2]))
}
poly2<-function(x){
  return(unname(mod2[["coefficients"]][1])*x)
}
curve(poly02(x),add=TRUE,col="blue")
curve(poly2(x),add=TRUE,col="red")
legend("topleft", legend = c("mod02","mod2"), col = c("blue","red"), lty = c(1, 1),cex=0.85)
plot(uscrimes[,4],uscrimes[,1],xlab="pctmetro",ylab="murder")
plot(uscrimes[,5],uscrimes[,1],xlab="pcths",ylab="murder")
par(mfrow=c(1,1))
# On choisit également le modèle 02 d'après les graphes

############## Question 1.4 ##############

n<-50
S<-(n-1)/n*var(uscrimes[,3])
T<-sqrt(n*S)*unname(mod02[["coefficients"]][2])/sqrt((summary(mod02)$sigma)^2) # Sous H0, T suit une loi de Student à n-2 degrés de liberté 
q<-qt(0.95,48)
# ZR={T >= q} donc on rejette H0, la pente est strictement positive

############## Question 2.1 ##############

# Y est le vecteur des variables à expliquer de taille (50,1)
# X est le vecteur des variables explicatives de taille (50,3) avec que des 1 en première colonne, pour l'intercept 
# B est le vecteur des coefficients de régression de taille (3,1)
# E est le vecteur des erreurs de taille (50,1)

############## Question 2.2 ##############

# L'estimateur de B vaut (Xt X)^-1 Xt Y
M1<-solve(matrix(c(50,700.8,555.5,700.8,10722.82,7919.22,555.5,7919.22,6278.23),nrow=3,ncol=3))
M2<-matrix(c(366.6,5664.32,4282.59),nrow=3,ncol=1)
B<-M1 %*% M2

############## Question 2.3 ##############

# SR = Yt Y -(Xt Y)t (Xt X)^-1 Xt Y
Y2<-3465.66
SR<-(Y2-t(M2) %*% M1 %*% M2)[1,1]

############## Question 2.4 ##############

# SR ~ sigma^2 khi2(47)

############## Question 2.5 ##############

est_var<-SR/47

############## Question 2.6 ##############

R2<-507.1816/777.7488
F<-(R2/2)/((1-R2)/(47))
# F suit sous H0 une loi de fiser de paramètres (2,47)
qf<-qf(0.95,2,47)
# On rejette H0, il existe au moins un des coefficents B1 ou B2 qui est non nul

############## Question 2.7 ##############

T2<-B[2]/(sqrt(est_var*M1[2,2]))
# T2 suit sous H0 une loi de student de paramètre 47
qt<-qt(0.975,47)
# On rejette H0, B1=/=0

############## Question 2.8 ##############

IC<-c(B[2]-qt(0.975,47)*sqrt(est_var*M1[2,2]),B[2]+qt(0.975,47)*sqrt(est_var*M1[2,2]))
