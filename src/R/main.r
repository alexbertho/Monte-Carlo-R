mc.pi <- function(n) {
    # Génère n coordonnées x et y aléatoires uniformément dans [0, 1]
    x <- runif(n, 0, 1)
    y <- runif(n, 0, 1)
    
    # Compte le nombre de points à l'intérieur du quart de cercle de rayon 1
    # et estime Pi à partir de la proportion
    estimation_pi <- 4 * (sum(x^2 + y^2 <= 1) / n)
    
    return(estimation_pi) 
}

# # Définir les dimensions de la matrice
# t <- 50  # Nombre d'estimations par colonne
# p <- 7   # Nombre de colonnes

# PIE <- matrix(0, nrow = t, ncol = p)

# tE <- numeric(p)

# # Calculer les estimations de pi et les temps moyens
# for (j in 1:p) {
#     n <- 10^j  # Calculer n comme 10 à la puissance j
#     temps <- numeric(t)
#     for (i in 1:t) {
#         temps[i] <- system.time(PIE[i, j] <- mc.pi(n))[3]
#     }
#     tE[j] <- mean(temps) # Calculer le temps moyen
# }


# ERR <- matrix(0, nrow = t, ncol = p)

# # Calculer l'erreur relative pour chaque estimation dans PIE
# for (j in 1:p) {
#     for (i in 1:t) {
#         ERR[i, j] <- abs(pi - PIE[i, j]) / abs(pi)  # Calcul de l'erreur relative
#     }
# }

# ERR <- abs(PIE/pi - 1)
# par(mfrow=c(1,2),mar=c(4,4,2,2)+0.1)
# boxplot(ERR, main='Erreur relative sur PI', log='y', xlab='#points', ylab='Rel. Error')
# plot(10 ** (1:p), tE, type='b', main='Temps moyen d\'une simulation', log='x', xlab='#points', ylab='Time')


creer_polygone <- function (x,y) {
  matrix(c(x, x[1], y, y[1]), ncol=2,dimnames=list(c(), c("x","y")))
}

reg_poly <- function(n, r=1) {
  theta <- seq(0, 2*pi, length.out=n)
  x <- r * cos(theta)
  y <- r * sin(theta)
  creer_polygone(x, y)
}

boite <- function(polygone) {
  x <- polygone[,"x"]
  y <- polygone[,"y"]
  matrix(c(min(x), max(x), min(y), max(y)), ncol=2, dimnames=list(c("min", "max"), c("x","y")))
}

points_aleatoires <- function(n, bo) {
  # Définir une fonction points_aleatoires qui prend en argument un couple (n, bo), où n est un entier et bo une boîte rectangulaire, et renvoie une matrice M contenant n points tirés uniformément au hasard dans le rectangle r=[xmin;xmax]*[ymin;ymax]. Plus précisément, la matrice M est de taille n*2 et chaque ligne contient un point tiré uniformément au hasard dans le rectangle r.
  x <- runif(n, bo["min","x"], bo["max","x"])
  y <- runif(n, bo["min","y"], bo["max","y"])
  matrix(c(x,y), ncol=2, dimnames=list(NULL, c("x","y")))
}

# Fonction auxiliaire pour vérifier si un point est à l'intérieur d'un polygone
appartient_poly <- function(point, polygone) {
  # Initialisation du compteur d'intersections
  intersections <- 0
  
  # Boucler sur les côtés du polygone
  for (i in 1:(nrow(polygone) - 1)) {
    # Définir les coordonnées des points du côté
    x1 <- polygone[i, 1]
    y1 <- polygone[i, 2]
    x2 <- polygone[i + 1, 1]
    y2 <- polygone[i + 1, 2]
    
    # Vérifier si le côté coupe la demi-droite partant du point
    if ((y1 <= point[2] && y2 > point[2]) || (y1 > point[2] && y2 <= point[2])) {
      # Calculer la coordonnée x de l'intersection
      x <- (point[2] - y1) * (x2 - x1) / (y2 - y1) + x1
      
      # Vérifier si l'intersection est à droite du point
      if (x > point[1]) {
        # Incrémenter le compteur d'intersections
        intersections <- intersections + 1
      }
    }
  }
  
  # Vérifier si le dernier côté coupe la demi-droite partant du point
  x1 <- polygone[nrow(polygone), 1]
  y1 <- polygone[nrow(polygone), 2]
  x2 <- polygone[1, 1]
  y2 <- polygone[1, 2]
  if ((y1 <= point[2] && y2 > point[2]) || (y1 > point[2] && y2 <= point[2])) {
    x <- (point[2] - y1) * (x2 - x1) / (y2 - y1) + x1
    if (x > point[1]) {
      intersections <- intersections + 1
    }
  }
  
  # Renvoyer TRUE si le nombre d'intersections est impair, FALSE sinon
  return(intersections %% 2 == 1)
}

# Fonction principale pour vérifier si des points sont à l'intérieur d'un polygone
appartient <- function(points, polygone) {
  # Initialisation du vecteur de résultats
  resultats <- rep(FALSE, nrow(points))
  
  # Boucler sur les points
  for (i in 1:nrow(points)) {
    # Vérifier si le point est à l'intérieur du polygone
    resultats[i] <- appartient_poly(points[i, ], polygone)
  }
  
  # Renvoyer le vecteur de résultats
  return(resultats)
}

carre <- creer_polygone(c(10,10,90,90), c(30, 70, 70, 30))
## Une permutation cyclique des points donne le même polygone
carre <- creer_polygone(c(10,90,90,10), c(70, 70, 30, 30))
# ## En revanche, le code suivant ne définit pas un rectangle,
# ## mais un polygone dont les arêtes se croisent.
papillon <- creer_polygone(c(10,90,10,90), c(30,70,70,30))
# ## pour finir, voici un losange.
losange <- creer_polygone(c(50,10,50,90),c(30,50,70,50))

# print(carre)


# plot(carre, type='l')
# lines(papillon -1, type='b', col='firebrick')
# lines(losange, type='l', col='darkblue')


x <- c(0,0,9,11,11,9,8,11,9,6,3,3,8,9,9,8,2,2)
y <- c(0,12,12,10,7,5,5,0,0,5,5,7,7,8,9,10,10,0)
surprise <- creer_polygone(x,y)
# plot(surprise,col="black", type="l")

# print(losange)
# bo <- boite(losange)
# print(bo)

bo <- matrix(c(3, 5, 6, 8),nrow=2, dimnames=list(c("min","max"), c("x","y")))
pts <- points_aleatoires(5, bo)
# print(pts)

## Réaliser un test de la fonction
carre <- creer_polygone(c(0, 0, 1, 1), c(0, 1, 1, 0))
cc <- seq(from=-0.25,to=1.25,by=0.25)
cat(points)
pin <- appartient(points,carre);
# Dessiner le résultat du test
# par(mar=c(2,2,3,2)+0.1)
# plot(carre, type='l', main="Test de la fonction appartient", xlim=range(carre[,1],points[,1]), ylim=range(carre[,2],points[,2]))
# points(points[pin,1], points[pin,2], col='firebrick', pch=20)
# points(points[!pin,1], points[!pin,2], col='darkblue', pch=20)



