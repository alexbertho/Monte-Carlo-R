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


