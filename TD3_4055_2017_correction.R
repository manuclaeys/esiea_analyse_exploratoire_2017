
###Load the data
d <-read.csv("C:/Users/claey/Documents/Cours/cour ESIEA/5A/My TD/TD 6/MyData.csv", sep = ",")



library(ggplot2)

summary(d$mine)

summary(d$imdb)

plot(density(d$mine))
plot(density(d$imdb))



#Notez les différences de densité sur les notes 
#données entre le visiteur et la bdd


#moyenne
mean(d$mine)
mean(d$imdb)

#écart-type
sd(d$mine)
sd(d$imdb)

#variance
var(d$mine)
var(d$imdb)

#la moyenne est de 6,64 pour les notes du visiteur
#la moyenne est de 7.51 pour les notes de imdb
#en observant l'écart type et la variance on constate que la 
#dispertion autour de la moyenne est plus réduite avec les 
#utilisateurs de imdb. 
#ce s'explique par une convergence vers la loi uniforme 
#lorsqu'on observe plusieurs échantillons (rappelez vous la vidéo
#que je vous avais montrée en cour)
#par contre cela ne fonctionne pas avec notre visiteur qui
#a un profil bien à lui 
hist(d$mine)

#( Les notes 6 et 7 sont les notes les plus fréquentes)
#et augmente l'écart type et par conséquence la variance

hist(d$imdb)

#La différence entre la courbe de densité et l'histogramme 
#se justifie par l'interprétation des notes dans un ensemble
#discret ou continue. 





###Corrélation
#visuellement, la corrélation se traduit par une représentation
#linéaire (ax + b = y) entre deux variables x et y. 
#Si le nuage de points se concentre autour d'une droite, 
#il y a fort à parier qu'il existe une relation de colinéarité 
#linéaire entre ces deux valeurs.


pairs(d[,c(17,18,12,14)])

#La fonction cor() vous permet d'avoir le coefficent 
#de linéarité
help(cor)
cor(d$mine, d$imdb)
cor(d$mine, d$Year)
cor(d$mine, d$Num..Votes)

cor(d$imdb, d$Year)
cor(d$imdb, d$Num..Votes)

cor(d$Year, d$Num..Votes)


#plus ce coef est, en valeur absolue, fort, plus la relation
#linéaire entre les deux variables est importante
#Le test de Pearson vous permet de tester statistiquement
#la corrélation, sous conditions d'une t-distribution
# cf pour plus d'info : http://www.sthda.com/french/wiki/test-de-correlation-entre-deux-variables




#Linear model 1
#On entraine le modèle sur les données de la database  IMDb afin de prédire les notes de l'utilisateur
summary(m1<-lm(mine~imdb, data=d))

#L'ordonnée (Intercept) indique que, en moyenne, ses notes sont plus d'un demi-point inférieur (-0.6). 
#Le coefficient positif de la note IMDb est positive et très proche de l'axe (0.9) 
#ce qui implique que 
# IMDb note , en moyenne, d'un point supérieur de celui de l'axe de prévision
# La note personnelle d'un point demi point inférieur de celui de l'axe de prévision. 
# La Figure 2 trace la relation entre les deux variables 

#Figure 2
p2 <- ggplot(d, aes(imdb, mine))+
  geom_point(position=position_jitter(width=0.1,height=.25),shape=16, size=4,alpha=0.6,
             aes(colour = new.genre, ))+
  stat_smooth(se = TRUE)+
  scale_x_continuous('IMDb ratings')+
  scale_y_continuous('Perso ratings')+
  theme_bw()+
  scale_colour_discrete(name="Genre")+
  scale_size_continuous(guide=FALSE)+
  theme(legend.position=c(0.15, 0.80))+
  geom_abline(size=1, aes(intercept=-0.6387, slope=0.9686))

p2

#La ligne noire est la forme de régression, 
#le bleu montre un lissage de loess non-paramétrique qui suggère une certaine 
#non-linéarité dans la relation que nous allons explorer plus tard.

#La régression locale, ou LOESS, est une méthode de régression non paramétrique 
#fortement connexe  
#qui combine plusieurs modèles de régression multiple au sein 
#d'un méta-modèle qui repose sur la méthode des k plus proches voisins. 
#« LOESS » est un acronyme qui peut être compris comme signifiant, en anglais, « LOcal regrESSion ».

#La régression locale est une alternative possible aux méthodes habituelles de régression, 
#comme la régression par les moindres carrés linéaire ou non linéaire, 
#dans les cas où ces dernières s'avèrent mal adaptées. Elle combine la simplicité de régression 
#linéaire par les moindres carrés avec la flexibilité de la régression non linéaire, 
#en effectuant une régression simple sur des sous-ensembles locaux de données. 
#L'un des principaux avantages de cette méthode est qu'elle rend inutile la définition d'une unique 
#fonction globale qui décrirait le modèle de régression, puisque la méthode consiste à 
#calculer autant de fonctions locales qu'il y a de segments de données.


help("aes")

#Notre utilisateur à surtout noté de la comédie, des films serieux
#et des films d'action.Certaines catégories ont un faible écart entre les 
#notes, en revanche si on prend la catégorie light, 
#pour certains films, notre utilisateur donne des notes plus basses que
#la moyenne des utilisateurs d'IMDb. 
#Notre modèle est perturbé par des catégories que notre utilisateur
#sous-évalue ou au contraire sur-évalue


#Régression linéaire simple
help(lm)

summary(m1<-lm(mine~imdb, data=d))
plot(m1)

#Le modèle sur la base des notes données par notre utilisateur 
#et la moyenne obtenue par IMDb sur les mêmes films.



#On affiche la racine d'erreur quadratique moyenne
#L'erreur quadratique moyenne est très utile pour comparer plusieurs estimateurs, 
#notamment lorsque l'un d'eux est biaisé. Si les deux estimateurs à comparer sont sans biais, 
#l'estimateur le plus efficace est simplement celui qui a la variance la plus petite. 
#On peut effectivement exprimer l'erreur quadratique moyenne en fonction du biais de l'estimateur

sqrt(mean(residuals(m1)^2)) 
#root mean squared error: 1.25
# c'est le même principe que pour un calcul de variance, 
#si on prend juste les différences entre valeurs mesurées et valeurs attendues, 
#on a des écarts positifs et négatifs qui se compensent. 
#Pour avoir que des écarts positifs, on peut soit prendre les valeurs absolues des écarts, 
#soit les carrés. 
#En prenant les carrés, on retombe sur des formules plus simples à mettre en oeuvre 
#(reste plus qu'à prendre la racine carrée pour retomber sur la bonne unité... ). 
#C'est la notion de moyenne quadratique par opposition à celle de moyenne arithmétique 
#(ce qui donne un écart-"type" plutôt qu'un écart moyen (nul) ou un écart-absolu moyen). 


sqrt(mean(residuals(m1)^2))


#Shalizi function pour une intervale de confiance
predlims <- function(preds,sigma) {
  prediction.sd <- sqrt(preds$se.fit^2+sigma^2)
  upper <- preds$fit+2*prediction.sd
  lower <- preds$fit-2*prediction.sd
  lims <- cbind(lower=lower,upper=upper)
  return(lims)
}

preds.lm <- predict(m1,se.fit=TRUE)
predlims.lm <- predlims(preds.lm,sigma=summary(m1)$sigma)
mean(d$mine <= predlims.lm[,"upper"]
     & d$mine >= predlims.lm[,"lower"]) 




plot(d$mine,preds.lm$fit,type="n", xlim=c(2,10), ylim=c(2,10),
     xlab="My actual ratings",ylab="Predicted ratings", main="")
segments(d$mine,predlims.lm[,"lower"],
         d$mine,predlims.lm[,"upper"], col="grey")
abline(a=0,b=1,lty="dashed")
points(d$mine,preds.lm$fit,pch=16,cex=0.8)





### Bien que le coefficient de partition IMDb est statistiquement très significative (error 0.0884)
###Le modèle d'ajustement est plutôt pauvre. La racine d'erreur quadratique moyenne est de 1,25 
###ce qui est important compte tenu de la variation des données. 
###Mais le mauvais ajustement est le plus clairement visible si nous traçons les données réelles
###par rapport aux prévisions.


#Il est possible d'observer la différence avec les données de IMDb pour les film noté par l'utilisateur. 
#et les notes de l'utilisateur en observant les données initiales. 

#Figure 4
d1<-subset(d, d$imdb>6.49 & d$imdb<7.5)
d2<-subset(d, d$imdb>7.51 & d$imdb<8.5)

p4<-ggplot (NULL, aes(mine))+
  geom_density(data = d1, fill='blue', alpha=0.4,aes(x=mine, y = ..density..))+
  geom_density(data = d2, fill='red', alpha=0.4,aes(x=mine, y = ..density..))+
  scale_x_continuous('Notes de l utilisateur comparées à la base d IMDb', breaks=seq(2,10,1))+
  scale_y_continuous('Density')+
  theme_bw()+theme(legend.position="none")
p4

#Comparez les différents pics de densité pour les deux courbes? 
#La Figure 4 montre la densité entre les notes de imdb - 6,5 à 7,5 (bleu) et celle de l'utilisateur  7.5- 8.5 (rouge). 
#Les moyenne pour les deux séries diffèrent quelque peu, mais le chevauchement de la densité est grande.
#En somme, la connaissance de la note IMDb fournit des informations, mais sur son propre 
#résultat mais pas sur le score de l'utilisateur. 


#Nous allons ajouter d'autres variables pour voir comment améliorer le modèle. 
#On voit souvent lorsqu'un film sort, que le fait d'ajouter un réalisateur connus 
#donnent un effet de levier.
#De la même façon, certain types de film sont plus facilement populaires que d'autre. 


#Linear model 2
summary(m2<-lm(mine~imdb+d$comedy +d$romance+d$mystery+d$Stanley.Kubrick..+d$Lars.Von.Trier..+d$Darren.Aronofsky..+year.c, data=d))
sqrt(mean(residuals(m2)^2)) #root mean squared error: 1.14

preds.lm <- predict(m2,se.fit=TRUE)
predlims.lm <- predlims(preds.lm,sigma=summary(m2)$sigma)
mean(d$mine <= predlims.lm[,"upper"]
     & d$mine >= predlims.lm[,"lower"]) 

#Les variables avec des *** sont pertinentes pour notre modèlisation


plot(d$mine,preds.lm$fit,type="n", xlim=c(2,10), ylim=c(2,10),
     xlab="My actual ratings",ylab="Predicted ratings", main="")
segments(d$mine,predlims.lm[,"lower"],
         d$mine,predlims.lm[,"upper"], col="grey")
abline(a=0,b=1,lty="dashed")
points(d$mine,preds.lm$fit,pch=16,cex=0.8)

#L'ajustement s'améliore quelque peu. 
#La racine de l'erreur quadratique moyenne de ce modèle est de 1,14 (contre 1.254 avant). 
#De plus, à la recherche de nouveau au réel par rapport évaluations prévues, 
#l'ajustement est mieux, surtout pour les films bien notés - pas surprenant étant donné 
#que les bon réalisateurs sont populaire.


#On séléctionne les film à partir de 1960
#La dernière variable dans la régression ci-dessous est alors l'année de sortie du film. 

d.60<-subset(d, Year>1960)
d.60$r<-residuals(lm(d.60$mine~d.60$imdb))


#Affichez un summary de la fonction lm sur le df d.60$r selon (~) sa variable Year
summary(lm(d.60$r~d.60$Year))


#On observe la régression linéaire selon les différentes années
#Figure 6. 
p6 <- ggplot(d.60, aes(Year, r))+
  geom_point(position=position_jitter(width=0.1,height=.25),shape=16, size=4,alpha=0.6,
             aes(colour = new.genre, ))+
  stat_smooth()+
  scale_x_continuous('Year of release')+
  scale_y_continuous('My ratings (residuals)')+
  theme_bw()+
  scale_colour_discrete(name="Genre")+
  scale_size_continuous(guide=FALSE)+
  theme(legend.position=c(0.15, 0.15))+
  geom_abline(size=1, aes(intercept=33.33, slope=-0.016659))


p6



predict(m1, d[1,], se.fit = TRUE)
