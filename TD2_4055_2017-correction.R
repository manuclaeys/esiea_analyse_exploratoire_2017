####TD2#####

Data <- read.csv("/home/manue/Documents/manue/Cours/cour ESIEA/4a/presidential_polls.csv", sep=',')

#/home/manue/Documents/manue/Cours/cour ESIEA/4a/td2/densityRawClinton.pdf
dataf <- as.data.frame(Data)

summary(dataf)


#Chargement des donn?es
#Les sondages de type ? polls-only ? vont pond?rer les r?sultats obtenus tel que
#les plus r?cents aient un poid plus fort que les anciens sondages
#Cependant, les diff?rences entre les sondages r?cents et anciens
#vont ?largir l'intervalle de confiance et par cons?quent, cr?er de l'incertitude.
#Les sondages de type ? polls-plus ? utilise des indices ?conomique et statistique pour
#r?duire l'incertitude gr?ce ? une r?duction des ?carts au cours du temps
#les sondages de type ? now-cast ? sont les moins incertains :
#il n'y a pas de pr?diction du r?sultat de la v?ritable ?lection, mais simplement 
#un estimateur des scores si l'?lection avait lieu le jour m?me. 
#elle est par cons?quente moins incertaine, mais peut-?tre moins r?aliste.



#En toute logique, les sondages vont
#avoir tendance ? se rapprocher du v?ritable r?sultat, au cours du temps,
#Cependant, des ?v?nements survenus lors de la campagne peuvent faire basculer des ?l?cteurs ind?cis ou 
#les pousser ? ne plus aller voter, les sondages seraient alors tr?s loin du r?sultat r?el

#Les sondages ? now-cast ? sont fiables lorsqu'ils sont fait ? quelques jours de l'?l?ction
#Si l'on cherche un bon estimateur ? plusieurs mois de l'?l?ction, il sera difficile d'exploiter 
#les r?sultats des sondages  ? now-cast ?

#La m?diane est beaucoup moins sensible aux valeurs extr?mes que la moyenne.
#La moyenne peut ?tre un bon esimateur si l'?cart type est faible. 
#On peut proc?der ? un centrage r?duction autour de la moyenne pour corriger cela
#==> https://fr.wikipedia.org/wiki/Variable_centr%C3%A9e_r%C3%A9duite


summary(dataf$type)

# Lorsque la variable est de type quantitatif, on
#obtient le minimum, le maximum, la moyenne, la m?diane et les 1er et
#3e quartiles. 
# Lorsque la variable est de type qualitatif (dans R on utilise g?n?ralement as.factor() ),
#on obtient les facteurs les plus fr?quents 
#Il existe trois types de sondage : polls-only, polls-plus et now-cast. 
#now-cast polls-only polls-plus 
#3412       3412       3412 
#Dans notre jeu de donn?e, il n'y a pas de biais d? ? une sur-repr?sentation d'un type de sondage



##Test de Kolmogorov-Smirnov

hist(dataf$rawpoll_clinton)
hist(dataf$rawpoll_trump)


hist(dataf$adjpoll_clinton)
hist(dataf$adjpoll_trump)


#Concernant Clinton, le mod?le ajust? "gonfle" son score.
#La fr?quence des sondages pr?sentant un score dans l'intervale [30 ; 40], pour les valeurs brutes
#diminue lorsqu'on observe l'histogrammes des scores ajust?s. 
#Les valeurs se d?callant vers la droite, il y a alors une fr?quence plus importante 
#dans l'intervalle [45 ; 60] pour les scores ajust?s
#Concernant Trump, que le mod?le centre et r?duit les valeurs autour de la moyenne
#Il y a une fr?quence plus importante pour un scors entre [40-45] dans le mod?le ajust?
mean(dataf$rawpoll_clinton)
mean(dataf$adjpoll_clinton)
mean(dataf$rawpoll_trump)
mean(dataf$adjpoll_trump)
sd(dataf$rawpoll_clinton)
sd(dataf$adjpoll_clinton)
sd(dataf$rawpoll_trump)
sd(dataf$adjpoll_trump)



x1 <- rnorm(nrow(dataf),mean = mean(dataf$rawpoll_clinton),sd=sd(dataf$rawpoll_clinton))
plot(density(x1))
ks.test(dataf$rawpoll_clinton,x1)

x2 <- rnorm(nrow(dataf),mean = mean(dataf$rawpoll_trump))
plot(density(x2))
ks.test(dataf$rawpoll_trump,x2)

ks.test(dataf$rawpoll_trump,dataf$rawpoll_clinton)


#La s?rie x1 correspond ? une g?n?ration de 10236 variables
#al?atoire suivant une loi normale, centr?e autour de la moyenne de rawpoll_clinton et d'?cart type = 1 (valeur par d?faut).
#10236 ?tant le nombre de sondages dont nous disponsons
#La figure 4 montre la repr?sentation graphique de la densit? de
#probabilit? de cette simulation x1. 


#m?me principe avec la s?rie x2 pour rawpoll_trump

#Le test de Kolmogorov-Smirnov permet de tester l'hypoth?se nulle selon
#laquelle deux ?chantillons suivent une m?me loi de probabilit?.
#On utilise nos s?rie x1 et x2 pour savoir si les s?ries
#rawpoll_clinton et rawpoll_trump suivent une loi normale centr? autour de la m?me moyenne (mais pas r?duite!)



#La p-value de ce test < 2,2?10e-16, elle est
# inf?rieure au seuil de 0,05 (indice de confiance ? 95%). 
#on rejette l'hypoth?se H0 en faveur de H1
#H0  : la s?rie suit une loi normale N(mu; 1).
#H1  : rejet de H0.
#la s?rie ne suit pas une loi normale N(mu; 1)
#


#On peut jouer avec l'?cart type pour faire d'autre hypoth?ses
x3 <- rnorm(nrow(dataf),mean = mean(dataf$rawpoll_clinton), sd=sd(dataf$rawpoll_clinton))
plot(density(x3))
ks.test(dataf$rawpoll_clinton,x3)

#On peut aussi utiliser shapiro via un ?chantillonage al?atoire de taille < 5000
shapiro.test(sample(dataf$rawpoll_clinton, 4999, replace = FALSE) )

#Regarder le r?sultat global de tous les sondages bruts ne permet pas de
#donner une bonne estimation du r?sultats des ?lections ? cause du bais d? ?
#la qualit? des sondages, le temps o? ils ont ?t? r?alis?s, la population 
#?chantillonn?e, le ou les ?tats observ?s, et le principe des ?l?ctions am?rication 
#o? le nombre de voix totals ne suffie pas ? gagner les pr?sidentielles 


#Le principe de l'analyse de la variance

# One Way Anova (Completely Randomized Design)
fit <- aov(rawpoll_clinton ~ type, data=dataf) 
fit
layout(matrix(c(1,2,3,4),2,2)) # optional layout
plot(fit) # diagnostic plots
summary(fit)

#La courbe Normal Q-Q repr?sente graphiquement le d?callage entre la distribution de nos donn?es et une distribution suivant
# la loi normale.
#Si la distribution ?tait normale la majorit? des points seraient centr? autour de (0,0) et il y aurait de moins en moins 
#de points r?partis sur la droite en pointill?.
#Ce n'est clairement pas le cas avec notre distribution.

#l'analyse de la variance n?c?ssite d'avoir une distribution suivant la loi normale. 
#A.N.O.V.A n'est pas adapt? pour notre jeu de donn?es
#L'A.N.O.V.A n?c?ssite ?galement d'avoir des ?chantillons ind?pendants 
#Il est impossible de savoir si une personnes a ?t? int?rog?e plusieurs fois par diff?rents sondages. 



#Une alternative ? l'ANOVA : le test de Kruskall-  Wallis

#Le test de Kruskal-Wallis est la g?n?ralisation ? K populations du test de la somme des rangs de
#Wilcoxon-Mann-Whitney bilat?ral. On le consid?re comme l'alternative non param?trique de l'ANOVA
#d?s que la distribution sous-jacente des donn?es n'est plus gaussienne. Il est extr?mement populaire
require(graphics)
boxplot(rawpoll_clinton ~ type, data = dataf)

kruskal.test(rawpoll_clinton ~ type, data=dataf)

boxplot(rawpoll_trump ~ type, data = dataf)

kruskal.test(rawpoll_trump ~ type, data=dataf)

#On ne rejette pas  l'hypoth?se nulle selon laquelle nos ?chantillons
#proviennent d'une population dont les param?tres de distribution
#sont ?quivalents.
#le type des sondages ne vient pas influencer la distribution des r?sultats des sondages


boxplot(rawpoll_clinton ~ grade, data = dataf)

kruskal.test(rawpoll_clinton ~ grade, data=dataf)

boxplot(rawpoll_trump ~ grade, data = dataf)

kruskal.test(rawpoll_trump ~ grade, data=dataf)



#On rejette l'hypoth?se nulle selon laquelle nos ?chantillons
#proviennent d'une population dont les param?tres de distribution
#sont ?quivalents.
#la qualit? des sondages vont influencer la distribution des scores


#Il faut donc observer les scores selon le grade de fa?on ind?pendante 


#Intervalle de confiance

rowClinA <- dataf[ which(dataf$grade=='A+'),]$rawpoll_clinton
summary(rowClinA)
a <- mean(dataf$rawpoll_clinton)
n <- length(rowClinA)


v <- var(rowClinA)

help(sd)

###calcule de l'intervalle de confiance pour une distribution gaussienne
 a <- mean(rowClinA)
 s <- sd(rowClinA)
 n <- length(rowClinA)
error <- 1.95996*s/sqrt(n)
left <- a-error
right <- a+error
left
right

#50% => 0.67449
#75% => 1.15035
#90% => 1.64485
#95% => 1.95996
#97% => 2.17009
#99% => 2.57583
#99.9% => 3.29053


#calcule de l'intervale de confiance pour une distribution t
moyenne <- mean(rowClinA)
ecartType <- sqrt(var(rowClinA ))
n <- length(rowClinA)
erreur <- qt(c(1 - 0.05/2) , df=n - 1) * (ecartType / sqrt(n))
intervalleGauche <- moyenne - erreur
intervalleDroit <- moyenne + erreur
intervalleGauche
intervalleDroit
#http://www.cyclismo.org/tutorial/R/confidence.html

library(gplots)
plotmeans(dataf$rawpoll_clinton ~ dataf$grade,xlab="grade",
          ylab="Clinton result", main="Mean Plot\nwith 95% CI") 




library(gplots)
plotmeans(dataf$rawpoll_clinton ~ dataf$state,xlab="grade",
          ylab="Clinton result", main="Mean Plot\nwith 95% CI") 







#Les intervalles selon les deux distibutions sont tr?s proches. L'intervalle de confiance ? 95 %
#calcul? ? la question pr?c?dente ? partir des r?sultats bruts de Clinton
#dans les sondages ayant obtenu la note A+ ([45;14 ; 46;57] pour une distribution gaussienne, [45;14 ; 46;58] 
#pour une distribution t) peut ?tre retrouv? sur la figure 9.



#Les derni?res questions ont ?t? d?battues en TD. 

