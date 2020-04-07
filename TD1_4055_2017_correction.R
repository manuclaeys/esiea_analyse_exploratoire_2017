#########TD1


        
Data <- read.csv("/home/manue/Documents/manue/Cours/cour ESIEA/4a/vgsales.csv", sep=',')

df <- as.data.frame(Data)
 

help("data.frame")
#The function data.frame() creates data frames, tightly coupled collections of 
#variables which share many of the properties of matrices and of lists, used as the fundamental 
#data structure by most of R's modeling software.
 

summary(df)
mean(df$JP_Sales)

#moyenne de la colonne JP_Sales est de 0.07778, 
#cette valeur est identique ? la moyenne donn?e par la fonction
#summary().

#Genre     
#Action      :3316  
#Sports      :2346  
#Misc        :1739  
#Role-Playing:1488  
#Shooter     :1310  
#Adventure   :1286  
#(Other)     :5113  

#A priori, le genre: "Action" a ?t? le plus vendu (3316 jeux) en observant la fonction summary().

#Si on souhaite v?rifier 
box1 <- boxplot( Global_Sales ~ Genre, col='yellow', 
         pch=23, 
         xlab = "Genre", ylab = "Global_Sales",
         data=df)


box1 <- boxplot( JP_Sales ~ Genre, col='yellow', 
                 pch=23, 
                 xlab = "Genre", ylab = "JP_Sales",
                 data=df)

box1 <- boxplot( EU_Sales  ~ Genre, col='yellow', 
                 pch=23, 
                 xlab = "Genre", ylab = "EU_Sales",
                 data=df)


box1 <- boxplot(  NA_Sales  ~ Genre, col='yellow', 
                 pch=23, 
                 xlab = "Genre", ylab = "NA_Sales",
                 data=df)
#Pas tr?s lisible...

mytable <- box1$stats
colnames(mytable)<-box1$names
rownames(mytable)<-c('min','lower quartile','median','upper quartile','max')
mytable 

#Il faut se m?fier des apriori ! Il y a beaucoup de jeux d'action mais les jeux d'action n'ont pas fait les meillieurs ventes!
#le genre Platform a une valeur m?diane pour GlobalSales de 0.28 versus 0.19 pour le genre Action

summary(df)
#A priori, le continent enregistrant le plus de ventes est celui de l'am?rique du Nord. 
#m?diane ? 0.0800 


#Nous avons observ? les m?dianes (plus fiables que les moyennes). 
#Cependant, certain genre de jeux ont ?t? populaires pendant (ou depuis) quelques ann?es. 
#Par ailleurs, un genre est moins repr?sent? (en terme de jeux existant) biaise l'analyse.
#Il en va de m?me pour certain jeux qui marchent tr?s bien dans certain continents plus que d'autre
#Cela peut donc fausser l'analyse des ventes lorsqu'elle est faite de fa?on globale


#La notion de densit?
plot(df$Year,df$Global_Sales)


aggregate(Global_Sales ~ Year, data = df, max)
#L'ann?e 2006 a enregistr? des ventes reccords !


hist(df$Global_Sales)
plot(density(df$Global_Sales))



#C'est un type, la t?te dans un four et les pieds dans un cong?lateur,
#Sa temp?rature moyenne est de 37?C. Le probl?me, c'est qu'il est mort!
#Les valeurs extr?mes faussent la moyenne de part leurs ?carts avec la valeurs m?diane.

#Un sous ensemble pour montrer le principe de la boxplot
boxplot(  df[df["Year"] == "2000" & df["Genre"] == "Action", ]$Global_Sales  )


#La variance

boxplot(df$Global_Sales)

help("boxplot")



#La fonction boxplot trace la boite ? moustache
#de donn?es num?riques (ici un vecteur).

#la fonction boxplot() est un moyen rapide de figurer le profil essentiel d'une s?rie statistique quantitative,
#c'est ? dire la m?diane, les quartiles, le minimum, maximum ou encore les d?ciles. Ici, la valeure extreme
#d?cale la m?diane ce qui rend difficile l'interpr?tation graphique
df$Year <- as.integer( as.character(df$Year))
                       
                       
df_filter <- df[df$Year > 2013 & is.na(df$Year) !=  TRUE , ]
df_filter

boxplot( EU_Sales ~ Genre, col='yellow', 
         pch=23, 
         xlab = "Genre", ylab = "EU_Sales",
         main='ann?es',
         data=df)


boxplot( Global_Sales ~ Genre, col='yellow', 
         pch=23, 
         xlab = "Genre", ylab = "Global_Sales",
         main='Global_Sales selon le genre',
         data=df)

#Graphiquement, le genre Sports ? l'air d'avoir r?alis? une vente reccord.
#Shooter, Platform et Role playing ont une forte concentration de points vers des valeurs ?lev?es de Global_Sales


mytable <- box1$stats


colnames(mytable)<-box1$names
rownames(mytable)<-c('min','lower quartile','median','upper quartile','max')
mytable 

#Platform a la meillieure valeur m?diane


#Ronald Fisher employa, le premier, le mot de variance, dans un article de 1918 intitul? 
#? The Correlation between Relatives on the Supposition of Mendelian Inheritance ? 
#o? il d?finit la variance comme le carr? de l'?cart type.

#Source (Wikepedia):
#La variance indique de quelle mani?re la s?rie statistique ou la variable al?atoire se 
#disperse autour de sa moyenne ou son esp?rance. Une variance ?lev?e indique que les valeurs sont 
#tr?s ?cart?es les unes des autres, et vice versa. Elle est nulle lorsque toutes les valeurs sont identiques. 
#Une variance n'est donc jamais n?gative.
#La variance permet d'obtenir l'?cart type, qui est la racine carr?e de la variance. 
#L'?cart type est souvent plus parlant que la variance pour appr?hender la dispersion.
#La variance est un des ?l?ments permettant de caract?riser une loi de probabilit?. 
#C'est le moment centr? d'ordre 2 de cette distribution. C'est ? ce titre qu'elle est syst?matiquement 
#donn?e dans la description d'une loi de probabilit?.


#Il est possible de savoir si un genre de jeux se vend aussi bien qu'un autre
#Il est ?galement possible de savoir si les ventes de l'EU suivent sont similaires 
#aux ventes globales.
#Nous allons extraire les ventes globales des jeux "action" et "sport"
COL0 = df$Global_Sales[df$Genre == 'Action']
COL1 = df$Global_Sales[df$Genre == 'Sports']

var(COL0)
var(COL1) 

#La variance pour les jeux de type "Sports" est plus grande que la variance des
#jeux de type "Action". Cela d?note une dispersion des valeurs plus importante pour les
#jeux de type "Sports" que les jeux de type "Action". 
#Un interpr?tation ? partir de la moyenne est encore plus fauss?e pour les jeux de type
#"Sport" que pour les jeux de type "Action"

#Un mod?le dit "gaussien"


df$NA_Sales_cr <-scale(df$NA_Sales)
# 2 choix
# 1)
df["Rank"==1,]$Global_Sales <- mean(df$Global_Sales) 

#2) 
df["Rank"==1,]$Global_Sales <- NA

##Le test de shapiro

shapiro.test(df$Global_Sales[df$Genre == 'Adventure'])
shapiro.test(df$Global_Sales[df$Genre == 'Strategy'])

#p-value < 2.2e-16 pour les deux jeux de donn?es
#on peut donc rejeter l'hypoth?se nulle. La distribution de
#des ventes globales pour les jeux Aventure et Strategy n'est pas normale.




for (i in 1:length(levels(df$Genre)) ){
  print(levels(df$Genre)[i])
 print( shapiro.test(df$Global_Sales[df$Genre == levels(df$Genre)[i]]) )
}

#Toutes les p-value sont inf?rieures ? 0.05, on peut donc penser qu'aucune vente
#d'aucun genre de jeux ne suit une loi normale.

shapiro.test(df$Global_Sales)
#Error in shapiro.test(Data$Global_Sales) : 
#la taille de l'?chantillon doit ?tre comprise entre 3 et 5000
#Kolmogorov-Smirnov permet de comparer deux distributions avec une taille>5000 
#On simule une distribution normale et on la compare avec notre distribution ? tester
x <- rnorm(nrow(Data),mean = mean(Data$Global_Sales),sd=sd(Data$Global_Sales))
ks.test(Data$Global_Sales,x)

#p-value sont inf?rieures ? 0.05, on rejette l'hypoth?se que cette distribution suit une loi normale.



#Time to decide

#Le graphique ci dessous pr?sente la moyenne avec l'intervalle de confiance des ventes en
#fonction du genre. 
#L'intervalle de confiance donne un intervalle de valeur dans lequel on pr?dit que 
#95% des valeurs seront situ?es dans cet intervalle



# Plot Means with Error Bars
library(gplots)

plotmeans(df$Global_Sales ~ df$Genre,xlab="Genre",
          ylab="Global sales", main="Genre") 


#Graphiquement, c'est le genre "Aventure" le moins vendu
#Graphiquement, c'est le genre "Plateforme" le plus vendu
#Il est possible que nous ayons un biais ? cause de l'?volution temporelles des 
#habitudes des joueurs



##analyse depuis 2012



df$Year <- as.character(df$Year)
df$Year <- as.integer( df$Year)
df.new <- subset.data.frame(df,df$Year > 2012)
#myData = myData[myData$A > 4,]
df.new = df.new[df.new$Year < 2017,]

plot(density(df.new$Global_Sales))
plot(df.new$Year,df.new$Global_Sales)

plotmeans(df.new$Global_Sales ~ df.new$Genre,xlab="Number of Cylinders",
          ylab="Global sales", main="Genre") 

boxplot(df.new$Global_Sales)



box3 <- boxplot( EU_Sales ~ Genre, col='yellow', 
         pch=23, 
         xlab = "Genre", ylab = "EU_Sales",
         main='COL vs RTWL',
         data=df.new)

box3

mytable <- box3$stats


colnames(mytable)<-box3$names
rownames(mytable)<-c('min','lower quartile','median','upper quartile','max')
mytable 


#Depuis 2014


df$Year <- as.character(df$Year)
df$Year <- as.integer( df$Year)
df.new <- subset.data.frame(df,df$Year > 2014)
#myData = myData[myData$A > 4,]
df.new = df.new[df.new$Year < 2017,]

plot(density(df.new$Global_Sales))
plot(df.new$Year,df.new$Global_Sales)

plotmeans(df.new$Global_Sales ~ df.new$Genre,xlab="Number of Cylinders",
          ylab="Global sales", main="Genre") 

boxplot(df.new$Global_Sales)



box4 <- boxplot( EU_Sales ~ Genre, col='yellow', 
                 pch=23, 
                 xlab = "Genre", ylab = "EU_Sales",
                 main='COL vs RTWL',
                 data=df.new)

box4

mytable <- box4$stats


colnames(mytable)<-box4$names
rownames(mytable)<-c('min','lower quartile','median','upper quartile','max')
mytable 

#Personnellement j'investirais sur un jeu de tir, ? vous de faire la m?me analyse pour la plateforme associ?e aux jeux de tir. 

