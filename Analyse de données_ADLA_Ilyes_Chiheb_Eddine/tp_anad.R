#ouverture de fichier
x= read.table(file.choose(),header = TRUE)
#Etude des statistiques des données
mat=as.matrix(x)
barplot(table(mat),main="histogramme",xlab="les modalites",ylab ="count")
#Visualisation de la fréquence des catégories des variables
summary(x)
matrice=as.matrix(x)
prop.table(table(matrice))
#Transformation du tableau de données en tableau disjonctif
disj = tab.disjonctif(x)
#Application de l'AFCM
afcm=MCA(x)
#Etude de tableau de valeur propre
afcm$eig
fviz_screeplot (afcm, addlabels = TRUE, ylim = c (0, 45))
#Représentation du biplot et les visualisations possibles 
plot(afcm)
fviz_mca_biplot (afcm, repel = TRUE, ggtheme = theme_minimal())
#Etude de tableau de contribution
afcm$var
afcm$var$contrib
afcm$ind$contrib
fviz_contrib (afcm, choice = "var", axes = 1, top = 25)
afcm$var$contrib
afcm$call$marge.col*100
afcm$var$coord
afcm$ind$contrib
afcm$call$marge.row*100
afcm$ind$coord
#Les questions les mieux représentées par l'AFCM
afcm$var$cos2
afcm$var$cos2[,1]+afcm$var$cos2[,2]
#Le Tableau de contingence
table(x$Q1,x$Q2)

#Les associations entre modalités
mcor=cor(disj)
#L'application d'une AFC sur le tableau de contingence
cont=table(x$Q3,x$Q4)
y= read.table(file.choose(),header = TRUE) 
afc=CA(y)
afc
afc$eig
sammury(afc)
#Interprétation profil colonne
afc$col$contrib
afc$call$marge.col
afc$col$coord
#Interprétation profil ligne
afc$row$contrib
afc$call$marge.row*100
afc$row$coord


#Utilisation de factoextra
fviz_screeplot (afcm, addlabels = TRUE, ylim = c (0, 45))
fviz_mca_biplot (afcm, repel = TRUE, ggtheme = theme_minimal())
fviz_mca_var (afcm,choice ="mca.cor",repel=TRUE, ggtheme = theme_minimal ())
fviz_mca_var (afcm,repel = TRUE,ggtheme = theme_minimal ())
fviz_mca_var(afcm, col.var = "cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE, ggtheme = theme_minimal())
fviz_cos2(afcm, choice = "var", axes = 1)
fviz_cos2(afcm, choice = "var", axes = 2)
fviz_cos2(afcm, choice = "var", axes = 1:2)
fviz_contrib (afcm, choice = "var", axes = 1, top = 25)
fviz_contrib (afcm, choice = "var", axes = 2, top = 25)
fviz_contrib (afcm, choice = "var", axes = 1:2, top = 25)
fviz_mca_var(afcm, col.var="contrib",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE,ggtheme=theme_minimal())
