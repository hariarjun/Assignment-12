#Question 1. Use the given link Data Set.
#Answer the below questions:
#  a. Perform ANOVA test on the discriminant analysis scores of nuclear localization signals of both nuclear
#and non-nuclear proteins by class variables (Target).

names(yeast)<- c("SequenceName", "mcg", "gvh", "alm", "mit", "erl", "pox", "vac", "nuc", "LocalizationSite")
pca <- princomp(yeast[, 2:9], cor=T) # principal components analysis using correlation matrix
pc.comp <- pca$scores
PrincipalComponent1 <- -1*pc.comp[,1] # principal component 1 scores (negated for convenience)
PrincipalComponent2 <- -1*pc.comp[,2] # principal component 2 scores (negated for convenience)
clustering.data <- cbind(PrincipalComponent1, PrincipalComponent2)
set.seed(100)
km <- kmeans(clustering.data, 8, iter.max = 30, nstart=30)
km
km$cluster
plot(PrincipalComponent1, PrincipalComponent2, col=km$cluster)
points(km$centers, pch=16)

aggregate(yeast[, 2:9],by=list(km$cluster),mean)
table(km$cluster, yeast$LocalizationSite)


# b. Which class is significantly different from others?
results <- aov(nuc~class,data=classnuc);
summary(result)
TukeyHSD(result)
#The ANOVA results show that there is significant difference in the discriminant score of nuclear localization signals of both nuclear and non-nuclear proteins across class variables. 
#The class NUC is significantly different and higher in its mean discriminant score than the other classes.  
