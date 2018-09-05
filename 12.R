   Assignment 12.1


(c) and (d)

yeast<-read.table("C:/Users/Admin/Documents/acd/yeast.txt",quote="\"",comment.char="")
View(yeast)
summary(yeast)
library(tidyverse)
  yeast<read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/yeast/yeast.data'stringsAsFactors=False)


pca <- princomp(yeast[, 2:9], cor=T) 

pc.comp <- pca$scores

PrincipalComponent1 <- -1*pc.comp[,1]


PrincipalComponent2 <- -1*pc.comp[,2] 
clustering.data <- cbind(PrincipalComponent1, PrincipalComponent2)

levels(yeast$group)


library(dplyr)
group_by(yeast, SequenceName) %>%
  summarise(
    
    count = n(),
    
    mean = mean(nuc, na.rm = TRUE),
    
    sd = sd(nuc, na.rm = TRUE)
    
  )					



km <- kmeans(clustering.data, 8, iter.max = 30, nstart=30) 
km


km$cluster
plot(PrincipalComponent1, PrincipalComponent2, col=km$cluster)

points(km$centers, pch=16)

names(yeast)<- c("SequenceName" , "mcg", "gvh", "alm", "mit", "erl", "pox", "v ac", "nuc", "LocalizationSite")
aggregate(yeast[, 2:9],by=list(km$cluster),mean)

table(km$cluster, yeast$LocalizationSite)
res.aov<-aov(nuc ~ LocalizationSite,data=yeast) 
summary(res.aov)

res.aov <- aov(nuc ~ pox, data = yeast)
summary(res.aov)

res.aov <- aov(nuc ~ erl, data = yeast)
summary(res.aov)

res.aov <- aov(nuc ~mit , data = yeast)
summary(res.aov)

res.aov <- aov(nuc ~alm , data = yeast)
summary(res.aov)

res.aov <- aov(nuc ~gvh , data = yeast)
summary(res.aov)

res.aov <- aov(nuc ~ mcg, data = yeast)
summary(res.aov)




