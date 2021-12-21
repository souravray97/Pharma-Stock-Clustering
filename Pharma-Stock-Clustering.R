#Clustering Problem 15.2


pharma.df <- read.csv("Pharmaceuticals.csv") # reading the csv file
View(pharma.df)

set.seed(1234)
row.names(pharma.df) <- pharma.df[,2]
pharma.df <- pharma.df[,-2]
View(pharma.df)

pharma1.df <- pharma.df[,-c(1, 11, 12, 13)]
View(pharma1.df)


pharma.df.norm <- sapply(pharma1.df, scale)
summary(pharma.df.norm)

row.names(pharma.df.norm) <- row.names(pharma1.df) 


d.norm <- dist(pharma.df.norm, method = "euclidean")
d.norm


hc_ward <- hclust(d.norm, method = "ward.D")
plot(hc_ward)


clusters <- cutree(hc_ward, k = 3)
clusters





library(dplyr)
pharma_new.df <- mutate(pharma.df, Cluster = clusters)
pharma_new.df <- pharma_new.df[order(pharma_new.df$Cluster),]
View(pharma_new.df)


MedRecom <- as.factor(pharma_new.df$Median_Recommendation)
Location <- as.factor(pharma_new.df$Location)
Exchange <- as.factor(pharma_new.df$Exchange)
Cluster <- pharma_new.df$Cluster

levels(MedRecom)

counts1 <- table(Cluster, MedRecom)
counts2 <- table(Cluster, Location)
counts3 <- table(Cluster, Exchange)



barplot(counts1, main = "Median Recommendation Comparison", 
        xlab = "Types", col = c("Red", "Blue", "Black"), legend = rownames(counts1)) 

barplot(counts2, main = "Location Comparison", 
        xlab = "Country", col = c("Red", "Blue", "Black"), legend = rownames(counts2), las = 3, cex.names = 0.5, 
        args.legend = list(x = "top", inset = c(-0.15, 0)))

barplot(counts3, main = "Exchange Comparison", 
        xlab = "Exchange Type", col = c("Red", "Blue", "Black"), legend = rownames(counts3), cex.names = 0.6,
        args.legend = list(x = "top", inset = c(-0.15,0)))















