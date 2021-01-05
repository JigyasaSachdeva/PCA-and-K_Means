#IDS 575- Assignment 3
#Problem 4
#Jigyasa Sachdeva
#UIN - 664791188

library(fpc)
#Creating a 60 x 50 matrix: 
#50 features; 20 observations for each category
#Mean shift exists for each category, keeping the sd same for simplicity
set.seed(123)
df <- as.data.frame((matrix(nrow = 60, ncol = 50)))
df[1:20, ] <- rnorm(df[1:20,], mean = 100, sd = 10)
df[21:40, ] <- rnorm(df[21:40,], mean = 200, sd = 10)
df[41:60, ] <- rnorm(df[41:60,], mean = 300, sd = 10)
df$label <- 1
df$label[21:40] <- 2  
df$label[41:60] <- 3 

#-----------------------------------------------------------------------------------------
#(a)

options(scipen =99)
#Removing target variable for performing PCA:
df1 <- df[,-51]
df.pca <- prcomp(df1, center =TRUE, scale = TRUE)
summary(df.pca)

#Installing packages and loading libararies for plotting PCA
str(df.pca)
#x is the new axes
#First principle axis 
pc1 <- df.pca$x[,1]
#Second principle axis
pc2 <- df.pca$x[,2]
#Plotting the 2 axis ans colour coding points by labels
plot(pc1, pc2, col= df$label, cex =0.85)


#-------------------------------------------------------------------------------------
#(b)
km_3 <- kmeans(df1, centers = 3, nstart = 100)
table(km_3$cluster)
table(df$label)
#Same
km_3$cluster
df$label
library(cluster)
clusplot(df, km_3$cluster, color = T, lines = 0, shade = T)

#-------------------------------------------------------------------------------------
#(c)
#2 clusters
km_2 <- kmeans(df1, centers = 2)
table(km_2$cluster)
table(df$label)
km_2$cluster
df$label
clusplot(df, km_2$cluster, color = T, lines = 0, shade = T)

#4 clusters
km_4 <- kmeans(df1, centers = 4)
table(km_4$cluster)
table(df$label)
km_4$cluster
df$label
clusplot(df, km_4$cluster, color = T, lines = 0, shade = T)

#----------------------------------------------------------------------------------------
#(d)
set.seed(123)
pc_df <- as.data.frame(cbind(pc1, pc2))
km_pc <- kmeans(pc_df, centers =3)
table(km_pc$cluster)

km_pc <- kmeans(pc_df, centers =3, nstart = 100)
table(km_pc$cluster)
clusplot(pc_df, km_pc$cluster, color = T, lines = 0, shade = T)


#--------------------------------------------------------------------------
#(e)
df_scaled <- as.data.frame(scale(df1, center = TRUE, scale =TRUE))
km_scaled <- kmeans(df_scaled, centers = 3)
table(km_scaled$cluster)
clusplot(df_scaled, km_scaled$cluster, color = T, lines = 0, shade = T)





