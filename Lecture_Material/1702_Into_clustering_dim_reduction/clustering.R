# -------- CLUSTERING and DIMENSIONALITY REDUCTION ---------


# If necessary, install the packages 
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("GEOquery")
install.packages("Rtsne")

#load packages you need
library("GEOquery")
library("Rtsne")
require("RColorBrewer")
library(ggplot2)
library(cluster)

data = getGEO("GSE159929")
metadata = data[["GSE159929_series_matrix.txt.gz"]]@phenoData@data

#get the data
bladder <- read.csv("/GSE159929_RAW/GSM4850577_Bladder_Counts.csv.gz")
heart <-  read.csv("/GSE159929_RAW/GSM4850581_Heart_Counts.csv.gz")
skin <-  read.csv("/GSE159929_RAW/GSM4850587_Skin_Counts.csv.gz")
lymph_node <-  read.csv("/Users/valentinagiunchiglia/Downloads/GSE159929_RAW/GSM4850583_Lymph.node_Counts.csv.gz")

# This time I decided to use single cell RNA seq data because I wanted to show you 
# the difference compared to bulk RNAseq data. However, one disadvantageous side of 
# scRNAseq is that the df tend to be much bigger. That's why throughout the 
# tutorial I will reduce the number of genes/cells that I keep, otherwise it would
# take too long to run. If I do some steps simply due to the time needs of this 
# tutorial, I will clearly state it.


# ----- Dimensionality reduction -------
# Why do we need to do dimensionality reduction? Dimensionality reduction allows
# for the transformation of data from high-dimensional into low-dimensional. This
# is important because in this way data are easier to work with and visualize. 
# There are multiple ways to reduce dimensions. It's important however to always 
# keep the data that are the most relevant for your analysis, and that contain 
# the most information. 

# IMPORTANT!!! THE STEPS BEFORE THE PCA ARE NECESSARY FOR ME TO BE ABLE TO RUN 
# THE REST OF THE CODE DURING THE LECTURE. OTHERWISE IT WOULD TAKE TOO LONG.
# THREFORE, IT COULD BE INACCURATE TO REMOVE CELLS AND GENES THE WAY I DID. 
# IF YOU WORK WITH SINGLE CELL I STRONGLY ADVISE YOU TO USE THE SEURAT PACKAGE, 
# HOWEVER THAT'S BEYOND THE SCOPE OF THIS LECTURE. 

#keep only the genes that are common in all dataset 
#lymph node is the smallest df, that's why we want to check which genes are in it
set_genes = row.names(lymph_node)

bladder_small = bladder[set_genes, ]
heart_small = heart[set_genes, ]
skin_small = skin[set_genes, ]
lymph_node_small = lymph_node[set_genes, ]

#why NA?
sum(is.na(rownames(bladder_small)))

#the issue is that there are multiple genes that are not present in all the df
#in order to avoid the NA it would be better to find the genes that are present
#in all the dataset. This means that we need to do an intersection 

list_of_data = list(bladder, heart, skin, lymph_node)

# get the rownames that are common
common_names = Reduce(intersect, lapply(list_of_data, row.names))

# create a function that keeps only the rows in the df that belong to the common
# genes
common_genes <- function(x) {
  x[row.names(x) %in% common_names,]
}

list_of_data = lapply(list_of_data, common_genes)

#the ouput is a list of 4 elements, where each element corresponds to a df. 
# To access each data frame we can simply do the following

bladder_final = list_of_data[[1]]
heart_final = list_of_data[[2]]
skin_final = list_of_data[[3]]
lymph_node_final = list_of_data[[4]]

#In order to make the CALCULATIONS FASTER in this tutorial, I will reduce the 
# number of cells per df. In order not to do it compeltely randomly I will remove 
# cells that have a lowest gene expression across all genes. Since our df consists of cells
# in the columns and genes in the rows, that means that I just have to sort the 
# columns based on the sum of all their rows. The higher is the sum, the higher is
# the general expression throughout the genes. 

bladder_final = bladder_final[, colSums(bladder_final) > 2000]
hist(colSums(bladder_final))

heart_final = heart_final[, colSums(heart_final) > 2000]
skin_final = skin_final[, colSums(skin_final) > 2000]
lymph_node_final = lymph_node_final[, colSums(lymph_node_final) > 2000]

sum(colSums(heart_final) < 2000)

#what is the problem with this method? I could have one cell that has one really high 
#expressed gene, while the other genes are zero. However, since the only reason why
# I am doing this now is to MAKE CODE FASTER for the tutorial, it's not a problem.

#now all our dataframes have the same number of rows and contain the same genes. 
#This is already a form of dimensionality reduction, since the number of rows, and
#the gene information that we are keeping is smaller than the original one. 

#Let's create a df that combines all tissue df together
final_df = data.frame(bladder_final, heart_final, skin_final,lymph_node_final)

# Now I am going to reduce the number of genes to 1500. Again the reason is that
# it would take too long to run the PCA with all the genes. 

final_df = t(final_df[1:1500, ])

#what is the t doind? the t transposed the dataframe. I need to do it because to 
#run PCA you need to have the dimensions that you want to reduce in the columns. 
#Since we want to reduce the number of genes, and we currently have the genes in t
#the rows, we need to transpose the dataframe by using t. 

#Now that we have our final dataframe we can start to do PCA, which will help us
#reduce the number of genes and keep only the genes with the most variance and that
#contain the most information

# ------ Principal Component Analysis  ------
# In simple words, the PCA tries to transform the axis in the coordinates space
# in a way that maximize the variance of the data.

final_df.pca <- prcomp(final_df)

# Let's look at the output of PCA!!
# First of all, you need to know that the PCA are ordered by variance explained. This 
# means that the first principal components are the most important, in the sense
# that they explain the most variance within the data. 

# As you can see, the output of PCA has five main elements. What do they mean in 
# simple terms? We will go through the three most relevant. 

# 1) sdev represents the standard deviation across each principal components. Why
# is it relevant? Because we want to keep only the PC that explain the most variance.
# Since we have the stdev, we can calculate the variance explained by each component, 
# plot it and evaluate how many of these components we want to keep. 

# 2) rotation: the rotations can be considered the loadings. Essentially the loadings
# define how relevant was each gene in the definition of each component. The genes 
# with the highest loadings in the first PC can be considered as the most relevant
# in most of the cases (unless there is batch effect)

# 3) x: represents the transformed data, or the newly principal components created.  

# ---- Variance explained by PCA -------
# When you run pca the first thing you want to do is to check how much variance 
# each PC explains. There are two possible plots to do this.

#First let's calculate the variance explained by each component using the sdev 
# information. The variance can be defined as sdev^2

#compute standard deviation of each principal component
std_dev <- final_df.pca$sdev[1:10]
#compute variance
pr_var <- std_dev ^ 2
#proportion of variance explained
prop_varex <- pr_var / sum(pr_var)

#to visualize it let's create the first possible plot
#plot proportion of variance explained
plot(
  prop_varex,
  xlab = "Principal Component",
  ylab = "Proportion of Variance Explained",
  type = "b",
)

#As you can observe from the image, the first principal components explain most
#of the variance, instead the last ones almost 0. Let's recreate the plot by 
# checking only the first 10

# What you can see from the plot is that there is an elbow right at PC 2. This 
# means that actually you should only keep the first two principal components to 
# continue the analysis, since the others are not as relevant

#Another possibility is to plot the cumulative variance explained
cumulative_variance = cumsum(pr_var )/ sum(pr_var)

plot(
  cumulative_variance,
  xlab = "Principal Component",
  ylab = "Proportion of Variance Explained",
  type = "b",
)

#This plot is not as useful to understand which one are the most relevant PCA. 
#However it gives you as information that already up to 10 PC explain almost 100%
# of the variance within the data. 

# Now that we know that PC1 and PC2 explain most of the variance, we can try to 
# plot them and see whether they create some kinds of clusters within our data.

#Let's extract the information about PC1 and PC2. Now you should already start to 
# see why PCA is so relevant. By selecting only the 2 first PC, we can actually 
# visualize our cells in 2D, which was impossible before
PCA1_2 = data.frame(final_df.pca[["x"]][, 1:3])

#create a list of labels. We want to extract the labels that tell us which tissue
#each cell belongs to. 
color_vector = c(rep("red", (ncol(bladder_final))), rep("blue", ncol(heart_final)), 
           rep("green", ncol(skin_final)), rep("yellow", ncol(lymph_node_final)))
labels = c(rep("bladder", (ncol(bladder_final))), rep("heart", ncol(heart_final)), 
                 rep("skin", ncol(skin_final)), rep("lymphNode", ncol(lymph_node_final)))

plot(PCA1_2$PC2, PCA1_2$PC1, col = color_vector)

#As you can observe the PC2 seems to be able to distinguish even if only partially
#between the different tissues. However it is not really straigthtforward. 
#This means that PC representation does not capture well enough the variability. 
#Another option could be that we need more genes to actually be able to see some
#separation, or that the PC1 is affected by batch effect. 

#IMPORTANT! You should always check for batch effect, batch effect is a measure of 
#technical variability/error that is present in your data. In concrete what it means
#is that the first principal component could capture most of the variability, but 
#this variability is not due to biological reason, but technical errors. 
# One easy way to visualize whether your components contain batch effect is to plot
#the PC you want to evaluate and color the cells based on the technical error that you 
#want to check

#batch effect can be introduced for example because samples were processed at 
#different times or in different places. Another reason, apart from batch effect,
#can be that data separate due to some variability, such as age of patients,
#that is not really biological

#let's extract the list of samples used
samples_list = c("GSM4850581","GSM4850577", "GSM4850587", "GSM4850583")


submission_date_samples = data.frame(metadata[rownames(metadata) %in% samples_list, 4])

age = data.frame(metadata[rownames(metadata) %in% samples_list, 11])

#Now that you have this information you could plot the PC again, but color based
#on the new labels. If you notice that the data cluster based on that, it means
#that the variance explained by the PC is not relevant, since it's due to batch
#effect or non strinctly biological variability. 


#Since PC2 seems to better distinguish among the different tissues, we will keep 
#on working with this one. Now that we picked the PC, we can look at the loading
#to see which genes were the most relevant for the establishment of PC2. 
#This is an example of feature extraction. 

loadings = data.frame(final_df.pca[["rotation"]])
loadings_pc2 = data.frame(loadings$PC2, genes = rownames(loadings))

#sort in descending order
loadings_pc2_sorted = data.frame(sort(loadings_pc2$loadings.PC2, decreasing = T))

#extract the top 100 genes with the highest loadings
top_100_genes = loadings_pc2_sorted$sort.loadings_pc2.loadings.PC2..decreasing...T.[1:100]

#keep oinly the rows in the df with the info about the highest genes
loadings_pc2_sorted_genes = loadings_pc2[loadings_pc2_sorted$sort.loadings_pc2.loadings.PC2..decreasing...T.
                                         %in% top_100_genes, ]

#now that we have the most important genes we can use this list to run for example
#differential gene expression analysis. 
final_genes_list = c(loadings_pc2_sorted_genes$genes)

#In this way we use PCA to conduct dimensionality reduction and keep only the most
#relevant genes. 

# ----- tSNE ---------
# tSNE is another dimensionality reduction method. It allows you to visualize data
# in n-dimension into a 2 dimensional space. 

#IMPORTANT!!
set.seed(1) # for reproducibility
#Why is it important to set a seed?
#tSNE, as well as k-means and other methods, are stochastic methods. These means
# that are based on random variable. If you don't set a seed, every time you run 
# tSNE you will get a different result. 
# tSNE is not really strong in reproducibility. Indeed, by changing the perplexity
# value, the tsne plot will change as well. 
tsne <- Rtsne(final_df, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)


#the information about the tsne output is contained in the Y 
plot(tsne$Y, main="tSNE", xlab="tSNE dimension 1", ylab="tSNE dimension 2", col = color_vector)


#ggplot option of tsne
set.seed(1)
colours <-
  colorRampPalette(c("royalblue", "red3", "forestgreen", "gold", "black"))(length(unique(color_vector)))[factor(color_vector)]

tsne_plot <-
  data.frame(x = tsne$Y[, 1], y = tsne$Y[, 2], col = colours)
ggplot(tsne_plot) + geom_point(aes(x = x, y = y, color = col)) + labs(
  colours =  color_vector
)


#As you can observe, if we add the color, you can see some clustering. The cells that 
# belong to the same tissue are close to each other, instead the cells that belong to
# different tissues are farther from each other. The clustering is not perfect, but
#it's a better visualization if compared to PCA. 


# ----- Clustering ---------------
#tSNE already gives an idea about clustering. However, the two easiest and most 
#straightforward methods to conduct clustering analysis are kmeans and hierarchical
#clustering. Both are unsupervised methods, which means that we don't actually
#assign any label to the cells/data. 

# ------ Kmeans ---------
#First step of kmeans is to understand how many are the appropriate clusters. 
#In our case, we would like to have potentially 4 clusters because we have 4 
#different tissues. However, to simply decide how many clusters to use, it's not
##the most objective way to do it. There are two methods that are commonly
#used in bioinformatics to conduct some clustering analysis. 

# ----- Elbow Plot ------
# The elbow method shows the total within-cluster sum of square (WSS) as a function 
# of the number of clusters.
#let's set a maximum of 8 possible clusters

k.max <- 8
elbow <- sapply(1:k.max,
                function(k) {
                  kmeans(final_df, k, iter.max = 18)$tot.withinss
                })

plot(
  1:k.max,
  elbow,
  type = "b",
  frame = FALSE,
  xlab = "Total number of clusters",
  ylab = "Within-clusters sum of squares"
)

#by looking at the elbow, you would say that the best number of clusters is either
#2 or 3 and not really 4 as we throught. This doesn't surpruse so much, since before
# we observed both in PCA and tsne that the separation among the different tissue
#cells was not so clear. 

#elbow method can be a bit tricky to interpret, since it's always a bit subjective
#the evaluation on where the elbow is located. The silhouette method is easier to 
#interpret. 


# ----- Silhouette Plot -------
# Concretely the silhouette plot shows a measure of how close the point in each cluster
#is to to points in the neighboring clusters. Concretely, if you have a score close 
#to 1, it meas that the points are farther from the neighbouring cluster, so the 
#clusters are better defined. Instead, if you have 0, it means that the points are
#located along the border. If you have negative values, it is a suggestion that
#the points are probably mislebeled. 


#one issue of the silhouette method is that it can take a long time to calculate 
#the distance matrix, mainly in the case of single cell when we have a lot of 
#data. In order to be able to show you how the silhouette works, I am going to run
#it on a really small set of data. 

df_small = final_df[1:50, ]
dist_matrix_small <- dist(final_df[1:50, ])


avg_sil <- function(k, df, distmat) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, distmat)
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 9
k.values <- 2:9

avg_sil_values_small <- sapply(k.values, avg_sil, df_small, dist_matrix_small)
#saveRDS(avg_sil_values, "./avg_sil_values.rds")


plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

#In order to understand this plot, you have to look at which cluster has the 
#biggest average. In that case, it means that the majority of the points are rather
#closer to 1 than 0. 

#Now let's try to visualize the kmeans both with 2 and 4
set.seed(1)
kmeans_2 = kmeans(final_df, 2, iter.max = 18)
kmeans_4 = kmeans(final_df, 4, iter.max = 18)

#if you look at the output, instead of cluster you have a label per cell, that 
#tells you whether the cell was assigned to the first or second cluster

plot(tsne$Y, main="tSNE-2 clusters", xlab="tSNE dimension 1", ylab="tSNE dimension 2", col = kmeans_2$cluster)
plot(tsne$Y, main="tSNE-4 clusters", xlab="tSNE dimension 1", ylab="tSNE dimension 2", col = kmeans_4$cluster)

#check how many were correctly assigned
confmat = table(data.frame("clus" = kmeans_4$cluster, "celltype" = labels))
pheatmap::pheatmap((confmat), cluster_cols=F, cluster_rows=F)

#in the heatmap, if the clusters were assigned properly, you would observe really
#red along the diagonal, which, as you can see, it's not the case. 


