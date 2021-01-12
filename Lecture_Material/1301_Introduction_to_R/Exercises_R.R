#install packages, check for your R version if you have issues

version

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("GEOquery")
library(GEOquery)

#download the data
data <- getGEO('GSE164191', GSEMatrix = TRUE)

#what if you want to exttract supplementary material_RAW DATA?

#print the class of data
class(data)

#find the expression object, containing info about the expression and access it from the list
expr_obj = data[[1]]

#create a data frame with the gene expression data 
data_e = exprs(expr_obj)
data_expr <- as.data.frame(exprs(data[[1]]))

#which one is tumor and which one is control?
metadata = data[["GSE164191_series_matrix.txt.gz"]]@phenoData@data

#create a list with only the column of df that contains info about the label
labels = list(metadata$title)
#access using the index 
labels = list(metadata[,1])

#what if you want to keep the row names? create a data frame with one column named "label" and rownames the sample
new_df = data.frame("labels" = metadata[,1], row.names = rownames(metadata))

#merge data frames with expression values and labels 
final_df = data.frame(new_df, t(data_expr), row.names = rownames(new_df))

#extract the first 200 col and 200 rows to work with a smaller df and check it worked
smaller_df = final_df[, 1:200]
simpler_labels = stringr::str_replace_all(smaller_df$labels, "[[:graph:]]+: ", "")
smaller_df$labels = simpler_labels



#create a new column of 1 and 0 where it is 1 if the sample is tumor and 0 if sample is benign
for(i in 1:nrow(smaller_df)) {
  if (smaller_df[i,1] == "Peripheral Blood_Healthy control") {
    smaller_df$num_labels[i] = 0
  } else if(smaller_df[i,1] == "Peripheral Blood_Colorectal cancer") {
    smaller_df$num_labels[i] = 1
  }
}

#check if it works correctly by comparing the number of 1 to number of control
sum(smaller_df$labels == "Peripheral Blood_Colorectal cancer") == sum(smaller_df$num_labels)

#convert the for loop into a function and use apply to apply it
convert = function(df, col, normal, tumor) {
  df2 = df
  if (df2[i,col] == normal) {
    df2$num_labels[i] = 0
  } else if(df2[i,col] == tumor) {
    df2$num_labels[i] = 1
  }
  return(df2)
}


#barplot of number of tumor/control samples

#choose five genes and plot the distribution of expression values

#calculate the mean expression of each gene in tumor and normal, calculate fold change and plot the distribution
#use a for loop 

#CREATE a function that does the same

