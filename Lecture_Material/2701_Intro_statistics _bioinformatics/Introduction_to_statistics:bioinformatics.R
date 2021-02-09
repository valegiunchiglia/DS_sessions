# -------- INTRODUCTION TO BIOSTATISTICS ------------

# Statistics can be divided into "descriptive statistics" and "inferential statistics".
# Descriptive statistics is used to describe your data. Instead inferential statistics
# is used to make prediction/generalize starting from the data sample that you have. 

# ----- DESCRIPTIVE STATISTICS ---------

# Two different types of data exist: categorical and numerical data. Based on the type
# of data, different types of descriptive statistics can be applied. 

#install packages you need
install.packages("arsenal")


if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("GEOquery")

#load packages you need
library("GEOquery")
library("arsenal")

#get the data
data <- getGEO('GSE164191', GSEMatrix = TRUE)
expr <- as.data.frame(exprs(data[[1]]))
metadata = data[["GSE164191_series_matrix.txt.gz"]]@phenoData@data


# ----- Categorical variables -------

#always rename the columns in a more appropriate format
colnames(metadata)[36] = "age"
colnames(metadata)[37] = "disease_status"
colnames(metadata)[38] = "gender"

#to do them all together
colnames(metadata)[36:38] = c("age", "disease_status", "gender")

categorical_table <- tableby(~ disease_status + gender + type + age, data=metadata)
categorical_table <- as.data.frame(summary(categorical_table, text = T))


# ------ Visualization for categorical variables --------

# Plots can be used to better visualize the data you have. They are easier to 
# interpret, and alwyas look good in your report. The main plot types you can use
# to visualize categorical data are barplot and histograms. 

# --- Barplot ----

# table takes a character vector and gives the count of the number of unique
# variables

barplot(table(metadata$gender), ylim = c(0, 80), main = "Gender", ylab = "Frequency", 
        xlab = "Gender", col = c("blue", "yellow"))

barplot(table(metadata$disease_status), main = "Disease type", ylab = "Frequency", 
        xlab = "Condition", col = c("green", "pink"))

metadata$age = as.numeric(metadata$age)
# ---- Histogram ----
hist(metadata$age, main = "Age distribution", ylab = "Frequency", 
     xlab = "Age", col = c("red", "yellow", "blue", "green", "pink"))

# ----- Numerical variables ------
#Extract information about the data

#info about a sample (column wise)
summary(expr)

#info about a gene (row wise)
expr["Means"] = rowMeans(expr)
expr["Min"] = min(expr)
expr["Max"] = max(expr)
expr["median"] = apply(expr,1, median, na.rm = TRUE)
expr["first_quantile"] <- apply(expr,1,quantile,probs=c(.25))
expr["third_quartile"] <- apply(expr,1,quantile,probs=c(.75))
expr["sd"] = apply(expr,1,sd)
expr["var"] = apply(expr,1,var)
expr["IQR"] = apply(expr, 1, IQR)

#What happens if I write 2 instead of one?
apply(expr, 2, IQR)

#I obtain the IQR or any other variable I am looking for but for each column 
#rather then each row!

#in a easier way
sum_genes = data.frame(summary(t(expr)))


# ---- Visualization for numerical variables ----
# In the case of numerical variables there are multiple alternatives on how to 
# visualize your data. Here are the most relevant and probbaly the only ones 
# you will have to use. 


# ----- Boxplot ------
boxplot(expr$GSM5001936)

# what's the problem with this plot? There are too many outliers. As a consequence, 
# you cannot really understand much from the above plot. To solve the issue you can 
# either limit the range of the axys, or visualize it in logarithmic scale

# Limit axis range
boxplot(expr$GSM5001936, ylim = c(0, 1500), main = "Sample GSM5001936")

# Logarithmic scale
boxplot(log2(expr$GSM5001936), horizontal=T)

# Quick reference for the meaning of the elements in the box
boxplot(expr$GSM5001936, ylim = c(0, 1500), main = "Sample GSM5001936")

#add info text to the plot
text(x=1.25, y=median(expr$GSM5001936), label="median", adj=0)
text(x=1.25, y=quantile(expr$GSM5001936, 0.75), label="75%", adj=0)
text(x=1.25, y=quantile(expr$GSM5001936, 0.25), label="25%", adj=0)

# The lower and upper whisker are either the min/max of the values, or the
# in case there are outliers it's calculated as follows:
# upper whisker = q_75 + 1.5*IQR ; lower whisker = q_25 - 1.5*IQR 
upper_whisker = min(
  quantile(expr$GSM5001936, 0.75) + 1.5*IQR(expr$GSM5001936),
  max(expr$GSM5001936)
)
text(x=1.25, y=upper_whisker,label="upper whisker", adj=0)
lower_whisker = max(
  quantile(expr$GSM5001936, 0.25) - 1.5*IQR(expr$GSM5001936),
  min(expr$GSM5001936)
)
text(x=1.25, y=lower_whisker, label="lower whisker", adj=0)

# ----- Q-Q plot -----
# Q-Q plots are useful to both compare the distribution between two samples, or 
# compare the sample distribution to the normal distribution. Understanding whether
# your data are normally distributed is particularly relevant for when you have to 
# complete inferential statistics. Indeed, some statistical tests can be used only 
# with normally distributed data. Therefore, the choice of which test to use depends on 
# whether this assumption is fulfilled or not. 

# We want to check whether the gene expression is normally distributed across samples

qqnorm(expr[1,])
qqline(expr[1,])

# As you can see the plot has a line that is more or less located along the diagonal. 
# This means that the data seem to be normally distributed

# To be sure if your data are normally distributed you can use the normality test, such 
# as shapiro test 
shapiro.test(c(as.numeric(expr[1,])))

# The shapiro test confirms that the data are normally distributed. Why? The test tries 
# to see whether there is a significant difference between the normal distribution 
# and the distribution of our data. Since the p-value is above 0.05, it means that
# there is no significant difference. 

# ----- Density Plot ----
# Density plots are useful to check how the expression of one gene is distributed 
# across samples. It is particularly useful to compare the distribution of the same gene
# in healthy vs tumor samples. In this way, you can already see whether the 
# expression of one gene changes if you have tumor data.

expr_final = data.frame(t(expr), "condition" = metadata$disease_status)

plot(density(expr_final$X1007_s_at[expr_final$condition == "normal"]), col = "red", 
     main = "Distribution X1007_s_at tumor vs healthy")
lines(density(expr_final$X1007_s_at[expr_final$condition == "colorectal cancer"]),
      col = "blue")

# We said before that the gene expression data are normally distributed, let's 
#check it again by plotting the normal distribution line
m <-mean(expr_final$X1007_s_at)
std<-sqrt(var(expr_final$X1007_s_at))
#To choose the type of line to use (such as dashed in the example) set lty. To change the thickness of the line use lwd. 
curve(dnorm(x, mean=m, sd=std), col="green", lwd=2, lty = 2, add=TRUE)

# ----- INFERENTIAL STATISTICS ------

# ------- Hypothesis Tests ---------- 
# The hypothesis testing is a statistical method used to check whether a specific 
# hypothesis is plausible or not. It consists in the following steps:
# 1) Defining a "NULL HYPOTHESIS", which is a statement/assumption that is thought 
# to be true
# 2) Define an"ALTERNATIVE HYPOTHESIS", which claims the opposite of the Null hypothesis
# and states what could be wrong about the claim of the null hypothesis
# 3) Conduct hypothesis testing to understand how likely it is to observe the data that
# you have, if the null hypothesis was true.
# 4) Set a level of significance, which is a threshold, below which it is too unlikely 
# to observe your data if the null hypothesis was true, therefore the Null hypothesis can
# be rejected. Usually 5% or 1%,

# The probability of observing your data, if the null hypothesis was true, is defined
# P VALUE. 

# There are different types of statistical tests, which have to be used based on the 
# hypothesis, on what you want to compare and the distribution of your data.  

# ------- EXAMPLE: Gene expression analysis --------
# In bioinformatics hypothesis testing is used to understand whether the genes 
# are differentially expressed between tumor and healthy samples (Gene expression 
# analysis). In this case, the null hypothesis is that there is no significant 
# difference between the expression of the genes. Instead the alternative hypothesis 
# is that the difference is significant. 

# The test used in this case is the t-test, because the data are noramlly distributed
# and because we want to compare the mean gene expression in healthy vs tumor. 

healthy = expr_final[expr_final$condition == "normal", ]
tumor =expr_final[expr_final$condition == "colorectal cancer", ]

pvalue = NULL

for(i in 1 : ncol(expr_final)) { 
  x = healthy[,i] 
  y = tumor[,i] 
  t = t.test(x, y)
  
  # Add the calculated values to the list
  pvalue[i] = t$p.value
}

# IMPORTANT: there are tests that are used with variables that are not normally 
# distributed, such as the Wilcoxon, Kruskal Wallis and permutation test. 
# I have used them mainly when handling batch effect. 
# Batch effect is defined as statistical artifacts that can mislead the analysis. 

# Plot a histogram of the p-values

hist(pvalue) #let's look at the values below the significance threshold of 0.05
hist(pvalue, xlim = c(0, 0.1), breaks = 100)
abline(v = 0.05,  col= "red")

#how many genes have a p-value below 0.05?
sum(pvalue <= 0.05)


# -------  Multiple comparison correction ----------
# In our case we conduct the t-test multiple times, because we keep on comparing
# the mean expression of each gene in healthy vs tumor samples. Since the test
# is repeated, it is defined as multiple comparison. When you conduct multiple 
# comparison, a correction of the pvalues must be completed, which is generally
# based on the number of tests completed. This is necessary in order to limit the 
# number of false positives. 

# There are multiple methods to correct the pvalues. The easiest is bonferroni.
# Bonferroni simply divides the value for the number of the tests.

adjusted_p = p.adjust(pvalue, method = "bonferroni")
sum(adjusted_p <= 0.05) 

# ------- Fold Change and Volcano Plot -------------
# P values are used to measure statistical significance. However, they are not the 
# only variable that must be considered. Indeed, it is important to evaluate 
# whether the data are biologically significant. To do this, the fold change is 
# often used, which is a measure of the change in mean expression value of the same
# gene in healthy vs tumor samples. 
mean_healthy = apply(healthy, 2, mean)

fun_mean_num <- function(x) {
  mean(as.numeric(x))
  
}

mean_healthy = apply(healthy, 2, fun_mean_num)

mean_healthy = apply(healthy, 2, function(x) mean(as.numeric(x), na.rm = TRUE))
mean_tumor = apply(tumor, 2, function(x) mean(as.numeric(x), na.rm = TRUE))

fold_change = mean_healthy/mean_tumor
fold_change = fold_change[-length(fold_change)]

# Also in the case of fold change, it is necessary to set a threshold. There is no
# correct or wrong value. 
fold_threshold = 1.5
sum(abs(fold_change) > fold_threshold)

# The volcano plot is a really easy and straightforward visualization of the genes
# that are both biologically and statistically significant. 
plot(log2(fold_change), -log10(pvalue), main = "Volcano plot", xlab = ("fold change"), 
     ylab = ("pvalue"))

abline(v = log2(fold_threshold), col = "red")
abline(v = -log2(fold_threshold), col = "blue")

abline(h = -log10(0.05), col = "pink")

# The last important step is to extract the top 20 genes that are both the 
# ones with the highest statistical and biological significance
expr_final = expr_final[, -ncol(expr_final)]
final_genes = data.frame(row.names = colnames(expr_final), adjusted_p, fold_change)
final_genes = final_genes[final_genes$adjusted_p < 0.05, ]
#sort it by pvalue
final_genes = final_genes[order(final_genes$adjusted_p), ]

# Correlation (add heatmap)
# The correlation in statistics measures how related are two variables, more in detail
# how their value changes in correspondence to one another. 
corr = cor(expr_final[, 1:20]) #take a sample or too many genes
                   
#We are using the default correlation method, which is Pearson. However, there is Spearman correlation
# that is usually better suited when you have a lot of outliers
diag(corr) = NA
library(pheatmap)
pheatmap(corr)
heatmap(corr)

#If the color of the heatmap is red or blue it means that there is some kind 
#of correlation between the data. The red areas mean that there is a positive 
#correlation, which put in simple terms it means that once the expression of one gene
#increases, it's correlated gene does it as well. Instead, the blue areas correspond 
#to negative correlation, which means the opposite: when the expression of one 
#gene increases, the other decreases. 

#The lines around the map correspond to the result of hierarchical clustering. 
#Hierarchical clustering is a clustering method that tries to group together the genes
#based on a distance matrix. This means that the more similar the genes are, the more 
#likely they are grouped within the same cluster. 


# ------- Regression Analysis -------------
# Regression Analysis is used to estimate a relationship between a dependent variable 
# and multiple independent variable. The easiest regression analysis is linear regression
# that tries to build a linear relationship between the dependent and independent 
# variable. However, in this case, with gene expression data, it doesn't make much sense
# to complete a linear regression regression. It is more appropriate to build a logistic 
# regression model. Logistic regression is a statistical techinique used to evaluate
# whether a binary variable (so a variable that can be either 0 or 1) can be predicted 
#using a set of continuous variables. In this case, we can try to evaluate whether the  
# expression of the ten genes that are the most significant can predict whether the sample 
# is going to be tumor or healthy (defines as 1 vs 0)

#extract the ten top differentially expressed genes from the original expression list
selected_genes = rownames(final_genes[1:10, ])
log_df = expr_final[,selected_genes]

#let's define as 1 the tumor sample and as 0 the healthy samples
log_df = data.frame(log_df, metadata$disease_status)
for(i in 1:nrow(log_df)) {
  if (log_df$metadata.disease_status[i] == "normal") {
    log_df$metadata.disease_status[i] = 0
  } else if(log_df$metadata.disease_status[i] == "colorectal cancer") {
    log_df$metadata.disease_status[i] = 1
  }
}

#IMPORTANT!!! 
log_df$metadata.disease_status = as.factor(log_df$metadata.disease_status)

# Now we have the dataset ready. However logistic regression is a type of machine 
# learning. As a result, it is necessary to have some data, on which we can train
# our model, and some data on which to test it. The standard practice is to split the
# dataset into train and test set, which are respectively 75% and 25% of the data. 

size <- floor(0.75 * nrow(log_df)) #let's extract how long should our dataset be
train_ind <- sample(seq_len(nrow(log_df)), size = size)
train <- log_df[train_ind, ] #takes only the rows that are in train_ind
test <- log_df[-train_ind, ]#takes only the rows that are not in train_ind

# Now let's train the model!

logit <- glm(metadata.disease_status ~ X209635_at + 
                 X1557067_s_at + X222018_at + X202479_s_at + X200882_s_at, data = train, 
               family = "binomial")

#let's check how the model looks
summary(logit)

# Let's test the results of the model on our train data and see how it performs.
test$pred <- predict(logit, newdata = test, type = "response")

#Let's set a threshold to specify what to define as tumor and what as healthy
for(i in 1:nrow(test)) {
  if (test$pred[i] < 0.5) {
    test$pred[i] = 0
  } else if(test$pred[i] >= 0.5) {
    test$pred[i] = 1
  }
}

#Try to calculate the precision and recall at home, and to make the precision and
#recall plot

# REFERENCES
# Data: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE164191
# Good book: "Modern statistics for biology", 
# https://web.stanford.edu/class/bios221/book/index.html



