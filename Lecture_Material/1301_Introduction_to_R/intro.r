###############################################################################
#                                                                             #
#   Introduction to the R language: a programming language for statistics     #
#                                                                             #
###############################################################################

# Hi and welcome to this tutorial on the basics of the R language. 
# Whether you're completely new or have had some experience but would like to revise
# some core concepts, I hope these lines of code help you! Now let's get started.

# The first thing you may have noticed when opening this .r file in RStudio is
# that lines that start with a "#" are different from the ones that do not.
# These are __commented__ lines, or comments: they don't do anything other than
# decorating your code with explanations or comments, both for you and other readers.
# In the case of this tutorial, I will use *stars* of _underscores_ in the comments
# to indicate important concepts or words you should pay special attention to.

# But let's go on with *real* lines that do *real* things on *not so real* data.
# Don't worry, soon enough you'll be able to use real data.


### 1. Vectors =======================================================
# The most basic unit of data in R is the vector. A vector is a collection of
# elements **of the same class** (numbers, characters, etc). 
# Run the lines below to create two empty vectors. To do that, select the lines
# with the mouse and press on "Run", at the top right corner of this pane.
empty_vector_1 <- c()
empty_vector_2 = c()

# You can tell they're empty because their length is zero
print(length(empty_vector_1))
print(length(empty_vector_2))

# If you look into your environment pane (upper right pane), you will see it's not empty
# anymore. Instead now you can see the names of the vectors you just created!
# We can create vectors (or any other object) by **assigning** them
# into a variable name, in this case, empty_vector_1 and empty_vector_2.
# The assignment operation can be done with "=" as well as with "<-"

# As I said before, vectors are collections of items of the same class. And you might
# ask "What is a class?". Well, glad you asked! Let's see it with some examples.
numbers = c(1,2,3)
character = c("one_word", "two words")
logical = c(TRUE, FALSE, F, T)

# Look at the objects we just defined in the environment. You will notice that "char"
# has "chr" next to it, "logical" has "logi" and "numbers" has "num". These short letters
# are indications of the classes of the elements in each of the vectors. You can also
# check the class of a vector by using the function class()
class(numbers)
class(character)
class(logical)

# Classes are a useful way of representing different data types. However, if we try
# to mix them, **many things can go wrong**. 

# For instance, let's see what happens when we try to calculate the mean of a vector
# with numerical, logical and character values inside
mixed_vector = c(1, TRUE, "a")
print(mean(mixed_vector))

# The output is Not Available (NA), and the console gives us a warning, saying that
# the"argument is not numeric or logical". But didn't we give a 1 and a TRUE? Those are numeric and logical!
# The key here is in the class of the vector:
class(mixed_vector)
print(mixed_vector)

# You will see 2 things in the console:
#   1. The class is character
#   2. All elements of the vector are in quotation marks
# What just happened is called **coercion**. 
# R tried to mix all elements by transforming them into a common class that they
# can all belong to. You cannot interpret the letter "a" as a number, but you can
# interpret the number 1 as a character (hence the quotation marks). Same goes for
# TRUE/FALSE: the truth value of the letter "a" is unclear, but you can easily
# transform TRUE into the word "TRUE".

# ---> TIP: whenever your code gives you an error, check that the class of the vector
#           is the appropriate one for the function you're trying to use.

## 1.1 Coercion ----
# Say that you accidentally have this vector,
accidentally_char = c("1", "2", "3", "4", "5")

# and you absolutely need to square each element and then sum the result. You will probably
# notice that the quotation marks are a problem for that and this won't work:
sum(accidentally_char*accidentally_char)

# We said before that a number can be converted to a letter (from 1 to "1"), and luckily
# for us, it also works the other way!
back_to_numeric = as.numeric(accidentally_char)
print(sum(back_to_numeric * back_to_numeric))

# Whenever you want to turn a class into another, you just have to use as.____()
# with ____ as the class you want, provided that the conversion is permitted.
# Writing these functions in your code will make it more robust and avoid accidental
# conversion that will make your program fail. It is good practice to use them.

## 1.2 Indexing ----
# To access the elements of a vector, use the [] notation. Here are some ways of indexing
# a vector. Run these lines one by one and try to understand how the more complex indexin works
abcde = c("a", "b", "c", "d", "e")
print(abcde)
print(abcde[1])
print(abcde[4])
print(abcde[2:4])
print(abcde[c(TRUE, FALSE, FALSE, TRUE, TRUE)])
print(abcde[-1])
print(abcde[-(3:5)])
print(abcde[c(5,2,3)])
indexing_vector = c(4, 4, 2, 1, 3, 4)
print(abcde[indexing_vector])

## [optional] vectors can also have names, and you can use those names as index
heights = c(180, 175, 167, 191)
names(heights) = c("Alice", "Bob", "Charlie", "Debbie")
print(heights)
heights["Bob"]

## 1.3 Misc
# Here are couple of convenient ways of creating vectors. You don't need to memorize them, 
# you can just revisit this section later and explore on your own with the functions
empty_vec_with_predefined_length = vector(mode = "numeric", length=10)
integer_sequence = 1:10
float_sequence = seq(0,1,0.1)
fixed_length_seq = seq(10,100, length.out = 42)
repeated_a = rep("A", 25)
concatenated_a = paste(repeated_a, collapse="")
element_wise_paste = paste("B", repeated_a, sep="-")

#
#
# You can use this space to experiment with the functions, 
# or type the commands directly in the console
#
#

### 2. More Complex Structures (matrix, dataframe, list) ======================
# Vectors are fine, but the real fun comes when we combine them to create collections of vectors.
# Almost always, the data we will use in R will be in the form of **a matrix, a data frame, or a list**.
# We can think of these data types as collections of vectors, but each one has their
# own behaviour, and it can be confusing to tell which one is the right one for our job.

# But worry not! here you will learn how to distinguish them and make always the right choice!
# Let's start with the simplest one: the matrix

### 2.1 Matrix -----
# A matrix is simply a collection of vectors of the same class. Remember that vectors
# do weird things when we combine classes? Well, matrices do just that, so you have to
# be careful not to accidentally turn a numeric matrix into a characer one!

# Initialize an empty matrix with the matrix() function
mat = matrix(NA, nrow=4, ncol=6)
print(mat)

# Or use a random number generating function, like runif(), which draws n random
# numbers from an uniform distribution (by default between 0 and 1)
mat = matrix(runif(n = 24), ncol=4, nrow=6)
print(mat)

# You can also "paste" vectors together to form a matrix. Do it column-wise with cbind()
col1 = 1:5; col2 = 6:10
cbind(col1, col2)
# Or row-wise with rbind()
rbind(col1, col2)

# Indexing works similar to vectors, but you can specify the position with [row, column]
print(mat[1,1])
## [optional] index the matrix with a single number in the square brackets and try to 
# figure out how the indexing works ( e.g. print(mat[5]) )

# Just like vectors have length(), matrices have nrow(), ncol(), and dim()
print(dim(mat))
nrow(mat)
ncol(mat)
length(mat)

# You can perform an operation between a single number and an entire matrix:
mat2 = matrix(1:24, nrow=4)
2 * mat2
# Or you can perform operations column-wise, if the sizes match
mat2 + 1:4
# If you apply a function on it, e.g. mean(), it will be performed on the entire matrix
mean(mat2)
# If you want to apply a function to each column, you can use apply(). The syntax is
# apply(matrix, rows/columns, function). Check out ?apply to find out more
apply(mat2, 2, mean) # 1 for rows, 2 for columns

### 2.2 Data Frame -----
# You can think of a dataframe as a flexible matrix. Here we can gather columns of different 
# data types, as long as the columns themselves are consistent. The sizes must also match, 
# or be a multiple of each other. To create a dataframe, simply specify what you want to have in it! 
col1 = 1:5; col2 = letters[1:5]
df1 = data.frame(col1, col2)
print(df1)
class(df1[,1])
class(df1[,2]) 
# If you saw "factor" instead of "character", don't worry. Factors are the way R represents categorical data. 
# We'll get to them at a later point. For now, think of them as characters with order.

# You can even give names to the columns and use them to index (btw this works on matrices too)
df1 = data.frame("numbers" = col1, "letters" = col2)
print(df1[, "numbers"])
print(df1[, "letters"])
df1$numbers

df1$numbers == df1[, "numbers"]
identical(df1$numbers, df1[, "numbers"])

### 2.3 List -----
# Last but not least, here comes the list! This is the most flexible data type.
# Each element of a list can have a different class and a different size. Create
# a list with the list() function
lista = list(1, c("a", "z"), c(TRUE, TRUE, FALSE), "name" = pi)

# Access elements in a list with double brackets [[]]
lista[[1]]
lista[["name"]]

### 3. Control structures (if, while, for) ====================================
# For now we have been learning about the building blocks of the R language, but 
# programming is more than just creating vectors. We want programs to be able to 
# respond in different ways to different situations. Or in other words, we want
# the program to execute a piece of code **only if a condition is met**, or repeat
# the same instructions **until a condition is met**

## 3.1 if statements: conditional execution ----
# Run the line below and look at the terminal. A prompt will appear asking you to
# enter a number. The value you give will be stored into the variable "input"
input = as.numeric( readline("Please enter a number: ") )

# Now we would  like a little programm to tell us whether that number is even or odd
# Basic syntax of an if statement:
# if (TRUE) {
#   do_this()
# } else {
#   do_that()
# }

if( (input %% 2) == 0){ # Is the remainder of diving my number by 2 equal to zero?
  # If so, say it is even
  print(paste("The number ", input, " is even"))
} else {
  # Otherwise, say it is odd
  print(paste("The number ", input, " is odd"))
}

# Some useful logical operators:
# AND: &      OR: |
# Bigger/Smaller than: > / <
# Bigger or equal/Smaller or equal than: >= / <=
# Is this element in that vector?: element %in% vector
# Is this object of that class?: is.class(vector)
#  note: is.class() is meant as a wildcard. Use e.g. is.numeric(), is.character(), is.list()...

## 3.2 while loops: ----
# "while this condition is true, do this action"
i = 0
while(i<10){
  print(i)
  i = i+1 # Don't forget this line!! otherwise loop gets stuck forever
}

## 3.3 for loop: iteration over a range ----
# This is the control structure that you will see more often out there in the wild
# Try to understand how  a for loop works by running the following line:
for (number in seq(1,10,2)){
  print(paste(number, number**2))
}

# All you need is a vector to iterate over. You can use such a vector to access other
# vector's elements, like this:
a = seq(3,30,3)
for (index in 1:length(a)){
  element_of_a_at_index = a[index]
  print(paste("Element number", index, "is equal to", element_of_a_at_index))
}

# Or, you can access the elements directly!
for (number in a){
  print(paste(number, "squared is equal to", number**2))
}

# Side note: for loops are actually a special case of while loops, but they are so convenient
# that they have their own syntax. Try writing the for loops above with while loops
# tip: the condition of the while loop can be some number is smaller than the length of a vector

### 4. Functions and vectorization ============================================
# For loop can be the easiest way to work with data at first. However, often it happens that
# you have to repeat the same commands multiple time, in this case it is useful to directly 
# define a function rather than copying-pasting the same line of codes and changing just few
# variables. In addition, for loops tend to be slow. It is much beter to replace them with 
# apply and function.
squared_fun <- function(number) {
  sqr = number**2
  return(sqr) #if you want to return something don't forget this command
}

# To use the function directly on one variable, you can call it directly.
sqr = squared_fun(2)

# In this case, we can apply this function to an entire vector
a = seq(3,30,3)
sqr_vec = squared_fun(a)

# This is the case because our function performs an operation (**2) which R can apply to each element in a vector
# This is called **vectorization** and it makes our code much faster. However, it is not always possible
# and in such cases, we could try writing a for loop instead. 

# But instead of that, R has another way of performing operations on every element of a vector
# that doesn't require for loops: the apply() family. 
# Check out the help page for apply(), sapply() and lapply(). 
# Now try to understand the syntax of lapply() from this example.
lapply(a, squared_fun) 

#first you give your vector or list and then the function which you want to apply on it

# lapply is used with list. However, you can define a function and apply it to a full data frame
# as well. In this case, you need to use function apply (NOT LAPPLY) and define whether you want 
# to use the function on rows, columns or both. 
df = data.frame(even = seq(2, 10, 2), odd = seq(1, 10, 2))
sqr_rows = data.frame(apply(df, 1, squared_fun)) #set the margin as 1 to apply it over rows
sqr_col = data.frame(apply(df, 2, squared_fun)) #set margin as 2 to apply it over columns
sqr_both = data.frame(apply(df, c(1, 2), squared_fun)) #set 1 and 2 to apply it to both


### 5. Visualization ==========================================================
# In base R you can already create different types of plots. The most common are histograms, 
# barplots, scatter plots and boxplots. But you can really plot anything. It depends on your data. 
# The most generic function to plot in R is "plot". The ouput is a scatterplot of the given data. 
data = datasets::pressure # Let's use a dataset that is pre-installed in R

plot(data$temperature, data$pressure) #The x and y of your plot can be defined through vectors. 
plot(data$temperature, 
     data$pressure, 
     type = "l",
     main = "Relationship between Tempreature and Pressure") #it is also possible to give as input two columns of a df

#In the plot you can change the colors (col), title (main), y axis (ylab), x axis (xlab) and much more. 
# To read about all the options, just print the following
?plot

# The functions for histogram and barplots are hist and barplot. To really understand how to use them, the
# best is to practice. That's why I created some exercises for you all!


### Bonus: Tidyverse ============================================
# If you find base R a bit confusing, there is an entire collection of packages
# that almost re-define the R syntax entirely: the tidyverse
# Check it out at https://www.tidyverse.org/
# My personal recommendations are ggplot2, dplyr and tidyr (in that order)
# In the solutions of the exerecises there is an ggplot implementation of all the plots. 
# It's a good way to start!