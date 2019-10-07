#require(...) is a method of both loading and checking for the presence of packages. 
#It attempts to load the named package and returns TRUE if successful, FALSE otherwise.
dir.create("BUGS_example")
setwd("BUGS_example")

tidy <- require(tidyverse)
dataTable <- require(data.table)
peakRAM <- require(peakRAM)



# Tidyverse is a set of packages which contain remarkaby powerful tools for a diverse range of tasks. 
# It is worth exploring every single package in the tidyverse set thoroughly, including the non-core packages.
# Fortunately, the documentation for each of these packages is excellent,
# and they are among the most used R packages, so vignettes and stack overflow examples are abundant

# Read about the core set at:
# https://www.tidyverse.org/packages/

# Read about the other packages tidyverse installs at:
# https://tidyverse.tidyverse.org/

# The "tidyverse" package is actually a collection of several packages, and library(tidyverse) simply loads all of these in one command.
# We will be using: dplyr, ggplot2, and forcats from the tidyverse set
if(!tidy){
  install.packages("tidyverse")
  library(tidyverse)
}

# data.table is, in my opinion, a non-optional package in R for any serious big data task.
# essentially, it improves expands upon the concept of a data.frame that exists in default R

# Almost anything that can be done with a data.frame (or even a tibble from tidyverse) can be done faster, 
# usually with simpler code, and often with less computer resource usage than otherwise.
# We'll touch on some of the critical advantages of the data.table later

if(!dataTable){
  install.packages("data.table")
  library(data.table)
}

# There are several packages which exist for the purposes of profiling your R code's performance characteristics. 
# microbenchmark is a more practical package for purely timing functions, but peakRAM times and records both the
# maximum memory used during a function call and the memory allocated (or freed) during the call.
# We'll use it sparingly, and later on.
if(!peakRAM){
  install.packages("peakRAM")
  library(peakRAM)
}

# This tutorial will be divided into a few steps:


#############################################################################
###################### Part 1: Practical Basics #############################
#############################################################################

#It's a scripting language.

peakRAM({
  
ten_million_vector_for <- 1:1e7

for(i in 1:1e7){
  ten_million_vector_for[i] <- ten_million_vector_for[i]^2
}

})

peakRAM({
  
ten_million_vector_vec <- 1:1e7

ten_million_vector_vec <- ten_million_vector_vec^2

})

# Size in MiB - it's exactly the same as the peakRAM for the for loop
object.size(ten_million_vector_for)/(1024^2)

# And they are indeed identical
identical(ten_million_vector_for, ten_million_vector_vec)

#Why was the second one about 10 times as fast?
#Why did the second one use more memory?

#Cleanup, because R won't.
rm(ten_million_vector_for, ten_million_vector_vec)



#Give a number of rows and columns to this function and see how long it takes to create an 
# all vs. all matrix of the products of 1:row and 1:col using different implementations to get the job done
# slow determines if the function will do the (MASSIVELY slower) nested for loops over cols and rows. Set this to F if you want to do larger row/col sets
test_versions <- function(row, col, slow = T){
  
if(slow){
forLoopOver_RC <- peakRAM({
a_big_matrix <- matrix(0, nrow = row, ncol = col)

for(i in 1:col){
  
  for(j in 1:row){
    
      a_big_matrix[j, i] <- i*j
    
  }
  
}

})
}

#Even small decisions like going over a shorter dimension can really shorten/lengthen code.
forLoopOverRows <- peakRAM({
  
a_big_matrix <- matrix(0, nrow = row, ncol = col)

for(j in 1:row){
  
  a_big_matrix[j,] <- j*1:col
  
}

})

forLoopOverCols <- peakRAM({
  
  a_big_matrix <- matrix(0, nrow = row, ncol = col)

  for(i in 1:col){
    
    a_big_matrix[,i] <- i*1:row
    
  }
  
})


#These only differ in that we only call 1:row or 1:col one time, outside the loop. If you do it inside, then R has to do it every time.
forLoopOverCols_E <- peakRAM({
  
  a_big_matrix <- matrix(0, nrow = row, ncol = col)
  seq <- 1:row
  
  for(i in 1:col){
    
    a_big_matrix[,i] <- i*seq
    
  }
  
})

forLoopOverRows_E <- peakRAM({
  
  a_big_matrix <- matrix(0, nrow = row, ncol = col)
  seq <- 1:col
  
  for(j in 1:row){
    
    a_big_matrix[j,] <- j*seq
    
  }
  
})


fully_vectorized <- peakRAM({
  
  a_big_matrix <- as.vector(1:row) %*%  t(as.vector(1:col))

})


if(slow){
  timingResults <- rbind(forLoopOver_RC, forLoopOverRows, forLoopOverCols, forLoopOverRows_E, forLoopOverCols_E, fully_vectorized)
  timingResults$Function_Call <- c("For over R/C", "For over rows", "For over cols", "Efficient rows", "Efficient cols", "Fully vectorized")
  timingResults$rows = row
  timingResults$cols = col 
}else{
  timingResults <- rbind(forLoopOverRows, forLoopOverCols, forLoopOverRows_E, forLoopOverCols_E, fully_vectorized)
  timingResults$Function_Call <- c("For over rows", "For over cols", "Efficient rows", "Efficient cols", "Fully vectorized")
  timingResults$rows = row
  timingResults$cols = col 
}


return(timingResults)
}

#Example 1000 row by 1000 column matrix
test_versions(1e4, 1e4, slow = F)

#Mapply is a loop which takes multiple input vectors/lists of equal length and does something with each set of matched items.
# i.e. this loop takes the sets (2,2) (2,3) (2,4) (3,2) (3,3) and (3,4) in that order as arguments passed to function(row_exp, col_exp)

# There's a whole family of apply functions - apply, sapply, lapply, mapply, tapply, and a few more. 

#This segment applies a set of 10^exponent values rows and columns and runs test_versions on matrices with that many rows a
#Remember, when we increment both exponents by 1
#The vectors at the end are the exponents fed to the function - the function will work on 10^exp1 rows and 10^exp2 cols

results <- mapply(function(row_exp, col_exp){
  col <- 10^col_exp
  row <- 10^row_exp
  tv <- test_versions(row=row, col=col, slow = ifelse(row*col > 1e7, F, T))
  return(list(tv))
}, c(2,2,2,3,3,3,4), c(2,3,4,2,3,4,4))
  
#Get these into 1 dataframe
results <- rbindlist(results)

results$mat_size <- results$rows*results$cols

# ggplot is a part of the tidyverse. The visualization tools in base R are good, but limited in function and power.
# If you are new to R, you should completely ignore base R visualizations and learn GGplot. 
# It has an easier syntax overall and will produce higher quality plots more quickly, and will force you to learn good habits along the way.

# There are many supplementary packages for ggplot, including ones for plotting on geographical maps, networks, trees, intearctive viz, and much more
# It is NOT the only package you'll need to do anything and everything, but you will be surprised at how much you can do

# Using only ggplot and simple code: 
#http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

# Almost every visualization you can think of, done well, in R: 
#https://www.r-graph-gallery.com/

ggplot(data=results, aes(x=mat_size, y = Elapsed_Time_sec, color=Function_Call)) +
  geom_line(size=1.35) +
  ylab("Runtime (Sec)") +
  xlab("Matrix size (Rows * Cols)")+
  ggtitle(label = "Matrix operations runtimes", subtitle = "How fast can we get?")


#GGplot syntax is always more or less as shown here: 
# ggplot(data = data.frame/tibble/data.table, aes(variable1 = colName1, variable2 = colName2, ... color/fill=grouping_colName)) +
# method of visualization as geom_<method>, e.g. geom_line, geom_point, geom_histogram, geom_tile, etc. +
# tweaks

ggplot(data=results, aes(x=mat_size, y = Peak_RAM_Used_MiB, color=Function_Call)) +
  geom_line(size=1.35) +
  ylab("Maximum RAM load (MiB)") +
  xlab("Matrix size (Rows * Cols)")+
  ggtitle(label = "Matrix operations resource use", subtitle = "What did we pay for it?")

# We'll touch on ggplot again later.



#############################################################################
######################## Part 2: Useful Tools ###############################
#############################################################################

# R (nearly always) expects data to be fully loaded into memory from files

# Similarly, creating a file is done by writing an R object in memory (such as a data.table)
# to a file outside of R

# Typically, these will be flat files - tables with some number of rows, and columns of data, 
# separated by a delimiter - usually a comma or tab - and by lines

# The base tools in R are surprisingly slow at doing this really fundamental task.

# I'm not going to make you download some huge file, so the speed differences will not be obvious
# Instead, we're going to make some of our own, and they'll be fairly small

# You can read more at:
# https://jozef.io/r917-fread-comparisons/


# We're going to make a file with 1 million lines. This is NOT a large file by the standards of most modern analysis.
# It is, however, large enough to demonstrate why data import/export control is important.

num_rows <- 1e6

#Make sure we get the same results
set.seed(1)

#1 to num_rows IDs, with random capital letters and normally distributed values to boot.
a_file <- data.table(ID = 1:num_rows)
a_file$letter <- sample(LETTERS, num_rows, replace = T)
a_file$value1 <- rnorm(num_rows, 0, 1)
a_file$value2 <- rnorm(num_rows, 3, 2)

#This is actually megabytes. Should be 26.7 Mb
object.size(a_file)/(1024^2)

#Writing

#Base R:
peakRAM(write.table(a_file, "I_O.tsv", sep="\t", col.names = T))

#data.table
peakRAM(fwrite(a_file, "I_O.tsv", sep="\t", col.names = T))

#Again, notice the zero memory usage of data.table

#Reading

peakRAM({
  a_file <- read.table("I_O.tsv", sep = "\t", header=T)
})

peakRAM({
  a_file <- fread("I_O.tsv", sep = "\t", header = T)
})

#Same deal for reading in the file



# Your call what you want to use, but really consider it.
# The only reason I wouldn't use more powerful packages is that each additional package is a new dependency for an end-user.


# Some other example advantages:

# Do some function, grouping by a variable

# Using base R
peakRAM(sum_of_val_1_agg <- aggregate(a_file$value1, by=list(a_file$letter), FUN=sum))

# Using data.table
peakRAM(sum_of_val_1_DT <- a_file[, sum(value1), by = letter])

# The key takeaway isn't even the speed, it's the fact that the operation could be done in less than 1/5th the memory





# This next example expects your version of R to be up-to-date. 
# I do not know how old a version of data.table can run the next few commands successfully.
# Mine is 3.6.0 - Planting of a Tree

# It's almost impossible to overstate how ridiculously powerful this capacity is for a memory-limited task
only_a <- fread(cmd = "grep 'A' I_O.tsv", header=T, sep = "\t")
head(only_a)

# Really, though. This is magic. It even works on windows without ANY extra steps.
sed_too <- fread(cmd = "sed 's/A/lol/g' I_O.tsv", header=T, sep = "\t")
head(sed_too)

# And you can even pipe. Again, this is working on windows with unix behavior.
only_a_lol <- fread(cmd = "grep 'A' I_O.tsv | sed 's/A/lol/g'", header=T, sep = "\t")
head(only_a_lol)


#Cleanup more
rm(only_a, sed_too, only_a_lol)


#############################################################################
################## Part 3: Visualization and Data Design ####################
#############################################################################

# How well data is designed is a relative term. 
# The desired task determines what you should be doing, and the tools you wish to use will make a big impact, too
# Since we're looking at visualization, it's a great time to think about tradeoffs


# So, here's some data.
head(a_file)


# Let's plot it.

# Technically, we plotted it.
ggplot(data=a_file, aes(x = value1)) +
  geom_histogram()

# This too. Totally useful.
ggplot(data=a_file, aes(x = value2)) +
  geom_histogram()

# How about both?
ggplot(data=a_file, aes(x = value1)) +
  geom_histogram() +
  geom_histogram(aes(x = a_file$value2), inherit.aes = F)

# Maybe separate them?
ggplot(data=a_file, aes(x = value1)) +
  geom_histogram(fill = "red") +
  geom_histogram(aes(x = a_file$value2), inherit.aes = F, fill = "blue")

# Wish we could see both, totally?
ggplot(data=a_file, aes(x = value1)) +
  geom_histogram(fill = "red", alpha = .5) +
  geom_histogram(aes(x = a_file$value2), inherit.aes = F, fill = "blue", alpha = .5)




# Was this the best way to plot this?
reformatted <- melt(a_file, id.vars = c("ID", "letter"))

# We doubled the rows and have the same number of cols. Why?
nrow(reformatted)
head(reformatted)


# Why would we do this? Is it better? Is it worse?
ggplot(reformatted, aes(x = value, fill = variable)) +
  geom_histogram(alpha = .5, position =  "identity")

# Now imagine we had 100 columns.






# General advantage of long format:
# categorization becomes MUCH more intuitive


# This is using dplyr's pipe "%>%", and produces a tibble instead of a data.frame or data.table. The formats are mostly interchangable.
peakRAM({other_adv <- reformatted %>% group_by(letter, variable) %>% summarise(mu=mean(value), sd= sd(value))})
head(other_adv)


#This is using data.table
setkeyv(reformatted, c("letter", "variable"))
peakRAM({other_adv_DT <- reformatted[, list(mean(value), sd(value)), by = key(reformatted)]})
head(other_adv_DT)

# For once, data.table is actually not more memory efficent (though it is still faster). Multiple tools in the kit are good to have.

# And there's all sorts of things you can do with this kind of data. 
ggplot(data = other_adv_DT, aes(x = letter, y = V1)) +
  geom_errorbar(aes(ymin = V1-V2, ymax = V1+V2)) +
  facet_wrap(~variable) +
  ylab("Mean")
















