# Week 1 Practice Code

# Type your name here:
'Lai Chi Long'
# Complete these coding tasks, then submit your R file for another student to compare against the example output.

# Problem 1 

# Create a data frame that includes two columns, one named "Animals" and the other named "Foods". The first column should be this vector (note the intentional repeated values): Dog, Cat, Fish, Fish, Lizard


#The second column should be this vector: Bread, Orange, Chocolate, Carrots, Milk


#### Write your code below:
Animals<-c('Dog','Cat','Fish','Fish','Lizard')
Animals
Foods<-c('Bread','Orange','Chocolate','Carrots','Milk')
Foods
df<-data.frame('Animals'=Animals,'Foods'=Foods)
df

# Problem 2

# Using the data frame created in Problem 2, use the table() command to create a frequency table for the column called "Animals".

#### Write your code below:
table(df$Animals)

# Problem 3

# Use read.csv() to import the survey data included in this assignment. Using that data, make a histogram of the column called "pid7".


#### Write your code below:
col<-read.csv('/Users/chilonglai/Library/CloudStorage/OneDrive-CityUniversityofHongKong-Student/academic/online/coursera/course/Statistics and data/Getting Started with Data Visualization in R/D1LYDGZLRAmS2AxmSxQJHw_244a6af25c32479990d299bf82de1a67_cces_sample_coursera 2023-02-06 02_43_02.csv')

hist(col$pid7)
