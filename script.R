library(readr)
library(dlookr)

# load dataset for later analysis
data_loans <- read_csv("./loan_sample_7.csv")

data <- data_loans # We always make a copy from the original dataset and work on the copy

# ANALYZE DATA QUALITY
# we start with checking the dimension, structure, head and tail of our dataset
dim(data) # this shows us the demension of our dataset (40'000 Rows, 17 Cols)
str(data) # structure of the dataset and its variables
head(data)
tail(data)

# We check the summary of all variables included.
summary(data)

# we check the overview from dlookr to get a better understanding of the data quality
overview(data)
overview <- overview(data)
plot(overview)

# we check for missing values
diagnose(data, 1:17) # diagnose shows us, that there are no missing values so far


#####################
# Exercise 1
#
#
#####################
