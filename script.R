library(readr)
library(dlookr)

# load dataset for later analysis
data_improt <- read_csv('./loan_sample_7.csv')
loan_data <- data_improt

# ANALYZE DATA QUALITY
# we start with checking the dimension, structure, head and tail of our dataset
dim(loan_data)
str(loan_data)
head(loan_data)
tail(loan_data)

# We check the summary of all variables included.
summary(loan_data)

# we check the overview from dlookr to get a better understanding of the loan_data quality
overview(loan_data)
overview <- overview(loan_data)
plot(overview)


# we check for missing values
diagnose(loan_data, 1:17) # diagnose shows us, that there are no missing values so far


#####################
# Exercise 1
#
#
#####################
