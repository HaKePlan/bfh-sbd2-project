library(readr)
library(dlookr)
library(dplyr)

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


#########
# How many numeric and how many categorical variables are included in the data?
# What categorical variable has the most levels in it?

overview |>
  filter(division == 'data type' & value > 0) |>
  select(metrics, value)

# from plot(overview) we know which are character variables
# lets check if each character variable is also a classifier
class_variables <- c('grade', 'home_ownership', 'verification_status', 'purpose', 'application_type')

result <- lapply(class_variables, function(x) loan_data |> distinct(across({{x}})))


grade <- loan_data |>
  distinct(grade)

home_ownership <- loan_data |>
  distinct(home_ownership)

verification_status <- loan_data |>
  distinct(verification_status)

purpose <- loan_data |>
  distinct(purpose)

application_type <- loan_data |>
  distinct(application_type)





grade <- loan_data |>
  distinct(grade) |>
  count()

home_ownership <- loan_data |>
  distinct(home_ownership) |>
  count()

verification_status <- loan_data |>
  distinct(verification_status) |>
  count()

purpose <- loan_data |>
  distinct(purpose) |>
  count()

application_type <- loan_data |>
  distinct(application_type) |>
  count()


