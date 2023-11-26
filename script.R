library(readr)
library(dlookr)
library(dplyr)
library(ggplot2)

# load dataset for later analysis
data_improt <- read_csv('./loan_sample_7.csv')
loan_data <- data_improt

#########
# Structure
# Check and report the structure of the data set.

# we start with checking the dimension, structure, head and tail of our dataset
dim(loan_data)
str(loan_data)
head(loan_data)
tail(loan_data)

# we check the overview from dlookr to get a better understanding of the loan_data quality
overview(loan_data)
overview <- overview(loan_data)
plot(overview)


#########
# Categorical variables

# How many numeric and how many categorical variables are included in the data?
# What categorical variable has the most levels in it?

overview |>
  filter(division == 'data type' & value > 0) |>
  select(metrics, value)

# from plot(overview) we know which are character variables
# lets check if each character variable is also a classifier
class_variables <- c('grade', 'home_ownership', 'verification_status', 'purpose', 'application_type')

result <- lapply(class_variables, function(x) loan_data |> distinct(across({{x}})))

#########
# Summarize variables
# Summarize the variables. Discuss the summary statistics obtained.

summary(loan_data)

#########
# Target variable
# Check the levels of the target variable by choosing the appropriate visualization. Is the target variable balanced?

loan_data <- loan_data |> mutate(Status = as.factor(Status))

table(loan_data$Status)

ggplot(loan_data, aes(x = Status, fill = Status)) +
  geom_bar() +
  ylab("Count") +
  xlab("Status of the loan") +
  scale_fill_manual(values = c('skyblue', 'brown2'), labels = c("0 (Not Defaulted)", "1 (Defaulted)"))

