library(readr)
library(dlookr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

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

#########
# Distribution of numeric variables
# Check the distribution of the numeric variables in the data set (include different visual representations).

numeric_vars <- loan_data |> select_if(is.numeric)

# Gather the data for plotting
gathered_data <- gather(numeric_vars, key = "Variable", value = "Value")

# Plot the distribution of numeric variables
ggplot(gathered_data, aes(x = Value, fill = Variable)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Distribution of Numeric Variables",
       x = "",
       y = "") +
  theme_minimal()

#########
# Checking for outliers
# Investigate whether certain variables contain outliers (hint: what does a box plot show?).

ggplot(gathered_data, aes(x = Value, fill = Variable)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Distribution of Numeric Variables",
       x = "",
       y = "") +
  theme_minimal()

boxplot(scale(numeric_vars[,1:11]), use.cols = TRUE)

diagnose_outlier(numeric_vars)

numeric_vars |> plot_outlier(diagnose_outlier(numeric_vars) |> filter(outliers_ratio >= 0.5) |> select(variables) |> unlist())


#########
# Dealing with outliers
# Elaborate your view on how to proceed in dealing with the outliers and – if necessary, take appropriate action.

# TODO investigate in how to deal with outliers (week 5)
# outlier <- function(x) {
#   quantiles <- quantile(x, c(.05, .95))
#   x[x < quantiles[1]] <- quantiles[1]
#   x[x > quantiles[2]] <- quantiles[2]
#   x
# }
#
# data_new_numeric <- map_df(numeric_vars[,-c(20:24)], outlier)
# cols <- numeric_vars[,c(20:24)]
# data_new_numeric <- cbind(data_new_numeric, cols)
#
# boxplot(scale(data_new_numeric[,c(1:19)]), use.cols = TRUE)

