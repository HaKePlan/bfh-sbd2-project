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

# lets sort the data a bit
loan_data <- loan_data |> mutate(Status = as.factor(Status))

numerci_loan <- loan_data[sapply(loan_data, is.numeric)]
non_numeric_loan <- loan_data[sapply(loan_data, function(x) !is.numeric(x))]
loan_data <- cbind(numerci_loan, non_numeric_loan)

# let us show the new overview plot
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

# TODO: define outliers as such, here we just assume all outliers as such but we did not verify and explain why
outlier <- function(x) {
  quantiles <- quantile(x, c(.05, .95))
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  x
}



clean_loan_data <- map_df(loan_data[,-c(11:17)], outlier)
cols <- loan_data[,c(11:17)]
clean_loan_data <- cbind(clean_loan_data, cols)

boxplot(scale(clean_loan_data[,c(1:10)]), use.cols = TRUE)


#########
# Distribution of numeric features in target
# Choose the appropriate visualization to investigate the distribution of the numeric features per the two levels of our target feature (i.e. default vs non-default).
# Discuss the visualizations. Which variables seem relevant in predicting the target feature?

for (i in 1:length(clean_loan_data[,-c(11:17)])) {
  print(
    ggplot(clean_loan_data, aes(y = clean_loan_data[,i], color = Status)) +
      geom_boxplot() +
      ylab(names(clean_loan_data[i])) +
      scale_color_manual(values = c('turquoise3', 'tomato'), labels = c("0 (Not Defaulted)", "1 (Defaulted)")) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  )
}
