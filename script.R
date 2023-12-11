library(readr)
library(dlookr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(Boruta)
library(ggcorrplot)
library(plotly)
library(ROSE)
library(caret)
library(DescTools)
library(ROCR)

# load dataset for later analysis
data_improt <- read_csv('C:/Users/LennyRuprecht/Downloads/loan_sample_7.csv')
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
# TODO: exclude loan amount and income form removing. this seems to be a bad idea?
## Lenny: Maybe its possible to cap loan amount and income, in order to minimize the influence of extreme values

##Lenny: Don't exactly know what you want here, so here is my interpretation of the outliers
### Loan Amount: There are 832 outlier observations. Which is about 2% of our data.
###               The mean loan amount of these outliers is around 29'822$ which is significantly
###               higher than the non-outliers with 11'275$.
### Interest Rate: There are 732 outlier observations. Which is about 1.8% of our data.
###               The mean interest rate is twice as big as it is without the outliers.
### Annual Income: There are 1421 outlier observations. Which is about 3.55% of our data.
###               The mean annaul income of the outliers is 3 times as high as it is normally
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

# TODO: elaborate which variables seem relevant in predicting the target feature?
## Lenny, not sure if it is correct what I'm doing :)


# Insert this block after your TODO comment for analyzing variable importance

# Regression model
default_model_lr <- glm(Status ~ ., data = clean_loan_data, family = binomial())

# Summary of the model to see coefficients and significance levels
model_summary <- summary(default_model_lr)

# Print the summary to view it
print(model_summary)


# Perform Boruta feature selection to find important variables
set.seed(123) # for reproducibility
boruta_output <- Boruta(Status~., data = clean_loan_data, doTrace = 0)
print(boruta_output)

# Get the names of important variables identified by Boruta
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)

# Plot the importance of attributes
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance from Boruta")







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


#########
# categorical variables and target
# Use a bar plot visualization to investigate the associations between the categorical variables and the target feature.

categorical_columns <- names(clean_loan_data)[12:16]

for (cat_col in categorical_columns) {
  print(
    clean_loan_data |>
    group_by({{cat_col}}, Status) |>
    summarise(count = n()) |>
    ggplot(aes(x = {{cat_col}}, y = count, fill = factor(Status))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = paste("Association between", cat_col, "and Status"),
        x = cat_col,
        y = "Count"
      ) +
      scale_fill_manual(
        values = c('turquoise3', 'tomato'),
        labels = c("0 (Not Defaulted)", "1 (Defaulted)")
      ) +
      theme_minimal()
  )
}

#########
# Correlations
# Visualize the correlations that emerge between the numerical features. Discuss the results.
# Which variables are highly correlated? Decide whether you keep all variables.

correlations <- cor(clean_loan_data[-c(11:17)])
p_value_mat <- cor_pmat(clean_loan_data[,-c(11:17)])
ggcorrplot(correlations, type = "lower", p.mat = p_value_mat)

boruta_output <- Boruta(Status~., data = clean_loan_data)
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")

#########
# Association between loan and income
# Plot an interactive scatter plot of the association between the loan amount requested and the annual income of the borrower.
# Discuss the plot. What can you tell about the association?

ggplotly(
  ggplot(clean_loan_data, aes(x = loan_amnt, y = annual_inc)) +
    geom_point(
      colour = "cornflowerblue",
      alpha = 0.5
    ) +
    labs(
      title = "Scatter Plot of Loan Amount vs. Annual Income (handeld outliers)",
      x = "Loan Amount",
      y = "Annual Income"
    )
)

ggplotly(
  ggplot(loan_data, aes(x = loan_amnt, y = annual_inc)) +
    geom_point(
      colour = "cornflowerblue",
      alpha = 0.5
    ) +
    labs(
      title = "Scatter Plot of Loan Amount vs. Annual Income (including outliers)",
      x = "Loan Amount",
      y = "Annual Income"
    )
)

#########
# Create a new balanced data set
# Create a new balanced data set where the two levels of the target variable will be equally represented;
# Create a bar plot of the newly created target variable. Why is this step necessary?

set.seed(7)
data_balanced <- ovun.sample(Status ~ ., data=clean_loan_data, method = "under")
balanced_loan_data <- data.frame(data_balanced[["data"]])

ggplot(balanced_loan_data, aes(x = Status, fill = Status)) +
  geom_bar() +
  ylab("Count") +
  xlab("Status of the loan") +
  scale_fill_manual(values = c('skyblue', 'brown2'), labels = c("0 (Not Defaulted)", "1 (Defaulted)"))


###############################################################
### Exercise 2
### Using the new balanced data set:
###############################################################

#########
# Divide the sample
# Divide the sample into training and testing set using 70% for training the algorithm.

set.seed(7)
div <- createDataPartition(y = balanced_loan_data$Status, p = 0.7, list = F)

# Training Sample
data.train <- balanced_loan_data[div,] # 70% here

# Test Sample
data.test <- balanced_loan_data[-div,] # rest of the 30% data goes here

default_model_lr1 <- glm(Status~., data = data.train, family = binomial())

summary(default_model_lr1)

# significant values
significant.variables <- summary(default_model_lr1)$coeff[-1,4] < 0.05
names(significant.variables)[significant.variables == TRUE]

#########
# Plot the ROC
# Plot the ROC and the Precision/Recall Curve and interpret the results.

# TODO intepret the curves correct
data.test$default_model_lr1_score <- predict(default_model_lr1, type='response', data.test)
default_model_lr1_pred <- prediction(data.test$default_model_lr1_score, data.test$Status)
default_model_lr1_roc <- performance(default_model_lr1_pred, "tpr", "fpr")

plot(default_model_lr1_roc, lwd=1, colorize = TRUE, main = "default_model_lr1: Logit - ROC Curve")
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1, lty=3)

default_model_lr1_precision <- performance(default_model_lr1_pred, measure = "prec", x.measure = "rec")
plot(default_model_lr1_precision, main="Fit1: Logit - Precision vs Recall")

#########
### Confusion matrix
# Produce the confusion matrix and interpret the results.

confusionMatrix(as.factor(round(data.test$default_model_lr1_score)), data.test$Status)

#########
# AUC values
# Report the AUC values and the overall accuracy and interpret the results.

default_model_lr1_auc <- performance(default_model_lr1_pred, measure = "auc")
cat("AUC: ", default_model_lr1_auc@y.values[[1]]*100)