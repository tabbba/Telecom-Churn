library(ggplot2)
library(corrplot)
library(dplyr)
library(tidyr)
library(skimr)
library(treemapify)
library(stringr)
library(glue)
library(dummy)
library(caret)
library(car)
library(pROC)
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)

data = read.csv("TelecomChurn.csv")

# 3333 rows and 20 columns

head(data)

str(data)
sum(is.na(data)) # no missing values 

summary(data)

skim(data)

# Count of Distinct Features
for (col in names(data)) {
  cat("Distinct values in", col, ":\n")
  print(unique(data[[col]]))
  cat("\n")
}

anyDuplicated(data) # no duplicates

# converting characters to factor
data$State = as.factor(data$State)
data$International.plan = as.factor(data$International.plan)
data$Voice.mail.plan = as.factor(data$Voice.mail.plan)
data$Churn = as.factor(data$Churn)

# checking the distribution of our target

# We will first calculate the proportions of Churn
churn_percent <- data %>%
  group_by(Churn) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = Count / sum(Count) * 100)

#print(churn_percent)

ggplot(churn_percent, aes(x = "", y = Percent, fill = Churn)) +
  geom_col(width = 1, color = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("False" = "#ffe8cc", "True" = "#ff8787"), labels = c("False" = "No", "True" = "Yes")) +
  labs(title = "Percentage of Churn", fill = "Churn") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18))


continuous_vars <- data[, sapply(data, is.numeric)]
#print(continuous_vars)
categorical_vars <- data[, sapply(data, is.factor) & !names(data) %in% "Churn"]
#print(categorical_vars)
# univariate analysis for categorical variables (State, International.plan, Voice.mail.plan)

long_data <- categorical_vars %>%
  pivot_longer(cols = c("International.plan", "Voice.mail.plan"), names_to = "Category", values_to = "Value")

ggplot(long_data, aes(x = Value, fill = Value)) +
  geom_bar(color = "black") +
  facet_wrap(~ Category, scales = "free_x", nrow = 2, ncol = 2) +
  labs(title = "Distribution of Categorical Variables", x = NULL, y = "Count") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "none"
  ) 
# voice mail and international plan are imbalanced

# also our target variable is imbalanced

# Treemap for state Distribution
state.count <- data %>% group_by(State) %>% summarise(count = n())

ggplot(state.count, aes(area = count, fill = count, label = glue("{State}\n{count}"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "center", size = 13, grow = TRUE) +
  labs(title = "Tree Map for State Distribution", fill = "State") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, colour = "gray5"),
    legend.position = "none",
    plot.background = element_rect(fill = "#f6fff8")
  ) +
  scale_fill_viridis_c()
 
# univariate analysis for numerical variables
### KDE plots
continuous <- data[, sapply(data, is.numeric) | names(data) == "Churn"]

continuous %>%
  pivot_longer(cols = -Churn, names_to = "metric", values_to = "value") %>%
  ggplot(aes(value, color = as.factor(Churn))) +
  geom_density(alpha = 0.3) +
  facet_wrap(vars(metric), scales = "free") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_color_manual(name = "Churn", labels = c("No", "Yes"), values = c("#15aabf", "#ff8787")) +
  labs(title = "Numeric Features", subtitle = "Univariate Distribution by Churn") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, colour = "gray5"),
    plot.subtitle = element_text(hjust = 0.5, colour = "gray5"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "top"
  )

#There's a noticeable difference in the distribution of customer service calls 
# between those who churned and those who did not. 
# Customers who churned seem to make more customer service calls, which could 
# indicate a relationship between customer satisfaction and churn.

# It appears that customers who churned tend to have slightly higher day minutes 
# and charges. This might suggest that customers with higher usage during the day are more likely to churn.
# something similar with total.day.charge

# the others follow more or less the same trend

# analysis of churn and customer service calls
data %>%
  group_by(Customer.service.calls, Churn) %>%
  summarise(Count = n(), .groups = "drop") %>%
  ggplot(aes(x = Customer.service.calls, y = Count, color = Churn)) +
  geom_line() +
  geom_point() +
  labs(title = "Churn by Customer Service Calls", x = "Customer Service Calls", y = "Count", color = "Churn") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# customers who churned made more customer service calls


# let's double check it with hypothesis testing
# H0: the average number of customer service calls is the same for customers who churned and those who did not
# H1: the average number of customer service calls is different for customers who churned and those who did not

# chi square
chisq.test(data$Customer.service.calls, data$Churn)
# p-value < 0.05, we reject the null hypothesis therefore the average number of customer service calls is different for customers who churned and those who did not


# now hypo for: Average Charges for Churners vs. Non-Churners
# H0: the average charges are the same for customers who churned and those who did not
# H1: the average charges are different for customers who churned and those who did not

# t-test
t.test(data$Total.day.charge ~ data$Churn)
# p-value < 0.05, we reject the null hypothesis therefore the average charges are different for customers who churned and those who did not


# hypo for Voice Mail Plan and Churn:
# H0: the average churn rate is the same for customers who have a voice mail plan and those who do not
# H1: the average churn rate is different for customers who have a voice mail plan and those who do not

# chi square
chisq.test(data$Voice.mail.plan, data$Churn)
# p-value = 5.151e-09 < 0.05, we reject the null hypothesis therefore the average churn rate is different for customers who have a voice mail plan and those who do not


# analysis of churn and day minutes and day charge

# customers who churned have higher total day minutes
# customers who churned have higher total day charge

long_data <- data %>%
  select(Churn, Total.day.minutes, Total.day.charge) %>%
  pivot_longer(cols = c(Total.day.minutes, Total.day.charge),
               names_to = "Measure",
               values_to = "Value")

ggplot(long_data, aes(x = Churn, y = Value, fill = Churn)) +
  geom_boxplot() +
  facet_wrap(~ Measure, scales = "free_y") + 
  labs(title = "Churn by Total Day Minutes and Total Day Charge",
       x = "Churn",
       y = "Value",
       fill = "Churn") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12)
  )


# churn by international plan
data %>%
  group_by(International.plan, Churn) %>%
  summarise(Count = n(), .groups = "drop") %>%
  ggplot(aes(x = International.plan, y = Count, fill = Churn)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = 1) +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.9),
            vjust = -0.5, 
            size = 3.5) +
  labs(title = "Churn by International Plan", x = "International Plan", y = "Count", fill = "Churn") +
  theme_minimal() 

# we see that more than half of the customers that have an international plan churned


cor_mat <-
  data %>% 
  select(where(is.numeric)) %>% 
  cor()

corrplot(
  main = "\n\nCorrelation Matrix",
  cor_mat,
  method = "color",
  order = "alphabet",
  type = "lower",
  diag = FALSE,
  number.cex = 0.8,
  tl.cex = 0.6,
  tl.srt = 45,
  cl.pos = "b",
  addgrid.col = "white",
  addCoef.col = "white",
  col = COL1("Purples"),
  bg="gray",
  tl.col = "grey50"
)


# with this plot we see that there is a perfect correlation between the charge variables and minutes variables 
# so we can drop all the minutes one 
# we drop Total.day.minutes, Total.eve.minutes, Total.night.minutes, Total.intl.minutes

data <- data %>%
  select(-c(Total.day.minutes, Total.eve.minutes, Total.night.minutes, Total.intl.minutes))


group_plt <- function(var_1, var_2 = Churn){
  
  for_title_1 <- as_label(enquo(var_1))
  for_title_1 <- str_to_title(str_replace_all(for_title_1, "_", " "))
  
  for_title_2 <- as_label(enquo(var_2))
  for_title_2 <- str_to_title(str_replace_all(for_title_2, "_", " "))
  
  
  data %>%
    select({{var_1}}, {{var_2}}) %>% 
    mutate(var_1_ex = {{var_1}},
           var_2_ex = {{var_2}}) %>% 
    count(var_1_ex, var_2_ex, name = "counts") %>% 
    mutate(perc = (counts / sum(counts)) * 100) %>%
    arrange(desc(counts)) %>%
    ggplot(aes("", counts)) +
    geom_col(
      position = "fill",
      color = "black",
      width = 1,
      aes(fill = factor(var_2_ex))
    ) +
    geom_text(
      aes(label = str_c(round(perc,1), "%"), 
          group = factor(var_1_ex)),
      position = position_fill(vjust = 0.5),
      color = "white",
      size = 5,
      show.legend = FALSE,
      fontface = "bold"
    ) +
    coord_polar(theta = "y") +
    scale_fill_manual (values = c("#193964", "#026AA3")) +
    theme_void() +
    facet_wrap(vars(str_c(var_1_ex, "\n", for_title_1)))+
    labs(
      title = glue(for_title_2, " proportion by ", for_title_1),
      subtitle = " ",
      fill =NULL
    ) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          strip.text = element_text(
            colour = "black",
            size = 12,
            face = "bold"
          ))
  
}
group_plt(Voice.mail.plan)
group_plt(International.plan)
group_plt(State)

ggplot(data, aes(x = State, fill = Churn)) + 
  geom_bar(width = 0.7, color = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0, colour = "gray29", size = 10)) +
  labs(title = "Churn by State", x = "State", y = "Count", fill = "Churn", subtitle = "Is Churn rate influenced by State?") +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(limits = c(-35, 100)) +
  scale_fill_manual(values = c("False" = "#ffe8cc", "True" = "#ff8787"), labels = c("False" = "No", "True" = "Yes")) +
  coord_polar(start = 0)

# Investigate the relationship between state and churn with a chi-squared test of independence
# H0: State and Churn are independent
# H1: State and Churn are dependent
contingency.table <- table(data$State, data$Churn)
chisq.test(contingency.table)
# p-value is significant, we reject the null hypothesis and conclude that state influences the churn rate


contingency.table <- table(data$Total.night.calls, data$Churn)
chisq.test(contingency.table)
# p-value is not significant, we fail to reject the null hypothesis and conclude that Total night calls does not influence the churn rate

#POINT 3
# Our task is to predict whether customers will churn or not. this task is useful for the company 
# to take actions to prevent customers from churning.
# Moreover, it can helps the company to understand the reasons behind the churn and take actions to 
# improve the service and customer satisfaction by offering also promotions to the customers that are more likely to churn.


# CONCLUSION UP TO NOW:
# we dropped the minutes variables because they are perfectly correlated with the charge variables
# we saw that the churn rate is higher for customers that have an international plan
# we saw that the churn rate is higher for customers that do not have a voice mail plan
# we saw that the churn rate is higher for customers that have a higher number of customer service calls
# we saw that the churn rate is higher for customers that have higher total day minutes and total day charge
# we saw that the churn rate is higher for customers that have higher total eve charge, night charge, and intl charge
# we saw that State also influences the churn rate



# POINT 4
# before proceeding with the full model we need to focus on some lower dimensional model in order to 
# investigate some interesting relationships between the variables

# we'll proceed with customer service call and churn

# We only include customers who have made a service call
service_calls <- data %>%
  filter(Customer.service.calls > 0)

# The proportion of customers who churned after making a service call:
churn_percent <- service_calls %>%
  group_by(Customer.service.calls, Churn) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = Count / sum(Count) * 100)

ggplot(churn_percent, aes(x = Customer.service.calls, y = Percent, fill = Churn)) +
  geom_col(width = 0.7, color = 1) +
  labs(title = "Churn by Customer Service Calls", x = "Customer Service Calls", y = "Percent", fill = "Churn") +
  theme_minimal() +
  scale_fill_manual(values = c("False" = "#ffe8cc", "True" = "#ff8787"), labels = c("False" = "No", "True" = "Yes")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )


set.seed(1)
data$Churn <- as.factor(data$Churn)
model <- glm(Churn ~ Customer.service.calls, family = binomial, data = data)
summary(model)

call_range <- seq(min(data$Customer.service.calls, na.rm = TRUE), 
                  max(data$Customer.service.calls, na.rm = TRUE), 
                  length.out = 100)
newdata <- data.frame(Customer.service.calls = call_range)
newdata$Probability <- predict(model, newdata = newdata, type = "response")
newdata$Odds <- newdata$Probability / (1 - newdata$Probability)
head(newdata)


# plotting the probability of churn based on customer service calls
ggplot(newdata, aes(x = Customer.service.calls, y = Probability)) +
  geom_line() + 
  labs(title = "Effect of Customer Service Calls on the Probability of Churn",
       x = "Number of Customer Service Calls",
       y = "Probability of Churn") +
  scale_y_continuous(labels = scales::percent_format()) +  # Convert y-axis into percentage format
  theme_minimal() +  # Use a minimal theme
  theme(
    plot.title = element_text(hjust = 0.5), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.title.x = element_text(face = "bold"),  
    axis.title.y = element_text(face = "bold")  
  )

# since we are dealing with an imbalanced dataset we undersample the majority class (no churn) to balance the dataset.
set.seed(1)
majority_indices <- which(data$Churn == 'False')
minority_count <- sum(data$Churn == 'True')
sampled_indices <- sample(majority_indices, size = minority_count, replace = FALSE)
balanced_data <- data[c(sampled_indices, which(data$Churn == 'True')), ]

levels(data$Churn) # original levels --> i needed this step since i was getting some errors due to the factorization
balanced_data$Churn <- as.factor(balanced_data$Churn)
levels(balanced_data$Churn)

table(balanced_data$Churn)

balanced_model <- glm(Churn ~ Customer.service.calls, family = binomial, data = balanced_data )
summary(balanced_model)

# plot for balanced data
call_range <- seq(min(data$Customer.service.calls, na.rm = TRUE), 
                  max(data$Customer.service.calls, na.rm = TRUE), 
                  length.out = 100)
newdata <- data.frame(Customer.service.calls = call_range)
newdata$Probability <- predict(balanced_model, newdata = newdata, type = "response")
newdata$Odds <- newdata$Probability / (1 - newdata$Probability)

ggplot(newdata, aes(x = Customer.service.calls, y = Probability)) +
  geom_line() + 
  labs(title = "Effect of Customer Service Calls on the Probability of Churn (Balanced Data)",
       x = "Number of Customer Service Calls",
       y = "Probability of Churn") +
  scale_y_continuous(labels = scales::percent_format()) +  # Convert y-axis into percentage format
  theme_minimal() +  # Use a minimal theme
  theme(
    plot.title = element_text(hjust = 0.5), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.title.x = element_text(face = "bold"),  
    axis.title.y = element_text(face = "bold")  
  )

# plotting the confusion matrix for comparison
# 0 --> False, 1 --> True
# Prediction on the unbalanced data
confusion.mat <- function(data, model, target, threshold) {
  predicted_Y <- ifelse(predict(model, type = "response", newdata = data) > threshold, 'True', 'False')
  actual_Y <- data[[target]]
  True.positive <- sum(predicted_Y == 'True' & actual_Y == 'True')
  True.negative <- sum(predicted_Y == 'False' & actual_Y == 'False')
  False.positive <- sum(predicted_Y == 'True' & actual_Y == 'False')
  False.negative <- sum(predicted_Y == 'False' & actual_Y == 'True')
  Confusion.Matrix <- matrix(c(True.positive, False.negative, 
                               False.positive, True.negative),
                             nrow = 2, byrow = TRUE)
  rownames(Confusion.Matrix) <- c("Actual Positive", "Actual Negative")
  colnames(Confusion.Matrix) <- c("Predicted Positive", "Predicted Negative")
  print(Confusion.Matrix)
}

confusion.mat(data, model, "Churn", 0.5) #Predictions on unbalanced data
confusion.mat(balanced_data,balanced_model, "Churn", 0.5) # Predictions on balanced data


# POINT 5 
# PREPROCESSING AND FEATURE ENGINEERING 

# we should first deal with the 51 states, we can group them by region. let's do it
# let's group them by north, south, east, west

# print all the states
unique(data$State)

map_region <- function(state) {
  west.regions <- c("WA", "OR", "ID", "MT", "WY", "CA", "NV", "UT", "CO", "AK", "HI")
  southwest.regions <- c("AZ", "NM", "TX", "OK")
  midwest.regions <- c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "IN", "MI", "OH")
  southeast.regions <- c("AR", "LA", "MS", "AL", "TN", "KY", "WV", "VA", "NC", "SC", "GA", "FL")
  northeast.regions <- c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "PA", "NJ", "DE", "MD", "DC")
  
  if (state %in% west.regions) {
    return("West")
  } else if (state %in% midwest.regions) {
    return("Mid West")
  } else if (state %in% southeast.regions) {
    return("South East")
  } else if (state %in% northeast.regions) {
    return("North East")
  } else {
    return("South West")  # in case there are any states not covered
  }
}

data$Region <- sapply(data$State, map_region)

# we'll use the regions column for the first models to avoid having high-cardinality encoded columns.
# we store the variable in order to use it later with more robust models (tree-based).
state.column <- data$State
data$State <- NULL

head(data)

# let's see the distribution of churn by region
group_plt(Region)


# now let's re-do the chi square
contingency.table <- table(data$Region, data$Churn)
chisq.test(contingency.table)
# H0: Region and Churn are independent
# H1: Region and Churn are dependent
#p-value > 0.05, we fail to reject the null hypothesis, therefore the region does not influence the churn rate


# let's see the distribution of churn by region
ggplot(data, aes(x = Region, fill = Churn)) + 
  geom_bar(width = 0.7, color = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0, colour = "gray29", size = 10)) +
  labs(title = "Churn by Region", x = "Region", y = "Count", fill = "Churn", subtitle = "Is Churn rate influenced by the Region?") +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_fill_manual(values = c("False" = "#ffe8cc", "True" = "#ff8787"), labels = c("False" = "No", "True" = "Yes")) 

# POINT 5
# data preprocessing

# - split the data into training and testing sets
# Set seed for reproducibility
set.seed(1)
ids.train <- sample(1:nrow(data), size = 0.75 * nrow(data), replace = FALSE)
data.train <- data[ids.train,]
data.val <- data[-ids.train,] 

# SCALING
cols_to_scale <- c("Account.length", "Area.code", "Number.vmail.messages", "Total.day.calls", "Total.day.charge", "Total.eve.calls", "Total.eve.charge", "Total.night.calls", "Total.night.charge", "Total.intl.calls", "Total.intl.charge", "Customer.service.calls")

train.mean <- apply(data.train[, cols_to_scale], MARGIN = 2, FUN = mean)
train.sd <- apply(data.train[, cols_to_scale], MARGIN = 2, FUN = sd)

data.train[, cols_to_scale] <- scale(data.train[, cols_to_scale], center = train.mean, scale = train.sd)
# Scale validation data using training data's parameters
data.val[, cols_to_scale] <- scale(data.val[, cols_to_scale], center = train.mean, scale = train.sd)

# Baseline Logistic Regression Model
logistic.baseline <- glm(Churn ~ ., data = data.train, family = "binomial")
summary(logistic.baseline)
par(mfrow = c(2,2))
plot(logistic.baseline)
vif(logistic.baseline) # all good

# Stepwise Model Selection
# Akaike Information Criterion
akaike.fw <- step(glm(Churn ~ 1, family = "binomial", data = data.train), scope = formula(logistic.baseline), direction = "forward")
akaike.back <- step(logistic.baseline, direction = "backward")
akaike.both <- step(logistic.baseline, direction = "both")

# Bayesian Information criterion
bayesian.fw <- step(glm(Churn ~ 1, family = "binomial", data = data.train), scope = formula(logistic.baseline), direction = "forward", k = log(nrow(data.train)))
bayesian.back <- step(logistic.baseline, direction = "backward", k = log(nrow(data.train)))
bayesian.both <- step(logistic.baseline, direction = "both", k = log(nrow(data.train)))

# Considering bidirectional elimination of both methods, BIC is more strict as it removes all the covariates representing the regions 

get.metrics = function(conf.mat) {
  true.positives <- conf.mat[1,1]
  true.negatives <- conf.mat[2,2]
  false.positives <- conf.mat[1,2]
  false.negatives <- conf.mat[2,1]
  num.observations <- true.positives + true.negatives + false.positives + false.negatives
  
  accuracy <- (true.positives + true.negatives) / num.observations
  precision <- (true.positives) / (true.positives + false.positives)
  recall <- true.positives / (true.positives + false.negatives)
  f1 <- 2 * ((precision * recall) / (precision + recall))
  
  metrics <- data.frame(t(c(accuracy, precision, recall, f1)))
  columns <- c("Accuracy", "Precision", "Recall", "F1")
  colnames(metrics) <- columns
  
  return(metrics)
}

# Validation set results
akaike.mat <- confusion.mat(data.val, akaike.both, "Churn", 0.5)
akaike.metrics <- get.metrics(akaike.mat)
akaike.metrics

bayesian.mat <- confusion.mat(data.val, bayesian.both, "Churn", 0.5)
bayesian.metrics <- get.metrics(bayesian.mat)
bayesian.metrics
# Slightly better results with akaike, maybe dropping all the region features as BIC does is too big of a loss of informations

# ROC curves

