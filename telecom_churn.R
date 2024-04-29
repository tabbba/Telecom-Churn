library(ggplot2)
library(corrplot)
library(dplyr)
library(tidyr)

data = read.csv("TelecomChurn (1).csv")

dim(data) # 3333 rows and 20 columns

head(data)

str(data)

sum(is.na(data)) # no missing values 

summary(data)

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

ggplot(churn_percent, aes(x = "", y = Percent, fill = Churn)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("False" = "violet", "True" = "skyblue"), labels = c("False" = "No", "True" = "Yes")) +
  labs(title = "Percentage of Churn", fill = "Churn") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5))


continuous_vars <- data[, sapply(data, is.numeric)]
categorical_vars <- data[, sapply(data, is.factor) & !names(data) %in% "Churn"]

# univariate analysis for categorical variables (State, International.plan, Voice.mail.plan)

long_data <- categorical_vars %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Value")

ggplot(long_data, aes(x = Value)) +
  geom_bar(fill = "skyblue", color = "black") +
  facet_wrap(~ Category, scales = "free_x", nrow = 2, ncol = 2) +
  labs(title = "Distribution of Categorical Variables", x = NULL, y = "Count") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "none"
  )
# voice mail and international plan are imbalanced

# also our target variable is imbalanced

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
  scale_color_discrete(name = "Churn Status", labels = c("No", "Yes")) +
  labs(title = "Numeric Features Univariate Distribution by Churn Status") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
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
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.9),
            vjust = -0.5, 
            size = 3.5) +
  labs(title = "Churn by International Plan", x = "International Plan", y = "Count", fill = "Churn") +
  theme_minimal() 

# we see that almost all customers that have an international plan churned

