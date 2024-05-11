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
library(pROC)
library(cluster)
library(factoextra)
library(sf)
library(latex2exp)
library(gridExtra)
library(tree)
library(randomForest)
library(xgboost)
library(dendextend)
library(purrr)
library(ROSE)


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

# customers who churned made more customer service calls (?)


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



us_map <- map_data("state") %>% 
  as_tibble()

us_map %>%
  ggplot(aes(long, lat, map_id = region)) + 
  geom_map(
    map =us_map,
    color = "gray80",
    fill = "gray30",
    size = 0.3
  ) + 
  coord_map("ortho", orientation = c(39, -98, 0)) + 
  labs(
    title = "US Map",
    subtitle = "Based on Latitude and Longitude"
  ) + theme(plot.title = element_text(hjust=0.5))

state_names <- c(KS = "kansas", OH = "ohio", NJ = "new jersey", OK = "oklahoma",
                 AL = "alabama", MA = "massachusetts", MO = "missouri", LA = "louisiana",
                 WV = "west virginia", IN = "indiana", RI = "rhode island", IA = "iowa",
                 MT = "montana", NY = "new york", ID = "idaho", VT = "vermont",
                 VA = "virginia", TX = "texas", FL = "florida", CO = "colorado",
                 AZ = "arizona", SC = "south carolina", NE = "nebraska", WY = "wyoming",
                 HI = "hawaii", IL = "illinois", NH = "new hampshire", GA = "georgia",
                 AK = "alaska", MD = "maryland", AR = "arkansas", WI = "wisconsin",
                 OR = "oregon", MI = "michigan", DE = "delaware", UT = "utah",
                 CA = "california", MN = "minnesota", SD = "south dakota", NC = "north carolina",
                 WA = "washington", NM = "new mexico", NV = "nevada", DC = "district of columbia",
                 KY = "kentucky", ME = "maine", MS = "mississippi", TN = "tennessee",
                 PA = "pennsylvania", CT = "connecticut", ND = "north dakota")

churn.tbl <- data %>% select(State, Churn) %>%
  group_by(State) %>%
  summarize(
    total_customers = n(),
    churned_customers = sum(Churn == "True"),
    churn_rate = (churned_customers / total_customers),
    churn_rate_txt = scales::percent(churn_rate)
  ) %>%
  mutate(full_state = state_names[as.character(State)]) %>%
  ungroup() %>% 
  left_join(us_map, by=c("full_state" = "region"))

us_map_sf <- st_as_sf(us_map, coords = c("long", "lat"), crs = 4326, agr = "constant")

centroids <- us_map_sf %>%
  group_by(region) %>%
  summarise(geometry = st_centroid(st_union(geometry)))

centroids_df <- as.data.frame(st_coordinates(centroids))
centroids_df$region <- centroids$region

churn.tbl <- churn.tbl %>%
  left_join(centroids_df, by = c("full_state" = "region"))

churn.tbl %>%
  ggplot(aes(long, lat)) + 
  geom_map(
    aes(map_id = full_state),
    map = us_map,
    color = "gray86",
    fill = "gray30",
    size = 0.3
  ) + coord_map("ortho", orientation = c(39, -98, 0)) + geom_polygon(aes(group = group, fill = churn_rate), color="black") +
  scale_fill_gradient2("",low = "#18BC9C", mid = "white", high = "#E31A1C", midpoint = 0.10, labels = scales::percent) + 
  geom_text(aes(x = X, y = Y, label = State), stat = "unique", size = 3, inherit.aes = FALSE, fontface = "bold") +
  theme_void() + 
  theme(
    plot.title = element_text(size=18, face="bold", color = "#2C3E50"),
    legend.position = "right"
  ) + 
  labs(
    title = "Churn Rate Across US States",
    x = "",
    y = ""
  ) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
  ) 

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

## the intercept's p-value (<2e-16) suggests that there is a statistically significant effect of having zero customer service calls on the likelihood of churn.
## the p-value for Customer.service.calls (<2e-16) suggests that there is a statistically significant association between the number of customer service calls and an increased likelihood of churn.

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
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dashed")
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

levels(data$Churn) 
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


## other interactions between different predictors:
### international plan and voice mail plan
set.seed(1)
data$Churn <- as.factor(data$Churn)
model <- glm(Churn ~ International.plan*Voice.mail.plan, family = binomial, data = data)
summary(model)
### Intercept: Very significant with a p-value < 2e-16, indicating a strong effect when both plans are absent.
### Both International.planYes and Voice.mail.planYes are highly significant
### Interaction term: while international plans are associated with higher churn, voice mail plans seem to mitigate churn risk. 
### However, customers who have both plans are particularly at risk of churning, possibly due to the higher costs or complexities associated with managing multiple service features.

### Customer service call and international plan
set.seed(1)
data$Churn <- as.factor(data$Churn)
model <- glm(Churn ~ Customer.service.calls*International.plan, family = binomial, data = data)
summary(model)
### Both International.planYes and Customer.service.calls are highly significant
### Interaction term: customers with more customer service interactions generally have higher churn rates. However, the negative interaction indicates that 
### the churn-increasing effect of service calls might be mitigated among customers with an international plan, possibly due to different expectations or experiences.

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
data$Region <- as.factor(data$Region)

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

# Oversampling minority class
data.train.balanced <- ovun.sample(Churn ~ ., data = data.train, method = "over", N=4260)$data
table(data.train.balanced$Churn)

# SCALING
cols_to_scale <- c("Account.length", "Area.code", "Number.vmail.messages", "Total.day.calls", "Total.day.charge", "Total.eve.calls", "Total.eve.charge", "Total.night.calls", "Total.night.charge", "Total.intl.calls", "Total.intl.charge", "Customer.service.calls")

train.mean <- apply(data.train.balanced[, cols_to_scale], MARGIN = 2, FUN = mean)
train.sd <- apply(data.train.balanced[, cols_to_scale], MARGIN = 2, FUN = sd)

data.train.balanced[, cols_to_scale] <- scale(data.train.balanced[, cols_to_scale], center = train.mean, scale = train.sd)
# Scale validation data using training data's parameters
data.val[, cols_to_scale] <- scale(data.val[, cols_to_scale], center = train.mean, scale = train.sd)

# Baseline Logistic Regression Model
logistic.baseline <- glm(Churn ~ ., data = data.train.balanced, family = "binomial")
summary(logistic.baseline)
baseline.pred <- ifelse(predict(logistic.baseline, newdata = data.val) > 0.5, 1, 0)
(baseline.cm <- table(baseline.pred, data.val$Churn))

get.metrics<- function(conf.mat) {
  true.positives <- conf.mat[2,2]
  true.negatives <- conf.mat[1,1]
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

(baseline.metrics <- get.metrics(baseline.cm))

# Stepwise Model Selection
# Akaike Information Criterion
akaike.fw <- step(glm(Churn ~ 1, family = "binomial", data = data.train.balanced), scope = formula(logistic.baseline), direction = "forward")
akaike.back <- step(logistic.baseline, direction = "backward")
akaike.both <- step(logistic.baseline, direction = "both")

# Bayesian Information criterion
bayesian.fw <- step(glm(Churn ~ 1, family = "binomial", data = data.train.balanced), scope = formula(logistic.baseline), direction = "forward", k = log(nrow(data.train.balanced)))
bayesian.back <- step(logistic.baseline, direction = "backward", k = log(nrow(data.train.balanced)))
bayesian.both <- step(logistic.baseline, direction = "both", k = log(nrow(data.train.balanced)))

# Considering bidirectional elimination of both methods, BIC is more strict as it removes all the covariates representing the regions 
# while AIC keeps them all.

# Validation set results
akaike.preds <- ifelse(predict(akaike.both, data.val) > 0.5, 1, 0)
(akaike.cm <- table(akaike.preds, data.val$Churn))
(akaike.metrics <- get.metrics(akaike.cm))

bayesian.preds <- ifelse(predict(bayesian.both, data.val) > 0.5, 1, 0)
(bayesian.cm <- table(bayesian.preds, data.val$Churn))
(bayesian.metrics <- get.metrics(bayesian.cm))

# Slightly better results with akaike, maybe dropping all the region features as BIC does is too big of a loss of informations

# ROC curves

par(mfrow = c(2, 2))

roc_full <- roc(data.val$Churn, as.numeric(baseline.pred), 
                plot = TRUE, main = "ROC Curve Full Model", col = "purple", lwd = 3, 
                auc.polygon = TRUE, print.auc = TRUE)

roc_akaike <- roc(data.val$Churn, predict(akaike.both, newdata = data.val, type = "response"), 
                  plot = TRUE, main = "AIC Model", col = "blue", lwd = 3, 
                  auc.polygon = TRUE, print.auc = TRUE)

roc_bayesian <- roc(data.val$Churn, predict(bayesian.both, newdata = data.val, type = "response"), 
                    plot = TRUE, main = "BIC Model", col = "red", lwd = 3, 
                    auc.polygon = TRUE, print.auc = TRUE)

par(mfrow = c(1, 1))

# AUC values
auc_akaike <- as.numeric(auc(roc(data.val$Churn, predict(akaike.both, newdata = data.val, type = "response"))))
auc_bayesian <- as.numeric(auc(roc(data.val$Churn, predict(bayesian.both, newdata = data.val, type = "response"))))
auc_full <- as.numeric(auc(roc(data.val$Churn, baseline.pred)))



# comparison (for now) between full, aic, bic 
akaike_df <- data.frame(Model = "Akaike",
                        Accuracy = akaike.metrics$Accuracy,
                        Precision = akaike.metrics$Precision,
                        Recall = akaike.metrics$Recall,
                        F1_Score = akaike.metrics$F1,
                        AUC = auc_akaike)

bayesian_df <- data.frame(Model = "Bayesian",
                          Accuracy = bayesian.metrics$Accuracy,
                          Precision = bayesian.metrics$Precision,
                          Recall = bayesian.metrics$Recall,
                          F1_Score = bayesian.metrics$F1,
                          AUC = auc_bayesian)

full_df <- data.frame(Model = "Full Logistic",
                      Accuracy = baseline.metrics$Accuracy,
                      Precision = baseline.metrics$Precision,
                      Recall = baseline.metrics$Recall,
                      F1_Score = baseline.metrics$F1,
                      AUC = auc_full)

comparison_df <- bind_rows(akaike_df, bayesian_df, full_df)
comparison_df

# LASSO
set.seed(1)
ctrl <- trainControl(method = "cv", number = 10)
lasso <- train(Churn ~ ., data = data.train.balanced, method = "glmnet", metric = "Accuracy", trControl = ctrl, tuneGrid = expand.grid(alpha = 1, lambda = seq(0, 0.15, length = 30)))
max(lasso$results$Accuracy)
lasso$bestTune

lasso.predict <- predict(lasso, data.val)
lasso.cm <- table(lasso.predict, data.val$Churn)
lasso.metrics <- get.metrics(lasso.cm)

lasso.plot <- lasso %>% 
  ggplot(aes(x = lambda, y = Accuracy)) + 
  geom_line() + 
  geom_point() +
  geom_text(aes(label = sprintf("%.3f", Accuracy)), check_overlap = TRUE, vjust = -0.5, size = 2.5) + 
  scale_x_continuous(limits = c(0, 0.10)) + 
  labs(x = TeX("Lambda ($\\lambda$)"), y = "Accuracy", title = "Accuracy vs. Lambda for Lasso Regularization") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

#RIDGE
set.seed(1)
ridge <- train(Churn ~ ., data = data.train.balanced, method = "glmnet", metric = "Accuracy", trControl = ctrl, tuneGrid = expand.grid(alpha = 0, lambda = seq(0, 0.15, length = 30)))
max(ridge$results$Accuracy)
ridge$bestTune

ridge.plot <- ridge %>% 
  ggplot(aes(x = lambda, y = Accuracy)) + 
  geom_line() + 
  geom_point() +
  geom_text(aes(label = sprintf("%.3f", Accuracy)), check_overlap = TRUE, vjust = -0.5, size = 2.5) + 
  labs(x = TeX("Lambda ($\\lambda$)"), y = "Accuracy", title = "Accuracy vs. Lambda for Ridge Regularization") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ridge.predict <- predict(ridge, data.val)
ridge.cm <- table(ridge.predict, data.val$Churn)
ridge.metrics <- get.metrics(ridge.cm)

par(mfrow = c(1,2))

auc.lasso <- roc(data.val$Churn, as.numeric(lasso.predict), 
                 plot = TRUE, main = "ROC Curve Lasso Model", col = "purple", lwd = 3, 
                 auc.polygon = TRUE, print.auc = TRUE)

auc.ridge <- roc(data.val$Churn, as.numeric(ridge.predict), 
                 plot = TRUE, main = "ROC Curve Ridge Model", col = "black", lwd = 3, 
                 auc.polygon = TRUE, print.auc = TRUE)

grid.arrange(lasso.plot, ridge.plot, top = "Penalized Approaches for Logistic Regression")

lasso_df <- data.frame(Model = "Logistic Lasso",
                       Accuracy = lasso.metrics$Accuracy,
                       Precision = lasso.metrics$Precision,
                       Recall = lasso.metrics$Recall,
                       F1_Score = lasso.metrics$F1,
                       AUC = as.numeric(auc.lasso$auc))

ridge_df <- data.frame(Model = "Logistic Ridge",
                       Accuracy = ridge.metrics$Accuracy,
                       Precision = ridge.metrics$Precision,
                       Recall = ridge.metrics$Recall,
                       F1_Score = ridge.metrics$F1,
                       AUC = as.numeric(auc.ridge$auc))

comparison_df <- rbind(comparison_df, ridge_df, lasso_df)
comparison_df

# The difference between the two in terms of accuracy is negligible. Ridge attains a higher lambda value in its best accuracy score.

# Decision Trees
# Split again to undo the scaling and make trees more interpretable for us
set.seed(1)
ids.train <- sample(1:nrow(data), size = 0.75 * nrow(data), replace = FALSE)
data.train <- data[ids.train,]
data.val <- data[-ids.train,] 

# Oversampling minority class
data.train.balanced <- ovun.sample(Churn ~ ., data = data.train, method = "over", N=4260)$data
table(data.train.balanced$Churn)

set.seed(1) # otherwise results might be inconsistent due to ties
tree.full <- tree(Churn ~ ., data = data.train.balanced)
par(mar=c(5,5,2,2))  # Adjust margin sizes to avoid the "Error in plot.new() : figure margins too large"

plot(tree.full)
text(tree.full, pretty = 0, cex = 0.7)

summary(tree.full)
tree.pred <- predict(tree.full, data.val, type = "class")
table(tree.pred, data.val$Churn)
get.metrics(table(tree.pred, data.val$Churn))

# Pruning Decision Trees with Cross Validation
cv.trees <- cv.tree(tree.full, FUN = prune.misclass)
(optimal.size <- cv.trees$size[which.min(cv.trees$dev)])
par(mfrow = c(1,2))
plot(cv.trees$size, cv.trees$dev/nrow(data.val), type = "b", xlab = "Size", ylab = "Error")
plot(cv.trees$k, cv.trees$dev/nrow(data.val), type = "b", col = 3, xlab = "K", ylab = "Error")

pruned.tree <- prune.misclass(tree.full, k = 0, best = optimal.size)
pruned.tree
plot(pruned.tree)
text(pruned.tree, pretty = 0, cex = 0.7)

pruned.pred <- predict(pruned.tree, data.val, type = "class")
table(pruned.pred, data.val$Churn)
trees.metrics <- get.metrics(table(pruned.pred, data.val$Churn))
trees.auc <- roc(data.val$Churn, as.numeric(pruned.pred))

cv_trees_df <- data.frame(Model = "CV Decision Trees",
                          Accuracy = trees.metrics$Accuracy,
                          Precision = trees.metrics$Precision,
                          Recall = trees.metrics$Recall,
                          F1_Score = trees.metrics$F1,
                          AUC = as.numeric(trees.auc$auc))

comparison_df <- rbind(comparison_df, cv_trees_df)
comparison_df

# Random Forests
set.seed(1)
fit.bag <- randomForest(Churn ~ ., data = data.train.balanced, mtry = ncol(data.train.balanced) - 1, ntree = 500, importance = T) 
importance(fit.bag)

layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) # No margin right side
plot(fit.bag, log="y")
par(mar=c(5,0,4,2)) # No margin left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("bottom", colnames(fit.bag$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(fit.bag)
mean(fit.bag$err.rate[,1]) # OOB test error estimation

validation.preds <- predict(fit.bag, newdata = data.val)
(bag.missclassification <- mean(validation.preds != data.val$Churn)) # True test error estimation
# If we used more trees the OOB test error estimation would have been closer to the true test error
table(validation.preds, data.val$Churn)
rf.metrics <- get.metrics(table(validation.preds, data.val$Churn))
rf.auc <- roc(data.val$Churn, as.numeric(validation.preds))

rf_df <- data.frame(Model = "Random Forests",
                    Accuracy = rf.metrics$Accuracy,
                    Precision = rf.metrics$Precision,
                    Recall = rf.metrics$Recall,
                    F1_Score = rf.metrics$F1,
                    AUC = as.numeric(rf.auc$auc))

comparison_df <- rbind(comparison_df, rf_df)
comparison_df

# XGBOOST
X.tr <- model.matrix(Churn ~ ., data = data.train.balanced)[, -1]
y.tr <- as.numeric(data.train.balanced$Churn)-1
X.val <- model.matrix(Churn ~ ., data = data.val)[, -1]
y.val <- as.numeric(data.val$Churn)-1

fit.xg <- xgboost(as.matrix(X.tr), label = y.tr, nrounds = 50, objective = "binary:logistic", eval_metric = "error")
xg.pred <- ifelse(predict(fit.xg, X.val)> 0.5, 1, 0)
mean(xg.pred != y.val)

table(xg.pred, data.val$Churn)
get.metrics(table(xg.pred, data.val$Churn))

train.errors <- fit.xg$evaluation_log$train_error
val.errors <- numeric(50)

for (i in 1:50) {
  pred_i = ifelse(predict(fit.xg, X.val, ntreelimit = i) > 0.5, 1, 0)
  val.errors[i] = mean(pred_i != y.val)
} 

plot(1:50, val.errors, type="b", xlab="number of trees", ylab="error", col=3, ylim = c(0,0.3))
points(1:50, train.errors, type="b")
legend("topright", legend = c("Train", "Test"), col=c(1, 3), lty=1, lwd=2, cex = 1)
which.min(val.errors)
abline(v = which.min(val.errors))

(feature.importance <- xgb.importance(model = fit.xg))
xgb.plot.importance(importance_matrix = feature.importance)

fitControl = trainControl(
  method = "cv",
  number = 10, 
  search="random")


tune_grid = 
  expand.grid(
    nrounds = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    eta = 0.3,
    max_depth=5,
    subsample = 1,
    colsample_bytree = 1,
    min_child_weight = 5,
    gamma = c(0.1, 0.2, 0.5, 0.75, 1)
  )

set.seed(1)
fit_xg_cv = train(Churn ~ ., data = data.train.balanced, 
                  method = "xgbTree", 
                  trControl = fitControl,
                  verbose = FALSE, 
                  tuneGrid = tune_grid,
                  objective = "binary:logistic", 
                  eval_metric = "error")

pred_xg_cv = predict(fit_xg_cv, data.val, type="raw")
mean(pred_xg_cv != data.val$Churn)
table(pred_xg_cv, data.val$Churn)
xgb.metrics <- get.metrics(table(pred_xg_cv, data.val$Churn))

auc.xgb <- roc(data.val$Churn, as.numeric(pred_xg_cv))

xgb_df <- data.frame(Model = "CV XGB",
                     Accuracy = xgb.metrics$Accuracy,
                     Precision = xgb.metrics$Precision,
                     Recall = xgb.metrics$Recall,
                     F1_Score = xgb.metrics$F1,
                     AUC = as.numeric(auc.xgb$auc))

comparison_df <- rbind(comparison_df, xgb_df)
comparison_df

# CLUSTERING
## third approach
### Kmeans clustering
#### usage behavior: account length, total charge (i wont include total minutes since there's perfect correlation with total charge), total calls

clustering_data <- read.csv("TelecomChurn.csv")
clustering_data <- clustering_data[, !(names(clustering_data) %in% c("Churn", "Area.code", "International.plan", "Voice.mail.plan", "Customer.service.calls", "State", "Account.length"))]
head(clustering_data)
# create a variable total_minute
clustering_data$Total.minutes <- clustering_data$Total.day.minutes + clustering_data$Total.eve.minutes + clustering_data$Total.night.minutes + clustering_data$Total.intl.minutes
clustering_data <- clustering_data[, !(names(clustering_data) %in% c("Total.day.minutes", "Total.eve.minutes", "Total.night.minutes", "Total.intl.minutes"))]
# total_charge
clustering_data$Total.charge <- clustering_data$Total.day.charge + clustering_data$Total.eve.charge + clustering_data$Total.night.charge + clustering_data$Total.intl.charge
clustering_data <- clustering_data[, !(names(clustering_data) %in% c("Total.day.charge", "Total.eve.charge", "Total.night.charge", "Total.intl.charge"))]
# total call
clustering_data$Total.calls <- clustering_data$Total.day.calls + clustering_data$Total.eve.calls + clustering_data$Total.night.calls + clustering_data$Total.intl.calls
clustering_data <- clustering_data[, !(names(clustering_data) %in% c("Total.day.calls", "Total.eve.calls", "Total.night.calls", "Total.intl.calls"))]

numerical_vars <- sapply(clustering_data, is.numeric)
clustering_data[numerical_vars] <- scale(clustering_data[numerical_vars])
head(clustering_data)

# elbow rule plot
fviz_nbclust(clustering_data, kmeans, method = "wss") +
  labs(subtitle = "WSS - Elbow method")
# avg silhouette plot
fviz_nbclust(clustering_data, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

#### assuming we decide on 3 clusters since it seams the best choice 
K <- 3
set.seed(1)  
km_result <- kmeans(clustering_data, centers = K, nstart = 25, iter.max = 100)

# create a data frame for plotting with pca
pca <- prcomp(clustering_data)
plot_data <- data.frame(pca$x[, 1:2])
plot_data$cluster <- factor(km_result$cluster)

# plotting
ggplot(plot_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.5) +
  labs(title = "Cluster Visualization with PCA",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

# silhouette results
silhouette <- silhouette(km_result$cluster, dist(clustering_data))
mean(silhouette[, 3])
# plot silhouette
fviz_silhouette(silhouette) +
  theme_minimal()

fviz_cluster(km_result, data = clustering_data, geom = "point", stand = FALSE, ellipse.type = "convex") +
  labs(title = "K-means Clustering") +
  theme_minimal()


# HIERARCHICAL CLUSTERING --> we use the correlation based method 
## distance matrix
dist_matrix <- dist(clustering_data)
## fitting the model, trying different methods
set.seed(1)
h_complete <- hclust(dist_matrix, method = "complete")
h_single <- hclust(dist_matrix, method = "single")
h_average <- hclust(dist_matrix, method = "average")
h_centroid <- hclust(dist_matrix, method = "centroid")
h_ward <- hclust(dist_matrix, method = "ward.D2")

## plotting corresponding dendrograms
plot(hclust(dist_matrix, method = "complete"), main = "Complete Linkage")
plot(hclust(dist_matrix, method = "single"), main = "Single Linkage")
plot(hclust(dist_matrix, method = "average"), main = "Average Linkage")
plot(hclust(dist_matrix, method = "centroid"), main = "Centroid Linkage")
plot(hclust(dist_matrix, method = "ward.D2"), main = "Ward's Method")

## silhouettes 
sil_width <- function(method) {
  cluster <- hclust(dist_matrix, method = method)
  silhouette_stats <- silhouette(cutree(cluster, k = 3), dist_matrix)
  mean(silhouette_stats[, "sil_width"])
}


methods <- c("complete", "single", "average", "centroid", "ward.D2")
sapply(methods, sil_width)
best_method <- "ward.D2"

hc_best <- hclust(dist_matrix, method = best_method)
clusters <- cutree(hc_best, k = 3)
pca <- prcomp(clustering_data, scale. = TRUE)  # Ensure data is scaled
plot_data <- data.frame(pca$x[, 1:2])  # Using the first two principal components
plot_data$cluster <- factor(clusters)
ggplot(plot_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.5) +  # Points with some transparency
  labs(title = "Cluster Visualization with PCA",
       subtitle = paste("Hierarchical Clustering with", best_method, "Method"),
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") 
