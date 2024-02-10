# Install necessary packages

packages_to_install <- c("ggplot2", "gridExtra", "tidyverse", "knitr")

if (any(!sapply(packages_to_install, requireNamespace, quietly = TRUE))) {
  install.packages(packages_to_install, dependencies = TRUE)
}


library(tidyverse)
library(ggplot2)
library(gridExtra)
library(knitr)

#LOADING AND PREPROCESSING DATA


# Load the data
data <- read.csv("C:/Users/gsaola/Downloads/melanoma.csv")

# Save data in to melanoma_data variable as table
melanoma_data <- as_tibble(data)

# Visualizing the first 5 rows of the data
head(melanoma_data)


#DATA ENGINEERING

# Converting categorical variables to factors
melanoma_data[, c("status", "sex", "ulcer", 
            "year")] <- lapply(melanoma_data[,c("status", 
              "sex", "ulcer", "year")], factor)


# Re-coding factors with respective labels
melanoma_data <- melanoma_data %>%
  mutate(sex = recode_factor(sex, `0` = "female", `1` = "male"),
  status = recode_factor(status, `1` = "died", `2` = "alive", `3` = "other"),
  ulcer = recode_factor(ulcer, `0` = "absent", `1` = "present"))

# Visualizing a single column in the data
melanoma_data[['status']]

# Converting time to years and storing in time_years variable
melanoma_data['time_years'] <- round((melanoma_data$time/365), 1)

# Define age groups
age_breaks <- c(0, 10, 20, 30, 40, 50, 60, 100)
age_labels <- c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61+")

# Define time_years group
time_breaks <- c(0, 5, 10, 15, 16)
time_labels <- c("0-5", "6-10", "11-15", "15+")


# Create a new variable age_group
melanoma_data$age_group <- cut(melanoma_data$age, breaks = age_breaks, labels = age_labels, include.lowest = TRUE)

# Create a new variable time_group
melanoma_data$time_group <- cut(melanoma_data$time_years, breaks = time_breaks, labels = time_labels, include.lowest = TRUE)

# Summarizing entire data statistics after re-coding
summary_stat <- summary(melanoma_data)
kable(summary_stat, format = "html", table.attr = 'class="table"')


# Limitation of the data is that they did not provide information about undelining sicknesses of the patients

# Age scatter plot color coding with time_years
#Areas affected by the cancer



#DATA EXPLORATION/VISUALIZATION

# distribution plot for continuous variables (Time and Thickness and age)
continuous_variables <- c("time", "thickness", "age")

# Set up the multiple layout
par(mfrow = c(2, 2), mar = c(2, 2, 1, 1))

# Loop through each continuous_variables to plot histogram
for (variable in continuous_variables) {
  # Create a new plot for each variable
  hist_data <- hist(melanoma_data[[variable]], plot = FALSE)
  
  # Plot the histogram
  hist(melanoma_data[[variable]],
       col = "lightblue",
       main = variable,
       xlab = "Variable",
       ylab = "Frequency",
       prob = TRUE,
       xlim = range(melanoma_data[[variable]]))  # Set x-axis limit
  
  # Overlaying density curve
  lines(density(melanoma_data[[variable]]), col = "red", lwd = 2)
}


# Visualizing categorical Variables

sex_plot <- ggplot(melanoma_data, aes(x = sex, fill = sex)) +
  geom_bar() +
  labs(title = "Count of gender",
       x = "Sex",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "top") +  # Adjust the legend position
  geom_text(
    aes(label = scales::percent((..count..)/sum(..count..))),
    stat = "count",
    vjust = 0.5
  )


age_group_plot <- ggplot(melanoma_data, aes(x = age_group, fill = sex)) +
  geom_bar() +
  labs(title = "Count of age group by gender",
       x = "age_group",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "top") +  # Adjust the legend position
  geom_text(
    aes(label = scales::percent((..count..)/sum(..count..))),
    stat = "count",
    vjust = 1.0
  )


time_group_plot <- ggplot(melanoma_data, aes(x = time_group, fill = sex, colors=sex)) +
  geom_bar() +
  labs(title = "Longevity by gender after cancer removal",
       x = "time_group",
       y = "Count") +
  theme_minimal() +
  geom_text(
    aes(label = scales::percent((..count..)/sum(..count..))),
    stat = "count",
    vjust = 1.0
  )


ulcer_plot <- ggplot(melanoma_data, aes(x = ulcer, fill = sex)) +
  geom_bar() +
  labs(title = "Bar Graph for Melanoma Sex",
       x = "time_group",
       y = "Count") +
  theme_minimal() +
  #theme(legend.position = "top") +  # Adjust the legend position
  geom_text(
    aes(label = scales::percent((..count..)/sum(..count..))),
    stat = "count",
    vjust = 1.0
  )


year_plot <- ggplot(melanoma_data, aes(x = year, fill = sex)) +
  geom_bar() +
  labs(title = "Bar Graph for Melanoma Sex",
       x = "time_group",
       y = "Count") +
  theme_minimal() +
  geom_text(
    aes(label = scales::percent((..count..)/sum(..count..))),
    stat = "count",
    vjust = 1.5,
    size=3
  )


ggplot(melanoma_data, aes(x = ulcer, y = time, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Time of Survival based on Ulcer and Gender",
       x = "ulcer", y = "time") +
  scale_fill_manual(values = c("male" = "lightblue", "female" = "#E2D3F4")) +
  theme_minimal()+


sex_plot
age_group_plot
time_group_plot
ulcer_plot
year_plot


# Visualizing distribution of age by status
ggplot(melanoma_data, aes(age)) +
  geom_histogram(aes(fill = status, color = status), bins = 20, 
                 position = "identity", alpha = 0.5) +
  scale_fill_viridis_d() +
  scale_color_viridis_d()

# QQPLOT FOR TIME, AGE AND THICKNESS VARIABLES BY GENDER

ggplot(data = melanoma_data, aes(sample = time)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(. ~ sex) +
  labs(title = "Q-Q Plot for Time by Sex")

ggplot(data = melanoma_data, aes(sample = age)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(. ~ sex) +
  labs(title = "Q-Q Plot for Age by Sex")

ggplot(data = melanoma_data, aes(sample = thickness)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(. ~ sex) +
  labs(title = "Q-Q Plot for Thickness by Sex")




## REGRESSION AND STATISTICAL MODELS


# Regression analysis for time and thickness

# Regression analysis
reg_model_thickness <- lm(time ~ thickness, data=melanoma_data)
reg_model_age <- lm(time ~ age, data=melanoma_data)
reg_model_thickness_age <- lm(thickness ~ age, data=melanoma_data)

# Display regression coefficients
print(summary(reg_model_thickness))
print(summary(reg_model_age))
print(summary(reg_model_thickness_age))

# Correlation computations
cor_thickness_time <- cor(melanoma_data$thickness, melanoma_data$time)
cor_age_time <- cor(melanoma_data$age, melanoma_data$time)
cor_age_thickness <- cor(melanoma_data$age, melanoma_data$thickness)

# Display regression coefficients
print(cor_thickness_time)
print(cor_age_time)
print(cor_age_thickness)


# testing for significance of time by gender
# Two-sample t-test for time by gender
t_test_time_gender <- t.test(time ~ sex, data = melanoma_data)

# Two-sample t-test for thickness by gender
t_test_thickness_gender <- t.test(thickness ~ sex, data = melanoma_data)


print(t_test_thickness_gender)
print(t_test_time_gender)
