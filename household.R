# -*- coding: utf-8 -*-
"""household.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1SkC2wRJZ0PO2IcBVIbLeZ7ocRBmEZJc0

I assumed that this is a proposal that leverages a modified data collected during a household census conducted in England in 2021. This dataset offers a wealth of insights into the demographic and socio-economic landscape, enabling strategic decision-making for businesses looking to tailor their products and services to the needs of specific consumer segments with focus on personalized home goods and lifestyle subscription services targeting young, urban professionals.
"""

install.packages("ggplot2")
install.packages("files")

library(tidyverse)

data <- read.csv("data-1.csv")
head(data,)

# sum(data$Age == min(data$Age))
head(data[data$Age == 0, ] )

# data
colSums(is.na(data))

clean_data <- data[complete.cases(data), ]

colSums(is.na(clean_data))
# head(clean_data)

summary(data)

age_breaks <- c(-Inf, 10, 20, 30, 40, 50, 60, Inf)
age_labels <- c("<10", "10-19", "20-29", "30-39", "40-49", "50-59", "60+")

# Categorize age into groups
clean_data$Age_Group <- cut(clean_data$Age, breaks = age_breaks, labels = age_labels, right = FALSE)

new_column_names <- c("Marital_Status", "Income", "Gender", "all_rooms_behind_door", "Ethnicity", "Highest_education")
colnames(clean_data)[c(4, 5, 6, 7, 8,9)] <- new_column_names

clean_data$Gender <- ifelse(clean_data$Gender == 0, "Female", "Male")
clean_data$all_rooms_behind_door <- ifelse(clean_data$all_rooms_behind_door == 0, "Yes", "No")
head(clean_data)

install.packages("patchwork")

library(dplyr)

"""This information can guide product curation and marketing strategies tailored to specific age groups."""

# 1. Demographic Overview - Population Pyramid
ggplot(clean_data, aes(x = Age_Group, fill = Gender)) +
  geom_bar(stat = "count", position = "dodge") +
  coord_flip() +
  labs(title = "Demographic Overview - Age Distribution",
       x = "Count",
       y = "Age Group")

"""The pie chart below displays the distribution of individuals across different marital statuses, offering a quick overview of family structures. This can be used to promote family-oriented products for married individuals and lifestyle-focused products for singles."""

# 2. Marital Status Composition - Pie Chart
ggplot(clean_data, aes(x = "", fill = Marital_Status)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  labs(title = "Marital Status Composition",
       fill = "Marital Status") +
  theme_void()

"""the income distribuion below can be used to develop pricing strategies and products that cater to different life stages."""

# 3. Income Distribution - Histogram
ggplot(clean_data, aes(x = Income)) +
  geom_histogram(binwidth = 5000, fill = "skyblue", color = "black") +
  labs(title = "Income Distribution",
       x = "Annual Income (in pounds)",
       y = "Count")

"""Since more people loves pricacy as indicated below, companies can emphasize privacy features in its offerings, aligning with the preferences of households that prioritize exclusive and private living spaces."""

# 5. Privacy Preferences - Stacked Bar Chart
ggplot(clean_data, aes(x = factor(all_rooms_behind_door), fill = factor(all_rooms_behind_door))) +
  geom_bar() +
  labs(title = "Privacy Preferences",
       x = "Response to Question H8",
       y = "Count")

"""**Observation**

  We have a fair distribution of gender
"""

install.packages('cowplot')

library(ggplot2)
library(dplyr)
library(patchwork)

options(repr.plot.width = 20, repr.plot.height = 6)

plot4 <- clean_data %>%
  count(Ethnicity) %>%
  ggplot(aes(x = Ethnicity, y = n)) +
  geom_bar(stat = "identity") +
  ggtitle("Ethnicity Distribution") +
  theme(axis.text.x = element_text(size = 15))

plot5 <- clean_data %>%
  count(Highest_education) %>%
  ggplot(aes(x = Highest_education, y = n)) +
  geom_bar(stat = "identity") +
  ggtitle("Education Distribution") +
  theme(axis.text.x = element_text(size = 15))

plot6 <- clean_data %>%
  count(Age_Group) %>%
  ggplot(aes(x = Age_Group, y = n)) +
  geom_bar(stat = "identity") +
  ggtitle("Age Group Distribution") +
  theme(axis.text.x = element_text(size = 15))

# Combine plots using patchwork
combined_plots <- plot4 + plot5 + plot6

# Display the combined plots
combined_plots

"""Any educational class both for males and females can be targeted. Both genders are closely educated in each academic classes."""

ggplot(clean_data, aes(x = Highest_education, fill = Gender)) +
  geom_bar(position = "dodge", show.legend = TRUE) +
  labs(title = "Distribution of Education Levels by Gender")

"""The whites are more educated therefore, companies should make products available for this set of people."""

ggplot(clean_data, aes(x = Highest_education, fill = Ethnicity)) +
  geom_bar(position = "dodge", color = "black", show.legend = TRUE) +
  labs(title = "Distribution of Education Levels by Ethnicity")

"""The married individuals will be able to afford the premium servicces as they are more at the upper levels of income"""

ggplot(clean_data, aes(x = Age, y = Income, color = Marital_Status)) +
  geom_point(size = 3) +  # Adjust the size as needed
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")  # Adjust the margins as needed
  ) +
  labs(title = "Scatter Plot of Age vs Income by Marital Status")

"""From the age group, there are more datapoints indicating that many of the samples studied are older people, 60 years and above"""

ggplot(clean_data, aes(x = Age, y = Income, color = Age_Group)) +
  geom_point(size = 3) +  # Adjust the size as needed
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")  # Adjust the margins as needed
  ) +
  labs(title = "Scatter Plot of Age vs Income by Marital Status")

library(ggplot2)
ggplot(data, aes(x = Age, y = INC, size = Age, color = Mar_Stat)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(3, 10)) +
  labs(title = "Age, Income, and Marital Status",
       x = "Age",
       y = "Annual Income (in pounds)")