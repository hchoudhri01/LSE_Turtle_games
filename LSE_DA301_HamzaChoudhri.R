## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data



# Install and import Tidyverse.

library(tidyverse)


# Import the data set.

sales = read.csv("turtle_sales.csv") %>% 
  as_tibble()


# Print the data frame.

sales


# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 

sales2 = sales %>% 
  dplyr::select(-c(Ranking, Year, Genre, Publisher))


# View the data frame.

sales2

# View the descriptive statistics.

summary(sales2)


################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.

sales2 %>% 
  ggplot(aes(NA_Sales, EU_Sales)) +
  geom_point()

sales2 %>% 
  ggplot(aes(NA_Sales, Global_Sales)) +
  geom_point()

## 2b) Histograms
# Create histograms.

sales2 %>% 
  ggplot(aes(x = EU_Sales)) +
  geom_histogram()

sales2 %>% 
  ggplot(aes(x = NA_Sales)) +
  geom_histogram()

sales2 %>% 
  ggplot(aes(x = Global_Sales)) +
  geom_histogram()


## 2c) Boxplots
# Create boxplots.

sales2 %>% 
  ggplot(aes(x = EU_Sales, y = Platform)) +
  geom_boxplot()


###############################################################################

# 3. Observations and insights

## Your observations and insights here ......

#The distribution for EU sales is skewed to the left

#The distribution for NA sales is skewed to the left

#The distribution for Global sales is skewed to the left

#We have outliers as seen in the box plot

#Global sales are linearly related with NA sales as seen in the scatter plot

#The relationship between EU sales and NA sales is not perfectly linear



###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.

sales2


# Check output: Determine the min, max, and mean values.

min(sales2$NA_Sales)
min(sales2$EU_Sales)
min(sales2$Global_Sales)

max(sales2$EU_Sales)
max(sales2$NA_Sales)
max(sales2$Global_Sales)

mean(sales2$EU_Sales)
mean(sales2$NA_Sales)
mean(sales2$Global_Sales)


# View the descriptive statistics.

summary(sales2)


###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.

sum1 = sales2 %>% 
  group_by(Product) %>% 
  mutate(sum = sum(EU_Sales, NA_Sales, Global_Sales))

# View the data frame.

sum1
# Explore the data frame.

summary(sum1)



## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.


# Create histograms.

sum1 %>% 
  ggplot(aes(sum)) +
  geom_histogram() +
  ggtitle("Distribution of sales across products")


# Create boxplots.


###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.

par(mar=c(2,2,2,2))
qqnorm(sum1$sum, pch = 1, frame = FALSE)

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.


install.packages("moments")
library(moments)

# Perform Shapiro-Wilk test.

shapiro.test(sum1$sum)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

skewness(sum1$sum)

kurtosis(sum1$sum)



## 3d) Determine correlation
# Determine correlation.

sum1 %>% 
  dplyr::select(where(is.numeric)) %>% 
  cor(.)



###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.


sum1 %>% 
  ggplot(aes(x = sum, y = Platform, fill = Platform)) +
  geom_violin() +
  theme(legend.position = "none")

###############################################################################

# 5. Observations and insights
# Your observations and insights here...

#Total sales differs across platforms and products





###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.

sum1

# Determine a summary of the data frame.

summary(sum1)


###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.

sum1 %>% 
  dplyr::select(where(is.numeric)) %>% 
  cor(.) %>% 
  round(.,3)

model1 = lm(EU_Sales ~ NA_Sales, data = sum1)

model1 %>% 
  broom::tidy() %>% 
  knitr::kable()

## 2b) Create a plot (simple linear regression)
# Basic visualisation.

sum1 %>% 
  ggplot(aes(NA_Sales, EU_Sales)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Relationship between EU sales and NA sales")


###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.


# Multiple linear regression model.

num_df = sum1 %>% 
  dplyr::select(where(is.numeric))

model2 = lm(Global_Sales ~ ., data = num_df)

model2 %>% 
  broom::tidy() %>% 
  knitr::kable()


###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

ypred = predict(model2, newdata = num_df)

ypred



###############################################################################

# 5. Observations and insights
# Your observations and insights here...

#Global sales can be predicted using the numeric features in the given data set



###############################################################################
###############################################################################




