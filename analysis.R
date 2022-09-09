library(ggplot2)
library(dplyr)
library(tidyr)

# Data set
life_expectancy <- read.csv("data/WHOLifeExpectancy.csv", stringsAsFactors = FALSE)

# Checking correlation between life expectancy and other factors to decide on removing the least correlated factors
removing_na <- life_expectancy %>%
  drop_na() %>%
  select(-Year, -Country, -Status)
test_correlation <- as.data.frame(cor(removing_na))
test_correlation <- test_correlation %>%
  select(Life.expectancy)

# Removing some less critical factors with the highest number of empty values and least correlation with life expectancy.
life_expectancy <- life_expectancy %>%
  select(-thinness..1.19.years, -thinness.5.9.years, -Measles, -Population, -under.five.deaths, -percentage.expenditure, -Total.expenditure, -Hepatitis.B, -Polio, -Diphtheria, -GDP)

# Renaming columns
life_expectancy <- life_expectancy %>%
  rename(Life_Expectancy = Life.expectancy, Adult_Mortality = Adult.Mortality , Infant_Deaths = infant.deaths, HIV_AIDS = HIV.AIDS, Income_Composition = Income.composition.of.resources)


# 1. How many developing and developed countries are present in the dataset?
total_countries <- life_expectancy %>%
  group_by(Status) %>%
  summarise(total_number_of_countries = n())

developed <- total_countries %>%
  filter(Status == "Developed")
developed <- developed[[1,2]]

developing <- total_countries %>%
  filter(Status == "Developing")
developing <- developing[[1,2]]

# Bar graph

total_countries_plot <- 
  ggplot(data = total_countries) +
  geom_col(mapping = aes(x = Status, 
                         y = total_number_of_countries)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Developed vs Developing", 
    x = "Country Status",
    y = "Total Number Of Countries",
  ) +
  theme(plot.title = element_text(hjust = 0.5))
#show(total_countries_plot)

# 2. What is the average life expectancy in developed vs developing countries?
average_life_expectancy <- life_expectancy %>%
  drop_na(Life_Expectancy) %>%
  group_by(Status) %>%
  summarise(mean_life_expectancy = mean(Life_Expectancy))

developed_avg <- average_life_expectancy %>%
  filter(Status == "Developed")
developed_avg <- round(developed_avg[[1,2]], 2)

developing_avg <- average_life_expectancy %>%
  filter(Status == "Developing")
developing_avg <- round(developing_avg[[1,2]], 2)

# Bar graph showing the average life expectancy in developed vs developing countries

average_life_expectancy_plot <- 
  ggplot(data = average_life_expectancy) +
  geom_col(mapping = aes(x = Status, 
                         y = mean_life_expectancy)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Average Life Expectancy Developed vs Developing", 
    x = "Country Status",
    y = "Average Life Expectancy",
  ) +
  theme(plot.title = element_text(hjust = 0.5))
#show(average_life_expectancy_plot)

# 3. Which are the 5 countries with the highest life expectancy?

highest_life_expectancy <- life_expectancy %>%
  arrange(desc(Life_Expectancy)) %>%
  select(Country, Status, Life_Expectancy) %>%
  head(5)


# 4. How is life expectancy correlated to the different factors?

cleaning_na <- life_expectancy %>%
  drop_na() %>%
  select(-Year, -Country, -Status)
factor_correlation <- as.data.frame(cor(cleaning_na))

factor_correlation <- factor_correlation %>%
  select(Life_Expectancy) 

# 5. Which two factors have the highest positive correlation to life expectancy?

highest_correlation <- factor_correlation %>%
  filter(Life_Expectancy != 1) %>%
  arrange(desc(Life_Expectancy)) %>%
  head(2)

income_cor <- highest_correlation %>%
  filter(row.names(highest_correlation) == 'Income_Composition')
income <- round(income_cor[[1]],2)
school_cor <- highest_correlation %>%
  filter(row.names(highest_correlation) == 'Schooling')
school <- round(school_cor[[1]],2)

# Income composition of Resources & Schooling are the highest correlated factors.
# They are positively correlated to life expectancy. This can be evaluated by 
# plotting both factors against life expectancy.

life_expectancy_filtered <- life_expectancy %>%
  drop_na()

# Plotting Life Expectancy vs Income Composition Of Resources 
income_plot <- ggplot(data = life_expectancy_filtered) +
  geom_point(mapping = aes(x = Life_Expectancy, y = Income_Composition, color = Status)) +
  labs(
    title = "Life Expectancy vs Income Composition (2013)", 
    x = "Life Expectancy",
    y = "Income Composition",
  )+
  theme(plot.title = element_text(hjust = 0.5))
#show(income_plot)

# Plotting Life Expectancy vs Schooling

schooling_plot <- ggplot(data = life_expectancy_filtered) +
  geom_point(mapping = aes(x = Life_Expectancy, y = Schooling, color = Status)) +
  labs(
    title = "Life Expectancy vs Schooling (2013)", 
    x = "Life Expectancy",
    y = "Schooling (Years)",
  )+
  theme(plot.title = element_text(hjust = 0.5))
#show(schooling_plot)


# 6. How does Infant and Adult mortality rates affect life expectancy?

# From the correlation matrix
infant_adult <- factor_correlation %>%
  filter(row.names(factor_correlation) == 'Infant_Deaths' |row.names(factor_correlation) == 'Adult_Mortality')

infant_cor <- factor_correlation %>%
  filter(row.names(factor_correlation) == 'Infant_Deaths')
infant <- round(infant_cor[[1]],2)
adult_cor <- factor_correlation %>%
  filter(row.names(factor_correlation) == 'Adult_Mortality')
adult <- round(adult_cor[[1]],2)

# Plotting Life Expectancy vs Infant Deaths 
infant_plot <- ggplot(data = life_expectancy_filtered) +
  geom_point(mapping = aes(x = Life_Expectancy, y = Infant_Deaths, color = Status)) +
  labs(
    title = "Life Expectancy vs Infant Deaths (2013)", 
    x = "Life Expectancy",
    y = "Infant Deaths (per 1000 population)",
  )+
  theme(plot.title = element_text(hjust = 0.5))
#show(infant_plot)

# Plotting Life Expectancy vs Adult Mortality

adult_plot <- ggplot(data = life_expectancy_filtered) +
  geom_point(mapping = aes(x = Life_Expectancy, y = Adult_Mortality, color = Status)) +
  labs(
    title = "Life Expectancy vs Adult Mortality (2013)", 
    x = "Life Expectancy",
    y = "Adult Mortality (per 1000 population)",
  )+
  theme(plot.title = element_text(hjust = 0.5))
#show(adult_plot)
