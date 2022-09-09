###############################################################################
### Assignment 3: Visualization ###
### Visual analysis of World Bank Indicators
###
### Below each prompt in the file, write the code necessary/indicated to generate
### the required plots. See the assignment page on Canvas for details.

###############################################################################
##### PART 1: Loading and Understanding the Data #####
### In this section you will load the data and necessary packages

# For this assignment, you'll be working with the database of World Bank Development 
# Indicators. You can explore also explore all of this data online at
# https://data.worldbank.org/products/wdi
#
# For this assignment you will be using a separate R package called `wbstats` that 
# will let you use basic functions to "access" the indicator database. (Note that 
# you can look at the package documentation at 
# https://cran.r-project.org/web/packages/wbstats/wbstats.pdf to see all of the 
# different functions and options it offers as well as how to use them. This 
# documentation is in the standard R package doc format, so is worth viewing at least 
# once to have a sense for what these look like!


# [1pt] Start by installing and loading (with library()) the `wbstats` package. 
# DO NOT include the install.packages() command in your script file.
library(wbstats)

# Also load in other required packages (`ggplot`, `dplyr`, etc) here.
# You can alternatively just load the whole `tidyverse` package
# Do not include any `install.package()` calls in this file
library(dplyr)
library(ggplot2)
library(tidyverse)

# [2pt] The World Bank organizes data about countries into a different indicators 
# (measures). For example: "Total Population" is an indicator, as is "Individuals 
# using the Internet (% of population)". You can view a complete list of the 
# indicators on the World Bank's website at https://data.worldbank.org/indicator?tab=all
# (you will be using this website repeatedly in this assignment).
#
# Get a listing of the available indicators by calling the package-provided 
# `wb_indicators()` function, which returns a data frame of information about them. 
# Print out the number of rows in this data frame to see how many indicators there 
# are (and this listing is missing a few!) Also inspect the data frame (such as 
# using View()) to see what information is about about each indicator.
#
# IMPORTANT: notice that each indicator contains an "Indicator ID", a special code
# used to refer to that indicator. This is because the names are so long and 
# complex, so the World Bank uses codes to refer to each piece of data. Instead of 
# "Individuals using the Internet...", you'd refer to indicator IT.NET.USER.ZS. 
# In general, you will be using these IDs as identifiers, rather than the full text
# of the indicator's title.
indicators <- wb_indicators()
View(indicators)
dim(indicators)
nrow(indicators)

# [2pt] You can find the codes for different indicator on the World Bank's website.
# You can visit the full list of indicators at https://data.worldbank.org/indicator?tab=all
# and click on each one to get more information on that indicator (including seeing 
# a sample visualization). You can find the indicator iD by clicking the "Details" 
# button, or by looking in the URL (it's part of the path).
# See the `examples/indicators.png` file in this project for an example.
#
# Using this website, find the indicator ID for the "CO2 emissions (kt)" indicator. 
# In a comment below, state the ID for this indicator to show that you looked it up.
#
# (It's also possible to look up indicator codes by using the provided `wb_search()` 
# function, but it can be a bit less reliable than checking the website (and requires 
# regular expressions to use well).

# ID for CO2 emissions (kt) from website : EN.ATM.CO2E.KT
CO2 <- indicators %>% 
  filter(indicator == 'CO2 emissions (kt)')
CO2_indicator_id <- CO2$indicator_id
print(CO2_indicator_id)
row_num <- which(indicators$indicator == 'CO2 emissions (kt)')
print(row_num)

# [3pt] Once you've identified an indicator of interest and its ID, you can use
# use the `wbstats` package to access the data for that indicator. You get data
# from the World Bank by using the `wb_data()` function. This function expects at 
# least two (named) arguments: `country` which should be a character vector of 
# countries to get data on (with a few special options), and `indicator` which 
# should be a character vector of indicator IDs to access. For example, you can 
# get % Internet Users data for all countries with the following:
#
#   wb_data(country = "all", indicator = c("IT.NET.USER.ZS"), mrv = 1, lang = "en")
#
# The `mrv = 1` argument ("most recent value") says to get just a single year's 
# worth of data (the most recent year). It's also possible to give a specific 
# range of years; see the `wbstats` documentation for details.
#
# Using the `wb_data()` function, get a data frame of the "CO2 emissions (kt)" 
# for all countries for the 1 most recent year (use "countries_only" as the `country` 
# argument to just get countries and not aggregations). Save (all) the data for 
# the top 10 countries with highest carbon emissions in a data frame called 
# `top_10_co2_countries`. You will need to do some light data wrangling to choose
# only these 10 rows.
#
# Note that for all data wrangling in this assignment, you can either use `dplyr` 
# functions, base R syntax (dollar signs and brackets), or a mix of both. I 
# strongly recommend you use `dplyr` primarily, but do what seems simplest and 
# makes sense to you.
countries_CO2 <- wb_data(country = "countries_only", indicator = c("EN.ATM.CO2E.KT"), mrv = 1, lang = "en")
names(countries_CO2)
names(countries_CO2)[5] <- "indicator_CO2"
names(countries_CO2)

top_10_co2_countries <- countries_CO2 %>% 
  arrange(desc(indicator_CO2)) %>% 
  head(10)
top_10_co2_countries

###############################################################################
##### PART 2: CO2 Emissions by Country #####
### In this section you will generate a bar chart of the total CO2 emissions of 
### the top-10 countries with the highest emission levels
###
### You can see an example of this plot in `examples/top_10_co2_plot.png`
###
### The instructions below have multiple steps as a single comment; it is up to
### you to organize your code below that.
###
### Throughout this assignment, you are welcome to adjust the styling of the plots
### (e.g., make text different sizes, use different colors, etc), so long as you 
### maintain the *effectiveness* and *expressiveness* of the plots.

# [2pt] Use the `ggplot2()` function to create the plot. The data will be your
# `top_10_co2_countries` from the previous section.
#

# [4pt] You will need to use column geometry (https://ggplot2.tidyverse.org/reference/geom_bar.html)
# to create the chart. The country's ISO3 code (the three-letter code used to 
# refer to that country, such as "USA" or "IND") will go on the x-axis, and the 
# emission amount will go on the y-axis.
# You can use the `reorder()` function to "sort" the country ISO3 codes (a factor, 
# the first argument) by the indicator value column (the second argument), and 
# then use that sorted list as the aesthetic mapping. See 
# https://www.r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html#reorder
# for an example
#
# [2pt] Use the `labs()` function to specify the title and axis labels for your 
# chart. The title should be "Top 10 Countries by CO2 Emissions", the x-axis 
# should be labeled "Country (iso3)", and the y-axis should be labeled with the
# complete indicator name.
# Optionally, you can effectively adjust the formatting of the numbers on the 
# y-axis using the scales package (https://scales.r-lib.org/). This will let you 
# put e.g., commas in the large numbers. Note that this will also involve 
# specifying a scale for youur plot!
# 
# [1pt] Once completed, save your plot in a variable called `top_10_c02_plot.` 
# Note that you can print() out this variable in order to see the plot generated
# when you run your script.

top_10_c02_plot <- 
  ggplot(data = top_10_co2_countries) +
  geom_col(mapping = aes(x = reorder((iso3c), indicator_CO2), 
                         y=indicator_CO2)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Top 10 Countries by CO2 Emissions", 
    x = "Country (iso3)",
    y = "CO2 emissions (kt)",
  )
show(top_10_c02_plot)



###############################################################################
##### PART 3: US Income Equality over Time #####
### In this section you will generate a line chart showing the change in income 
### inequality (in the USA) over time.
###
### You can see an example of this plot in `examples/us_inequality_plot.png`
###
### The instructions below have multiple steps as a single comment; it is up to
### you to organize your code below that.

# You'll first need to access and wrangle the data in order to plot it. Save the
# wrangled data in a data frame called `us_income_years`.
#
# [2pt] Use the `wb_data()` function to access data for the following 3 indicators 
# for the country "USA" (you'll need to look up their IDs): 
# "Income share held by highest 10%", "Income share held by lowest 20%", and 
# "Income share held by second 20%". Get the 20 most recent years worth of data.
#
date <- wb_data(country = "US", indicator = c("SI.DST.10TH.10"), mrv = 20, lang = "en") %>%
  select(date) %>%
  unlist()
highest_10 <- wb_data(country = "US", indicator = c("SI.DST.10TH.10"), mrv = 20, lang = "en") %>%
  select(SI.DST.10TH.10) %>%
  unlist()
lowest_20 <- wb_data(country = "US", indicator = c("SI.DST.FRST.20"), mrv = 20, lang = "en") %>%
  select(SI.DST.FRST.20) %>%
  unlist()
second_20 <- wb_data(country = "US", indicator = c("SI.DST.02ND.20"), mrv = 20, lang = "en") %>%
  select(SI.DST.02ND.20) %>%
  unlist()
us_income_years <- data.frame(date, highest_10, lowest_20, second_20)
View(us_income_years)

# [1pt] You'll need to mutate the data frame and convert the `date` column into a 
# numeric value (using `as.numeric()`) so that you can plot it easier.
#
us_income_years <- us_income_years %>%
  mutate(date = as.numeric(date))
View(us_income_years)

# [2pt] Also mutate the data frame and create columns for the "wealth of the top 10%"
# (e.g., `wealth_top_10`), and for the "wealth of the bottom 40%" (e.g., `wealth_bottom_40`) 
# (which is the lowest and second lowest 20% combined).
#
us_income_years <- us_income_years %>%
  mutate(wealth_top_10 = highest_10, wealth_bottom_40 = lowest_20 + second_20)
View(us_income_years)

# [3pt] You'll need to pivot this data into *long* format, gathering the values 
# from the two columns ("top 10%" and "bottom 40%") into a single column. This 
# will allow you to plot them as two separate lines using a single geometry.
# Optionally, so you can order your legend correctly, you should mutate the long
# data frame to convert the "category" column into a factor (using the `factor()` 
# function), with the `wealth_top_10` as the first level.
#
# In the end, your data frame should have 40 rows; 1 for each year-and-category 
# (top 10% or bottom 40%).

us_income_years <- us_income_years %>%
  select(date, wealth_top_10, wealth_bottom_40) %>%
  pivot_longer(!date, names_to = "category", values_to = "percent")
View(us_income_years)

# You can then create your line plot:
#
# [1pt] The plot will use your us_income_years data frame as a data source.
# 
# [5pt] The plot should include both point geometry and smooth line geometry:
# https://ggplot2.tidyverse.org/reference/geom_point.html
# https://ggplot2.tidyverse.org/reference/geom_path.html
# (so you can see the points and the trend—the smoothed trend looks better).
# Each should have the date mapped to the x-axis, the value mapped to the y-axis,
# and the category (top 10% or 40%) mapped to the color.
#
# [2pt] Specify appropriately detailed title and axis labels for your chart.
# Also use an appropriate *scale function* (for colors that are *discrete*) to 
# customize the labels of the color mapping legend and making them readable 
# (e.g., "Top 10% of Pop." and "Bottom 40% of Pop.")
#
# [1pt] Once completed, save your plot in a variable called `us_wealth_plot`. 
# Note that you can print() out this variable in order to see the plot generated
# when you run your script.
us_wealth_plot <- ggplot(us_income_years, aes(date, percent)) +
  geom_point(mapping = aes(colour = category)) +
  geom_smooth(mapping = aes(colour = category), formula = y ~ x, method = lm) +
  labs(title = "US Wealth (In)Equality Over Time",
       x = "Year", y = "Percentage of income held") +
  scale_color_discrete(name = "Group", labels = c("Top 10% of Pop.", "Bottom 40% of Pop."))
show(us_wealth_plot)




###############################################################################
##### PART 4: Health Expenditures by Country #####
### In this section you will generate a plot showing the amount spent on 
### healthcare across "high-income" countries.
###
### You can see an example of this plot in `examples/health_costs_plot.png`
###
### The instructions below have multiple steps as a single comment; it is up to
### you to organize your code below that.

# You will again need to access and wrangle the data in order to plot it. Note 
# that this wrangling is more complex than the previous plots. Save your fully-
# wrangled data in a data frame called `health_costs` (though there will be some
# steps before you get there!)
#
# [2pt] You'll first need to a list of "high income" countries to get the data on. 
# You can get access to general information about countries by calling the 
# `wb_countries()` function. Filter this data frame for countries that are 
# "High Income", and then extract (pull) a vector of the ISO3 codes for these 
# countries (there should be around 80 of them).
#
country_codes <- wb_countries() %>% 
  filter(income_level == 'High income') %>% 
  pull(iso3c)
print(country_codes)

# [2pt] Use the `wb_data()` function to access data on the following 4 indicators
# for the high-income countries (you will need to look up the indicator IDs):
# - "Current health expenditure per capita (current US$)"
# - "Domestic general government health expenditure per capita (current US$)"
# - "Domestic private health expenditure per capita (current US$)"
# - "Out-of-pocket expenditure per capita (current US$)"
# You should get the 1 most recent year.
#
ind_1 <- indicators %>% 
  filter(indicator == 'Current health expenditure per capita (current US$)')
health_per_capita_id <- ind_1$indicator_id

ind_2 <- indicators %>% 
  filter(indicator == 'Domestic general government health expenditure per capita (current US$)')
govt_health_spend <- ind_2$indicator_id

ind_3 <- indicators %>% 
  filter(indicator == 'Domestic private health expenditure per capita (current US$)')
pvt_health_spend <- ind_3$indicator_id

ind_4 <- indicators %>% 
  filter(indicator == 'Out-of-pocket expenditure per capita (current US$)')
out_of_pck <- ind_4$indicator_id

all_4_indicators <- c(health_per_capita_id,govt_health_spend, pvt_health_spend, out_of_pck)
high_inc_health_spend <- wb_data(country = country_codes, indicator = all_4_indicators, mrv = 1)
high_inc_health_spend <- high_inc_health_spend %>% 
  select(-iso2c)
#Renaming columns
names(high_inc_health_spend)[4] <- 'Total Spending'
names(high_inc_health_spend)[5] <- 'Government Spending'
names(high_inc_health_spend)[7] <- 'Private Spending'
names(high_inc_health_spend)[6] <- 'Out of Pocket Costs'
View(high_inc_health_spend)

# [3pt] You will need to pivot this data into a *longer* format. You want a *names* 
# column (e.g., `indicatorID`) of indicator names, and a *values* column of their values.
# After you pivot, you filter out any countries with `NA` values. The `drop_na()` 
# function works great for this.
#
refined_high_inc_health <- high_inc_health_spend %>% 
  select(-country, -date)

health_costs <- refined_high_inc_health %>% 
  gather(key = spend_type,
         value = amount_spend,
         -iso3c)

#removing NA values
health_costs <- drop_na(health_costs)
nrow(health_costs) 
View(health_costs)

# [2pt] In order to make sure that your chart legend is readable, replace the ID 
# codes with understandable text (e.g., "Total Spending", "Government Spending", 
# "Private Spending", and "Out of Pocket Costs"). Note that I find it easier to 
# do this replacement using base R syntax (bracket notation) than dplyr, as a 
# separate set of 4 statements.
#

# [2pt] Additionally, you'll need a separate data frame (e.g., `total_health_costs`) 
# of just the "Total Spending" data.
total_health_costs <- refined_high_inc_health %>% 
  select(iso3c,`Total Spending`)
total_health_costs <- drop_na(total_health_costs)
nrow(total_health_costs)
names(total_health_costs)[2] <- 'Total_Health_Cost'
View(total_health_costs)

# Once you have your data ready, you can create your plot:
# 
# [1pt] Your plot will use the `health_costs` data frame as the primary data source.
#
# [1pt] Your plot will include multiple geometries that will share aesthetics. 
# Because of this, you will define your "default" aesthetic mapping as an argument
# to the ggplot() function. You should map the country's `iso3c` code to the x-axis
# (`reorder()` it by value, as you did in the first plot); the indicator value to 
# the y-axis; and the indicator name/ID to the color.
#
# [3pt] Your plot's primary geometry will be point geometry. Specify that the 
# `shape` of each point will be based on the indicator.
#
# [4pt] Your plot will also need to include lines from the bottom axis to the 
# total cost point (for readability). You can do this by adding in a `linerange` 
# geometry https://ggplot2.tidyverse.org/reference/geom_linerange.html. This 
# geometry should use the `total_health_costs` as its data source, have a minimum-y
# aesthetic of 0 and a maximum-y aesthetic of the value column (which will come 
# from the `total_health_costs`).
# Add the `linerange` geom *before* the point geom to have the points appear "on top".
#
# [2pt] Use a scale function to give different colors to the points. I used 
# Colorbrewer's "Dark2" palette, but you can choose a different palette (or define
# your own set of colors).
#
# [2pt] Specify appropriately detailed title and axis labels for your chart. 
# Remember to also provide an identical label for the color & shape aesthetics 
# to style the legend.
#
# [2pt] Finally, use the `theme()` function (https://ggplot2.tidyverse.org/reference/theme.html) 
# to specify a "theme" and styling of your plot. In particular, you can set the
# `axis.text.x` to be an `element_text()` value (https://ggplot2.tidyverse.org/reference/element.html) 
# with a smaller size and an angle--this will make the labels not overlap each other.
# You can also specify the `legend.position` in order to place the legend somewhere
# else (like in the otherwise blank space. I used `c(.2,.8)` as a position--the 
# numbers are the "ratio" of how far along the axis to place the plot).
# Search the documentation and other resources for examples of these (common) adjustments.
#
# [1pt] When completed, save your plot in a variable called `health_costs_plot`. 
# Note that you can print() out this variable in order to see the plot generated
# when you run your script.

health_costs_plot <- ggplot()+
  geom_linerange(data = total_health_costs, mapping = aes(x = reorder(iso3c,Total_Health_Cost), 
                                                          ymin = 0, ymax = Total_Health_Cost)) +
  geom_point(data = health_costs, mapping = aes(x = iso3c,
                                              y = amount_spend,
                                              color = spend_type, shape = spend_type)) +
  scale_colour_brewer(palette = "Dark2") +
  labs(
    title = "Health Care Expenditures (per capita)",
    x = "Country",
    y = "Current US $",
    color = "Type", 
    shape = "Type"
  ) +
  theme(axis.text.x = element_text(size =5, angle=90),
        legend.position= c(.2,.8))
show(health_costs_plot)



###############################################################################
##### PART 5: Map: Changes in Forestation around the World #####
### In this section you will generate a choropleth map of the forestry changes 
### for each country country plotted on a global map.
###
### You can see an example of this plot in `examples/forested_map_plot.png`
###
### The instructions below have multiple steps as a single comment; it is up to
### you to organize your code below that.
###
### You may create this map using ggplot2 or using the `leaflet` package; use of 
### other external mapping packages is not allowed. The below instructions cover 
### how to do this using ggplot2.

# As before, you'll first need to wrangle the indicator data you need. Save your 
# fully-wrangled data in a data frame called `forest_area`.
#
# [2pt] Use the `wb_data()` function to access data for the "Forest area (% of land area)" 
# indicator (you'll need to look up its ID). Get data for "countries_only", and 
# data for the most recent 20 years.
#
id <- indicators %>% 
  filter(indicator == 'Forest area (% of land area)')
forest_area_id <- id$indicator_id
forest_area_id

# Getting the data
forest_df <- wb_data(country = 'countries_only', indicator = forest_area_id, mrv = 22)
# Removing unnecessary columns
refined_forest_df <- forest_df %>% 
  select(-iso2c, -country, -unit, -obs_status)
forest_area <- refined_forest_df %>% 
  select(-footnote, -last_updated)
View(forest_area)
names(forest_area)[3] <- 'Forest_area_in_percentage'

# [4pt] You'll need to calculate the change in forest area between the earliest
# and most recent years (1999 and 2018). To do this, first spread out (pivot_wider) 
# the values--the ISO3 number will be the primary id, the names will come from the 
# `date`, and the values will come from the indicator column. Then add a new column 
# (e.g., `forest_change`) that is the difference between the 2018 value and the 
# 1999 value (`2018 - 1999`).
# Because the column names will be strings that look like numbers (e.g., "1997"), 
# it's easier to access the column values using double-bracket notation than using
# dplyr. Alternatively, you can rename the columns for easier access.
#
forest_wider <- forest_area %>% 
  pivot_wider(names_from = date, values_from = `Forest_area_in_percentage`)
View(forest_wider)
# changing column names from numeric to string
names(forest_wider)[2] <- 'area_in_1999'
names(forest_wider)[21] <- 'area_in_2018'

# creating a new column of change in area, 
#and dropping the unnecessary columns
forest_change_df <- forest_wider %>% 
  mutate(area_change = area_in_2018 - area_in_1999) %>% 
  select(iso3c, area_in_1999, area_in_2018, area_change)
View(forest_change_df)
# Positive value shows increase in forest area, and negative value shows decrease in forest area

max(forest_change_df$area_change, na.rm = TRUE)
min(forest_change_df$area_change, na.rm = TRUE)
# Range of values is almost -16.88 to 14.94. Total range of roughly 32.

# [5pt] To make your choropleth map be readable and effective, you won't want to 
# try and assign a different color to each of 260 different values (that will be 
# a lot of colors and hard to distinguish!) Instead, you should break up (factor) 
# the data in a small number of groups (called "bins"). Each "bin" will represent
# a range of values--for example, one bin might represent values from 0-5%, one bin
# values from 5%-10%, and so forth. You will then be able to give each "bin" a 
# color, so that your map will only have 5 or 6 different colors representing 
# different "levels" or "tiers" of forestry loss, rather than 260 colors. 
# In short: you will color by a categorical value, rather than a continuous value!
#
# Use the `cut()` function to create a different column (i.e., `change_labels`) of 
# "labels" representing each "bin" of data. This function takes as arguments a 
# vector to divide (e.g., `forest_area$change`), and as a vector of breaks--the 
# values that should act as dividing lines or "cut-offs" for each bin level. 
# For example, you'd use "15%" as a break point to divide the data into bins of 
# 0%-15%" and 15%-30%. Also specify a labels argument that is a vector of 
# appropriate labels to use for naming each factor level (e.g., `c("0%-5%", "5%-10%")`). 
# See https://rpubs.com/pierrelafortune/cutdocumentation (among others tutorials) 
# for a more detailed example of using this function.
# You can look at the example image for a good set of "break points"
# You can assign this new factor (the result of the `cut()` function) to an 
# additional column in your data frame (e.g., `forest_area$change_as_factor`)




# Once you have the indicator data, you'll need to prepare the map. For ggplot2, 
# you'll need a set of polygons which represent each country in the world and 
# can form the basis for a geometric object layer.
#
# [1pt] Get a data frame of these polygons by calling the `map_data("world")` 
# function provided by ggplot2.
#
# [3pt] However, this data frame only lists countries by name, and country names
# are not standardized across data sets (e.g., different data sets may have 
# "United States", "USA", "US", etc). Thus you need to provide the map data a 
# three-letter country code (called an ISO3 code) based on their country name.
#
# You can find the ISO3 codes by using the `iso.alpha()` function from the `maps` 
# package (https://cran.r-project.org/web/packages/maps/maps.pdf) 
# (which you will need to install and load separately, at the top of your file).
# Pass the `iso.alpha()` function the `region` vector of the map data (the country
# names), and a `n = 3` argument to get the three-letter country codes. Mutate the
# map data frame to add a column of ISO3 codes for each country (the value returned
# from the `iso.alpha()` function).



# With the map data in hand, you can combine that with your indicator data to 
# create a plottable data frame.
#
# [1pt] *left join* the world map data frame (on the left) to the `forest_area`. 
# Join by ISO3 country code. This will create a giant data frame with a copy of 
# the indicator value in each point of the polygon.



# Finally, the data is ready so you can create the choropleth map:
# 
# [1pt] The data for your plot should be the joined map/forest-area data frame.
#
# [4pt] Use polygon geometry to create the plot. Map the `long` value to the 
# x-coodinate, the `lat` value to the y-coordinate, and `group` points together.
#
# [3pt] Map the *fill* (not the color!) to the change value--this will color the 
# inside of the polygons, not just the outlines. You must also specify a colorbrewer
# scale for the map fill; I used the "RdYlGn" palette (in reverse order).
#
# [2pt] Your plot should use a map coordinate system, such as `coord_quickmap()`
# https://ggplot2.tidyverse.org/reference/coord_map.html
# 
# [2pt] You can easily get rid of the x and y axis labels by including a void theme 
# https://ggplot2.tidyverse.org/reference/ggtheme.html
# You'll still need to add a title to the plot.
#
# [1pt] When completed, save your plot in a variable called `world_forest_plot`. 
# Note that you can print() out this variable in order to see the plot generated 
# when you run your script.
#
# This map will show a lot of data and geometry, so may take a couple seconds to 
# generate. Be patient!
# Some countries may be missing values for some indicators or years if the data 
# was unavailable. It's okay if these countries are left "blank" in your map).

# STAGE 1: Creating bins
forest_change_df_with_labels <-  forest_change_df %>% 
  mutate(change_as_factor = cut(forest_change_df$area_change, breaks=c(-20,-10, -5, 0, 5, 10, Inf), 
                                labels=c("-20% to -10%", "-10% to -5%", 
                                         "-5% to 0%", "0% to 5%", "5% to 10%", ">10%")))
View(forest_change_df_with_labels)
forest_area <- forest_change_df_with_labels

# STAGE 2 : Getting dataframe of the polygons
library(maps)
map_data_df <- ggplot2::map_data("world")
dim(map_data_df)
# it has 99338 rows and 6 columns
map_data_df <- map_data_df  %>% 
  mutate(iso_code = iso.alpha(x = map_data_df$region, n=3))
names(map_data_df)[7] <- 'iso3c'

View(map_data_df)

# Joining the two datasets
merged_df <-  map_data_df %>% 
  left_join(forest_area, by = 'iso3c')

### STAGE 3 - plotting
dim(merged_df)
View(merged_df)

world_forest_plot <- ggplot(merged_df, aes(long, lat, group = group)) +
  scale_fill_brewer(palette = "RdYlGn") +
  geom_polygon(aes(fill = change_as_factor)) +
  coord_quickmap() +
  theme_void() +
  labs(title = "Change in Forested Area over 1999 - 2018",
       fill = "Change") 
show(world_forest_plot)



###############################################################################
##### PART 6: Your Own Plot #####
### In this section you will create a visualization of something that is 
### important to _you_ based on the World Bank data.

# For this visualization, you can choose to visualize any information from the 
# World Bank data set that you wish. For example, you could visualize differences 
# in Internet usage, economic development, or anything else. Look through the 
# available indicators for topics that seem like they might be interesting.
# https://data.worldbank.org/indicator?tab=all
#
# Your visualization will need to use at least three "data features" (think: columns).
# This means you'll need to use either multiple indicators, use multiple years
# from a single indicator, or use multiple years from multiple indicators. 
# Pro tip: you can often easily produce an "interesting" analysis by taking two 
# seemingly unreleated topics and then comparing them to show that the are actually related!
#
# You will almost certainly need to do some light data wrangling to get the 
# information you want. Think about what question your visualization will be able
# to answer, and then what data you'll need for that question.
# (But don't overthink this; the goal here is to practice making visualizations, 
# not to be a time sink!)
#
# Your visualization must be created with the ggplot2 package. It will need to 
# meet the following requirements:
# - At a minimum, it will need to include either 2 simple geometries (points, 
#   lines, columns) _or_ 1 "complex" geometry (e.g., polygons, hex bins, etc.). 
#   A visual element such as facets count as a simple geometry (so having a single
#   point geometry with facets would be sufficient).
# - It will need to encode three (3) or more features (columns) to different aesthetics
#   (e.g., x, y, and color).
# - It needs to include an adjusted scale for at least one of the aesthetics. 
#   Picking a color palette is sufficient.
# - (You are not required to specify position adjustments or coordinate systems, 
#   though you are welcome to if you wish)
# - It must include appropriate titles and labels. In particular, make sure that 
#   any "legend" labels are clear and understandable.
# When your designing your visualization, think about how you can make it both 
# effective and expressive.
#
# When completed, save your plot in a variable with a descriptive name (and not 
# just `my_plot`). Note that you can print() out this variable in order to see
# the plot generated when you run your script.

# How did the consumer price index data change in 10 years from 2010 to 2020?

consumer_price_index <- wb_data(country = "countries_only", indicator = c("FP.CPI.TOTL"), mrv = 12) %>%
  filter(date >= 2010) %>%
  filter(date <= 2020) %>%
  select(iso3c, country, date, value = FP.CPI.TOTL)

# label the consumer price index change on each country
consumer_price_index <- consumer_price_index %>%
  pivot_wider(names_from = date, values_from = value) %>%
  mutate(change_pct = (`2020`/`2010` - 1)*100) %>%
  mutate(Change = cut(change_pct, breaks=c(-10, 0, 10, 20, 40, Inf), labels=c("-10% to 0%", "0% to 10%", "10% to 20%", "20% to 40%", "> 40%"))) %>%
  select(iso3c, country, change_pct, Change)

#  Calling world map data to create a column representing ISO3 code for each country
world_map <- map_data("world")
world_map <- world_map %>%
  mutate(iso3c = iso.alpha(region, n=3))

# left join world map data to consumer price index data
joined_map_cpi <- consumer_price_index %>%
  left_join(world_map, by = "iso3c")

# Plotting consumer price index data on Map
consumer_price_index_plot <- ggplot(joined_map_cpi, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Change)) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  labs(title = "Change in Consumer Price Index from 2010 to 2020", 
       fill='Change') +
  coord_quickmap()

show(consumer_price_index_plot)


