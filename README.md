install.packages("gapminder")
library(gapminder)

# Explore the gapminder dataset
str(gapminder)

# Check the nature of the "Continent" column
unique(gapminder$continent)

# Display the number of rows and columns
dim(gapminder)

# Find the number of missing values
sum(is.na(gapminder))

# Show the first 6 rows
head(gapminder)

# Find the average Life Expectancy across all countries
mean(gapminder$lifeExp, na.rm = TRUE)

# Select every column except pop
gapminder[, -which(names(gapminder) == "pop")]

# Select specific columns
gapminder[, c("year", "lifeExp", "country")]

# Life Expectancy in the United States and Thailand in 2000
subset(gapminder, (country == "United States" & year == 2000) | (country == "Thailand" & year == 2000))

# Store data for Asian countries
gapminder_asia <- subset(gapminder, continent == "Asia")

# Lowest life expectancy
min_life_exp <- min(gapminder$lifeExp)
subset(gapminder, lifeExp == min_life_exp)

# Display 1952 data sorted by gdpPercap
subset(gapminder, year == 1952)[order(subset(gapminder, year == 1952)$gdpPercap, decreasing = TRUE), ]

# Most populous European country in 1992
subset(gapminder, year == 1992 & continent == "Europe")[which.max(subset(gapminder, year == 1992 & continent == "Europe")$pop), ]

# Introduce a new column for total GDP
gapminder$totalGDP <- gapminder$gdpPercap * gapminder$pop

# Shortest life expectancy in the Americas in 1962
subset(gapminder, year == 1962 & continent == "Americas")[which.min(subset(gapminder, year == 1962 & continent == "Americas")$lifeExp), ]

# Create subset for 2007
gapminder_2007 <- subset(gapminder, year == 2007)

# Scatterplot for pop and lifeExp
plot(gapminder_2007$pop, gapminder_2007$lifeExp)

# Scatterplot for pop and gdpPercap colored by continent
plot(gapminder_2007$pop, gapminder_2007$gdpPercap, col = gapminder_2007$continent)

# Mean life expectancy continent-wise
mean_life_continent <- tapply(gapminder$lifeExp, gapminder$continent, mean, na.rm = TRUE)

# Bar plot for mean life expectancy
barplot(mean_life_continent, horiz = TRUE)

# Display records for the United States in 1957, 2002, and 2007
subset(gapminder, country == "United States" & year %in% c(1957, 2002, 2007))

# Average life expectancy in the United States for 2007
mean(subset(gapminder, country == "United States" & year == 2007)$lifeExp)

# Average life expectancy per continent in 2007
mean_life_continent_2007 <- tapply(subset(gapminder, year == 2007)$lifeExp, subset(gapminder, year == 2007)$continent, mean, na.rm = TRUE)

# Total population continent-wise
total_pop_continent <- tapply(gapminder$pop, gapminder$continent, sum)

# Sort total population in descending order
total_pop_continent[order(total_pop_continent, decreasing = TRUE)]
