#'The data shown here is from Survey of Consumer Finances (SCF) and it's centered around
#'different demographics (weight, sex, race, age, and education) and wealth (assets, liabilies, income).
#'I would like to examine wealth, as defined by assets-liabilities, in comparison to race and education.
#'Primary Questions: Over the past 30 years, what have been... 
#'1. The trends in median wealth, distinguished by race and education?
#'2. Trends in wealth for homeowners age 25+? 
#'3. How did the 2007-8 financial crisis impact housing wealth; what are racial differences?

#Setting Up ####
library(readr)
library(stringr)
#install.packages("tidyverse")
#library(tidyverse)
library(tidyr)
library(lubridate)
library(dplyr)
library(gt)
library(ggplot2)

#Reading In and Exploring Data
wealthData <- read.csv("/Users/ivy/Desktop/DDD-I21/SCFP8916.csv")
wealth <- wealthData #Thou shalt never write over your original data
colnames(wealth)
ncol(wealth)
nrow(wealth)
wealth[1:5,]
summary(wealth)
length(unique(wealth$education))
length(unique(wealth$race))
length(unique(wealth$year))

#Formatting Dates and Checking Class
wealth$year[1]
class(wealth$year)
wealth$fix.date <- as.Date(as.character(wealth$year), format ="%Y", "%Y") #formatting year as a date instead of integer
class(wealth$fix.date) 
wealth$year.date <- format(as.Date(wealth$fix.date, format= "%Y-%m-%d"), "%Y")
class(wealth$age)
class(wealth$race)
class(wealth$asset_total)
class(wealth$education)

#Cleaning Data
#deleting columns with irrelevant data
#wealth.cleaned <- wealth %>% select(-"weight", -"sex", -"income") 
#'Should NOT have done this; Reverted references of wealth.cleaned back to wealth. 
#'My logic was that because the variables would not be referenced at all, 
#'it would be more efficient to remove them but I now realize that unless there are many NAs,
#'it's not worth it to delete unused data from future dataframes because 
#'a scenario like this may happen where important data is ignored after a hasty decision.


#New Variable Creation to Answer Primary Questions ####
#Renaming "debt" columns to "liabilities" for consistency in future references
colnames(wealth)[9] <- "liability_total"
colnames(wealth)[9]
colnames(wealth)[10] <- "liability_housing"
colnames(wealth[10])

#Calculating total wealth (assets-liabilities) and storing in a new column 
wealth$total_wealth <- wealth$asset_total - wealth$liability_total
#Calculating housing wealth and storing in a new column
wealth$housing_wealth <- wealth$asset_housing - wealth$liability_housing


#Creating Dataframes and Variables for Tables ####
#Dataframe: Median Wealth by Race, using aggregate function
wealth.by.race.df <- aggregate(x=wealth$total_wealth, 
                              by=list(wealth$race,wealth$year.date),
                              FUN=median)
#Another method would be to use group_by (dplyr) first, then summarise() into a new dataframe for each variable comparison. Example:
    #wealth.by.race.df <- wealth %>% 
    #group_by(wealth$year.date, wealth$race)
    #summarise(each.dataframe = median(wealth$total_wealth))
#Reformatting the dataframe so "race" becomes a name/key, "Median Wealth" becomes a value, and years aren't repeated
wealth.by.race.df.table <- pivot_wider(
  wealth.by.race.df,
  id_cols = NULL,
  names_from = Group.1,
  values_from = x)
#Renaming columns which have been affected by use of aggregate function
colnames(wealth.by.race.df.table)[1] <- "Year"
colnames(wealth.by.race.df.table)[1]
colnames(wealth.by.race.df.table)[2] <- "Black"
colnames(wealth.by.race.df.table)[2]
colnames(wealth.by.race.df.table)[4] <- "Other"
colnames(wealth.by.race.df.table)[4]
colnames(wealth.by.race.df.table)[5] <- "White"
colnames(wealth.by.race.df.table)[5]

#Dataframe: Median Wealth by Education
wealth.by.education.df <- aggregate(x=wealth$total_wealth,
                                    by=list(wealth$education,wealth$year.date),
                                    FUN=median)
#Reformat & Rename
wealth.by.education.df.table <- pivot_wider(
  wealth.by.education.df,
  id_cols = NULL,
  names_from = Group.1,
  values_from = x)
colnames(wealth.by.education.df.table)[1] <- "Year"
colnames(wealth.by.education.df.table)[2] <- "Graduated College"
colnames(wealth.by.education.df.table)[3] <- "No College"
colnames(wealth.by.education.df.table)[4] <- "Some College"


#Dataframe: Housing Wealth by Race, Without Age Condition 
#'This dataframe shows that the age condition of "25 and older" is necessary.
#'Otherwise, there are many more 0s (no housing wealth at all), 
#'without taking into account that those under the age of 25 most likely live with parents 
#'even if they possess assets and liabilities greater than 0.
housing.wealth.by.race.df <- aggregate(x=wealth$housing_wealth,
                                       by=list(wealth$race,wealth$year.date),
                                       FUN=median)
#Reformat & Rename
housing.wealth.by.race.df.table <- pivot_wider(
  housing.wealth.by.race.df,
  id_cols = NULL,
  names_from = Group.1,
  values_from = x)
colnames(housing.wealth.by.race.df.table)[1] <- "Year"
colnames(housing.wealth.by.race.df.table)[2] <- "Black"
colnames(housing.wealth.by.race.df.table)[4] <- "Other"
colnames(housing.wealth.by.race.df.table)[5] <- "White" 


#Dataframe: Housing Wealth by Race, Age>=25
age.greater.than.25 <- wealth %>% 
  filter(age >= 25)
housing.wealth.by.race.age.df <- aggregate(x=age.greater.than.25$housing_wealth,
                                           by=list(age.greater.than.25$race, age.greater.than.25$year.date),
                                           FUN=median)
#Reformat & Rename
housing.wealth.by.race.age.df.table <- pivot_wider(
  housing.wealth.by.race.age.df,
  id_cols = NULL,
  names_from = Group.1,
  values_from = x)
colnames(housing.wealth.by.race.age.df.table)[1] <- "Year"
colnames(housing.wealth.by.race.age.df.table)[2] <- "Black"
colnames(housing.wealth.by.race.age.df.table)[4] <- "Other"
colnames(housing.wealth.by.race.age.df.table)[5] <- "White"


#Tables ####
#Table 1: Median Wealth by Race
wealth.by.race.df.table %>% 
  gt(rowname_col = "median wealth") %>% 
  tab_header(title = "Median Wealth by Race") %>% 
  fmt_currency(columns = vars(Black, Hispanic, Other, White),
               currency = "USD")

#Table 2: Median Wealth by Education
wealth.by.education.df.table %>%
  gt(rowname_col = "median wealth") %>%
  tab_header(title = "Median Wealth by Education") %>% 
  fmt_currency(columns = vars("Graduated College", "No College", "Some College"),
               currency = "USD") %>% 
  tab_spanner(label = "Education Level",
              columns = vars("Graduated College", "No College", "Some College")) %>% 
  cols_move_to_end(columns = vars("Graduated College"))

#Table 3: Housing Wealth by Race, 25 years or older
housing.wealth.by.race.age.df.table %>% 
  gt(rowname_col = "median housing wealth") %>% 
  fmt_currency(columns = vars(Black, Hispanic, Other, White),
               currency = "USD") %>% 
  tab_header(title = "Median Housing Wealth by Race",
             subtitle = "of those 25 years and older" ) %>% 
  tab_spanner(label = "Race",
              columns = vars(Black, Hispanic, Other, White)) 


#Graphs ####
#Graph 1: Median Wealth by Race 
graph1Year <- as.factor(wealth.by.race.df.table$Year)
ggplot(data = wealth.by.race.df.table,
       aes(x = graph1Year, y = medianWealth) +
         geom_line(size = 1) +
         ggtitle("Median Wealth by Race") +
         xlab("Year") +
         ylab("Median Wealth in USD") +
         scale_x_discrete(name = "Years",
                   limits = c("1989", "1992", "1995", "1998",
                              "2001", "2004", "2007","2010", "2013", "2016"))
       )
        #scale_x_date(date_breaks = "3 Years", date_labels = "%Y")


#Graph 2: Housing Wealth by Race, 25 years or older
graph2Year <- as.factor(housing.wealth.by.race.age.df$Year)
ggplot(data = housing.wealth.by.race.age.df.table,
       aes(x = graph2Year, y = ) +
         geom_line(size = 1) +
         ggtitle("Housing Wealth by Race") +
         xlab("Year") +
         ylab("Median Wealth in USD") +
         scale_x_discrete(name = "Years",
                          limits = c("1989", "1992", "1995", "1998",
                                     "2001", "2004", "2007","2010", "2013", "2016"))
       )


#'Last Comments: Note that weights (column 1 of original data, can be seen with view(wealth)) were not included which is a mistake. 
#'Column one (weights) should have been incorporated in order to adjust for limited sampling size as it is not the census. 
#'Could multiply into the wealth for "weighted total_wealth", and then median could be found after adjustments for a more accurately representative analysis & conclusion.
#'End.