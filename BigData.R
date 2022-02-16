#The project is based on free data Food Access CSV from the CORGIS Dataset Project

#Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

####
#Vehicle Access.x Mile - Housing units without vehicle count beyond x mile from supermarket
#Low Access Numbers.Children.x Mile - Kids population count beyond x mile from supermarket
#Low Access Numbers.Low Income People.x Mile - Low income population count beyond x mile from supermarket
#Low Access Numbers.People.x Mile - Population count beyond x mile from supermarket
#Low Access Numbers.Seniors.x Mile - Seniors population count beyond x mile from supermarket
####
  
#Import data
df <- read.csv2("food_access.csv")

#Understanding data
View(df)
str(df)
summary(df)



####STATES POPULATION
#Sum of population
states_pop <- df %>%
  group_by(State) %>%
  summarise(Population=sum(Population)) %>%
  arrange(desc(Population))

View(states_pop)

#Chart for states population in desc order
states_pop %>%
  arrange(desc(Population)) %>%
  ggplot(., aes(x=reorder(State, -Population), y=Population)) + 
  geom_bar(stat="identity", width = 0.5, fill="tomato") + 
  labs(title="State Population", 
       subtitle="in descending order") +
  xlab("State name") +
  ylab("Population (in M)") +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))

#The 5 most populous states are: California, Texas, New York, Florida and Illinois
#We will check whether the low level of food access is related to the population



#### DISTANCE OF SUPERMARKET
#Population count beyond x mile from supermarket grouped by states
supermarket_states <- df %>%
  group_by(State) %>%
  summarise(Population=sum(Population),
            Miles05=sum(Low.Access.Numbers.People.1.2.Mile),
            Miles1=sum(Low.Access.Numbers.People.1.Mile),
            Miles10=sum(Low.Access.Numbers.People.10.Miles),
            Miles20=sum(Low.Access.Numbers.People.20.Miles))

View(supermarket_states)

#Low income population count beyond x mile from supermarket by states
#To see which areas are low-income
low_income_supermarket <- df %>%
  group_by(State) %>%
  summarise(Population=sum(Population),
            Miles05=sum(Low.Access.Numbers.Low.Income.People.1.2.Mile),
            Miles1=sum(Low.Access.Numbers.Low.Income.People.1.Mile),
            Miles10=sum(Low.Access.Numbers.Low.Income.People.10.Miles),
            Miles20=sum(Low.Access.Numbers.Low.Income.People.20.Miles))

View(low_income_supermarket)


#Low access to healthy food is defined as being far from a supermarket, supercenter, or large grocery store
#Low-income census tracts where a significant number (at least 500 people) or share (at least 33 percent) of the population is
#greater than 1 mile from the nearest supermarket for an urban area
#or greater than 10 miles for a rural area.


#So we will do the rest of the calculations on the 10 miles variable


#### 10 MILES
#States population with over 10 miles far from a supermarket
ten_miles <- df %>%
  group_by(State) %>%
  summarise(Population=sum(Population),
            PeopleTenMiles=sum(Low.Access.Numbers.People.10.Miles),
            LowIncomePeopleTenMiles=sum(Low.Access.Numbers.Low.Income.People.10.Miles),
            KidsTenMiles=sum(Low.Access.Numbers.Children.10.Miles),
            SeniorsTenMiles=sum(Low.Access.Numbers.Seniors.10.Miles))

#New column with percentage of population with over 10 miles
ten_miles <- ten_miles %>%
  mutate(., "PercentageOfPopulation" = (.[3] / .[2])) %>%
  arrange(desc(PercentageOfPopulation))

View(ten_miles)
#Top 5 states here are: North Dakota, South Dakota, Montana, Alaska and Wyoming

#On average, over 3% of the population in each state has over 10 miles to the nearest supermarket
ten_miles_mean <- mean(unlist(ten_miles[7]))

#Quartiles
quantile(unlist(ten_miles[7]))
#Variance and standard deviation- the dispersion from the mean is low
var(unlist(ten_miles[7]))
sd(unlist(ten_miles[7]))

#Chart for states population in desc order
ten_miles %>%
  ggplot(., aes(x=reorder(State, -unlist(.[7])), y=unlist(.[7]))) + 
  geom_bar(stat="identity", width = 0.5, fill="tomato") + 
  geom_hline(yintercept = ten_miles_mean, linetype = "dashed") +
  labs(title="Percentage of population beyond 10 mile from supermarket ", 
       subtitle="in descending order") +
  xlab("State name") +
  ylab("Percentage of population (in %)") +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))


#Correlation between population overall and with over 10 miles to supermarket
#is 0.4439473, so the correlation is weak
cor(unlist(ten_miles[2]), unlist(ten_miles[3]),  method = "pearson")

#Ten states with the lowest food access in relation to the distance from the supermarket
lowest_food_access <- ten_miles %>%
  top_n(., 10, unlist(ten_miles[7]))

