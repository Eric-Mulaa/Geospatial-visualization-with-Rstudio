tornado <- read.table(file = "us_tornado_dataset.csv", header = TRUE, sep = ",")

library(tidyverse)
library(ggplot2)

# Having a look at the dataset is important for understanding the type of data I'm dealing with
head(tornado)
str(tornado)
summary(tornado)

# There are no NAs in this data
sum(is.na(tornado))

# Let's see the total number of tornadoes that we are dealing with
tornado %>%
  count() %>%
  print()
# let's now see the number of tornadoes group by year
yearly_tornadoes <- tornado %>%
  group_by(yr) %>%
  count() %>%
  arrange(desc(n))
head(yearly_tornadoes)
tail(yearly_tornadoes)
# A plot of the same
ggplot(yearly_tornadoes, aes(x = yr, y = n)) + 
  geom_line(col = "#98107E") + 
  labs(title = "Yearly Tornadoes",
       caption = "1950-2021",
       x = "Year",
       y = "Number of Tornadoes")

# From the above data we see that 2004 and the most tornadoes in USA, followed by 
# 2011, 2008, and then 2019 for the period between 1950-2021, While 1950 had the least
# number of recorded tornadoes.

# we will now see which month has experienced most tornadoes for period 1950-2021
monthly_tornadoes <- tornado %>%
  group_by(mo) %>%
  count()
# I will proceed to visualize this information
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
monthly_tornadoes$abv <- mymonths[ monthly_tornadoes$mo ]
ggplot(monthly_tornadoes, aes(x = abv, y = n)) + 
  geom_col(fill = "#98107E") +
  scale_x_discrete(limits = monthly_tornadoes$abv) +
  labs(title = "Monthly Tornadoes",
       caption = "1950-2021",
       x = "Month",
       y = "Number of Tornadoes")
# Most tornadoes have been happening in May.
# We can see that the number of tornadoes keep on increasing from January and
# is at the peak in May, after which the number of tornadoes start declining. 

# Let's see where tornado occurs most in the US
colnames(tornado)[colnames(tornado) == "st"] <- "state"
states <- tornado %>%
  group_by(state) %>%
  summarise(value = n()) %>%
  arrange(desc(value))
head(states)
# Texas has experience most tornadoes since 1950 to 2021. 
# I will proceed to visualize the above information 
states <- states %>%
  mutate(color = case_when(value < 500 ~ "Less than 500",
                           value <= 1000 ~ "501 - 1,000",
                           value <= 2000 ~ "1,001 - 2,000",
                           value <= 4000 ~ "2,001 - 4,000",
                           value > 4000 ~ "More than 4,000",
                           TRUE ~ "No Tornadoes"))
# First, I will arrange the colors
states$color <- fct_relevel(states$color, c("More than 4,000",
                                                        "2,001 - 4,000",
                                                        "1,001 - 2,000",
                                                        "501 - 1,000",
                                                        "Less than 500"))
# I will now visualize the values in USA map
# First, I will load the usmap package.
install.packages("usmap")
library(usmap)
install.packages("showtext")
library(showtext)
plot_usmap(data = states, values = "color", labels = FALSE) +
  scale_fill_manual(values = c( "Less than 500" = "purple",
                                "501 - 1,000" = "black",
                                "1,001 - 2,000"= "yellow",
                                "2,001 - 4,000"= "orange",
                                "More than 4,000" = "red",
                                "No Tornadoes" = "green")) +
  labs(title = "Tornadoes by State",
       caption = "1950 - 2021",
       fill = "color",
       x = "",
       y = "") + theme(legend.position = "bottom",
                       legend.title = element_blank(),
                       legend.text=element_text(size = 8), 
                       axis.ticks = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.title.x=element_blank()) 
# From the above map we can see that the most tornado-prone regions in the 
# US is the central and southeastern states along a corridor sometimes 
# called "Tornado Alley."

#Let analyze fatalities
tornado %>%
  summarise(total_fatalities = sum(tornado$fat))
# There has been 6112 fatalities between 1950 and 2021

yearly_fatalities <- tornado %>%
  group_by(yr) %>%
  summarise(total = sum(fat))
ggplot(yearly_fatalities, aes(x = yr, y = total)) + 
  geom_line(col = "#98107E") + 
  labs(title = "Yearly Fatalities",
       caption = "1950-2021",
       x = "Year",
       y = "Fatalities")

# 2011 had the most fatalities

# Let see fatalities by states
state_fatalities <- tornado %>%
  group_by(state) %>%
  summarise(value = sum(fat)) %>%
  arrange(desc(value))

state_fatalities <- state_fatalities %>%
mutate(color = case_when(value >= 1 & value <= 10 ~ "Less than 10",
                         value > 10 & value <= 50 ~ "11 - 50",
                         value > 51 & value <= 200 ~ "51 - 200",
                         value > 201 & value <= 400 ~ "201 - 400",
                         value > 400 ~ "More than 400",
                         TRUE ~ "No Fatalities"))
# I will now arrange the colors
state_fatalities$color <- fct_relevel(state_fatalities$color, c("More than 400","201 - 400",
                                                          "51 - 200",
                                                          "11 - 50",
                                                          "Less than 10", 
                                                          "No Fatalities"))  

plot_usmap(data = state_fatalities, values = "color", labels = FALSE) +
  scale_fill_manual(values = c( "Less than 10" = "purple",
                                "11 - 50" = "black",
                                "51 - 200"= "yellow",
                                "201 - 400"= "orange",
                                "More than 400" = "red",
                                "No Fatalities"="green")) +
  
  labs(title = "Tornado Fatalities by State",
       caption = "1950-2021",
       fill = "color",
       x = "",
       y = "") + theme(legend.position = "bottom",
                       legend.title = element_blank(),
                       legend.text=element_text(size = 8), 
                       axis.ticks = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.title.x=element_blank())

# Alabama had most fatalities, followed by Texas, Mississippi, and Oklahoma.

# We will now see fatalities that were associated with different types of tornado
# magnitude. 
# Fatalities of -9 will be filtered out.
# I will start by visualizing data from 1950 to 2007 January. Fujita scale was
# used during this time.
mag_f <- tornado %>%
  filter(mag != -9)
  
mag_f <- mag_f %>%
  filter(yr <= 2007 & mo <= 1) %>%
  group_by(mag) %>%
  summarise(average_fatalities = round(mean(fat)), 1)
ggplot(mag_f, aes(y = average_fatalities, x = mag)) + 
  geom_col(fill = "#98107E") + 
  labs(title = "Average Number of Fatalities Per Tornado Magnitude",
       caption = "1950-2007",
       x = "Tornado Magnitude")
#It is clear that as tornado magnitude increases, fatalities increase too.
       

# I will visualize the average number of fatalities as from February 2007
# These were measured using the enhanced Fujita scale which has a slight differebce
# from the Fujita scale.
mag_ef <- tornado %>%
  group_by(mag) %>%
  filter(yr >= 2007 & mo >= 2 & mag != -9) %>%
  summarise(average_fatalities = round(mean(fat)), 1)
ggplot(mag_ef, aes(y = average_fatalities, x = mag)) + 
  geom_col(fill = "#98107E") + 
  labs(title = "Average Number of Fatalities Per Tornado Magnitude",
       caption = "2007-2021",
       x = "Tornado Magnitude")

#From the above visual it is clear that lower tornado magnitudes are
# associated with lower fatalities and vice verser.









  






