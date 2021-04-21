Bod_Data <- BOD

?BOD

#Section 1

#C is True

#Section 2

library(dplyr)
library(dslabs)

data(murders)

glimpse(murders)
head(murders)
tail(murders)
summary(murders)

#The murder dataset shows the amount of gun murders that have occured in each
#state of America. This data has been taken from the FBI reports. Within the 
#dataset each state has been named and their abbreaviations have also been given.
#The states regions are also specified along with the amount of murders that
#have occured and their total populations. 

murders_1 <- murders %>% 
  select(state, population)

murders_1 %>% 
  filter(state != "Florida")

no_south <- murders %>% 
  filter(region != "South")

#There are 34 states within this category 

murders_South_West <- murders %>% 
  select(region, population) %>% 
  filter(region == "South") %>% 
  mutate(Total_pop = sum(population))
#The total population size of the Southern regions is 115674434

murders_South_West <- murders %>% 
  select(region, population) %>% 
  filter(region == "West") %>% 
  mutate(Total_pop = sum(population))
#The total population size of the Southern regions is 71945553

murders_NE <- murders %>% 
  select(region, population) %>%
  filter(region =="Northeast") %>% 
  mutate(Total_pop = sum(population))
          
library(ggplot2)

Murders_plot <- murders %>%
  select(region,total) %>% 
  filter(region == "South") %>% 
  mutate(sum(total))

Murd_Plot <- ggplot(data = Murders_plot, aes(x = region, y = total)) +
  geom_boxplot(aes(colour = region)) +
  facet_wrap(~region) +
  labs(x = "Region", y = "Murders", title = "Murders per region") +
  theme_bw()
Murd_Plot

ggplot(data = murders, aes(x = region, y = total )) +
  geom_boxplot() +
  labs(y = "Murders", x = "Region") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggplot(data = murders, aes(x = region, y = population)) +
  geom_boxplot() +
  labs(y = "Population (millions)", x = "Region")+
  scale_y_continuous(breaks = c(0, 10000000, 20000000, 30000000), labels = c(0, 10, 20, 30))

#see graph for population size comparisons 

murders_3 <-  murders %>% 
  filter(total>20) %>% 
  filter(total<100)
 
murders_slice<- slice(murders, rows = 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26)

library(tibble)
murders_tibble <-as_tibble(murders) %>% 
  group_by(region)


#Section 3

library(dplyr)
library(dslabs)
data(heights)

#The heights dataset is a collection of data showing the heights
#of a group of male amd female individuals. this data is tidy 
#is easily understandable. The inidividuals heights are givin in
#inches

glimpse(heights)
head(heights)
tail(heights)
summary(heights)
str(heights)
dim(heights)

heights %>% 
  summarise(Mean = mean(height),
            Standard_Deviation = sd(height),
            med_wt = median(height),
            mean_wt= sum(height)/n(),
            sd_wt = sd(height),
            min_wt = min(height),
            qrt1_wt = quantile(height, p = 0.25),
            med_wt = median(height), 
            min_hei= min(height),
            max_he= max(height))

#Section 4

#Section 5

Seasonal_data <- data.frame(year = c(2015, 2016, 2017, 2018),
                            winter = c(41, 39, 47, 40),
                            spring = c(41, 46, 57, 45),
                            summer = c(75, 52, 85, 66),
                            Autumn = c(57, 66, 52, 56))
library(dplyr)
library(tidyr)
library(tidyverse)

#Hypothesis: 
#I believe the temperatures experienced during the winter and autumn 
#seasons will be the coldest whereas the warmest temperatures will
#be experienced in Spring and summer 

Seasonal_plot <- Seasonal_data %>% 
  gather(Season, Temp, -1)

ggplot(Seasonal_plot, aes(x = Season, y = Temp)) +
  geom_point(aes(group = year, colour = year)) +
  labs(x = "Season", y = "Temperature value",
       title = "Annual temperature fluctuations")

ggplot(Seasonal_plot, aes(x = year, y = Temp)) +
  geom_point(aes(group = Season, colour = Season)) +
  labs(x = "year", y = "Temperature value",
       title = "Annual temperature fluctuations")

#As seen on the two plots, Winter and Autumn were the coldest seasons 
#and summer and spring were, on average, warmer seasons. Summer had the hottest
#temperature and winter had the coldest temperature 

cats_data<- tibble(cats = c("A", "B", "C"),
                   position = c("1-2-3", "3-1-2", "2-3-1"),
                   minutes = c(3, 3, 3),
                   seconds = c(12, 44, 15))
Cats_1<- cats_data %>% 
  separate(position, into = c("First_place","Second_place", "Third_place"),  
                              (sep = "-")) %>% 
  unite(total_time, c("minutes", "seconds"), sep = ",")

#Section 6

data <- mtcars
#reassigning the dataset to "data"
data %>% 
  rownames_to_column("model") %>% 
  filter(cyl == 4) %>%
  #filtering out the values where the "cyl" is equal to zero 
  mutate(hp_per_1000 = hp/wt) %>%
  #creating a new column called hp_per_1000
  select(model,mpg,hp,wt,hp_per_1000) %>%
  #using the select function to isolate the above values
  arrange(desc(hp_per_1000))
#using the arrange function to organize the values in terms of hp_per_1000
  
data %>% 
  group_by(gear, am)
#grouping the data values by "gear' and "am" 


data %>% filter(mpg > 20)
#filtering out the values where "mpg" is greater than 20

data_mpg <- data %>% 
  separate(mpg, c("Miles", "Gallon"), sep= ".")

#unfortunately i am unable to separate any of this data as this data won't 
#work with the separate function, although the dataset below made it easy for me
#utilize the separate function 

data2 <- data.frame(dates = c("Jan/2/2018",
                             "Feb/24/2019",
                             "Jan/14/2015",
                             "Mar/4/2016",
                             "Nov/23/2017",
                             "Jul/1/2016"))
data2 %>% 
  separate(dates, c("Month","Day","Year"), sep="/")
