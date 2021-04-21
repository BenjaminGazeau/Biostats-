library(tidyverse)
head(faithful)


eruption.lm <- lm(eruptions ~ waiting, data = faithful)
summary(eruption.lm)

slope <- round(eruption.lm$coef[2], 3)
# p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0, so...
p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)


#Assignment Work
#Example of bad dataset that cant be transformed into dataframe

ldeaths <- ldeaths
ldeaths <- as_tibble(ldeaths)
install.packages("tsbox")
library(tsbox)
ldeaths <- ts_df(ldeaths)
view(mtcars)

snakes
Snakes_1<- read.csv("snakes.csv")
  
ggplot(data = Snakes_1, aes(x = snake, y = openings)) +
  geom_boxplot(aes(fill = snake), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

Snakes_plot <- Snakes_1 %>% 
  filter(day == 1)
ggplot(data = Snakes_plot, aes(x= snake, y= openings, fill = snake))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette="YlGnBu")+
  labs(x = "Snake", y = "Openings", colour = "snake", title = "Snake openings occuring on the first day")

library(corrplot)  
Ecklonia_data<- read.csv("ecklonia.csv") %>% 
  select(-species, -site, -ID)

cor.test(x = Ecklonia_data$stipe_length, Ecklonia_data$frond_length,
         use = "everything", method = "pearson")

ecklonia_pearson <- cor(Ecklonia_data)

ecklonia_pearson

# Create ordinal data
Ecklonia_data$length <- as.numeric(cut((Ecklonia_data$stipe_length+Ecklonia_data$frond_length), breaks = 3))

# Run test on any variable
cor.test(Ecklonia_data$length, Ecklonia_data$digits)


ecklonia_norm <- Ecklonia_data %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

cor.test(Ecklonia_data$primary_blade_length, Ecklonia_data$primary_blade_width, method = "kendall")


r_print <- paste0("r = ", 
                  round(cor(x = Ecklonia_data$stipe_length, Ecklonia_data$frond_length),2))

ggplot(data = Ecklonia_data, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_bw()

corrplot(ecklonia_pearson, method = "circle")

library
library(gplots)

heatmap(ecklonia_pearson, scale = "none", Colv = NA, Rowv = NA,
        col= colorRampPalette(brewer.pal(8, "Oranges"))(25))

  
legend(x="topleft", legend=c("0", "0.5", "1"), 
       fill=colorRampPalette(brewer.pal(8, "Oranges"))(3))

heatmap(ecklonia_pearson, scale = "none", Colv = NA, Rowv = NA,
        col= colorRampPalette(brewer.pal(8, "Oranges"))(25))

library(plyr)

