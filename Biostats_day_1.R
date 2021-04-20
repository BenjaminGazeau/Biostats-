library(tidyverse)
chicks <-  as_tibble(ChickWeight)
chicks %>% 
  summarise(length = n())
# note the distinction between 'nrow()' and the true sample size 

weight_chick <- chicks %>% 
  filter(Time == 20) %>% 
  summarise(Mean = mean(weight))

chicks %>% 
  summarise(Mean = mean(weight),
            Standard_Deviation = sd(weight),
            med_wt = median(weight),
            Kurtosis= kurtosis(weight),
            mean_wt= sum(weight)/n(),
            sd_wt = sd(weight),
            min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = median(weight, p = 0.75),
            max_wt = max(weight),
            lower_wt = range(weight)[1],
            upper_wt = range(weight)[2],
            range_wt = range(weight)[2] - range(weight)[1])

#missing values
dat1 <- c(NA, 12, 76, 34, 23)
mean(dat1, na.rm = TRUE)

  


            