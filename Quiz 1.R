#QUIZ 1

ls("package:datasets")
Orange <- datasets::Orange
  
Orange_1<- Orange[c(1, 2, 3, 4, 5, 6, 30, 31, 32, 33, 34, 35),]

Orange_2<- Orange %>%
  filter(Tree == "1") %>% 
  summarise(mean_circ = mean(circumference))

Orange %>%
filter(Tree == "2") %>% 
  summarise(mean_circ = mean(circumference))

Orange %>%
  filter(Tree == "3") %>% 
  summarise(mean_circ = mean(circumference))

Orange %>%
  filter(Tree == "4") %>% 
  summarise(mean_circ = mean(circumference))

Orange %>%
  filter(Tree == "5") %>% 
  summarise(mean_circ = mean(circumference))

Orange %>%
  filter(Tree == "1") %>% 
  summarise(median_circ = median(circumference))

Orange %>%
  filter(Tree == "2") %>% 
  summarise(median_circ = median(circumference)))

Orange %>%
  filter(Tree == "3") %>% 
  summarise(median_circ = median(circumference))

Orange %>%
  filter(Tree == "4") %>% 
  summarise(median_circ = median(circumference))

Orange %>%
  filter(Tree == "5") %>% 
  summarise(median_circ = median(circumference))

Orange %>% 
  filter(Tree == "1") %>% 
  summarise(SD_circ = sd(circumference))

Orange %>% 
  filter(Tree == "2") %>% 
  summarise(SD_circ = sd(circumference))

Orange %>% 
  filter(Tree == "3") %>% 
  summarise(SD_circ = sd(circumference))

Orange %>% 
  filter(Tree == "4") %>% 
  summarise(SD_circ = sd(circumference))

Orange %>% 
  filter(Tree == "5") %>% 
  summarise(SD_circ = sd(circumference))

Orange %>%
  filter(Tree == "1") %>% 
  summarise(mean_age = mean(age))

Orange %>%
  filter(Tree == "2") %>% 
  summarise(mean_age = mean(age))

Orange %>%
  filter(Tree == "3") %>% 
  summarise(mean_age = mean(age))

Orange %>%
  filter(Tree == "4") %>% 
  summarise(mean_age = mean(age))

Orange %>%
  filter(Tree == "5") %>% 
  summarise(mean_age = mean(age))

Orange %>%
  filter(Tree == "1") %>% 
  summarise(median_age = median(age))

Orange %>%
  filter(Tree == "2") %>% 
  summarise(median_age = median(age)))

Orange %>%
  filter(Tree == "3") %>% 
  summarise(median_age = median(age))

Orange %>%
  filter(Tree == "4") %>% 
  summarise(median_age = median(age))

Orange %>%
  filter(Tree == "5") %>% 
  summarise(median_age = median(age))

Orange %>% 
  filter(Tree == "1") %>% 
  summarise(SD_age = sd(age))

Orange %>% 
  filter(Tree == "2") %>% 
  summarise(SD_age = sd(age))

Orange %>% 
  filter(Tree == "3") %>% 
  summarise(SD_age = sd(age))

Orange %>% 
  filter(Tree == "4") %>% 
  summarise(SD_age = sd(age))

Orange %>% 
  filter(Tree == "5") %>% 
  summarise(SD_age = sd(age))

Orange %>% 
  summarise(Mean = mean(age),
            Standard_Deviation = sd(age),
            med_wt = median(age),
            Kurtosis= kurtosis(age),
            mean_wt= sum(age)/n(),
            sd_wt = sd(age),
            min_wt = min(age),
            qrt1_wt = quantile(age, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = median(weight, p = 0.75),
            max_wt = max(weight),
            lower_wt = range(weight)[1],
            upper_wt = range(weight)[2],
            range_wt = range(weight)[2] - range(weight)[1])

library(e1071)
kurtosis(Orange)
skewness(Orange)

quantile(Ora$nge)
quantile(chicks$weight)


