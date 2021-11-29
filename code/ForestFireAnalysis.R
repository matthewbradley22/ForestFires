library(readr)
library(tidyverse)
library(Hmisc)


forestFires <- read_csv("/Users/matthewbradley/Downloads/forestfires.csv")
View(forestFires)

#Look for any outliers
summary(forestFires)

#Are months/days important?

ggplot(data = forestFires, aes(x = month, y = area))+
  geom_bar(stat = "identity")+
  ggtitle("Total area burned in each month")

ggplot(data = forestFires, aes(x = day, y = area ))+
  geom_bar(stat = "identity")+
  ggtitle("Total area burned in each day")

#Histogram 
hist(forestFires[-c(3,4,13)], main = "Independent Variable Distributuions")

#Normalize area data
hist((forestFires$area), main = "Forest Fire Area", xlab = "Hectares (2.47 acres)")
hist(log(forestFires$area + 1), main = "Forest Fire Area", xlab = "ln(Hectares (2.47 acres))")

#Correlation matrix
forestFires %>% select_if(is.numeric) %>% cor %>% 
  as.data.frame() %>% rownames_to_column() %>% 
  pivot_longer(-1) %>% 
  ggplot(aes(rowname, name, fill = value))+
  geom_tile()+
  geom_text(aes(label=round(value, digits = 2)))+
  scale_fill_gradient2(low = "white", mid = "gold", high = "firebrick") 

#Scatter Plots
forestFires %>% select_if(is.numeric) %>% 
  pivot_longer(1:10, names_to = "key") %>% 
  ggplot(aes(value, area))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~key, scales = "free")
  


#Scatter plots of log data
forestFires %>% select_if(is.numeric) %>% 
  pivot_longer(1:10, names_to = "key") %>% 
  mutate(area = log(area  + 1))%>% 
  ggplot(aes(value, area))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~key, scales = "free")+
  ylab("area (ln(hectares))")
