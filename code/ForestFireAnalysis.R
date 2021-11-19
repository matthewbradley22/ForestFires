library(readr)
library(tidyverse)

forestFires <- read_csv("/Users/matthewbradley/Downloads/forestfires.csv")
View(forestFires)


forestFires %>% select_if(is.numeric) %>% cor %>% 
  as.data.frame() %>% rownames_to_column() %>% 
  pivot_longer(-1) %>% 
  ggplot(aes(rowname, name, fill = value))+
  geom_tile()+
  geom_text(aes(label=round(value, digits = 2)))+
  scale_fill_gradient2(low = "white", mid = "gold", high = "firebrick") 


forestFires %>% select_if(is.numeric) %>% 
  pivot_longer(1:10, names_to = "key") %>% 
  ggplot(aes(value, area))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~key, scales = "free")




forestFires %>% select_if(is.numeric) %>% 
  pivot_longer(1:10, names_to = "key") %>% 
  mutate(area = log(area  + 1, base =10))%>% 
  ggplot(aes(value, area))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~key, scales = "free")
