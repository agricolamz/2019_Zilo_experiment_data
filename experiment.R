setwd("/home/agricolamz/work/materials/2019.07.11-19_Zilo")
library(tidyverse)
theme_set(theme_bw())
df <- read_csv("experiment_questionary.csv")
df %>% 
  filter(`first reaction` == "+") %>% 
  count(translation, expectations) %>% 
  group_by(translation) %>% 
  mutate(ratio = n/sum(n),
         type = factor(1:length(n)),
         length = length(type)) %>% 
  ungroup() %>% 
  mutate(translation = reorder(translation, length)) %>% 
  ggplot(aes(translation, ratio, fill = type))+
  geom_col(position = "stack", show.legend = FALSE)+
  geom_text(aes(label = paste(expectations, round(ratio*100), "%")),  position = position_stack(vjust = 0.5), color = "white")+
  coord_flip()+
  labs(caption = paste(length(unique(df$speaker_id)), "спикера"),
       x = "")+
  scale_fill_hue(l=50)

# sociolinguistics --------------------------------------------------------
df %>% 
  filter(`first reaction` == "+") %>%
  mutate(age = 2019-age) %>% 
  distinct(f_id, speaker_id, sex, age, expectations) %>% 
  ggplot(aes(expectations, age, color = sex))+
  geom_jitter(width = 0.02)+
  coord_flip()+
  facet_wrap(~f_id, scale = "free")+
  labs(caption = paste(length(unique(df$speaker_id)), "спикера"),
       y = "", x = "")
  
df %>% 
  distinct(speaker_id, sex) %>% 
  count(sex) ->
  sexes

df %>% 
  mutate(age = 2019-age) %>% 
  distinct(speaker_id, sex, age) %>% 
  ggplot(aes(age, fill = sex))+
  geom_dotplot(dotsize = 1)+
  labs(caption = paste0("females: ", sexes$n[1], ", males: ", sexes$n[2]),
       y = "")+
  scale_x_continuous(breaks=1:6*10+1940)+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank())
  