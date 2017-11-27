#Loading Required Libraries 

library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(scales)

#Load Input Data

complete_data <- read.csv(".../multipleChoiceResponses.csv",header =T, stringsAsFactors = F)


#Gender Distribution
complete_data %>% 
  filter(GenderSelect!='') %>% 
  group_by(GenderSelect) %>% 
  count() %>% 
  ggplot(aes(x = GenderSelect,y = (n / sum(n))*100))+
  geom_bar(stat = 'identity') + ylab('Percent') + theme_solarized() +
  theme(axis.text = element_text(size = 6)) + ggtitle('Gender Distribution of Kaggle Survey Respondents')

complete_data %>% filter(GenderSelect %in% c('Male','Female')) %>%
  group_by(Country,GenderSelect) %>% 
  summarise(count = n()) %>%
  arrange(desc(count)) %>% ggplot() + 
  geom_bar(aes(Country,count, fill = GenderSelect), stat = 'identity') +
  #facet_grid(.~GenderSelect)  + 
  theme_solarized() +
  theme(axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.5)) +
  ggtitle('Country wise Survey Respondends - M/F') 
  

complete_data %>% filter(GenderSelect %in% c('Male','Female') & Country!="") %>%
  group_by(Country,GenderSelect) %>% 
  summarise(count = n()) %>%
  spread(GenderSelect,count) %>% 
  mutate(F2M = (Female/Male)*100) %>% 
  arrange(desc(F2M)) %>%
  #mutate(F2M = percent(F2M)) %>% 
  ggplot() +
  geom_bar(aes(Country,F2M, fill = F2M), stat = 'identity') +
theme_solarized() +
  theme(axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) +
  ggtitle('Female to Male Ratio - Country Wise') + scale_fill_continuous_tableau()



complete_data %>% filter(GenderSelect == 'Female') %>% ggplot() +
  geom_histogram(aes(Age),binwidth = 1) + theme_solarized()


complete_data %>% filter(GenderSelect == 'Male') %>% ggplot() +
  geom_histogram(aes(Age),binwidth = 1) + theme_solarized()


complete_data %>% filter(GenderSelect %in% c('Male','Female')) %>%  
  ggplot() + geom_histogram(aes(Age),binwidth = 1) + 
  theme_solarized() + facet_grid(.~GenderSelect) +
  ggtitle('Age Distribution - Male vs Female')


complete_data %>% group_by(LanguageRecommendationSelect) %>% summarize(count = n())

complete_data %>% filter(GenderSelect %in% c('Male','Female')) %>% group_by(LanguageRecommendationSelect,GenderSelect) %>% 
  summarize(count = n()) %>%
  spread(GenderSelect,count) %>% 
  mutate(F2M = Female/Male) %>% 
  arrange(desc(F2M)) %>%
  mutate(F2M = F2M * 100) %>%  ggplot() +
  geom_bar(aes(LanguageRecommendationSelect,F2M, fill = F2M), stat = 'identity') +
  theme_solarized() + 
  theme(axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) +
  scale_fill_continuous_tableau() + ggtitle('F2M Ratio of Languages used by Kagglers')



