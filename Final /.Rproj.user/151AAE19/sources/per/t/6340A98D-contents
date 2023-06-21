library(purrr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(stringr)
require(maps)
require(viridis)
library(plotly)

covid19 = read.table("COVID19.txt", header=T, sep="\t")
covid19$Sex %<>% as.factor
covid19$AgeGroup %<>% as.factor
covid19$COVIDProp[652] <- 0
covid19$InfluenzaProp[652] <- 0
covid19$PneumoniaProp[652] <- 0




covid19 %<>% filter(Sex == "Male",AgeGroup == "85 years and over")

states_map <- map_data("state")
states_map %<>% rename(State = region)
states_map$State %<>% str_to_title()

covid19_map <- left_join(states_map,covid19,by = "State")

covid19_map_plot <- ggplot(covid19_map,aes(long,lat,group = group))+ 
  geom_polygon(aes(fill = COVIDProp,text = State),color = "white")+
  scale_fill_viridis_c(option = "C")

covid19_map_plot %<>% ggplotly
covid19_map_plot

influenza_map_plot <- ggplot(covid19_map,aes(long,lat,group = group))+
  geom_polygon(aes(fill = InfluenzaProp,text = State),color = "white")+
  scale_fill_viridis_c(option = "C")

influenza_map_plot %<>% ggplotly
influenza_map_plot

pneumonia_map_plot <- ggplot(covid19_map,aes(long,lat,group = group))+
  geom_polygon(aes(fill = PneumoniaProp,text = State),color = "white")+
  scale_fill_viridis_c(option = "C")
pneumonia_map_plot %<>% ggplotly
pneumonia_map_plot




response_values <- c(rep(covid19$Sex %>% levels %>% extract(1),7),rep(covid19$Sex %>% levels %>% extract(2),7)) %>% 
         map2(rep(covid19$AgeGroup %>% levels,2),~covid19 %>%
         filter(Sex == .x,AgeGroup == .y) %>% extract2("COVIDProp")) 

factor_means <- c(rep(covid19$Sex %>% levels %>% extract(1),7),rep(covid19$Sex %>% levels %>% extract(2),7)) %>% 
         map2(rep(covid19$AgeGroup %>% levels,2),~covid19 %>%
         filter(Sex == .x,AgeGroup == .y) %>% extract2("COVIDProp") %>% mean) %>% unlist

fitted_values <- c(1:14) %>% map2(c(rep(52,14)),~rep(factor_means[.x],.y)) 

residuals <- response_values %>% map2(fitted_values,~.x - .y)

