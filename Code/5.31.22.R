library(tidyverse)
library(tidytuesdayR)
library(ggrepel)
library(ggpubr)
library(tidyquant)


tuesdata <- tidytuesdayR::tt_load('2022-05-31')

reputation <- tuesdata$reputation
poll <- tuesdata$poll



left_join(reputation, poll, by = c("company", "industry")) %>% 
    select(-c(change:rq)) %>% 
    set_names(c("company", "industry", "category", "score_by_category", 
                "rank_by_category", "rank_overall", "score_overall")) %>%
    filter(rank_overall %in% c(1:10)) %>% 
    ggplot(aes(category, score_by_category, group = company, color = company))+
    geom_line(aes(linetype = company %in% c("Trader Joe's", "HEB Grocery")), size = 1) +
    scale_linetype_manual(values = c("TRUE" = "solid","FALSE" = "dotted")) +
    guides(linetype = "none")+
    geom_point(size = 3)+
    annotate(geom = "label", label = "Highest Ranked Overall:\nTrader Joe's", x = 2.5, y = 84, size = 3.5)+
    geom_curve(aes(x = 2.7, y = 83.7, xend = 3.3, yend = 83.3,color = "Trader Joe's"),
               size = 0.7,lty=1,curvature = 0.2,
               arrow = arrow(length = unit(0.03, "npc")))+
    
    annotate(geom = "label", label = "Second Highest Ranked Overall:\nHEB Grocery", x = 6, y = 85, size = 3.5)+
    geom_curve(aes(x = 6.2, y = 84.7, xend = 6, yend = 84,color = "HEB Grocery"),
               size = 0.7,lty=1,curvature = 0.2,
               arrow = arrow(length = unit(0.03, "npc")))+
    scale_y_continuous(label = scales::comma)+
    theme_pubclean()+
    theme(legend.position = "off")+
    labs(caption="Source: Axios and Harris Poll\n\n#TidyTuesday\nVisualization by Noah Silver - @noahsilver12",
         y=NULL, x = NULL,
         title="2022 Axios-Harris Poll: Top 10 Highest Ranked Companies Overall",
         subtitle = "Company scores by reputation category, based on nationally representative survey of 33,096 Americans conducted in Spring 2022")


