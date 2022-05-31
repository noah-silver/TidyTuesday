library(tidytuesdayR)
library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)
library(psych)


tuesdata <- tidytuesdayR::tt_load('2022-05-24')

sevens <- tuesdata$sevens
fifteens <- tuesdata$fifteens


sevens %>% 
    filter(stage == "Final") %>% select(tournament, venue, winner) %>% 
    lapply(.,as_factor) %>% 
    lapply(.,as.integer) %>% 
    as.data.frame() %>% 
    pairs.panels(.,
                 labels = str_to_sentence(colnames(.)),  # Variable names
                 smooth = TRUE,      # If TRUE, draws loess smooths
                 col =  "#FCCC25FF" ,
                 bg = "black",
                 scale = F,      # If TRUE, scales the correlation text font
                 density = T,     # If TRUE, adds density plots and histograms
                 ellipses = T,    # If TRUE, draws ellipses
                 method = "pearson", # Correlation method (also "spearman" or "kendall")
                 pch = 21,           # pch symbol
                 lm = T,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
                 cor = T,         # If TRUE, reports correlations
                 jiggle = T,     # If TRUE, data points are jittered
                 factor = 2,         # Jittering factor
                 hist.col =  "#FCCC25FF" ,       # Histograms color
                 stars = TRUE,       # If TRUE, adds significance level with stars
                 ci = T,
                 main = "Women's Rugby: Sevens Tournament Champions Correlation Plots")



sevens %>%
    filter(stage == "Final") %>% 
    mutate(year = floor_date(date, "year") %>% year()) %>%
               mutate(winner = 
                          as_factor(winner) %>% 
                          fct_lump(n=15) %>% 
                          fct_relevel(c("Other", "France","Finland","Fiji",
                                        "Russia","South Africa", "United States", "Thailand",
                                        "China", "Japan", "Kazakhstan", "England", "Canada",
                                        "Australia", "Brazil", "New Zealand"))) %>%
    select(year, tournament, venue, winner, loser, margin) %>% 
    group_by(winner) %>% 
               summarize(total_margin = sum(margin)) %>% 
               ungroup() %>% 
    ggplot(aes(winner, total_margin))+
    geom_col(aes(fill = winner))+
    scale_y_continuous(label = scales::comma)+
    coord_flip()+
    theme_classic()+
    theme(legend.position = "off")+
    labs(title = "Women's Rugby: Sevens Tournament Champions",
         subtitle = "Top 15 Winningest Teams (1997 - Present)",
         caption = "Source: ScrumQueens\n\n#TidyTuesday\nVisualization by Noah Silver - @noahsilver12",
         y = "All-Time Margin of Victory",
         x = NULL)

ggsave("Plots/Rugby_Col.png")



