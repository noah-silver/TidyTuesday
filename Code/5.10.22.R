library(tidytuesdayR)
library(tidyverse)
library(lubridate)
library(tidyquant)
library(ggsankey)
library(ggalluvial)

nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')


nyt_titles %>% 
    arrange(-total_weeks) %>% 
    head(n=20) %>% 
    make_long(title, total_weeks, debut_rank, best_rank, author) %>% 
    ggplot(aes(x=x,
               node=node,
               next_x=next_x,
               next_node=next_node,
               fill = factor(node))) +
    geom_sankey(flow.alpha = 0.75, node.color = 1) +
    scale_fill_viridis_d(option = "D", alpha = 0.95) +
    theme_tq()+
    theme(
        legend.position="none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )+
    scale_x_discrete(labels = c("Title", "Total Weeks", "Debut Rank", "Best Rank", "Author"))+
    labs(
        title = "Top 20 NYT Bestsellers",
        subtitle=  "Sankey Diagram detailing weeks on the NYT Bestseller List, debut rank,\nand best rank by book for the Top 20 NYT Bestsellers",
        caption=
            "
        Source: Post45 Data \n#TidyTuesday\nVisualization by Noah Silver - @noahsilver12 ",
        x=NULL
    )

    

ggsave("nytplot.png", plot = last_plot())
