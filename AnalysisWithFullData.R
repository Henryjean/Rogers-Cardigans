# Load packages
library(tidyverse)
library(extrafont)
library(magick)
library(waffle)
library(readxl)

# Read in the data and clean up some of the color codes
df <- read.csv("caridgancolors.csv")
df$colorcode <- gsub("\\#", "", df$colorcode)
df$colorcode <- ifelse(!is.na(df$colorcode), paste0("#", df$colorcode), NA)
df$colorcode <- ifelse(df$colorcode == "#NA", NA, df$colorcode)
df <- df %>% arrange(episodenumber) %>%  mutate(id = row_number())


#Custom theme 
theme_owen <- function () { 
  theme_minimal(base_size=12, base_family="BellTopo Sans") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}



# Barcode Chart Code ------------------------------------------------------



p <- df %>% 
  ggplot(aes(x=as.factor(id))) + 
  geom_bar(aes(fill = colorcode), width = .95, alpha = 1) + 
  scale_fill_identity() +
  ylim(0,1) +
  labs(fill = "", x= "", 
       title = "The Many Colors of Mister Rogers Cardigans", 
       subtitle = "Cardigan colors presented in order of apperance", 
       caption = "") +
  theme_owen() + 
  guides(fill=guide_legend(ncol=2)) + 
  scale_x_discrete(breaks = c(20,  745),
                   labels = c("1969",  "2001")) + 
  theme(axis.title.y=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(), 
        legend.position = 'none', 
        panel.grid.major = element_blank(),
        plot.title.position = 'plot', 
        plot.title = element_text(size = 16, face = 'bold'), 
        plot.subtitle = element_text(size = 8, face = 'italic'))  + 
  coord_fixed(ratio = 150, clip = 'off')


p <- cowplot::ggdraw(p) + 
  theme(plot.background = element_rect(fill="floralwhite", color = NA))

ggsave("BarcodeChart.png", p, width = 6, height = 2.75)
footy <- image_read("footer.png")
graf <- image_read("BarcodeChart.png")
image_composite(graf, footy, offset = "+0+770") %>% image_write("BarcodeChart.png")



# Bar Chart Code ----------------------------------------------------------



tmp <- df %>% group_by(colorcode) %>% summarise(tot_cc = n())

p <- tmp %>% filter(!is.na(colorcode)) %>% 
  ggplot(aes(reorder(colorcode, tot_cc), tot_cc, fill  = colorcode)) + 
  geom_bar(width = .75, color = 'gray40', stat = 'identity', size = .25) + 
  geom_text(aes(reorder(colorcode, tot_cc), tot_cc, label = tot_cc), 
            nudge_y = 2.5, size = 3, 
            family = "BellTopo Sans")  +
  scale_fill_identity() + 
  theme_owen() +
  coord_flip() + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        plot.title = element_text(size = 16, face = 'bold'), 
        plot.subtitle = element_text(size = 9, face = 'italic'),
        plot.title.position = 'plot', 
        plot.margin = margin(5.5,5.5,15,5.5, 'pt')) + 
  scale_y_continuous(breaks = seq(0, 100, 10), expand = expansion(mult = c(0, .1)))  + 
  scale_x_discrete(breaks = NULL) + 
  labs(x = " ", 
       y = "Episode Appearances", 
       title = "The Many Colors of Mister Rogers Cardigans", 
       subtitle = "Total number of times each cardigan color appeared in an episode between 1969 and 2001")

ggsave("BarChart.png", p,  width = 6, height = 6, dpi = 300)
footy <- image_read("footer.png")
graf <- image_read("BarChart.png")
image_composite(graf, footy, offset = "+0+1745") %>% image_write("BarChart.png")



# Waffle Chart Code -------------------------------------------------------



p <- df %>% group_by(year, colorcode) %>% tally() %>% 
  ggplot(aes(fill = colorcode, values = n)) + 
  geom_waffle(color = "black", size=.15, n_rows = 5, flip = T) +
  scale_fill_identity() +
  facet_wrap(~year, nrow = 3, strip.position = 'bottom') + 
  theme_owen() +
  coord_equal(clip  = 'off') +
  scale_x_discrete(expand=c(0,0))  +
  scale_y_continuous(breaks =  NULL, expand = c(0,0)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        plot.title = element_text(size = 16, face = 'bold'),
        plot.subtitle = element_text(size = 9, face = 'italic', margin=margin(b = 30, unit = "pt")), 
        plot.title.position = 'plot', 
        plot.margin = margin(15,5.5,15,5.5, 'pt'))  + 
  labs(x = " ", 
       y = "Episode Appearances", 
       title = "The Many Colors of Mister Rogers Cardigans", 
       subtitle = "Each square represents the color of the cardigan worn in one episode that year")

p <- cowplot::ggdraw(p) + 
  theme(plot.background = element_rect(fill="floralwhite", color = NA))

ggsave("waffleChart.png", p,  width = 6, height = 6, dpi = 300)
footy <- image_read("footer.png")
graf <- image_read("waffleChart.png")
image_composite(graf, footy, offset = "+0+1745") %>% image_write("waffleChart.png")

