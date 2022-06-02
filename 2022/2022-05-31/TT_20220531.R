# Tidy Tuesday Week 13 - College Sports Equity
# 2022-03-29 
# The purpose of this week's project is to show the differences in spending for sports that have both men's and women's teams

# Libraries
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(scales)

font_add_google("Merriweather")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Reading data and wrangling
sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

sports_exp <- sports %>%
  filter(!str_detect(sports, 'Track and Field')) %>% 
  group_by(sports) %>%
  mutate(diff_exp = exp_men - exp_women) %>%
  summarise(diff_mean = mean(diff_exp, na.rm = TRUE)) %>%
  mutate(pos = diff_mean >= 0) %>%
  na.omit()

# Plot 
ggplot(sports_exp, aes(x = diff_mean, y = reorder(sports, -diff_mean), fill = pos)) +
  geom_col(position = "identity") +
  scale_x_continuous(labels = comma) +
  theme_bw() +
  theme(text=element_text(family="Merriweather"),
    axis.text = element_text(size = 12),
    legend.position = "top",
    legend.justification = "left",
    plot.caption.position="plot",
    plot.caption=element_text(hjust=0, size=8.3, color="grey20"),
    plot.title.position="plot",
    plot.subtitle = element_text(size=12),
    plot.margin = margin(t = 20,  
                         r = 50,  
                         b = 40,  
                         l = 20)) +
  scale_fill_manual(name = "", 
                    labels = c("Women's team spends more", "Men's team spends more"), 
                    values = met.brewer("Greek", 2)) + 
  labs(x="Annual Spending Difference (in USD)",
       y = "",
       caption="\n#TidyTuesday week 13 | Data from Equity in Athletics Data Analysis | @laganzar",
       title="College spending differential for sports that have both men's and women's teams",
       subtitle="Gymnastics has the largest differential in favor of women, while basketball spends more on men than women")
ggsave("plot.png", height = 8, width = 12, bg="white")

