# Tidy Tuesday Week 12 - Baby Names
# 2022-03-22
# The purpose of this week's Tidy Tuesday is to plot the popularity of the name "Tyler" over time

# Libraries
library(tidyverse)
library(tidytuesdayR)
library(glue)

# Reading data and wrangling
tuesdata <- tidytuesdayR::tt_load(2022, week = 12)
babynames <- tuesdata$babynames

babynames %>%
  filter(name == "Tyler") %>%
  ggplot(aes(x = year, y = prop)) +
  geom_line() +
  facet_wrap(~ sex)

tyler <- babynames %>%
  filter(name == "Tyler" & sex == "M") %>%
  mutate(pct_total = prop*100)

# Fonts
sysfonts::font_add_google("Comfortaa", "comfort")
showtext::showtext_auto()

# Annotate

description <- glue("Tyler is a regularly used name that rose",
                    "to prominence in the 1980s and peaked",
                    "in the year 1994, but has since declined.")

# Plotting

tyler %>%
  ggplot(aes(x = year, y = prop)) +
  geom_line(size = 1.5, color = "#ff5733") +
  geom_vline(xintercept = 1994, lty = "dashed", color = "white", size = 0.8) +
  scale_y_continuous(breaks = seq(0, 0.02, 0.005), limits = c(0, 0.02), labels = scales::percent_format()) +
  scale_x_continuous(breaks = seq(1880, 2020, 10)) +
  theme_minimal() +
  theme(plot.margin = margin(rep(15, 4)),
        plot.background = element_rect(fill = "#22333b"),
        panel.grid = element_blank(),
        plot.caption = element_text(size = 8, color = "#ff5733", margin = margin(t = 10)),
        axis.text = element_text(color = "white", size = 11),
        axis.title.y = element_text(color = "white", margin = margin(r = 10), size = 12)) +
  labs(x = element_blank(),
       y = "Percent share in the total count of child's name",
       caption = "Data: H. Wickham | Plot: LA-Ganzar | Inspired by: @Topenomics") +
  annotate(geom = "point", x = 1994, y = max(tyler$prop),
           size = 5, color = "#ff5733") +
  annotate(geom = "text", x = 1884, y = 0.018,
           label = "How popular is the \nname Tyler?",
           size = 9, family = "comfort", hjust = 0, vjust = 0,
           lineheight = 0.8, color = "#ff5733") +
  annotate(geom = "text", x = 2008, y = max(tyler$prop),
           label = "Peak year \nat 1994", 
           size = 5, lineheight = 0.8, color = "#ff5733") + 
  annotate(geom = "text", x = 1884, y = 0.015,
           label = "Tyler is a regularly used name that rose\nto prominence in the 1980s and peaked \nin the year 1994, but has since declined.", 
           size = 4.5, hjust = 0, vjust = 0, lineheight = 0.9, color = "#ff5733")
ggsave("/Users/leighannganzar/Desktop/Post-Doc/tidytuesday/tidytuesday/2022/2022-03-22/plot.png", 
       height = 6, width = 10, dpi = 300)



                  
