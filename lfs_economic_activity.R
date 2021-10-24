library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(Cairo)
library(extrafont)
library(magick)
library(ggtextures) # for "geom_isotype_bar" function
library(forcats)

# See default fonts
windowsFonts()

# Import extra fonts in "extrafont" package
font_import()

# Load fonts
loadfonts(device = "win")

# Import Data
lfs_2014_2021 <- read_xls("lfs_2014_2021.xls")
lfs_2014_2021$year <- as.factor(lfs_2014_2021$year)

# Data Wrangling
lfs_economic_activity <- lfs_2014_2021 %>% 
  select(year, month, agriculture_n:services_n) %>% 
  pivot_longer(cols = agriculture_n:services_n, names_to = "economic_activity", values_to = "n")

lfs_2021_aug <- lfs_2014_2021 %>% 
  filter(year == "2021", month %in% c(5, 6, 7, 8)) %>% 
  select(year, month, agriculture_n:services_n) %>% 
  pivot_longer(cols = agriculture_n:services_n, 
               names_to = "economic_activity",
               values_to = "population") %>% 
  mutate(image = list(image_read("agriculture.png"), 
                      image_read("industry.png"),
                      image_read("constraction.png"),
                      image_read("services.png"),
                      image_read("agriculture.png"), 
                      image_read("industry.png"),
                      image_read("constraction.png"),
                      image_read("services.png"),
                      image_read("agriculture.png"), 
                      image_read("industry.png"),
                      image_read("constraction.png"),
                      image_read("services.png"),
                      image_read("agriculture.png"), 
                      image_read("industry.png"),
                      image_read("constraction.png"),
                      image_read("services.png")),
         
         economic_activity = case_when(economic_activity == "industry_n" ~ "Industry",
                                       economic_activity == "agriculture_n" ~ "Agriculture",
                                       economic_activity == "constraction_n" ~ "Constraction",
                                       economic_activity == "services_n" ~ "Services"),
         
         month = case_when(month == "5" ~ "May",
                           month == "6" ~ "June",
                           month == "7" ~ "July",
                           month == "8" ~ "August"))

# Draw Plot
ggplot(lfs_2021_aug, aes(x = economic_activity, y = population / 1000, image = image)) +
  facet_wrap(~ fct_relevel(month, "May", "June", "July", "August")) +
  geom_isotype_bar(img_width = grid::unit(0.55, "native"),
                   img_height = grid::unit(1.8, "native"),
                   fill = c("red", "blue", "orange", "black",
                            "red", "blue", "orange", "black",
                            "red", "blue", "orange", "black",
                            "red", "blue", "orange", "black"),
                   stat = "identity",
                   width = 0.6) +
  geom_text(aes(label = population,
                vjust = -1),
            size = 9,
            family = "Stencil",
            colour  = c("red", "blue", "orange", "black",
                        "red", "blue", "orange", "black",
                        "red", "blue", "orange", "black",
                        "red", "blue", "orange", "black"),
  ) +
  labs(title = "Economic Activity Status in Turkey",
       subtitle = "(May, June, July, August 2021 Labour Force Survey - Population in thousands)",
       caption = "Source: www.tuik.gov.tr"
  ) + 
  xlab("") + 
  ylab("") +
  ylim(0, 18) +
  theme(
    text = element_text(family = "Stencil"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 36, colour = "#FF4500"),
    plot.subtitle = element_text(hjust = 0.5, size = 24, colour = "#FF4500", face = "bold.italic"),
    plot.caption = element_text(size = 24, colour = "#FF4500", face = "bold.italic"),
    legend.position = "none",
    panel.background = element_rect(fill = "#F5F5F5"),
    plot.background = element_rect(fill = "#00868B"),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 18, face = "bold", color = c("red", "orange", "blue", "black")),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    strip.background = element_rect(fill = "#66CDAA"),
    strip.text = element_text(size = 24, face = "bold.italic", colour = "#FAFAD2")
  )

# Save
ggsave("lfs_economic_activity.png",
       width = 50,
       height = 30,
       units = "cm",
       dpi = 500
      )


