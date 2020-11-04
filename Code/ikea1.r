library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(ggtext)
library(sysfonts)
library(glue)
library(treemapify)
library(ggimage)

font_add_google(name="Noto Sans",family="Noto Sans")
showtext_auto()

tidy_year <- 2020
tidy_week <- 45

tuesdata <- tt_load(tidy_year,week=tidy_week)

df_ikea_orig <- tuesdata$ikea

df_ikea_count <- df_ikea_orig %>% 
  add_count(category) %>% 
  add_tally() %>% 
  mutate(perc_total = round(n/nn*100,1),
         cat_label = glue("{category} 
                          (N={n}) ({perc_total}%)")) %>% 
  select(n,cat_label) %>% 
  distinct(cat_label,.keep_all=TRUE)

ggplot(df_ikea_count,aes(area = n,label=cat_label,fill=n))+
# need to set limits otherwise images will always be in middle
  coord_cartesian(xlim=c(0,1),ylim=c(0,1)) +
  geom_treemap(linetype = "solid",color="white",lwd=50)+
  geom_treemap_text(color="white",
                    place = "centre",
                    size=22,
                    min.size = 22,
                    fontface = "bold",
                    family = 'Noto Sans')+
  scale_fill_gradient(low = "#24CEA6", high = "#003399") + 
  geom_image(aes(.6,.83,image = "shopping_cart.png"),size=.035,asp = 2) +
  geom_image(aes(.63,.83,image = "stick.png"),size=.015,asp = 2)+
  geom_image(aes(.9,.83,image = "shopping_cart.png"),size=.035,asp = 2)+
  geom_image(aes(.93,.83,image = "stick.png"),size=.015,asp = 2)+
  geom_image(aes(.28,.73,image = "shopping_cart.png"),size=.035,asp = 2)+
  geom_image(aes(.31,.73,image = "stick.png"),size=.015,asp = 2)+
  geom_image(aes(.93,.95,image = "shopping_cart.png"),size=.035,asp = 2)+
  geom_image(aes(.96,.95,image = "stick.png"),size=.015,asp = 2)+
  geom_image(aes(.65,.34,image = "shopping_cart.png"),size=.035,asp = 2)+
  geom_image(aes(.68,.34,image = "stick.png"),size=.015,asp = 2)+
  geom_image(aes(.44,.4,image = "stick1.png"),size=.03,asp = 2)+
  geom_image(aes(.44,.45,image = "shopping_cart1.png"),size=.03,asp = 2)+
  labs(title = "IKEA",
       subtitle = "PRODUCTS PER CATEGORY",
       caption = "Visualization: Bill Schmid @schmid_07 | Source: Kaggle")+
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#FFCC00",color=NA),
    plot.margin = margin(25,25,25,25),
    plot.caption = element_text(
      size = 12,
      color = "#003399",
      family = 'Noto Sans',
      face = "bold",
      margin = margin(20,0,0,0)),
    plot.subtitle = element_text(
      size = 35,
      color = "#003399",
      family = 'Noto Sans',
      face = "bold",
      hjust = .5,
      margin = margin(0,0,20,0)),
    plot.title = element_text(
      size = 70,
      color = "#003399",
      family = 'Noto Sans',
      face = "bold",
      hjust = .5,
      margin = margin(10,0,20,0)))

ggsave("ikea.png",width = 20,height = 12)







