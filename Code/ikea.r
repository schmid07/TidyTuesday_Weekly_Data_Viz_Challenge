library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(ggtext)
library(sysfonts)
library(glue)
library(treemap)
install.packages("treemapify")
library(treemapify)

font_add_google(name="Noto Sans",family="Noto Sans")
font_add_google(name="Poppins",family="Poppins")

showtext_auto()

tidy_year <- 2020
tidy_week <- 45

tuesdata <- tt_load(tidy_year,week=tidy_week)

df_ikea_orig <- tuesdata$ikea

df_ikea_orig %>% distinct(item_id,category)

df_ikea_count <- df_ikea_orig %>% 
  add_count(category) %>% 
  add_tally() %>% 
  mutate(perc_total = round(n/nn*100,1),
         cat_label = glue("{category} 
                          (N={n}) ({perc_total}%)")) %>% 
  select(n,cat_label) %>% 
  distinct(cat_label,.keep_all=TRUE)

ggplot(df_ikea_count,aes(area = n,label=cat_label,fill=n))+
  geom_treemap(linetype = "solid",color="black",lwd=4)+
  geom_treemap_text(color="white",
                    reflow = T,
                    place = "centre",
                    size=18,
                    fontface = "bold",
                    min.size = 0,
                    family = 'Noto Sans')+
  scale_fill_gradient(low = "#24CEA6", high = "#003399") + 
  labs(title = "IKEA",
       subtitle = "PRODUCTS PER CATEGORY",
       caption = "Visualization: Bill Schmid @schmid_07 | Source: Kaggle")+
  theme(
    legend.position = "none",
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



