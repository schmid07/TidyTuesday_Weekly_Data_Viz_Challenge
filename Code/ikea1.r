library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(ggtext)
library(glue)
library(treemapify)
library(ggimage)
library(here)

font_add_google(name="Noto Sans", family="Noto Sans")
showtext_auto()

tidy_year <- 2020
tidy_week <- 45

tuesdata <- tt_load(tidy_year, week=tidy_week)

df_ikea_orig <- tuesdata$ikea

df_ikea_count <- df_ikea_orig %>% 
  add_count(category) %>% 
  add_tally() %>% 
  mutate(perc_total = round(n / nn * 100, 1),
         cat_label = glue("{category} 
                          (N={n}) ({perc_total}%)")) %>% 
  select(n, cat_label) %>% 
  distinct(cat_label, .keep_all=TRUE)



img <- c(here("img", "stick.png"),
         here("img", "stick1.png"),
         here("img", "shopping_cart.png"),
         here("img", "shopping_cart1.png"))
         
image_df <- 
  tribble(
    ~x,  ~y, ~figure, ~size,
    .6,  .83,  img[3], .035,
    .63, .83,  img[1], .015,
    .9,  .83,  img[3], .035,
    .93, .83,  img[1], .015,
    .28, .73,  img[3], .035,
    .31, .73,  img[1], .015,
    .93, .95,  img[3], .035,
    .96, .95,  img[1], .015,
    .65, .34,  img[3], .035,
    .68, .34,  img[1], .015,
    .44, .4,   img[2], .03,
    .44, .45,  img[4], .03
  )

ggplot(df_ikea_count, aes(area = n, label = cat_label, fill=n))+
  # need to set limits otherwise images will always be in middle
  coord_cartesian(xlim = c(0, 1),ylim = c(0, 1)) +
  geom_treemap(linetype = "solid", color = "white", lwd = 50)+
  geom_treemap_text(color="white",
                    place = "centre",
                    size = 22,
                    min.size = 22,
                    fontface = "bold",
                    family = 'Noto Sans')+
  scale_fill_gradient(low = "#24CEA6", high = "#003399") + 
  geom_image(data=image_df,aes(x = x,y = y,image = figure),
             size=image_df$size, asp = 2, inherit.aes = FALSE) + 
  labs(title = "IKEA",
       subtitle = "PRODUCTS PER CATEGORY",
       caption = "Visualization: Bill Schmid @schmid_07 | Source: Kaggle") +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#FFCC00", color=NA),
    plot.margin = margin(t= 25, r = 25, b = 25, l = 25),
    plot.caption = element_text(
      size = 12,
      color = "#003399",
      family = 'Noto Sans',
      face = "bold",
      margin = margin(t = 20)),
    plot.subtitle = element_text(
      size = 35,
      color = "#003399",
      family = 'Noto Sans',
      face = "bold",
      hjust = .5,
      margin = margin(b = 20)),
    plot.title = element_text(
      size = 70,
      color = "#003399",
      family = 'Noto Sans',
      face = "bold",
      hjust = .5,
      margin = margin(t = 10, b = 20)))

path <- here::here("plots", 
                   glue::glue(tidy_year, "_", tidy_week, "_with_images", ".png"))

ggsave(path, width = 20, height = 12)


# experimenting with treemap package
# tm <- treemap::treemap(df_ikea_count,vSize="n",index="cat_label")
# tm <- tm$tm 
# tm_plot <- tm %>% 
#   mutate(x1=x0+w,
#          y1=y0+h,
#          x = (x0+x1)/2,
#          y = (y0+y1)/2)
# 
# ggplot(tm_plot,aes(xmin = x0,ymin=y0,xmax=x1,ymax=y1,label=cat_label))+
#   geom_rect(aes(size=vSize),fill="#003399",color="white")+
#   geom_richtext(aes(x=x,y=y),label.size=NA)+
#   labs(title = "IKEA",
#        subtitle = "Products per Category",
#        caption = "Visualization: Bill Schmid @schmid_07 | Source: Kaggle")+
#   theme_void()+
#   theme(
#     legend.position = "none",
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     axis.ticks = element_blank(),
#     plot.background = element_rect(fill = "#FFCC00",color=NA),
#     plot.margin = margin(25,25,25,25),
#     plot.caption = element_text(
#       size = 12,
#       color = "#003399",
#       family = 'Noto Sans',
#       face = "bold",
#       margin = margin(20,0,0,0)),
#     plot.subtitle = element_text(
#       size = 35,
#       color = "#003399",
#       family = 'Noto Sans',
#       face = "bold",
#       hjust = .5,
#       margin = margin(0,0,20,0)),
#     plot.title = element_text(
#       size = 70,
#       color = "#003399",
#       family = 'Noto Sans',
#       face = "bold",
#       hjust = .5,
#       margin = margin(10,0,20,0)))

# df_ikea_count <- df_ikea_orig %>% 
#   add_count(category) %>% 
#   add_tally() %>% 
#   mutate(perc_total = round(n / nn * 100, 1),
#          cat_label = glue("{category} <span style='font-size:15pt;color:#FFCC00;'>
#                           (N={n}) ({perc_total}%)</span>")) %>% 
#   select(n, cat_label) %>% 
#   distinct(cat_label, .keep_all=TRUE)
