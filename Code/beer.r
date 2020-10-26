library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(sysfonts)
library(tidytext)
# tidytext is called for reorder within 
library(tidycensus)
library(patchwork)
library(ggtext)
library(ggfittext)

# pulling in fonts
font_add_google(name="Crimson Text",family="Crimson Text")
font_add_google(name="BioRhyme",family="BioRhyme")
font_add_google(name="Space Mono",family="Space Mono")

showtext_auto()

# reading in dataset
tuesdata <- tt_load(2020,week=43)

beer_awards <- tuesdata$beer_awards

state_pops <- get_estimates(geography = "state", 
                            year = 2019, variables = "POP") 

state_fulls <- tibble("NAME" = state.name,
                      "state" = state.abb,
                      "region" = state.region)

# which states have taken home most medals by year, past 10 years

initial_tidy <- beer_awards %>% 
  group_by(state,year) %>% 
  count(sort=TRUE) %>%
  rename(medals_awarded = "n") %>% 
  ungroup() %>% 
  group_by(year) %>% 
  filter(year>2014) %>% 
  left_join(state_fulls,by="state")

# for plot 1
top_states_by_year <- initial_tidy %>% 
  slice_max(medals_awarded,n=5) 

# for plot 2
top_states_per_capita <- initial_tidy %>% 
  left_join(state_pops,by="NAME") %>% 
  mutate(medals_per_capita = medals_awarded/value) %>%
  slice_max(medals_per_capita,n=5)

# region count for subtitle
region_total <- initial_tidy %>% 
  group_by(region) %>% 
  count(wt=medals_awarded)

# setting theme
beer_themes <- theme_minimal() + 
  theme(
    strip.text = element_text(size=18,
                              family = "Crimson Text",
                              face = "bold",
                              color = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "#353531",color=NA),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "Space Mono",color="white",size = 14),
    plot.title = element_text(
      size = 20,
      family = 'BioRhyme',
      face = "bold",
      hjust = .5,
      margin = margin(10,0,0,0),
      color = "white"))

plot_1 <- ggplot(top_states_by_year,aes(reorder_within(NAME,medals_awarded, year)))+
  geom_col(aes(y=medals_awarded,fill=region),width=.4)+
  geom_bar_text(aes(y=medals_awarded),place="right",
                contrast = TRUE,
                fullheight = FALSE,
                size =10) +
  scale_fill_manual(values = c("#E6AB02","#7570B3","#D95F02","#1B9E77")) + 
  coord_flip()+
  facet_wrap(~year, scales = "free_y",nrow=5)+
  # scale_x_reordered from tidytext package
  scale_x_reordered() +
  labs(title = "Total Medals")+
  beer_themes

plot_2 <- ggplot(top_states_per_capita,aes(reorder_within(NAME,medals_per_capita, year)))+
  geom_col(aes(y=medals_per_capita,fill=region),width=.4)+
  scale_fill_manual(values = c("#E6AB02","#1B9E77")) + 
  coord_flip()+
  facet_wrap(~year, scales = "free_y",nrow=5)+
  scale_x_reordered()+
  labs(title = "Total Medals Per Capita")+
  beer_themes

plot_1 + plot_2 + 
  plot_annotation(title = "Great American Beer Festival Competition",
                  subtitle = "From 2015-2020, the <b style='color:#1B9E77'>West</b>has 
                  won 771 medals, both the <b style='color:#D95F02'>North Central</b> and 
                  <b style='color:#7570B3'>South</b> have won 171 medals, and the 
                  <b style='color:#E6AB02'>Northeast</b>has <br> won 107 medals. Colorado and 
                  Oregon are consistently in the top 5, based on total medals AND total medals
                  per capita.",
                  caption = "Visualization: Bill Schmid @schmid_07 | Source: Great American Beer Festival",
                  theme = theme(
                    plot.title = element_text(
                      size = 50,
                      face = 'bold',
                      family = 'BioRhyme',
                      hjust = .5,
                      color = "white",
                      margin = margin(50,0,0,0)),
                    plot.subtitle = element_markdown(
                      margin = margin(5,0,30,0),
                      family = 'Crimson Text',
                      face = 'italic',
                      hjust = .5,
                      size =20,
                      color="white"),
                    plot.caption = element_text(
                      size = 15,
                      color = 'white',
                      family = 'Crimson Text'),
                    plot.background = element_rect(fill = "#353531",color="NA")))

ggsave("beer.png")