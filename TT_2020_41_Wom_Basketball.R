library(tidyverse)
library(tidytuesdayR)
library(gt)
library(paletteer)
library(webshot)

webshot::install_phantomjs()

# pulling in data ---------------------------------------------------------

tuesdata <- tt_load(2020,week=41)
tournament <- tuesdata$tournament %>% 
  mutate(school=as_factor(school))

# lookup table for seed points
seed_point_table <- 
  tribble(
    ~seed, ~points,
    1, 	100,
    2, 	72.7,
    3, 	54.5,
    4,	48.5,
    5,	33.3,
    6,	33.3,
    7,	27.3,
    8,	21.2,
    9,	18.2,
    10,	18.2,
    11,	18.2,
    12,	15.2,
    13,	9.09,
    14,	6.06,
    15,	3.03,
    16,	0
  )


# wrangling ---------------------------------------------------------------

# dataset starts in 1982 and ends in 2018 so not quite 4 decades. Spans 37 years
sample_years <- 37

# seed points by decade
seed_pts <- tournament %>% 
  left_join(seed_point_table,by="seed") %>% 
  mutate(decade=floor(year/10)*10) %>%  
  group_by(school,decade) %>% 
  summarise(total_decade_pts=sum(points)) %>% 
  ungroup() %>% 
  mutate(decade_avg=case_when(
    decade<1990 ~ total_decade_pts/8,
    decade>2009 ~ total_decade_pts/9,
    TRUE ~ total_decade_pts/10 ))

# seed points top 10 overall
top_10_overall <- seed_pts %>% 
  group_by(school) %>% 
  summarise(total_overall=sum(total_decade_pts)) %>% 
  mutate(overall=total_overall/sample_years) %>% 
  slice_max(overall,n=10) %>% 
  select(-total_overall) 

# pull top 10 for filter in next section
top_10_overall_filter <- top_10_overall %>% 
  pull(school)

top_10_final <- seed_pts %>%
  filter(school %in% top_10_overall_filter) %>%
  select(-total_decade_pts) %>% 
  pivot_wider(names_from=decade,values_from=decade_avg) %>% 
  mutate(across(dplyr::everything(),~replace_na(.x,0))) %>% 
  left_join(top_10_overall,by="school") %>%
  ungroup() %>% 
  arrange(desc(overall)) %>% 
  # below mutate adds in images to the dataset
  mutate(img=paste0("https://raw.githubusercontent.com/schmid07/TT-2020-Week-41/main/img/",school,".jpg")) %>% 
  select(school,img,everything())


# creating table ----------------------------------------------------------

final_table <- top_10_final %>% 
  gt() %>% 
  text_transform(
    locations = cells_body(vars(img)),
    fn = function(x){
      web_image(url = x, height = 20)
    }) %>% 
  cols_width(
    vars(img)~px(55),
    vars(school,overall)~(px(120)),
    vars("1980","1990","2000","2010")~px(95)
  ) %>% 
  cols_align(
    columns = vars(img),
    align = "center") %>% 
  data_color(
    columns=vars("1980","1990","2000","2010",overall),
    colors = scales::col_numeric(
      palette = as.character(paletteer::paletteer_d("ggsci::green_material", n = 5)),
      domain = NULL
    )
  ) %>% 
  fmt_number(columns=vars("1980","1990","2000","2010",overall),
               decimals=1) %>% 
  tab_style(style=list(cell_borders
               (
                 sides="left",
                 color="black",
                 weight=px(3)
               )
    ),
    locations=list(
      cells_body(
        columns=vars(overall)
        )
      )
    ) %>% 
  tab_style(
    style=list(cell_borders
               (
      sides="bottom",
      color="black",
      weight=px(3)
               )
      ),
    locations=list(
      cells_column_labels(
        columns=gt::everything()
      )
    )
) %>% 
  cols_label(
    img="",
    school="SCHOOL",
    overall="OVERALL",
    "1980"="1980s",
    "1990"="1990s",
    "2000"="2000s",
    "2010"="2010s"
  ) %>% 
  tab_source_note("TABLE: @schmid_07 | ORIGINAL TABLE: FiveThirtyEight | DATA: NCAA") %>% 
  tab_header(md("**Which women's programs have been most successful during the NCAA tournament era?**"),
             "Seed points in NCAA Tournaments held for women's programs, by decade and overall since 1982"
  ) %>% 
  tab_spanner(
    label="SEED POINTS PER TOURNAMENT, BY DECADE",
    columns=3:6) %>% 
  tab_footnote(
    footnote="Seed points award a score on a 100-point scale; a No.1 seed gets 100 points, while the rest descend in proportion to the seed's expected wins during the tournament",
    locations=cells_column_labels(columns=6))

gtsave(final_table,"tt_2020_week41.png")
gtsave(final_table,"tt_2020_week41.html")
