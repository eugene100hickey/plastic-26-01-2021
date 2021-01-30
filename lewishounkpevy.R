library(tidyverse)
library(sf)
library(patchwork)
#### Init ####
source("lewishounkpevy-source.R")
library("rnaturalearth")
library("rnaturalearthdata")
world <- ne_countries(returnclass = 'sf')

theme_lewis <- function (coulprinc =  "#022873", coulsec = "#A79674", 
                         base_size = 12, legend.direction = "horizontal", 
                         legend.position = "bottom") 
{
  theme_bw(base_family = "Arial", base_size = base_size) + 
    theme(axis.ticks = element_blank(), 
          axis.text = element_text(face = "bold", size = rel(0.5)),
          axis.title.x = element_text(face = "bold",
                                      size = rel(2/3), hjust = 0.5, vjust = 0.5,
                                      colour = coulprinc,    
                                      margin = margin(b = 0.5, t = 0.5, unit = "cm")), 
          axis.title.y = element_text(face = "bold", 
                                      size = rel(2/3), hjust = 0.5, vjust = 0.5,
                                      colour = coulprinc, 
                                      margin = margin(r = 0.5, unit = "cm")), 
          title = element_text(face = "plain", hjust = 0.5), 
          plot.title = element_text(size = base_size, face = "bold", 
                                    colour = coulprinc, vjust = 1), 
          plot.subtitle = element_text(size = rel(2/3), face = "bold", 
                                       colour = coulsec, vjust = 1, hjust = 0.01,
                                       margin = margin(b = 0.5, unit = "cm")), 
          plot.caption = element_text(size = rel(0.5), face = "plain", 
                                      colour = coulsec, vjust = 0.5, hjust = 0), 
          legend.text = element_text(face = "bold", rel(3/5), colour = coulsec), 
          legend.direction = legend.direction, 
          legend.position = legend.position, 
          legend.title = element_blank(), 
          panel.grid.major = element_line(linetype = "solid", 
                                          colour = "#E7E9EA", size = 0.5),
          panel.border = element_rect(fill = NA, colour = "#E7E9EA"),
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = NA, colour = NA, size = 0.5))
}

#### Get the Data ####
plastics <- tidytuesdayR::tt_load(2021, week = 5)$plastics

tidyplastics <- plastics %>%
  select(-parent_company) %>% 
  group_by(country, year, num_events, volunteers) %>% 
  summarise(across(empty:grand_total, sum, na.rm = TRUE), 
            .groups = "drop") 


#### plots ####

plasticstarrynight <- tidyplastics %>% 
  ggplot() + 
  aes(x = grand_total , y = country, 
      color = as.factor(year)) +
  geom_text(label="★", size = 5, family = "HiraKakuPro-W3") +
  # geom_point() +
  xlim(0, 10000) +
  scale_color_manual(values = c("#440154FF","#FDE725FF")) +
  labs(title = "Plastic Quantity Per Country",
       x = "Plastic Quantity",
       y = NULL,
       subtitle = "Starry Night 😇",
       caption = "Source : Break Free from Plastic
       \nTidyTuesday Plastic Pollution
       \nby: Lewis Hounkpevi") +
  theme_lewis(legend.direction = "vertical", 
              legend.position =  "right",
              base_size = 15) +
  theme(axis.text.y = element_text(size = 5),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "#5555AA", 
                                        colour = NA))

#### Volunteers map ####

volunteers_map <- map(c(2019, 2020), function(x){world %>% 
    select(geounit, region_wb, geometry)  %>% 
    left_join(tidyplastics %>% 
                filter(year == x) %>%
                mutate(tranche = case_when(volunteers < 10 ~ "1- Less than 10",
                                           volunteers < 20 ~ "2- between 10 and 20",
                                           volunteers < 100 ~ "3- between 20 and 100",
                                           volunteers < 400 ~ "4- between 100 and 400",
                                           volunteers >= 400 ~ "5- 400 and more")),
              by = c("geounit" = "country")) %>% 
    st_as_sf(crs = 4326)}) %>% 
  map(function(x){
    ggplot(data = x) +
      geom_sf(aes(fill = tranche)) +
      scale_fill_viridis_d() +
      
      geom_sf_text(size = 2.5, aes(label = volunteers)) +
      # facet_grid( ~ year, shrink = FALSE, drop = FALSE) +
      labs( y = NULL,  x = NULL) +
      theme_lewis() +
      theme(axis.text = element_blank(),
            legend.text=element_text(size=rel(0.5)),
            axis.ticks = element_blank(),
            legend.position = "bottom",
            legend.direction = "horizontal")})


volunteers_map_patch <- (volunteers_map[[1]] +
                           volunteers_map[[2]]+
                           plot_layout(ncol = 2)) +
  plot_annotation(title = "Volunteers map per year", 
                  caption = "Source : Break Free from Plastic
       \nTidyTuesday Plastic Pollution
       \nby: Lewis Hounkpevi",
       theme = theme_lewis())


