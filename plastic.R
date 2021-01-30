library(tidyverse)
library(Rtsne)
library(ggrepel)

set.seed(42)

url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv"
plastics <- readr::read_csv(url)

plastics_clean <- plastics %>% 
  select(-empty) %>% 
  dplyr::filter(country != "EMPTY") %>% 
  mutate(country = str_to_title(country),
         country = str_replace_all(country, "United Kingdom.*", "UK"),
         country = str_replace_all(country, "United States Of America", "USA"),
         country = str_replace_all(country, "United Arab Emirates", "UAE"),
         country = str_replace_all(country, ".*China.*", "China"))

plastic_long <- plastics_clean %>% 
  pivot_longer(cols = -c(country, year, parent_company, volunteers, num_events),
               names_to = "parameters", 
               values_to = "values")

z <- plastic_long %>% 
  group_by(country, parameters) %>% 
  summarise(total = sum(values), 
            volunteers = mean(volunteers),
            brands = n()) %>% 
  replace_na(list(total = 0)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = parameters, values_from = total)

z1 <- Rtsne(z %>% 
              select(-c(brands, country, volunteers)) %>% 
              as.matrix() %>% 
              normalize_input() %>% 
              dist(), 
            perplexity = 10,
            theta = 0.0)
tibble(x = z1$Y[,1], 
       y = z1$Y[,2], 
       label = z$country, 
       volunteers = z$volunteers %>% log(),
       brands = z$brands) %>% 
  ggplot(aes(x, y, label = label)) + 
  geom_point(aes(fill = volunteers), 
             colour = "transparent", 
             shape=21, 
             size=5,
             alpha = 0.5,
             show.legend = F) + 
  geom_text_repel(aes(col = brands), 
                  family = "Ink Free", 
                  size = 8, 
                  show.legend = F) +
  scale_color_gradient(low = "darkorange", 
                        high = "firebrick4") +
  scale_fill_gradient(low = "lightblue1", high = "darkblue") +
  theme_void() +
  theme(panel.background = element_rect(fill = "snow1")) +
  annotate(geom = "text", 
           label = "t-sne plot\nof plastics\ndata",
           x = 25, y = 15,
           size = 20,
           family = "Ink free",
           colour = "darkolivegreen") +
  annotate(geom = "label", 
           label = "darker labels indicate more\nbrands of plastic",
           x = -30, y = -14,
           size = 8,
           family = "Ink free",
           colour = "firebrick4") +
  annotate(geom = "label", 
           label = "darker circles indicate more\nvolunteers",
           x = -30, y = -22,
           size = 8,
           family = "Ink free",
           colour = "darkblue")
  

# plastic_long %>% ggplot(aes(values, col = parameters)) + 
#   stat_density(geom = "line", position = "identity", size = 2) + 
#   scale_x_log10() +
#   scale_y_log10() +
#   theme_minimal() +
#   theme(axis.text = element_blank()) +
#   scale_color_viridis_d(option = "magma") +
#   coord_cartesian(ylim = c(0.00001, 1))
