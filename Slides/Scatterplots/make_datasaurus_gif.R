#| message: false
#| warning: false
library(tidyverse)
library(datasauRus)
library(gganimate)
library(gifski)

# 1. Prepare Data
base_data <- datasaurus_dozen %>%
  filter(dataset %in% c("circle", "dino", "slant_down")) %>%
  mutate(dataset2 = factor(dataset) %>% as.numeric() %>% factor()) %>%
  mutate(point_id = row_number())

# CALCULATE BOUNDS: Find the edges of the data so we don't zoom out to 0
min_x <- min(base_data$x)
min_y <- min(base_data$y)

# 2. Construct the Animation States
anim_data <- bind_rows(
  # State 1: Univariate (Split)
  base_data %>% 
    mutate(state = "1. Univariate",
           anim_x = x,
           anim_y = min_y, # Collapse Y to the bottom edge of the DATA
           group_id = paste0(point_id, "_x")), 
  
  base_data %>% 
    mutate(state = "1. Univariate",
           anim_x = min_x, # Collapse X to the left edge of the DATA
           anim_y = y,
           group_id = paste0(point_id, "_y")),
  
  # State 2: Bivariate (Joint)
  base_data %>% 
    mutate(state = "2. Bivariate",
           anim_x = x,
           anim_y = y,
           group_id = paste0(point_id, "_x")),
  
  base_data %>% 
    mutate(state = "2. Bivariate",
           anim_x = x,
           anim_y = y,
           group_id = paste0(point_id, "_y")) 
)

# 3. Plot
anim <- ggplot(anim_data, aes(x = anim_x, y = anim_y, group = group_id)) +
  geom_point(size = 2, alpha = 0.75) + 
  facet_wrap(vars(dataset2), ncol = 3) +
  theme(text = element_text(size = 34),
        legend.position = "none") + 
  labs(y = NULL, x = NULL, title = NULL) +
  
  # Transition settings
  transition_states(state, transition_length = 3, state_length = 1, wrap = TRUE) +
  ease_aes('cubic-in-out')

# 4. Render and Export
final_animation <- animate(anim, 
                           nframes = 100, 
                           fps = 20, 
                           width = 1114.286, 
                           height = 445.7144,
                           renderer = gifski_renderer(loop = TRUE))

anim_save("Slides/Scatterplots/datasaurus_transition.gif", animation = final_animation)

