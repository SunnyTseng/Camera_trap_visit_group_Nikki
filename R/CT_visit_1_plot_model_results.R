

# library -----------------------------------------------------------------

library(tidyverse)
library(here)


# import data -------------------------------------------------------------

# model fitting data and the predicted value
model_data <- read_csv(here("data", "camera_interval_data_Dec_11_2024.csv")) %>% 
  rename(photo_p = n_photo_num,
         photo_lci = n_photo_num_lci,
         photo_uci = n_photo_num_uci,
         none_p = n_visit_num,
         none_lci = n_visit_num_lci,
         none_uci = n_visit_num_uci,
         min15_p = n_min15,
         min15_lci = n_min15_lci,
         min15_uci = n_min15_uci,
         min30_p = n_min30,
         min30_lci = n_min30_lci,
         min30_uci = n_min30_uci,
         min60_p = n_min60,
         min60_lci = n_min60_lci,
         min60_uci = n_min60_uci,
         day1_p = n_day1,
         day1_lci = n_day1_lci,
         day1_uci = n_day1_uci,
         raw_count = visitnumber_sum,
         threshold = time_thresh) %>%
  mutate(photo_p = photo_p/10,
         photo_lci = photo_lci/10,
         photo_uci = photo_uci/10) %>%
  mutate(raw_count = if_else(threshold == "Photo_num",
                             raw_count/10,
                             raw_count)) %>%
  mutate(threshold = factor(threshold,
                            levels = c("Photo_num", "None", "min15", "min30", "min60", "day1"),
                            labels = c("No. of images", 
                                       "None", 
                                       "15 mins", 
                                       "30 mins", 
                                       "1 hour", 
                                       "1 day")))

# model output from Stata
model_output <- read_lines(here("data", "nbreg_count_results_Dec_11_2024.txt"))


# figure for the number of visits -----------------------------------------

fig_visit <- model_data  %>%
  pivot_longer(
    cols = matches("_p$|_lci$|_uci$"), # Match columns ending 
    names_to = c("type", ".value"),    # Split into 'type' and value columns
    names_pattern = "(.+)_(p|lci|uci)$" # Regex to extract type and value suffix
  ) %>%
  drop_na() %>%
  group_nest(week, threshold) %>%
  mutate(raw_count = map_dbl(data, ~ .x$raw_count %>% mean()),
         p = map_dbl(data, ~ .x$p %>% mean()),
         lci = map_dbl(data, ~ .x$lci %>% mean()),
         uci = map_dbl(data, ~ .x$uci %>% mean())) %>%
  select(-data) %>%
  
  # start the ggplot
  ggplot(aes(x = week, group = threshold, colour = threshold)) +
  
  # main data points and lines
  geom_point(aes(y = raw_count), data = . %>% filter(threshold == "No. of images"),
             colour = "black", size = 4, alpha = 0.2) +
  geom_line(aes(y = p, linetype = threshold), data = . %>% filter(threshold == "No. of images"), 
            colour = "black", linewidth = 2, alpha = 0.7) +
  geom_point(aes(y = raw_count), data = . %>% filter(threshold != "No. of images"),
             size = 4, alpha = 0.2) +
  geom_line(aes(y = p), data = . %>% filter(threshold != "No. of images"),
            linewidth = 2, alpha = 0.7) + 
  
  # arrange the axis, text, and mapped items
  scale_colour_brewer(palette = "Dark2") +
  scale_linetype_manual(values = "dashed",
                        name = NULL) +
  scale_y_continuous(name = "No. of Visits",
                     sec.axis = sec_axis(trans =~ . * 10, name = "No. of Images"),
                     limits = c(0, 13)) +
  scale_x_continuous(breaks = seq(0, 50, by = 10)) +
  labs(x = "Week of the Year",
       colour = "Threshold") +
  
  # format the theme
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),    
        axis.title = element_text(size = 14),       
        legend.text = element_text(size = 12),     
        legend.title = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 5)),
        axis.title.y = element_text(margin = margin(r = 5)),
        axis.title.y.right = element_text(margin = margin(l = 5))) 
  
fig_visit

ggsave(
  filename = here("docs", "CRI_visit.png"),
  plot = fig_visit,
  width = 20, 
  height = 12,   
  dpi = 300,
  units = "cm")





# figure for the coefficients ---------------------------------------------

coef_table <- tibble(threshold = c("No. of images", "None", 
                                   "15 mins", "30 mins", "1 hour", "1 day"),
                     
                     week_coef = c(.2446736, .1999065, .2140635, .1984859, .1841223, .1023013),
                     week_low = c(.1496747, .1295728, .1451504, .1315697, .1187383, .0490814),
                     week_high = c(.3396724, .2702401, .2829767, .2654021, .2495063, .1555211),
                     
                     week2_coef = c(-.004237, -.0036666, -.0039526, -.0036668, -.003405, -.0017702),
                     week2_low = c(-.0057853, -.0048234, .0005868, -.0047886, -.0045051, -.0027067),
                     week2_high = c(-.0026887, -.0025098, -.0028025, -.002545, -.0023048, -.0008337)) %>%
  mutate(threshold = factor(threshold, levels = c("No. of images", "None", "15 mins", 
                                                  "30 mins", "1 hour", "1 day")))



fig_coef <- coef_table %>%
  mutate(coef_type = ifelse(week_coef > 0, "Week", "Week squared")) %>%
  mutate(coef_type = factor(coef_type, levels = c("Week", "Week squared"))) %>%

  # main data points and lines
  ggplot(aes(y = threshold)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = week_low, xmax = week_high), height = 0.2, size = 1, alpha = 0.5) +
  geom_errorbarh(aes(xmin = week2_low, xmax = week2_high), height = 0.2, size = 1, alpha = 0.5) +
  geom_point(aes(x = week_coef, shape = "Week"), size = 4, alpha = 0.8) +
  geom_point(aes(x = week2_coef, shape = "Week squared"), size = 4, alpha = 0.8) +
  
  # arrange the axis, text, and mapped items
  labs(x = "Coefficient", y = "", shape = "Variable") +  
  guides(shape = guide_legend(position = "inside")) +
  
  # format the theme
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 5)),
        legend.position.inside = c(0.86, 0.85))  

fig_coef

ggsave(
  filename = here("docs", "CRI_coef.png"),
  plot = fig_coef,
  width = 18, 
  height = 10,   
  dpi = 300,
  units = "cm")








