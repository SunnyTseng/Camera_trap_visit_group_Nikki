

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
                             raw_count))

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
  
  ggplot(aes(x = week, group = threshold, colour = threshold)) +
  geom_point(aes(y = raw_count), data = . %>% filter(threshold == "Photo_num"),
             colour = "black", size = 4, alpha = 0.2) +
  geom_line(aes(y = p), data = . %>% filter(threshold == "Photo_num"), 
            colour = "black", linetype = 5, linewidth = 2, alpha = 0.7) +
  geom_point(aes(y = raw_count), data = . %>% filter(threshold != "Photo_num"),
             size = 4, alpha = 0.2) +
  geom_line(aes(y = p), data = . %>% filter(threshold != "Photo_num"),
            linewidth = 2, alpha = 0.7) + 
  
  ylim(0, 13) +
  labs(x = "Week of a year",
       y = "Number of visits") +
  scale_colour_brewer(palette = "Dark2") +
  
  theme_bw() 


  
fig_visit

  
  





# figure for the coefficients ---------------------------------------------

table_start <- which(str_detect(stata_output, "visitnumber_sum \\|"))
table_end <- which(str_detect(stata_output, "alpha \\|")) - 1
coeff_table <- stata_output[table_start:table_end]


# Convert the table into a tibble
coeff_df <- coeff_table %>%
  str_trim() %>%
  str_split_fixed("\\s+\\|\\s+", n = 2) %>%
  as_tibble() %>%
  rename(Variable = V1, Values = V2) %>%
  separate(Values, into = c("Coef", "Std_Err", "z", "P", "Conf_Lower", "Conf_Upper"), sep = "\\s+", convert = TRUE)

# Convert numeric columns to appropriate types
coeff_df <- coeff_df %>%
  mutate(across(Coef:Conf_Upper, as.numeric))
