## Packages and Themes
# Install the packages and set the theme
library(tidyverse)
library(scales)
library(ggdist)
library(sysfonts)
library(showtext)

font_add_google("Spline Sans")
showtext_auto()

theme_clean <- theme_bw(base_family = "Spline Sans") + 
  theme(legend.position = "top",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 20, face = "italic",
                                     margin = margin(b=12)),
        plot.caption = element_text(size = 20,
                                    vjust = -1),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title.position = "plot",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 20, face = "bold"))
theme_set(theme_clean)


## Input tables
# Source: https://nsandi-corporate.com/news-research/news/new-interest-rates-selected-nsi-accounts
prize_fund_interest_rate <- 0.038
bonds_winning_denom <- 22000

nsi_prize_table <- dplyr::tribble(
  ~prize_value, ~estimated_number_prizes,
  25, 2170903,
  50, 1830825,
  100, 1830825,
  500, 49335,
  1000, 16455,
  5000, 1561,
  10000, 781,
  25000, 313,
  50000, 157,
  100000, 78,
  1000000, 2)

total_prize_value <- sum(nsi_prize_table$prize_value*nsi_prize_table$estimated_number_prizes)
total_prize_count <- sum(nsi_prize_table$estimated_number_prizes)
eligible_bonds_estimate <- total_prize_count*bonds_winning_denom

# Add getting nothing as a 'prize'
nsi_prize_t0 <- nsi_prize_table %>%
  dplyr::add_row(
    prize_value = 0,
    estimated_number_prizes = eligible_bonds_estimate - total_prize_count) %>%
  dplyr::mutate(prize_prob = estimated_number_prizes / eligible_bonds_estimate)

# Simulate the probabilities
# Note there is a very small chance that an ineligible sample could occur
# For example, there could be three £1m wins from the same set of bonds
player_bond_value <- 20000
number_of_simulations <- 25000
number_of_months <- 12
sim_months <- number_of_months*number_of_simulations
set.seed(54321)

# The replicate function does a simulation for each of the 12 months,
# 250000 times
simulation_list_df <- replicate(
  sim_months,
  {
    sum(sample(x = nsi_prize_t0$prize_value,
                size = player_bond_value,
                replace = TRUE,
                prob = nsi_prize_t0$prize_prob) %>% 
          as_tibble())
  },
  simplify = TRUE) %>%
  dplyr::as_tibble()

simulation_summary_df <- simulation_list_df %>%
  dplyr::group_by(value) %>%
  dplyr::summarise(count_of_simulations = n()) %>%
  dplyr::mutate(
    percentage_simulations = count_of_simulations/sim_months,
    prize_yn = case_when(value == 0 ~ "Yes", TRUE ~ "No")) 

simulations_annual_df <- simulation_list_df %>%
  tibble::add_column(sim_month = 1:sim_months) %>%
  dplyr::mutate(
    month_number = case_when(
      sim_month %% number_of_months == 0 ~ number_of_months,
      TRUE ~ sim_month %% number_of_months),
    simulation_number = 1 + (sim_month - month_number)/number_of_months) %>%
  dplyr::group_by(simulation_number) %>%
  dplyr::summarise(annual_value = sum(value)) %>%
  dplyr::mutate(annual_value_pct = annual_value/player_bond_value)

# This calculates the estimated chance of getting no monthly prize from £20k of bonds
dhyper(x = 0,
       m = total_prize_count,
       n = eligible_bonds_estimate - total_prize_count,
       k = player_bond_value) %>% 
  as_tibble() %>% 
  summarise(prob_total = sum(value))

# We turn these calculations into a function
nsi_simulation_table_func <- function(bond_value, num_sims, num_months){
  s1 <- replicate(
    num_sims*num_months,
    {
      sum(sample(x = nsi_prize_t0$prize_value,
                 size = bond_value,
                 replace = TRUE,
                 prob = nsi_prize_t0$prize_prob) %>% 
            as_tibble())
    },
    simplify = TRUE) %>%
    dplyr::as_tibble() %>%
    tibble::add_column(sim_month = 1:(num_sims*num_months)) %>%
    dplyr::mutate(
    month_number = case_when(
      sim_month %% num_months == 0 ~ num_months,
      TRUE ~ sim_month %% num_months),
    simulation_number = 1 + (sim_month - month_number)/num_months) %>%
    dplyr::group_by(simulation_number) %>%
    dplyr::summarise(annual_value = sum(value)) %>%
    dplyr::mutate(annual_value_pct = annual_value/bond_value,
                  bond_amount = bond_value)
  return(s1)
}

num_sims_value <- 10000
num_months_value <- 12

nsi_simulation_df <- dplyr::bind_rows(
  nsi_simulation_table_func(bond_value = 5000, num_sims_value, num_months_value),
  nsi_simulation_table_func(bond_value = 10000, num_sims_value, num_months_value),
  nsi_simulation_table_func(bond_value = 15000, num_sims_value, num_months_value),
  nsi_simulation_table_func(bond_value = 20000, num_sims_value, num_months_value),
  nsi_simulation_table_func(bond_value = 25000, num_sims_value, num_months_value),
  nsi_simulation_table_func(bond_value = 30000, num_sims_value, num_months_value),
  nsi_simulation_table_func(bond_value = 35000, num_sims_value, num_months_value),
  nsi_simulation_table_func(bond_value = 40000, num_sims_value, num_months_value),
  nsi_simulation_table_func(bond_value = 45000, num_sims_value, num_months_value),
  nsi_simulation_table_func(bond_value = 50000, num_sims_value, num_months_value))

## Graphs
summary_title <- "If you invest £20,000 in NS&I premium bonds, you should expect no monthly prize around 40% of the time."
summary_subtitle <- "Prize return distribution for each month (from £0 to £500), assuming the person holds £20,000 in the UK government's National Savings & Investments premium bonds. This is based on simulations using estimated prizes for April 2025."
summary_caption <- paste0("Source: NS&I estimates for April 2025, published on 18 February 2025.\nAuthor's calculations, assuming £20,000 is invested and using ",
                          format(sim_months, big.mark = ",", scientific = FALSE),
                          " simulations.")

simulation_summary_gg <- simulation_summary_df %>%
  dplyr::filter(value <= 500) %>%
  ggplot(aes(x = value, y = percentage_simulations, fill = prize_yn)) +
  geom_col() +
  scale_fill_manual(guide = "none",
                    values = c("#fe8c11","#008080")) +
  scale_x_continuous(labels = scales::label_currency(prefix = "£")) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.45),
                     labels = scales::percent_format()) +
  labs(title = summary_title,
       subtitle = str_wrap(summary_subtitle, 140),
       caption = summary_caption,
       x = "Monthly prize value",
       y = "Estimated probability")
  
sims_title <- "As the investment in NS&I premium bonds increases, the likely range of returns narrows: centering on 3.3%."
sims_subtitle <- "Median quantile 90% intervals for the estimated relative annual return of NS&I premium bonds, at specified amounts. The graph is constrained to only show annual value percentages of 10% or under. The relative return can exceed this threshold, using estimated prizes for April 2025."
sims_caption <- paste0("Source: NS&I estimates for April 2025, published on 18 February 2025.\nAuthor's calculations, assuming £20,000 is invested and using ",
                       format(num_sims_value*num_momths_value, big.mark = ",", scientific = FALSE),
                       " simulations for each investment amount.")

nsi_simulation_gg <- nsi_simulation_df %>% 
  dplyr::filter(annual_value_pct <= 0.10) %>% 
  dplyr::mutate(bond_amount_fmt = paste0("£", format(bond_amount, big.mark = ",", scientific = FALSE))) %>% 
  ggplot(aes(y = bond_amount_fmt, x = annual_value_pct)) + 
  ggdist::stat_dotsinterval(quantiles = 100,
                            point_interval = median_qi,
                            .width = 0.9,
                            slab_colour = "#008080", slab_fill = "#008080") +
  scale_x_continuous(labels = scales::label_percent()) +
  scale_y_discrete(limits = rev) + 
  labs(title = sims_title,
       subtitle = str_wrap(sims_subtitle, width = 140),
       caption = sims_caption,
       x = "Annual return (as a percentage of the investment amount)",
       y = "Investment amounts")

## Saving the outputs
png("R/Analysis_2025/01_Data_Visualisation/simulation_summary_gg.png",
    width = 1800, height = 1000, unit = "px")
simulation_summary_gg
dev.off()

png("R/Analysis_2025/01_Data_Visualisation/nsi_simulation_gg.png",
    width = 1800, height = 1000, unit = "px")
nsi_simulation_gg
dev.off()