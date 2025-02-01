## Packages and Themes
# Install the packages and set the theme
library(tidyverse)
library(sysfonts)
library(showtext)

font_add_google("Spline Sans")
showtext_auto()

theme_clean <- theme_bw(base_family = "Spline Sans") + 
  theme(legend.position = "top",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
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

## Jeffrey's Test and the Beta distribution
alpha0 <- 0.5
beta0 <- 0.5
number_of_obligors <- 100
number_of_defaults <- 3
input_probability_of_default <- 0.01

# Updating rule
alpha1 <- alpha0 + number_of_defaults
beta1 <- beta0 + number_of_obligors - number_of_defaults

prior_label <- paste0(
  "Jeffrey's prior: Beta(", alpha0, ", ", beta0, ").")

posterior_label <- paste0(
  "Posterior distribution: Beta(", alpha1, ", ", beta1, ")\n",
  number_of_defaults, " defaults from ", number_of_obligors, " obligors.")

beta_df <- dplyr::tibble(
  x = seq(0, 1, length.out = 1001),
  prior = dbeta(x, alpha0, beta0),
  posterior = dbeta(x, alpha1, beta1))

beta_tidy_df <- beta_df %>%
  tidyr::pivot_longer(cols = 2:3,
                      names_to = "distribution",
                      values_to = "density")

## Graphs
dist_title <- "With more information, our view of the portfolio or grade's probability of default evolves."
dist_subtitle <- "Probability density functions of two Beta distributions, where the first is Jeffrey's prior. The area under each curve is equal to 1 (100%)."
dist_caption <- "Source: Beta functions in R, applying updating rules of Beta distributions for Binomial data."

beta_dist_gg <- beta_tidy_df %>%
  ggplot(aes(x = x, y = density,
             group = distribution, colour = distribution)) +
  geom_line(linewidth = 2) +
  scale_colour_manual(guide = "none",
                      values = c("#fe8c11", "#008080")) +
  annotate("text", x = 0.1, y = 25,
           label = prior_label, size = 7,
           fontface = "bold", hjust = 0, colour = "#008080") +
  annotate("text", x = 0.1, y = 20,
           label = posterior_label, size = 7,
           fontface = "bold", hjust = 0, colour = "#fe8c11") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0.5)) +
  labs(title = dist_title,
       subtitle = str_wrap(dist_subtitle, width = 140),
       caption = dist_caption,
       x = "Grade or portfolio probability of default (PD)",
       y = "Probability density")

test_title <- paste0(
  "After ", number_of_defaults, " defaults from ", number_of_obligors,
  " obligors, the posterior probability of the grade or portfolio PD being under ",
  format(100*input_probability_of_default), "% is ",
  format(100*pbeta(input_probability_of_default, shape1 = alpha1, shape2 = beta1),
         digits = 2), "%.")
test_subtitle <- paste0(
  "Posterior distribution of the grade or portfolio probability of default (PD), given ",
  number_of_defaults, " defaults from ", number_of_obligors,
  " obligors, shown on the interval (0, 0.2). The prior distribution was Beta(",
  alpha0, ", ", beta0, "). Here, the input PD was ",
  format(100*input_probability_of_default), "%.")

beta_test_gg <- beta_df %>%
  dplyr::filter(x <= 0.2) %>%
  ggplot(aes(x = x, y = posterior)) +
  geom_line(linewidth = 2, colour = "#fe8c11") +
  geom_area(stat = "function",
            fun = dbeta,
            args = list(shape1 = alpha1, shape2 = beta1),
            fill = "#feae58",
            xlim = c(0, input_probability_of_default)) +
  geom_vline(xintercept = input_probability_of_default,
             linetype = "dashed", linewidth = 1.5) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0.5)) +
  annotate(geom = "text",
           x = input_probability_of_default, y = 5,
           size = 7, hjust = -0.02, fontface = "bold",
           label = "Input probability") +
  annotate(geom = "text", x = 0.07, y = 20,
           size = 7, hjust = 0,
           label = "The test is asking: what proportion of the\nposterior distribution (for the grade or portfolio default probability)\nis under the input (model) value.\nIf this proportion is under a threshold, then the test fails.") +
  labs(title = test_title,
       subtitle = str_wrap(test_subtitle, 130),
       caption = dist_caption,
       x = "Grade or portfolio probability of default (PD)",
       y = "Probability density")

## Save the outputs
png("R/Analysis_2025/02_Distributions/beta_dist_gg.png",
    width = 2000, height = 1200, unit = "px")
beta_dist_gg
dev.off()

png("R/Analysis_2025/02_Distributions/beta_test_gg.png",
    width = 2000, height = 1200, unit = "px")
beta_test_gg
dev.off()