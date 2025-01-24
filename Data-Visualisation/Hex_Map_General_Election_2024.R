## Packages and Themes
# Install the packages and set the theme
library(tidyverse)
library(sysfonts)
library(showtext)
library(curl)
library(readxl)
library(janitor)
library(sf)
library(patchwork)

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

## Download the 2024 General Election results
hocl_constituency_url <- "https://researchbriefings.files.parliament.uk/documents/CBP-10009/HoC-GE2024-results-by-constituency.xlsx"
hocl_sheet <- "Data"
hocl_range <- "A3:AD653"

temp <- tempfile()
temp <- curl::curl_download(
  url = hocl_constituency_url, destfile = temp,
  quiet = TRUE, mode = "wb")

hocl_2024_df <- readxl::read_excel(
  temp,
  sheet = hocl_sheet,
  range = hocl_range) %>%
  janitor::clean_names() %>%
  dplyr::rename(
    ons_code = ons_id)

## Hex map
ak_map_source <- "https://automaticknowledge.org/wpc-hex/uk-wpc-hex-constitcode-v5-june-2024.gpkg"

ak_map <- tempfile()
ak_map <- curl::curl_download(
  url = ak_map_source, destfile = ak_map,
  quiet = TRUE, mode = "wb")

ak_constituencies <- sf::st_read(ak_map,
                                 layer = "uk-wpc-hex-constitcode-v5-june-2024")
hexmap_df <- dplyr::left_join(
  hocl_2024_df, ak_constituencies,
  by = c("ons_code" = "GSScode")) %>%
  dplyr::mutate(winning_party =
      case_when(constituency_name == "Chorley" ~ "Speaker",
                first_party == "Con" ~ "Conservatives",
                first_party == "Lab" ~ "Labour",
                first_party == "LD" ~ "Liberal Democrats",
                first_party == "Green" ~ "Green",
                first_party == "RUK" ~ "Reform UK",
                first_party == "Ind" ~ "Independents",
                first_party == "PC" ~ "Plaid Cymru",
                first_party == "SNP" ~ "Scottish National",
                first_party == "SF" ~ "Sinn Fein",
                first_party == "DUP" ~ "Democratic Unionist",
                first_party == "APNI" ~ "Alliance",
                first_party == "SDLP" ~ "Social Democratic and Labour",
                first_party == "TUV" ~ "Traditional Unionist Voice",
                first_party == "UUP" ~ "Ulster Unionist"))

## Hex map of the UK
hexmap_title <- "Labour won a large majority in the 2024 election, whilst the Conservatives were reduced to an historic low."
hexmap_subtitle <- "Hex map and seat count for the 2024 General Election in the United Kingdom."
hexmap_caption <- "Source: House of Commons Library constituency results (CBP-10009). Hex maps by Philip Brown and Alasdair Rae, Automatic Knowledge."

hexmap_gg1 <- ggplot() +
  geom_sf(data = hexmap_df, aes(geometry = geom, fill = winning_party)) +
  scale_fill_manual(guide = "none",
                    values = c("#F6CB2F", "#0087DC", "#D46A4C",
                               "#02A95B", "#FC86C2", "#E4003B",
                               "#FAA61A", "#005B54", "#12B6CF",
                               "#FDF38E", "#326760", "#2AA82C",
                               "black", "#0C3A6A", "#48A5EE")) +
  labs(title = "Hex map (equal area)") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

hexmap_gg2 <- hexmap_df %>%
  dplyr::group_by(winning_party) %>%
  dplyr::summarise(count_of_seats = n(), .groups = "drop") %>%
  dplyr::mutate(winning_party = fct_reorder(winning_party, count_of_seats)) %>%
  ggplot() +
  geom_col(aes(x = count_of_seats, y = winning_party, fill = winning_party)) +
  scale_fill_manual(guide = "none",
                    aesthetics = c("fill", "colour"),
                    values = c("#F6CB2F", "black", "#0C3A6A",
                               "#48A5EE", "#2AA82C", "#02A95B",
                               "#005B54", "#D46A4C", "#12B6CF",
                               "#FC86C2", "#326760", "#FDF38E",
                               "#FAA61A", "#0087DC", "#E4003B")) +
  geom_text(aes(label = count_of_seats, colour = winning_party,
                x = count_of_seats, y = winning_party),
            hjust = -0.3, fontface = "bold", size = 7) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,450)) + 
  geom_vline(xintercept = 326, linetype = "dashed",
             linewidth = 1.5) +
  geom_text(x = 330, y = 5,
            label = "Seats needed\nfor a majority",
            hjust = 0, fontface = "bold", size = 5) +
  labs(title = "House of Commons seats by party",
       x = "Number of seats", y ="")

hexmap_gg <- hexmap_gg1 + hexmap_gg2 +
  patchwork::plot_annotation(
    title = hexmap_title,
    subtitle = str_wrap(hexmap_subtitle, 140),
    caption = hexmap_caption)

## Save the output
png("R/Analysis_2025/01_Data_Visualisation/hexmap_gg.png",
    width = 2000, height = 1200, unit = "px")
hexmap_gg
dev.off()