# Import libraries
library(sf)
library(tigris) # Mapping
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
library(rvest) # For pulling data from wikipedia
library(tidyverse) # Tidying data
library(patchwork) # Combinind plots
library(lubridate) # Working with dates
library(here) # Easier file referencing
library(glue) # Alternative to paste0
library(colorspace) # Colour palettes


# Read and clean covid-19 cases on a county basis
county_cases <-
  read_csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv") %>%
  pivot_longer(-c(countyFIPS, `County Name`, State, stateFIPS),
    names_to = "date",
    values_to = "confirmed_cases"
  ) %>%
  janitor::clean_names() %>%
  mutate(date = mdy(date)) %>%
  rename(
    state_abb = state,
    county = county_name
  ) %>%
  rowwise() %>%
  mutate(
    state = state.name[grep(state_abb, state.abb)[1]]
  )

# Get populations for all counties
## If we already have this data downloaded read it in, otherwise download iteratively for
## each state.
if (file.exists("./data/all_county_pops.csv")) {
  county_pops <- read_csv("./data/all_county_pops.csv")
} else {
  county_pops <-
    state.name %>%
    str_to_lower() %>%
    str_remove_all(" ") %>%
    map_dfr(
      ~ glue("https://www.{ .x }-demographics.com/counties_by_population") %>%
        read_html() %>%
        html_node(xpath = glue("/html/body/div[3]/div[1]/div/table")) %>%
        html_table() %>%
        mutate(state = glue("{ .x }"))
    ) %>%
    janitor::clean_names() %>%
    filter(!is.na(population)) %>%
    mutate(
      population = gsub(",(?)", "", population) %>% as.numeric(),
      rank = as.numeric(rank)
    ) %>%
    filter(!is.na(rank)) %>%
    mutate(
      # Clean names... Probably more elegant way to do this
      state = case_when(
        state == "newhampshire" ~ "New Hampshire",
        state == "newjersey" ~ "New Jersey",
        state == "newmexico" ~ "New Mexico",
        state == "newyork" ~ "New York",
        state == "northcarolina" ~ "North Carolina",
        state == "northdakot" ~ "North Dakota",
        state == "newjersey" ~ "New Jersey",
        state == "westvirginia" ~ "West Virginia",
        state == "northdakota" ~ "North Dakota",
        state == "southdakota" ~ "South Dakota",
        state == "southcarolina" ~ "South Carolina",
        state == "rhodeisland" ~ "Rhode Island",
        TRUE ~ as.character(state)
      ),
      state = str_to_title(state)
    )
  
  write_csv(county_pops, here("./data/all_county_pops.csv"))
} 


# Get map of country
country_map <- states() %>%
  st_as_sf() %>%
  filter(
    NAME %in% state.name,
    NAME != "Alaska",
    NAME != "Hawaii"
  )

# Get map of all counties
county_maps <- counties() %>% sf::st_as_sf()


# Combine it all together
complete_df <-
  # Take our county map and combine it with county info (covid cases and pop)
  county_maps %>%
  left_join(
    county_cases %>%
      left_join(county_pops, by = c("county", "state")) %>%
      filter(county != "Statewide Unallocated") %>%
      mutate(
        COUNTYFP = as.character(county_fips) %>% str_sub(., -3),
        STATEFP = as.character(county_fips) %>% str_remove(string = ., pattern = ".{3}$") %>% str_pad(width = 2, side = "left", pad = "0"),
        per_capita = (confirmed_cases / population) * 100000
      ) %>%
      select(county_fips, state_fips, STATEFP, COUNTYFP, county, everything()),
    # Left join by
    by = c("STATEFP", "COUNTYFP")
  ) %>%
  filter(
    # Limit it to mainland US
    state_abb %in% state.abb,
    state_abb != "AK",
    state_abb != "HI"
  ) %>%
  mutate(
    label = glue("{ day(date) } { month(date, label = TRUE) }")
  )

# What dates to plot?
date_seq <- c("2020-03-01", "2020-04-01")

# Find limits for colour scale
lims <- c(min(complete_df$per_capita[(complete_df$per_capita > 0 & (complete_df$date >= "2020-03-01" & complete_df$date <= "2020-04-30"))], na.rm = T), 
          max(complete_df$per_capita[complete_df$date >= "2020-03-01" & complete_df$date <= "2020-04-30"], na.rm = T)
          )


plot_list <- list()

for (i in 1:length(date_seq)) {
  plot_label <- complete_df$label[which.max(complete_df$date == date_seq[i])]

  filtered_df <-
    complete_df %>%
    filter(date == date_seq[i]) %>%
    filter(per_capita > 0)

  plot_list[[i]] <-
    ggplot() +
    # Hacky way to ensure dimensions are same between plots
    geom_sf(data = country_map, aes(geometry = geometry), fill = NA, colour = NA) + 
    geom_sf(data = filtered_df, aes(fill = per_capita, geometry = geometry), colour = NA) +
    coord_sf(
      crs = 4326,
      expand = F,
      clip = "off"
    ) +
    ggthemes::theme_map() +
    scale_fill_continuous_sequential(
      palette = "Inferno",
      name = "Cases per capita",
      limits = lims,
      trans = "log10",
      na.value = NA,
      breaks = c(0.1, 10, 1000),
      labels = c(0.1, 10, 1000),
      guide = guide_colorbar(
        frame.colour = "black",
        frame.linewidth = 1,
        frame.linetype = 1,
        draw.llim = FALSE,
        draw.ulim = FALSE
      )
    ) +
    theme(
      legend.position = "bottom"
    ) +
    annotate(
      geom = "text",
      x = -124.849,
      y = 49.00243 + 3,
      label = plot_label,
      vjust = 0.5,
      hjust = 0,
      size = 2.75,
      family = "sans"
    )
}

plot_wrapped <-
  wrap_plots(plot_list, ncol = 2, nrow = 1, guides = "collect") +
    plot_annotation(
      title = "A month of coronavirus",
      subtitle = "Coronavirus cases at the county level",
      caption = glue("Code: https://github.com/m1kejay/covid-19/\nCounty-level covid-19 data: https://usafacts.org/")
    ) &
    theme(
      legend.position = "bottom",
      legend.justification = "right",
      plot.caption = element_text(colour = "grey30"),
      plot.title = element_text(size = ceiling(12 * 1.1), face = "bold"),
      plot.subtitle = element_text(size = ceiling(12 * 1.05), colour = "grey30")
    )


ggsave(filename = "./output/march-april-comparison.png", plot = plot_wrapped, height = 5, width = 10, unit = "in", dpi = 300)
