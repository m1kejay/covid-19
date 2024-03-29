Analysis of COVID-19 data in early 2020
================
Mike Jay.
Last updated 2020-05-16

# Importing and cleaning data

Let’s begin by loading packages necessary for the analysis here:

``` r
# Libraries used in this analysis

library(sf) # Mapping
library(tigris) # Mapping 
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
library(rvest) # For pulling data from websites
library(tidyverse) # Tidying data
library(patchwork) # For plotting graphs together
library(lubridate) # Working with dates
library(here) # Easier file referencing
library(glue) # Alternative to paste0
library(colorspace) # Colour palettes
library(countrycode) # Get country codes to help de-clutter plots
library(cowplot) # Organising multiple plots 
library(gghighlight) # Highlight subsets
library(ggtext) # Richer text formatting
library(ggrepel) # Repel labels in plots
library(gtable) # To reconstruct ggplots
library(grid) # To help reconstruct ggplots
library(wbstats) # Useful for getting GDP stats
```

## Country-level plots

Before we can do any plotting, we of course have to import the data and
do a little bit of tidying. Country-level covid-19 data comes from John
Hopkins’ github repository which is located here:
<https://github.com/CSSEGISandData/COVID-19/>.

``` r
# location of data
data_url_prefix <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_"

# case/file nmaes
case_type <- c("confirmed", "deaths")

# date regex for the pivot longer call
date_regex <- "^\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}"

# Map across case_type and concatenate data into one dataframe
df <- map_dfr(
  case_type,
  ~ read_csv(glue(data_url_prefix, ., "_global.csv")) %>%
    mutate(case_type = .x) %>%
    select(case_type, everything())
) %>%
  pivot_longer(
    # Pivot by date
    cols = matches(date_regex),
    names_to = "date",
    values_to = "cases"
  ) %>%
  # quick clean of columns
  janitor::clean_names() %>%
  rename(
    "province" = "province_state",
    "country" = "country_region"
  ) %>%
  # Remove cruise ship data
  filter(!(country %in% c("Diamond Princess", "MS Zaandam"))) %>%
  # Data is split by provinces/states, so each country has multiple rows.
  # I'm not interested in *within* country variation, so group by country/day and sum cases
  group_by(case_type, country, date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  group_by(case_type, country) %>%
  mutate(
    date = mdy(date),
    n_days = date - max(date),
    continent = countrycode(sourcevar = country, origin = "country.name", destination = "continent"),
    country_code = countrycode(sourcevar = country, origin = "country.name", destination = "iso2c"),
    # Fix Kosovo
    country_code = if_else(country == "Kosovo", "XK", country_code),
    continent = if_else(country == "Kosovo", "Europe", continent)
  ) %>%
  ungroup()

# Write and read csv back in
file_name <- here(glue("data/covid_19_{Sys.Date()}.csv"))
write_csv(df, file_name)

# Redefining na  so NA gets encoded as the country_code for Namibia
df <- read_csv(file_name, na = "NaN")
df
```

    ## # A tibble: 42,780 x 7
    ##    case_type country     date       cases n_days continent country_code
    ##    <chr>     <chr>       <date>     <dbl>  <dbl> <chr>     <chr>       
    ##  1 confirmed Afghanistan 2020-01-22     0   -114 Asia      AF          
    ##  2 confirmed Afghanistan 2020-01-23     0   -113 Asia      AF          
    ##  3 confirmed Afghanistan 2020-01-24     0   -112 Asia      AF          
    ##  4 confirmed Afghanistan 2020-01-25     0   -111 Asia      AF          
    ##  5 confirmed Afghanistan 2020-01-26     0   -110 Asia      AF          
    ##  6 confirmed Afghanistan 2020-01-27     0   -109 Asia      AF          
    ##  7 confirmed Afghanistan 2020-01-28     0   -108 Asia      AF          
    ##  8 confirmed Afghanistan 2020-01-29     0   -107 Asia      AF          
    ##  9 confirmed Afghanistan 2020-01-30     0   -106 Asia      AF          
    ## 10 confirmed Afghanistan 2020-01-31     0   -105 Asia      AF          
    ## # ... with 42,770 more rows

This code block takes the raw data from github and does a few key
things:

  - By using `map_dfr()`, it takes the two files,
    `time_series_covid19_confirmed_global.csv` and
    `time_series_covid19_deaths_global.csv`, and binds the data together
    into one dataframe.
  - Converts the new dataframe from wide to long format.
  - Removes the ‘Diamond Princess’ and ‘MS Zaandam’ (cruise ship) data.
  - Aggregates across each country/day the number of cases.
  - Finally it introduces two new variables: `continent` and
    `country_code` (using the `countrycode` package) which will be
    useful for plotting purposes and pulling in statistics from
    `worldbank`.

Now we can move onto plotting the data…

### Confirmed cases

``` r
# Define caption which  I'll put on most graphs
custom_caption <- glue("Last updated {Sys.Date()}\nData: https://github.com/CSSEGISandData/COVID-19/")

days_vs_confirmed <-
  df %>%
  filter(case_type == "confirmed") %>%
  group_by(country) %>%
  filter(cases > 0) %>%
  mutate(dummy_min = -as.numeric(min(n_days))) %>%
  filter(cases == max(cases), n_days == max(n_days)) %>%
  ungroup() %>%
  group_by(continent, country, dummy_min) %>%
  summarise(cases = sum(cases)) %>%
  ungroup()

days_vs_confirmed %>%
  ggplot(aes(x = dummy_min, y = cases)) +
  geom_point(col = "#CCCCCC") +
  geom_point(
    data = days_vs_confirmed %>% filter(country %in% c("US", "China", "Iran")), 
    col = "darkred"
    ) +
  scale_y_log10(
    breaks = c(1, 10, 100, 1000, 10000, 100000, 1e6),
    labels = scales::label_comma(accuracy = 1)
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, 10)
  ) +
  theme_minimal() +
  theme(
    aspect.ratio = 1,
    panel.grid.minor = element_blank(),
    plot.caption = element_text(colour = "grey45")
  ) +
  facet_wrap(continent ~ .) +
  geom_label_repel(
    data = days_vs_confirmed %>% filter(country %in% c("US", "China","Iran")),
    aes(label = country),
    size = 3,
    label.size = NA,
    fill = "#FFFFFF",
    label.padding = unit(0.1, "cm"),
    ylim = c(2.5, NA) # This is hacky and only works with current data... Trying to resolve overlap with data
  ) +
  labs(
    title = "Cases vs days since first confirmed cast",
    subtitle = "Total number of confirmed cases against the number of days since first confirmed case",
    x = "Days since first confirmed case",
    y = "Total confirmed cases",
    caption = custom_caption
  )
```

<img src="README_figs/README-days-vs-confirmed-cases-1.png" width="672" />

So we can see some correlation between the number of days since the
first case for a given country and the total number of cases.

Next let’s make a lollipop chart so we can better appreciate cases on a
country-by-country basis:

``` r
# Minor breaks for log-scale axis grid
minor_breaks <- rep(1:10, 31) * (10^rep(0:30, each = 30))


# Plot
df %>%
  group_by(country) %>%
  filter(case_type == "confirmed", cases > 0) %>%
  mutate(dummy_min = -as.numeric(min(n_days))) %>%
  filter(cases == max(cases), n_days == max(n_days)) %>%
  ungroup() %>%
  arrange(desc(cases)) %>%
  slice(1:75) %>%
  ggplot(aes(x = cases, y = fct_reorder(country, cases))) +
  geom_segment(aes(
    x = 0,
    xend = cases,
    y = fct_reorder(country, cases),
    yend = fct_reorder(country, cases),
    colour = dummy_min
  ), size = 1.25) +
  geom_point(
    aes(x = cases, y = fct_reorder(country, cases), colour = dummy_min), 
    size = 3
    ) +
  scale_x_log10(
    name = "Number of confirmed cases (log scaled)",
    expand = c(0, 0),
    breaks = 10^seq(0, 10),
    labels = scales::label_comma(),
    minor_breaks = minor_breaks,
    limits = c(99.7, NA)
  ) +
  scale_y_discrete(
    name = ""
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  scale_colour_continuous_sequential(
    name = "Number of days since\n first confirmed case",
    palette = "Greens",
    rev = TRUE,
    #limits = c(0, NA) # Comment out since we're so far into the pandemic now
  ) +
  labs(
    title = "Confirmed Cases of COVID-19",
    subtitle = "Top 75 countries with the most confirmed cases of COVID-19",
    caption = custom_caption
  ) +
  theme_minimal() +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0),
    legend.position = "bottom",
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.direction = "horizontal",
    legend.box.background = element_rect(fill = "#FFFFFF", colour = NA),
    panel.grid.minor = element_line(linetype = 1, color = "#F6F6F6"),
    panel.grid.major.y = element_blank(),
    plot.caption = element_text(colour = "grey45")
  ) +
  guides(
    colour = guide_colourbar(
      title.position = "top",
      title.hjust = 0.5,
      frame.colour = "#000000",
      draw.ulim = FALSE,
      draw.llim = FALSE,
      barwidth = unit(5, "cm")
    )
  ) +
  facet_grid(continent ~ ., space = "free", scales = "free_y")
```

<img src="README_figs/README-confirmed-cases-lollipop-1.png" width="768" />

Let’s get a bird’s eye view for all countries in the dataset:

``` r
# Mutate n_days so it makes more sense (to me)
correct_days <-
  df %>%
  filter(case_type == "confirmed") %>%
  filter(cases > 0) %>%
  group_by(continent, country_code) %>%
  mutate(
    n_days = n_days + abs(min(n_days)),
    n_days = as.numeric(n_days)
  ) %>%
  ungroup() %>%
  arrange(continent, country_code)

# Get distinct continent/country_code for labels/background
labels <-
  df %>%
  filter(case_type == "confirmed") %>%
  filter(cases > 0) %>%
  group_by(continent, country_code) %>%
  mutate(
    n_days = n_days + abs(min(n_days)),
    label = max(n_days) %>% as.numeric()
  ) %>%
  filter(n_days == max(n_days)) %>%
  select(continent, country, country_code, cases, label) %>%
  distinct() %>%
  ungroup() %>%
  arrange(continent, country_code)

# Max values for x/y labels
max_days <- max(correct_days$n_days)
max_cases <- max(correct_days$cases)

# X and Y title labels... This will need to be updated manually if new countries
# are added to dataset
ann_x <-
  tibble::tribble(
    ~continent, ~country_code, ~label, ~n_days, ~cases,
    "Oceania", "FJ", "Day", max_days / 2, 0L
  )
ann_y <-
  tibble::tribble(
    ~continent, ~country_code, ~label, ~n_days, ~cases,
    "Oceania", "FJ", "Cases", 0L, max_cases / 2
  )

# Define plot
p <-
  ggplot(data = correct_days) +
  # Background fill
  geom_rect(
    data = labels, 
    aes(fill = continent), 
    xmin = -Inf, 
    xmax = Inf, 
    ymin = -Inf, 
    ymax = Inf
    ) +
   # geom_line(
   #   data = correct_days %>% select(-continent, -country_code),
   #   aes(x = n_days, y = cases, group = country),
   #   colour = "#FFFFFF",
   #   alpha = 0.3
   # ) +
  # Total cases over time
  geom_line(
    aes(x = n_days, y = cases, group = 1), 
    colour = "#000000", 
    size = 0.5
    ) +
  # Country code in top left
  geom_text(
    data = labels,
    aes(x = 1, y = max_cases - max_cases*0.12, label = country_code),
    hjust = 0,
    vjust = 0,
    colour = "#FFFFFF",
    size = 3,
    fontface = "bold"
  ) +
  # Number of cases, top left
  geom_text(
    data = labels,
    aes(x = 1, y = max_cases - max_cases*0.3, label = scales::comma(cases)),
    hjust = 0,
    vjust = 0,
    colour = "#FFFFFF",
    size = 3,
    fontface = "bold"
  ) +
  facet_wrap(vars(continent, country_code), ncol = 14) +
  # Add y-axis values
  scale_y_continuous(
    breaks = c(0, max_cases),
    labels = scales::label_comma(),
    name = ""
  ) +
  # Add x-axis values
  scale_x_continuous(
    breaks = c(0, max_days),
    name = ""
  ) +
  # Define palette
  scale_fill_discrete_qualitative(
    palette = "Dynamic",
    name = ""
  ) +
  # Fine tune some visual elements
  theme(
    aspect.ratio = 1,
    strip.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "right",
    panel.spacing = unit(0.1, "lines"),
    panel.grid = element_blank(),
    plot.caption = element_text(colour = "grey45"),
    plot.margin = unit(c(0, 0, 0, 1),"cm")
  ) +
  # Turn clipping off so we can get text outside the figure
  coord_cartesian(
    clip = "off"
  ) +
  # Add y-axis label
  geom_text(
    data = ann_y, aes(x = n_days, y = cases, label = label),
    hjust = 2,
    size = 3.5,
    colour = "grey15"
  ) +
  # Add x-axis label
  geom_text(
    data = ann_x, aes(x = n_days, y = cases, label = label),
    vjust = 3,
    size = 3.5,
    colour = "grey15"
  ) +
  # Add labels
  labs(
    title = "Total confirmed cases for each country",
    subtitle = "Number of confirmed cases from the day with the first confirmed case",
    caption = custom_caption
  ) 

# Function to filter out grobs
# https://stackoverflow.com/questions/36779537/ggplot2-facet-wrap-y-axis-scale-on-the-first-row-only/36780639#36780639
gtable_filter_remove <- function(x, name, trim = TRUE) {
  matches <- !(x$layout$name %in% name)
  x$layout <- x$layout[matches, , drop = FALSE]
  x$grobs <- x$grobs[matches]
  if (trim) {
    x <- gtable_trim(x)
  }
  x
}

# generate plot grob
p_grob <- ggplotGrob(p)

# Filter out unwanted facet labels
# This will also require manually updating if new countries are added
p_filtered <- gtable_filter_remove(p_grob, name = paste0("axis-l-", 1:13, "-1"), trim = FALSE)
p_filtered <- gtable_filter_remove(p_filtered, name = paste0("axis-b-", 2:14, "-13"), trim = FALSE)
p_filtered <- gtable_filter_remove(p_filtered, name = paste0("axis-b-", 2:14, "-14"), trim = FALSE)

# Assign grob back to plot 
p_filtered_plot <- ggpubr::as_ggplot(p_filtered)
p_filtered_plot
```

<img src="README_figs/README-country-by-country-1.png" width="1008" />

Has there been any slow down in the number of new cases?

``` r
# Find sum of daily counts across all countries
daily_cases_all <-
df %>%
  select(case_type, country, date, cases, continent) %>%
  pivot_wider(
    names_from = case_type,
    values_from = cases
  ) %>%
  group_by(continent, country) %>%
  arrange(country, date) %>%
  mutate(new_cases = confirmed - lag(confirmed)) %>%
  ungroup() %>%
  group_by(date) %>%
  summarise(total_new_cases = sum(new_cases, na.rm = TRUE)) %>%
  ungroup()

daily_cases_all %>%
  ggplot(aes(x = date, y = total_new_cases)) +
  geom_col(fill = "#CCCCCC") +
  scale_x_date(
    date_labels = "%d %b",
    date_breaks = "7 day",
    expand = c(0, 0),
    limits = c(min(df$date), NA)
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.caption = element_text(colour = "grey45")
  ) +
  labs(
    x = "Date",
    y = "Daily count",
    title = "Global cases",
    subtitle = "New cases on a daily basis",
    caption = custom_caption
  )
```

<img src="README_figs/README-daily-count-novel-cases-1.png" width="672" />

How does each continent contribute to the increase in novel cases?

``` r
daily_cases_select <- df %>%
  select(case_type, country, date, cases, continent) %>%
  pivot_wider(
    names_from = case_type,
    values_from = cases
  ) %>%
  arrange(country, date) %>%
  group_by(country) %>%
  mutate(new_cases = confirmed - lag(confirmed)) %>%
  ungroup() %>%
  group_by(continent, date) %>%
  summarise(total_new_cases = sum(new_cases, na.rm = T))

ggplot() +
  geom_col(data = daily_cases_all, aes(x = date, y = total_new_cases), fill = "#CCCCCC", colour = NA) +
  geom_col(data = daily_cases_select, aes(x = date, y = total_new_cases, fill = continent)) +
  scale_x_date(
    date_breaks = "14 day",
    date_labels = "%d %b",
    expand = c(0, 0),
    limits = c(min(df$date), NA)
  ) +
  theme_minimal() +
  theme(
    aspect.ratio = 1,
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(colour = "grey45"),
    legend.position = "none"
  ) +
  labs(
    x = "Date",
    y = "Daily count",
    title = "Novel cases on each continent",
    caption = custom_caption
  ) +
  facet_wrap(continent ~ ., nrow = 1) +
  scale_fill_discrete_qualitative(
    palette = "Dynamic"
  )
```

<img src="README_figs/README-daily-count-novel-cases-continent-1.png" width="1152" />

and as a percent of cases:

``` r
# Arbitrary order of the fill
order <- rev(c("Oceania", "Africa", "Americas", "Europe", "Asia"))

daily_cases_select %>%
  ggplot(aes(x = date, y = total_new_cases, fill = fct_relevel(continent, order))) +
  geom_area(position = "fill", alpha = 1, colour = "grey30") +
  theme_minimal()  +
  scale_x_date(
    expand = c(0, 0.1),
    date_breaks = "14 day",
    date_labels = "%d %b",
    limits = c(min(df$date), NA),
    name = "Date"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(),
    expand =  c(0, 0.01),
    breaks = c(0, 0.5, 1),
    name = "Share of new cases"
  ) +
  scale_fill_discrete_qualitative(
    palette = "Dynamic",
    name = ""
  ) +
  theme(
    panel.grid.minor = element_blank()
  )
```

<img src="README_figs/README-share-of-new-cases-continent-1.png" width="768" />

and what about some select countries?

``` r
daily_cases_select <- df %>%
  select(case_type, country, date, cases, continent) %>%
  pivot_wider(
    names_from = case_type,
    values_from = cases
  ) %>%
  filter(country %in% c("China", "Italy", "US")) %>%
  arrange(country, date) %>%
  group_by(country) %>%
  mutate(new_cases = confirmed - lag(confirmed)) %>%
  ungroup() %>%
  group_by(country, date) %>%
  summarise(total_new_cases = sum(new_cases, na.rm = T))

china_annotation <-
daily_cases_select %>%
  filter(country == "China", date == "2020-02-13")

ggplot() +
  geom_col(data = daily_cases_all, aes(x = date, y = total_new_cases), fill = "#CCCCCC") +
  geom_col(data = daily_cases_select, aes(x = date, y = total_new_cases, fill = country)) +
  geom_richtext(
    data = china_annotation,
    aes(x = date-19, y = total_new_cases + 50000),
    label = "<span style='font-size:9pt'>**2020-02-13**
    <br style = 'display: block; content: ''; margin-top: 0'>
    China changes their
    <br style = 'display: block; content: ''; margin-top: 0'>
    diagnostic criteria</span>",
    colour = "#000000",
    hjust = 0,
    vjust = 1,
    fill = "#FFFFFF",
    label.colour = NA
  ) +
  scale_x_date(
    date_breaks = "7 day",
    date_labels = "%d %b",
    expand = c(0, 0),
    limits = c(min(df$date), NA)
  ) +
  theme_minimal() +
  theme(
    aspect.ratio = 1,
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(colour = "grey45"),
    legend.position = "none"
  ) +
  labs(
    x = "Date",
    y = "Daily count",
    title = "Novel cases on a daily basis for select countries",
    caption = custom_caption
  ) +
  facet_wrap(country ~ .) +
  scale_fill_discrete_qualitative(
  )
```

<img src="README_figs/README-cases-per-select-countries-1.png" width="960" />

(I added that annotation for China back when a blip of 15,000 daily
cases was note-worthy)

#### Confirmed cases per capita

Let’s take a look at cases per capita (i.e. cases per 100,000 people):

``` r
populations <- wb_data("SP.POP.TOTL", start_date = 2017) %>%
  select(iso2c, SP.POP.TOTL) %>%
  rename(
    pop = SP.POP.TOTL,
    country_code = iso2c
  )

cases_per_capita <-
correct_days %>%
  left_join(populations, by = "country_code") %>%
  filter(pop > 10^6, cases >= 1) %>%
  mutate(per_capita = 100000 * (cases/pop)) 


cases_per_capita %>%
  ggplot(aes(x = n_days, y = per_capita, group = country_code)) +
  geom_line(aes(colour = continent), alpha = 0.4) +
  theme_minimal() +
  theme(
    aspect.ratio = 1
  ) +
  scale_y_continuous(
    name = "Cases per capita"
  ) +
  scale_x_continuous(
    name = "# days since first case"
  ) +
  facet_grid(. ~ continent) +
  theme(
    legend.position = "none",
    panel.grid.minor.y = element_blank()
  ) +
  scale_colour_discrete_qualitative() +
  labs(
    title = "Cases per capita"
  )
```

<img src="README_figs/README-cases-per-capita-1.png" width="672" />

#### GDP vs. cases per capita

We can see from above Africa has very few confirmed cases of covid-19.
Could it be that perhaps poorer countries lack the infrastructure or
resources to test for covid-19? Let’s see what GDP vs confirmed cases
looks like:

``` r
gdp <- wb_data("NY.GDP.PCAP.CD", start_date = 2017) %>%
  select(iso2c, NY.GDP.PCAP.CD) %>%
  rename(
    gdp = NY.GDP.PCAP.CD,
    country_code = iso2c
  )

df %>%
  left_join(gdp, by = "country_code") %>%
  filter(case_type == "confirmed") %>%
  group_by(country_code) %>%
  filter(cases == max(cases), n_days == max(n_days)) %>%
  ungroup() %>%
  ggplot(aes(x = gdp, y = cases)) +
  geom_point(colour = "#CCCCCC") +
  scale_y_log10(
    labels = scales::label_comma(accuracy = 1)
  ) +
  scale_x_log10(
    labels = scales::label_comma()
  ) +
  geom_smooth(method = "lm", colour = "#000000", se = FALSE) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    aspect.ratio = 1,
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "right",
    plot.caption = element_text(colour = "grey45")
  ) +
  labs(
    title = "Number of cases vs GDP",
    x = "GDP (log scaled)",
    y = "Cases (log scaled)",
    caption = custom_caption
  )
```

<img src="README_figs/README-gdp-vs-confirmed-cases-1.png" width="672" />

``` r
df %>%
  left_join(gdp, by = "country_code") %>%
  filter(case_type == "confirmed") %>%
  group_by(country_code) %>%
  filter(cases == max(cases), n_days == max(n_days)) %>%
  ungroup() %>%
  ggplot(aes(x = gdp, y = cases)) +
  geom_point(aes(colour = continent)) +
  scale_y_log10(
    labels = scales::label_comma(accuracy = 1)
  ) +
  scale_x_log10(
    labels = scales::label_comma()
  ) +
  geom_smooth(method = "lm", colour = "#000000", se = FALSE) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    aspect.ratio = 1,
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "none",
    plot.caption = element_text(colour = "grey45")
  ) +
  labs(
    title = "Number of cases vs GDP for each continent",
    x = "GDP (log scaled)",
    y = "Cases (log scaled)",
    caption = custom_caption
  ) +
  scale_colour_discrete_qualitative(
    palette = "Dynamic",
    name = ""
  ) +
  facet_wrap(. ~ continent, nrow = 1, scales = "free_y")
```

<img src="README_figs/README-gdp-vs-confirmed-cases-continent-1.png" width="960" />

## Covid-19 associated deaths

What’s the death toll looking like?

``` r
# Convert to wide format
df_wide_all <-
  df %>%
  select(case_type, country, date, cases, continent) %>%
  pivot_wider(
    names_from = case_type,
    values_from = cases
  ) %>%
  filter(deaths > 0)

# Which countries to highlight
labs <-
  df_wide_all %>%
  group_by(country) %>%
  filter(deaths == max(deaths)) %>%
  ungroup() %>%
  arrange(desc(deaths)) %>%
  slice(1:5)
  # filter(country == "US" | country == "Germany")  

# Convert countries to wide format for just countries to highlight
df_wide_most_deaths <-
  df %>%
  select(case_type, country, date, cases, continent) %>%
  pivot_wider(
    names_from = case_type,
    values_from = cases
  ) %>%
  filter(country %in% labs$country) %>%
  filter(deaths > 0)

ggplot() +
  geom_line(
    data = df_wide_all, 
    aes(x = date, y = deaths, group = country), 
    colour = "#CCCCCC"
    ) +
  geom_line(
    data = df_wide_most_deaths, 
    aes(x = date, y = deaths, group = country, colour = fct_reorder(country, deaths)),
    size = 1
    ) +
  geom_point(data = df_wide_most_deaths,  aes(x = date, y = deaths, group = country, colour = fct_reorder(country, deaths))) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(1, 3, 1, 1), "lines"),
    plot.caption = element_text(colour = "grey45")
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = c(0, 0),
    sec.axis = sec_axis(~., breaks = labs %>% pull(deaths), labels = labs %>% pull(country))
  ) +
  scale_x_date(
    date_labels = "%d %b",
    date_breaks = "7 day",
    expand = c(0, 0),
    limits = c(as.Date("2020-03-01"), NA)
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  scale_color_discrete_qualitative() +
  labs(
    title = "Number of deaths from COVID-19",
    x = "",
    y = "Total number of deaths",
    caption = custom_caption
  )
```

<img src="README_figs/README-cum-deaths-1.png" width="672" />

### Deaths per capita

``` r
df %>%
  filter(case_type == "deaths") %>%
  left_join(populations, by = "country_code") %>%
  filter(pop > 10^6, cases > 0) %>%
  mutate(per_capita = 100000 * (cases/pop)) %>%
  ggplot(aes(x = date, y = per_capita, group = country_code, colour = continent)) +
  geom_line(alpha = 0.7) +
  facet_grid(. ~ continent) +
  theme_minimal() +
  theme(
    aspect.ratio = 1,
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)
  ) +
  scale_x_date(
    date_labels = "%d %b",
    date_breaks = "21 day",
    expand = c(0, 0),
    limits = c(min(df_wide_all$date), NA)
  ) +
  scale_colour_discrete_qualitative() +
  labs(
    title = "Deaths per capita",
    y = "Deaths per capita",
    x = "",
    caption = custom_caption
  ) +
  scale_y_log10(
    label = scales::label_comma()
  )
```

<img src="README_figs/README-death-per-capita-continent-1.png" width="960" />

## Mortality rates

What is the fatality rate of COVID-19? Let’s look at the number of
deaths divided by the number of confirmed cases. This comes with many
caveats. Most notably that the analysis is probably incorrect. See the
following link for a discussion on how best to calculate fatality rates:
<https://www.worldometers.info/coronavirus/coronavirus-death-rate/#correct>

``` r
# Find rates for each row
mortality_rates <-
  df %>%
  select(case_type, country, date, cases, continent) %>%
  pivot_wider(
    names_from = case_type,
    values_from = cases
  ) %>%
  filter(confirmed > 2000) %>%
  mutate(mortality_rate = 100 * deaths / (confirmed)) %>%
  ungroup()

# Summarise data
summary_latest_mortality_rate <-
  mortality_rates %>%
  group_by(country) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  summarise(
    median_mort = median(mortality_rate),
    mean_mort = mean(mortality_rate),
    sd = sd(mortality_rate),
    se = sd / sqrt(n())
  )

# Which countries to highlight (arbitrary)
highlight_countries <- c("Italy", "Iran", "United Kingdom", "China", "US", "Sweden", "Germany")

# Labels for highlighted countries
labs <-
  mortality_rates %>%
  group_by(country) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  arrange(desc(mortality_rate)) %>%
  filter(country %in% highlight_countries) %>%
  mutate(
    label = glue("  { country } ({ deaths } / { confirmed })")
  )

ggplot() +
  geom_line(
    data = mortality_rates, 
    aes(x = date, y = mortality_rate, group = country), 
    colour = "#CCCCCC"
    #alpha = 0.2
    ) +
  geom_hline(
    yintercept = summary_latest_mortality_rate$median_mort, 
    linetype = 2, 
    size = 0.75
    ) +
  geom_line(
    data = mortality_rates %>% filter(country %in% highlight_countries),
    aes(x = date, y = mortality_rate, group = country, colour = country),
    size = 1
  ) +
  geom_point(
    data = mortality_rates %>% filter(country %in% highlight_countries) %>% filter(date == max(date)),
    aes(x = date, y = mortality_rate, group = country, colour = country),
    size = 2.5,
    shape = 21,
    fill = "#FFFFFF",
    stroke = 1.5
  ) +
  geom_label(
    aes(x = as.Date("2020-01-30"), y = (summary_latest_mortality_rate$median_mort + 0.39)), 
    label = "Median", 
    label.size = NA, 
    fill = "#FFFFFF", 
    label.padding = unit(0.1, "cm")
    ) +
  theme_minimal() +
  labs(
    title = "Fatality rate of COVID-19",
    subtitle = "Fatality rate (deaths / confirmed cases) on a daily basis for each country with 2000+ confirmed cases",
    x = "",
    y = "Mortality rate (%)",
    caption = custom_caption
  ) +
  scale_x_date(
    expand = c(0, 0),
    date_breaks = "7 day",
    date_labels = "%d %b"
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = 0:50,
    expand = c(0.01, 0.25),
    sec.axis = sec_axis(~., breaks = labs %>% pull(mortality_rate) + 0.1, labels = labs %>% pull(label))
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(colour = "grey45")
  ) +
  scale_color_discrete_qualitative(palette = "Dark 3")
```

<img src="README_figs/README-fatality-rate-over-time-1.png" width="864" />

## Cases within the United States

``` r
# Import daily _state_ level
daily_state_cases  <-
  read_csv("https://covidtracking.com/api/v1/states/daily.csv") %>%
  rowwise(state) %>%
  mutate(
    state_abb = state,
    state = state.name[grep(state, state.abb)[1]]
  )

if (!file.exists(here("./data/state_info.csv"))) {
  #Extract and clean state information (interested in population mainly)
 state_info <-
"https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States" %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table(fill = TRUE) %>%
  janitor::clean_names() %>%
  filter(row_number() != 1) %>%
  rename(
    state = flag_name_postal_abbreviation_12,
    state_abb = flag_name_postal_abbreviation_12_2,
    established = ratificationor_admission_c,
    pop = population_d_14,
    total_area = total_area_15,
    land_area = land_area_15,
    water_area = water_area_15
    ) %>%
  select(
    state,
    state_abb,
    established,
    pop,
    ends_with("_area")
    ) %>%
  mutate_at(
    .vars = c(4:7),
    ~ gsub(",(?)", "", .) %>% as.numeric
    ) %>%
  mutate(
    established = mdy(established),
    state = gsub("\\[[A-Z]\\]", "", state)
  )

write_csv(state_info, here("./data/state_info.csv"))
}

state_info <- read_csv(here("./data/state_info.csv"))


states_map <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

states_map_complete <-
  states_map %>%
  left_join(state_info %>% mutate(ID = str_to_lower(state)), by = c("ID")) %>% # Combine with state info (population levels etc..)
  left_join(daily_state_cases, by = "state") %>%                               # Combine with covid-19 cases data
  mutate(per_capita =  100000 * (positive/pop))                                # Calculate per cpaita information.

states_map_complete %>%
  filter(dateChecked == max(dateChecked)) %>%
  ggplot() +
  geom_sf(aes(geometry = geom, fill = per_capita), colour = "#FFFFFF") +
  scale_fill_continuous_sequential(
    trans = "log10",
    palette = "Inferno",
    name = "Cases per capita"
  ) +
  geom_sf_text(data = states_map_complete %>% filter(state %in% c("New York")),
               stat = StatSfCoordinates, 
               fun.geometry = sf::st_centroid, 
               aes(geometry = geom, label = state_abb.x), 
               colour = "#FFFFFF",
               size = 3) +
  coord_sf(expand = T) +
  ggthemes::theme_map() + 
  labs(
    title = "Confirmed cases per capita"
  ) +
  theme(
    legend.position = "bottom"
  )
```

<img src="README_figs/README-state-import-1.png" width="672" />

``` r
states_map_complete %>%
  as.data.frame() %>%
  filter(positive > 10) %>%
  ggplot(aes(x = dateChecked, y = per_capita, group = state)) +
  geom_line(colour = "grey30", alpha = 0.5) +
  geom_line(data = . %>% filter(state_abb.x == "NY"), colour = "red", size = 1) +
  geom_line(data = . %>% filter(state_abb.x == "IL"), colour = "blue", size = 1) +
  theme_minimal() +
  scale_y_log10(
    label = scales::label_comma()
  ) +
  labs(
    x = "",
    y = "Confirmed cases per 100,000 per state",
    title = "Cases per capita over time for each state",
    subtitle = "red = NY, blue = IL",
    caption = custom_caption
  )
```

<img src="README_figs/README-cases-per-capita-per-state-1.png" width="672" />

### County-level statistics

Let’s look at cases for each county in the US to get a slightly more
nuanced view.

First, let’s read in the covid-19 cases
data:

``` r
# https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv

# Read and clean covid-19 cases on a county basis 
county_cases <-
  read_csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv") %>%
  pivot_longer(-c(countyFIPS, `County Name`, State, stateFIPS), 
               names_to = "date", 
               values_to = "confirmed_cases") %>%
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
```

Quick detour… How many new cases are there on a daily basis within each
state (Using this county data as above data has some irregularities? -
Jumps to 0 for confirmed cases) Also let’s split data based on those
states which have news-worthy reports of protests? Later lets bin data
on reopening dates…

``` r
county_cases %>%
  group_by(state_abb, state, date) %>%
  summarise(confirmed_cases = sum(confirmed_cases, na.rm = T)) %>%
  ungroup() %>%
  arrange(state, date) %>%
  group_by(state_abb, state) %>%
  mutate(daily_change = confirmed_cases - lag(confirmed_cases)) %>%
  ungroup() %>%
  filter(!is.na(state)) %>%
  mutate(protesting = if_else(state %in% c("Colorado", "Michigan", "Ohio", "North Carolina", "Minnesota", "Utah", "Virginia", "Kentucky", "Maryland", "Indiana"), "Currently protesting", "No protests"),
         protesting = fct_rev(protesting)) %>%
  ggplot(aes(x = date, y = daily_change)) +
  geom_line(aes(group = state, colour = protesting), alpha = 0.5) +
  geom_smooth(colour = "black") +
  facet_wrap(vars(protesting)) +
  scale_y_log10() +
  scale_x_date(
    date_labels = "%d %b",
    date_breaks = "7 day",
    expand = c(0, 0),
    limits = c(as.Date("2020-03-01"), NA)
  ) +
  theme_minimal() +
  theme(
    aspect.ratio = 1,
    legend.position = "none"
  ) +
  labs(
    x = "",
    y = "Daily new cases"
  )
```

<img src="README_figs/README-unnamed-chunk-2-1.png" width="672" />

Some reports the drop we see in daily cases is almost entirely due to
drops in NY… let’s see…

``` r
county_cases %>%
  mutate(dummy = if_else(state_abb == "NY", "NY", "Rest of US")) %>%
  group_by(dummy, date) %>%
  summarise(confirmed_cases = sum(confirmed_cases, na.rm = T)) %>%
  ungroup() %>%
  arrange(dummy, date) %>%
  group_by(dummy) %>%
  mutate(daily_change = confirmed_cases - lag(confirmed_cases)) %>%
  ungroup() %>%
  select(-confirmed_cases) %>%
  pivot_wider(names_from = dummy, values_from = daily_change) %>%
  mutate(Total = `Rest of US` + NY) %>%
  pivot_longer(-date, names_to = "dummy", values_to = "daily_change") %>%
  ggplot(aes(x = date, y = daily_change)) +
  geom_line(aes(group = dummy, colour = dummy), alpha = 0.5) +
  geom_point(aes(group = dummy, colour = dummy), alpha = 0.25) +
  geom_smooth(aes(group = dummy, colour = dummy), se = FALSE) +
  #geom_smooth(colour = "black") +
  scale_x_date(
    date_labels = "%d %b",
    date_breaks = "7 day",
    expand = c(0, 0),
    limits = c(as.Date("2020-03-01"), NA)
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    aspect.ratio = 1
  ) +
  labs(
    colour = "",
    x = "",
    y = "Daily new cases",
    subtitle = "loess fit (y ~ x)"
  )
```

<img src="README_figs/README-unnamed-chunk-3-1.png" width="672" />

Yikes, maybe some merit to that….

-----

Now we want to find the populations for each county within each state. I
came across a website - or strictly many websites - which follow the
same template where `x` in
`www.x-demographics.com/counties_by_population` is the state name
without spaces. I realised I can iterate over this website(s) for each
state using `map_dfr()` and end up with a dataframe containing the
county-level populations for all
states.

``` r
# Check if we've already scraped the data as to not hammer their servers.
if (!file.exists("./data/all_county_pops.csv")) {
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
      #TODO: come up with more elegant solution, rowwise agrep?
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
  # Write to file
  write_csv(county_pops, here("./data/all_county_pops.csv"))
}

county_pops <- read_csv("./data/all_county_pops.csv")
```

Now let’s import the county shape files:

``` r
# Get map of all counties
county_maps <- read_sf(here('./data/sf_files/county'))
```

This next block ties all our data together. It’s easier to read this
chunk inside out.

  - First we take our `county_cases` (covid-19 incidences) and join that
    with `county_pops` (populations) using the `county` and `state`
    variables to uniquely match rows.
  - Once this is done, I add two new columns, `COUNTYFP` and `STATEFP`.
    These are unique identifying numbers for each county and state.
      - This information already exists in the `county_fips` column in
        `county_cases` but I’ve reformatted them so it matches the
        structure in `county_maps`.
  - At this point we’ll also calculate per-capita rates of covid-19
    cases.
  - Now we have `county_cases` and `county_pops` combined, we next
    combine all that information into `county_maps`.
  - Finally, limit the data to mainland US and add a custom label which
    will be useful later.

<!-- end list -->

``` r
# Combine it all together
complete_df <-
  # Take our county map and combine it with cases data
  county_maps %>%
  left_join(
    # Before joining above, combine case data with county populations
    county_cases %>%
      left_join(county_pops, by = c("county", "state")) %>%
      filter(county != "Statewide Unallocated") %>%
      # Mutate this nested left-joined database so we have STATEFP/COUNTYFP
      mutate(
        COUNTYFP = as.character(county_fips) %>% str_sub(., -3),
        STATEFP = as.character(county_fips) %>% str_remove(string = ., pattern = ".{3}$") %>% str_pad(width = 2, side = "left", pad = "0"),
        per_capita = (confirmed_cases / population) * 100000
      ) %>%
      select(county_fips, state_fips, STATEFP, COUNTYFP, county, everything()),
    # Left join by the STATEFP/COUNTYFP we just worked out
    by = c("STATEFP", "COUNTYFP")
  ) %>%
  filter(
    # Limit it to mainland US
    state_abb %in% state.abb,
    state_abb != "AK",
    state_abb != "HI"
  ) %>%
  mutate(
    label = glue("{ day(date) } { month(date, label = TRUE) }")# '{ year(date) - 2000 }")
  )
```

Doing all of the above leaves us with a dataframe which looks like the
following:

``` r
str(complete_df)
```

    ## tibble [357,305 x 21] (S3: sf/tbl_df/tbl/data.frame)
    ##  $ STATEFP        : chr [1:357305] "21" "21" "21" "21" ...
    ##  $ COUNTYFP       : chr [1:357305] "007" "007" "007" "007" ...
    ##  $ COUNTYNS       : chr [1:357305] "00516850" "00516850" "00516850" "00516850" ...
    ##  $ AFFGEOID       : chr [1:357305] "0500000US21007" "0500000US21007" "0500000US21007" "0500000US21007" ...
    ##  $ GEOID          : chr [1:357305] "21007" "21007" "21007" "21007" ...
    ##  $ NAME           : chr [1:357305] "Ballard" "Ballard" "Ballard" "Ballard" ...
    ##  $ LSAD           : chr [1:357305] "06" "06" "06" "06" ...
    ##  $ ALAND          : num [1:357305] 6.39e+08 6.39e+08 6.39e+08 6.39e+08 6.39e+08 ...
    ##  $ AWATER         : num [1:357305] 69473325 69473325 69473325 69473325 69473325 ...
    ##  $ geometry       :sfc_MULTIPOLYGON of length 357305; first list element: List of 1
    ##   ..$ :List of 1
    ##   .. ..$ : num [1:294, 1:2] -89.2 -89.2 -89.2 -89.2 -89.2 ...
    ##   ..- attr(*, "class")= chr [1:3] "XY" "MULTIPOLYGON" "sfg"
    ##  $ county_fips    : num [1:357305] 21007 21007 21007 21007 21007 ...
    ##  $ state_fips     : num [1:357305] 21 21 21 21 21 21 21 21 21 21 ...
    ##  $ county         : chr [1:357305] "Ballard County" "Ballard County" "Ballard County" "Ballard County" ...
    ##  $ state_abb      : chr [1:357305] "KY" "KY" "KY" "KY" ...
    ##  $ date           : Date[1:357305], format: "2020-01-22" "2020-01-23" ...
    ##  $ confirmed_cases: num [1:357305] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ state          : chr [1:357305] "Kentucky" "Kentucky" "Kentucky" "Kentucky" ...
    ##  $ rank           : num [1:357305] 109 109 109 109 109 109 109 109 109 109 ...
    ##  $ population     : num [1:357305] 8090 8090 8090 8090 8090 8090 8090 8090 8090 8090 ...
    ##  $ per_capita     : num [1:357305] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ label          : 'glue' chr [1:357305] "22 Jan" "23 Jan" "24 Jan" "25 Jan" ...
    ##  - attr(*, "sf_column")= chr "geometry"
    ##  - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA NA NA NA NA NA NA ...
    ##   ..- attr(*, "names")= chr [1:20] "STATEFP" "COUNTYFP" "COUNTYNS" "AFFGEOID" ...

This dataframe now has the geometry for each county and crucially the
number of cases (and per-capita information) for each day from
2020-01-22 to 2020-05-15.

Now let’s build some
maps\!

``` r
map_seq_plot <- function(df, min_date, max_date, range, title, subtitle, caption, ext) {
  
  # TODO: Take any number of specific days (e.g. 01-01, 01-02, 01-03...)
  # TODO: Some way to specify colour range - do we want min/max to be across whole 
  #       dataset, or just cover the values in the range/maps present
  
  # Do we want to cover a consecutive range of dates or two specific dates?
  if (range == "yes") {
    # Sequence of dates to plot
    date_seq <- seq(as.Date(min_date), as.Date(max_date), by = 1)
    
    # Lims for colour bar
    custom_lims <- c(
      min(df$per_capita[(df$per_capita > 0 & (df$date >= min_date & df$date <= max_date))], na.rm = T),
      max(df$per_capita[df$date >= min_date & df$date <= max_date], na.rm = T)
    )
  } else {
    # Discrete dates to plot
    date_seq <- c(as.Date(min_date), as.Date(max_date))
    
    # Lims for colour bar
    custom_lims <- c(
      min(df$per_capita[(df$per_capita > 0)  & (df$date == min_date | df$date == max_date)], na.rm = T),
      max(df$per_capita[df$date == max_date | df$date == max_date], na.rm = T)
    )
  }
  
  # Get base map
  country_map <- tigris::states() %>%
    sf::st_as_sf() %>%
    filter(
      NAME %in% state.name,
      NAME != "Alaska",
      NAME != "Hawaii"
    )
  
  # Initialise plot list
  plot_list <- list()
  
  # Loop through and plot each date
  for (i in 1:length(date_seq)) {
    
    # Data for day i
    filtered_df <-
      df %>%
      filter(date == date_seq[i]) %>%
      filter(per_capita > 0)
    
    # Title label
    plot_label <- df$label[which.max(df$date == date_seq[i])]
    
    # Plot and assign to plot_list
    plot_list[[i]] <-
      ggplot() +
      geom_sf(data = country_map, aes(geometry = geometry), fill = NA, colour = NA) +
      geom_sf(data = filtered_df, aes(fill = per_capita, geometry = geometry), colour = NA) +
      coord_sf(
        crs = 4326,
        expand = F,
        clip = "off"
      ) +
      ggthemes::theme_map() +
      scale_fill_viridis_c(
        name = "Cases per capita",
        limits = custom_lims,
        option = "inferno",
        direction = -1,
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
      ## For some reason colorspace does not work here.... It can't pass on limits
      ## TODO: figure out if bug or poor implementation
      # colorspace::scale_fill_continuous_sequential(
      #   palette = "Inferno",
      #   name = "Cases per capita",
      #   limits = custom_lims
      # )
      # )
  }
  
  # How many columns? Limit to 7 but less if only plotting < 7
  n_col <- ifelse(length(date_seq) < 7, length(date_seq), 7)
  
  # Combine/wrap plots using patchwork
  plot_wrapped <-
     wrap_plots(plot_list, ncol = n_col, guides = "collect") +
    plot_annotation(
      title = title,
      subtitle = subtitle,
      caption = caption
    ) &
    theme(
      legend.position = "bottom",
      legend.justification = "right",
      plot.caption = element_text(colour = "grey30"),
      plot.title = element_text(size = ceiling(12 * 1.1), face = "bold"),
      plot.subtitle = element_text(size = ceiling(12 * 1.05))
    )
  
  # Save to file
  print(glue::glue("Output saved to: output/{ min_date }-{ max_date }.{ ext }"))
  ggsave(filename = glue::glue("./output/{ min_date }-{ max_date }.{ ext }"), plot = plot_wrapped, height = 6.5, width = 10, unit = "in")
}
```

``` r
min_date <- "2020-03-01"
max_date <- "2020-03-31"
map_seq_plot(df = complete_df, 
             min_date = min_date, 
             max_date = max_date, 
             range = "yes", 
             title = "A month of coronavirus",
             subtitle =  "Covid-19 cases throughout March",
             caption = glue("Last updated: { Sys.Date() }"),
             ext = "png")

knitr::include_graphics(glue("./output/{ min_date }-{ max_date }.png"))
```

``` r
min_date <- as.Date("2020-04-01")
max_date <- as.Date(max(complete_df$date, na.rm = T))

map_seq_plot(df = complete_df, 
             min_date = min_date, 
             max_date = max_date, 
             range = "yes", 
             title = "1st April to Present",
             subtitle =  "Covid-19 cases per capita from beinning of April to present",
             caption = glue("Last updated: { Sys.Date() }"),
             ext = "png")
```

    ## Output saved to: output/2020-04-01-2020-05-15.png

``` r
knitr::include_graphics(glue("./output/{ min_date }-{ max_date }.png"))
```

<img src="./output/2020-04-01-2020-05-15.png" width="1500" />

``` r
# hacky way to get one day only
min_date <- as.Date(max(complete_df$date, na.rm = T))
max_date <- as.Date(max(complete_df$date, na.rm = T))

map_seq_plot(df = complete_df, 
             min_date = min_date, 
             max_date = max_date, 
             range = "yes", 
             title = glue("Cases as of { max_date}."),
             subtitle =  "Covid-19 cases per capita for most recent day",
             caption = glue("Last updated: { Sys.Date() }"),
             ext = "png")
```

    ## Output saved to: output/2020-05-15-2020-05-15.png

``` r
knitr::include_graphics(glue("./output/{ min_date }-{ max_date }.png"))
```

<img src="./output/2020-05-15-2020-05-15.png" width="1500" />

#### Illinois and surrounding states

Let’s take a closer look at my home state, Illinois, and surrounding
states. This is basically same process as above but while figuring out
how to do this stuff, I came across a neat trick: you can simply
`group_by()` and `summarise()` to get boundaries for each state from the
`county` shape files.

``` r
max_date <- max(complete_df$date, na.rm = T)

complete_df %>%
  filter(state_abb %in% c("IL", "WI", "MI", "IN")) %>% #, "MO", "IA"
  filter(date == max(date, na.rm = T)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = per_capita), colour = "grey90") +
  geom_sf(data = . %>% group_by(state_abb) %>% summarise(), colour = "grey15", fill = NA) +
  #geom_sf_label(data = . %>% group_by(state) %>% summarise(), aes(label = state), fun.geometry = sf::st_centroid) +
  coord_sf(crs = 4326, 
           expand = TRUE,
           clip = "off") +
  scale_fill_continuous_sequential(
    palette = "Inferno",
    name = "Cases per capita",
    trans = "log10",
    na.value = "grey90",
    labels = scales::label_number(accuracy = 0.1),
    guide = guide_colorbar(
     barwidth = unit(5, "cm"),
     frame.colour = "black",
     frame.linewidth = 1,
     frame.linetype = 1,
     draw.llim = FALSE,
     draw.ulim = FALSE
    )
  ) +
  ggthemes::theme_map() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    panel.spacing = unit(0.5, "lines"),
  ) +
  labs(
    title = "Covid-19 cases in Illinois and neighbouring states",
    subtitle = glue("Cases as of {max_date }"),
    caption = glue("Last updated: { Sys.Date() }")
  ) +
  annotate(geom = "text", x = -92, y = 38, label = "Illinois") +
  annotate(geom = "text", x = -83.5, y = 39, label = "Indiana") +
  annotate(geom = "text", x = -82, y = 45, label = "Michigan") +
  annotate(geom = "text", x = -93, y = 43, label = "Wisconsin") 
```

<img src="README_figs/README-illinois-and-surrounding-states-1.png" width="480" />

#### Illinois over time

How have cases within Illinois progressed over the past month?

``` r
complete_df %>%
  filter(state_abb %in% c("IL")) %>%
  filter(date >= max(date, na.rm = T) - days(30)) %>%
 # mutate(label = format(date, "%d %b")) %>%
  ggplot(aes(fill = per_capita)) +
  geom_sf(aes(geometry = geometry)) +
  coord_sf(crs = 4326, 
           expand = TRUE) +
  scale_fill_continuous_sequential(
    palette = "Inferno",
    name = "Cases per capita",
    trans = "log10",
    na.value = "white",
    labels = scales::label_number(accuracy = 0.1),
      guide = guide_colorbar(
      barwidth = unit(5, "cm"),
      frame.colour = "black",
      frame.linewidth = 1,
      frame.linetype = 1,
      draw.llim = FALSE,
      draw.ulim = FALSE
    )
  ) +
  ggthemes::theme_map() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    panel.spacing = unit(0.5, "lines"),
  ) +
  facet_wrap(date ~ ., ncol = 7) 
```

<img src="README_figs/README-illnois-over-time-1.png" width="672" />

Remarkable increase in just a month. From very few counties having 0 to
~ 10 cases per capita to nearly every county having \> 50 cases per
100,000 people.

#### Chicago

What does Chicago cases look like? Unfortunately, this is not current as
I had to copy/paste the data into R (data is from 2020-04-13). I was
unable to find a good source for these data that I could automatically
import into
R.

``` r
# https://data.nbcstations.com/national/2020/coronavirus/local-maps/il_zip.html
# TODO: understand how to scrape the above page

chicago_cases <-
data.frame(
         zip = c(53142L,60002L,60004L,60005L,60007L,
                 60008L,60010L,60012L,60013L,60014L,60015L,60016L,60018L,
                 60020L,60021L,60022L,60025L,60026L,60030L,60031L,60033L,
                 60035L,60040L,60041L,60042L,60043L,60044L,60045L,60046L,
                 60047L,60048L,60050L,60051L,60053L,60056L,60060L,60061L,
                 60062L,60064L,60067L,60068L,60069L,60070L,60073L,60074L,
                 60076L,60077L,60081L,60083L,60084L,60085L,60087L,
                 60088L,60089L,60090L,60091L,60093L,60096L,60097L,60098L,
                 60099L,60101L,60102L,60103L,60104L,60106L,60107L,60108L,
                 60110L,60112L,60115L,60118L,60119L,60120L,60123L,60124L,
                 60126L,60130L,60131L,60133L,60134L,60135L,60136L,60137L,
                 60139L,60140L,60141L,60142L,60143L,60148L,60152L,60153L,
                 60154L,60155L,60156L,60160L,60162L,60163L,60164L,60165L,
                 60169L,60171L,60172L,60173L,60174L,60175L,60176L,60177L,
                 60178L,60181L,60185L,60187L,60188L,60189L,60190L,
                 60191L,60192L,60193L,60194L,60195L,60201L,60202L,60203L,
                 60301L,60302L,60304L,60305L,60401L,60402L,60403L,60404L,
                 60406L,60408L,60409L,60410L,60411L,60415L,60417L,60418L,
                 60419L,60422L,60423L,60425L,60426L,60428L,60429L,60430L,
                 60431L,60432L,60433L,60434L,60435L,60436L,60438L,60439L,
                 60440L,60441L,60442L,60443L,60445L,60446L,60447L,60448L,
                 60449L,60450L,60451L,60452L,60453L,60455L,60456L,60457L,
                 60458L,60459L,60461L,60462L,60463L,60464L,60465L,
                 60466L,60467L,60468L,60469L,60471L,60472L,60473L,60475L,
                 60476L,60477L,60478L,60480L,60481L,60482L,60484L,60487L,
                 60490L,60491L,60501L,60502L,60503L,60504L,60505L,60506L,
                 60510L,60513L,60514L,60515L,60516L,60517L,60521L,60523L,
                 60525L,60526L,60527L,60532L,60534L,60538L,60540L,60542L,
                 60543L,60544L,60545L,60546L,60548L,60551L,60554L,60555L,
                 60558L,60559L,60560L,60561L,60563L,60564L,60565L,60585L,
                 60586L,60601L,60604L,60605L,60606L,60607L,60608L,
                 60609L,60610L,60611L,60612L,60613L,60614L,60615L,60616L,
                 60617L,60618L,60619L,60620L,60621L,60622L,60623L,60624L,
                 60625L,60626L,60628L,60629L,60630L,60631L,60632L,60633L,
                 60634L,60636L,60637L,60638L,60639L,60640L,60641L,60642L,
                 60643L,60644L,60645L,60646L,60647L,60649L,60651L,60652L,
                 60653L,60654L,60655L,60656L,60657L,60659L,60660L,60661L,
                 60706L,60707L,60712L,60714L,60803L,60804L,60805L,60827L,
                 60901L,60914L,60915L,60950L,60954L,60958L,60964L,
                 61008L,61016L,61021L,61032L,61065L,61068L,61071L,61073L,
                 61080L,61081L,61101L,61102L,61103L,61104L,61107L,61108L,
                 61109L,61111L,61114L,61115L,61201L,61235L,61241L,61244L,
                 61252L,61264L,61265L,61282L,61342L,61401L,61462L,61554L,
                 61603L,61604L,61611L,61614L,61615L,61701L,61704L,61705L,
                 61761L,61764L,61801L,61802L,61820L,61821L,61822L,61853L,
                 61874L,61910L,61920L,61938L,62002L,62010L,62024L,62025L,
                 62034L,62035L,62040L,62052L,62056L,62060L,62069L,
                 62090L,62095L,62201L,62203L,62204L,62205L,62206L,62207L,
                 62208L,62220L,62221L,62223L,62226L,62231L,62233L,62234L,
                 62236L,62249L,62254L,62258L,62265L,62269L,62278L,62286L,
                 62288L,62294L,62295L,62298L,62301L,62305L,62448L,62454L,
                 62471L,62521L,62522L,62526L,62568L,62618L,62650L,62684L,
                 62702L,62703L,62704L,62707L,62711L,62801L,62864L,62901L,
                 62959L,62966L,62992L),
       cases = c(6L,41L,70L,53L,75L,66L,35L,38L,37L,
                 62L,87L,440L,163L,15L,6L,17L,168L,76L,50L,111L,33L,
                 134L,33L,14L,7L,14L,42L,79L,64L,58L,50L,71L,29L,
                 118L,188L,105L,60L,115L,231L,45L,132L,22L,57L,187L,
                 101L,218L,175L,23L,31L,76L,763L,200L,20L,89L,168L,85L,
                 92L,14L,17L,96L,202L,184L,39L,61L,195L,159L,140L,65L,
                 90L,8L,31L,19L,11L,133L,115L,19L,114L,54L,101L,
                 153L,40L,6L,15L,45L,133L,23L,7L,27L,27L,111L,12L,161L,
                 88L,63L,34L,172L,67L,30L,135L,48L,85L,44L,41L,15L,
                 31L,26L,32L,36L,12L,86L,114L,39L,151L,41L,11L,43L,
                 18L,71L,38L,6L,156L,142L,15L,12L,100L,38L,22L,27L,
                 406L,60L,44L,127L,6L,243L,15L,361L,94L,72L,41L,172L,
                 54L,71L,57L,203L,103L,119L,129L,80L,73L,88L,132L,273L,
                 60L,147L,48L,231L,91L,20L,187L,61L,155L,13L,54L,
                 41L,15L,53L,74L,248L,84L,16L,49L,33L,122L,34L,111L,
                 57L,19L,66L,305L,80L,14L,26L,110L,39L,211L,40L,6L,88L,
                 149L,14L,12L,24L,57L,59L,70L,73L,66L,29L,17L,71L,
                 249L,180L,31L,60L,25L,32L,43L,77L,43L,38L,99L,27L,
                 142L,46L,45L,49L,41L,25L,64L,63L,18L,67L,9L,15L,10L,
                 34L,16L,109L,27L,57L,49L,58L,82L,39L,120L,41L,6L,
                 97L,26L,189L,522L,457L,125L,83L,331L,196L,238L,213L,
                 215L,422L,426L,506L,603L,254L,257L,769L,368L,431L,386L,
                 470L,799L,256L,115L,648L,70L,472L,296L,286L,363L,
                 825L,375L,390L,57L,409L,508L,551L,100L,360L,381L,509L,
                 270L,269L,88L,149L,116L,182L,318L,187L,22L,120L,245L,
                 93L,237L,83L,608L,102L,158L,178L,52L,18L,16L,9L,7L,
                 9L,59L,12L,13L,12L,10L,68L,16L,8L,6L,30L,21L,33L,
                 30L,27L,21L,54L,36L,12L,20L,12L,65L,6L,12L,89L,6L,
                 11L,90L,22L,9L,27L,43L,8L,7L,12L,14L,16L,12L,33L,
                 17L,8L,18L,8L,12L,18L,7L,12L,26L,8L,6L,6L,10L,9L,
                 41L,8L,7L,76L,23L,8L,43L,9L,8L,8L,7L,6L,9L,10L,13L,
                 11L,13L,35L,22L,25L,35L,31L,23L,55L,36L,39L,23L,
                 32L,9L,10L,12L,8L,34L,13L,10L,9L,10L,6L,25L,15L,13L,
                 39L,6L,14L,69L,16L,12L,24L,18L,13L,20L,20L,8L,9L,
                 7L,11L,40L,69L,44L,12L,8L,11L)
)


read_sf(here::here('./data/sf_files/chicago_zips/geo_export_ebae7c39-2563-4d20-bdb6-3f3b9fb5c350.shp')) %>%
  left_join(chicago_cases %>% mutate(zip = as.character(zip)), by = "zip") %>%
  ggplot() +
  geom_sf(aes(fill = cases), colour = "grey90") +
  geom_sf(data = . %>% summarise(), colour = "black", fill = NA) +
  geom_sf_text(aes(label = zip),
    size = 2,
    colour = "white") +
  #coord_quickmap() +
  coord_sf(crs = "EPSG:4269", 
            expand = TRUE) +
  scale_fill_continuous_sequential(
    palette = "Inferno",
    name = "Cases per capita",
    trans = "log10",
    na.value = "grey90",
    labels = scales::label_number(accuracy = 0.1),
    guide = guide_colorbar(
     barwidth = unit(5, "cm"),
     frame.colour = "black",
     frame.linewidth = 1,
     frame.linetype = 1,
     draw.llim = FALSE,
     draw.ulim = FALSE
    )
  ) +
  ggthemes::theme_map() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    panel.spacing = unit(0.5, "lines"),
  ) +
  labs(
    title = "Confirmed cases per capita within Chicago",
    subtitle = "Cases as of 2020-04-25",
    caption = glue("Last updated: { Sys.Date() }")
  )
```

<img src="README_figs/README-Chicago-enlarged-1.png" width="672" />
