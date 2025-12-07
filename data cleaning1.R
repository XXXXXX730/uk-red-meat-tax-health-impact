############################################################
# Meat tax health-impact modelling: UK, 1990–2022
#
# Data sources
#   - Per-capita meat consumption by type (kg/person/year)
#       Our World in Data (OWID) "Per capita meat consumption by type"
#       Grapher: https://ourworldindata.org/grapher/per-capita-meat-consumption-by-type-kilograms-per-year
#       Filtered for United Kingdom (GBR), 1990–2022.
#
#   - DALYs attributable to diet high in red meat
#       Global Burden of Disease (GBD) study, Institute for Health Metrics and Evaluation (IHME)
#       Extracted via GBD Compare (Risk factor: "Diet high in red meat",
#       Measure: DALYs, Metric: Rate, Location: United Kingdom,
#       Years: 1990–2022, all ages, both sexes).
#       https://vizhub.healthdata.org/gbd-compare
#
#   - Population denominators
#       World Bank Open Data, World Development Indicators (WDI)
#       Indicator: SP.POP.TOTL (total population), Country: United Kingdom,
#       Annual estimates for 1990–2022.
#       https://data.worldbank.org
#
# Purpose
#   - Merge GBD red-meat–attributable DALY data with OWID meat supply data
#     and World Bank population counts
#   - Simulate a 20% ad valorem tax on red meat
#   - Estimate Potential Impact Fraction (PIF)
#   - Compute baseline and post-tax DALYs, and DALYs averted, over 1990–2022
############################################################

## 0. Load packages ----

library(tidyverse)   # Wickham et al., 2019
library(skimr)       # Waring et al., 2022
library(ggplot2)     # part of tidyverse; listed explicitly for clarity
library(patchwork)   # for combining plots

## 1. Import data ----

daly  <- read.csv("daly_data.csv")             # GBD-derived DALYs per 100k
meat  <- read.csv("meat_consumption.csv")      # FAO/OWID meat supply data

# Quick structure check
skimr::skim(daly)
skimr::skim(meat)

## 2. Add population data and merge ----

# UK mid-year population estimates (1990–2022) From the open world bank population estimates
population_uk <- tibble(
  Year = 1990:2022,
  population = c(
    56928327, 57076711, 57247586, 57424897, 57580402,
    57718614, 57865745, 58019030, 58166950, 58487141,
    58682466, 58892514, 59119673, 59370479, 59647577,
    59987905, 60401206, 60846820, 61322463, 61806995,
    62276270, 62766365, 63258810, 63711000, 64139000,
    64620000, 65088000, 65607000, 66289000, 66631000,
    66744000, 66984000, 67604000
  )
)

# Merge DALY + meat + population by Year
merged <- daly %>%
  inner_join(meat,  by = "Year") %>%
  inner_join(population_uk, by = "Year")

glimpse(merged)
head(merged)

## Assumed columns:
## - Year
## - Value   : DALYs per 100,000 attributable to diet high in red meat
## - total.red.meat : total red meat supply (kg/person/year) : This was computed in Excel by summing sheep/goat/beef/buffalo/pig meat

## 3. Convert red meat intake and apply tax scenario ----

# Convert kg/person/year → g/person/day
merged <- merged %>%
  mutate(
    red_meat_g_day = total.red.meat * 1000 / 365
  )

# Tax and elasticity parameters
tax_rate   <- 0.20     # 20% ad valorem tax
elasticity <- -0.7     # own-price elasticity of demand for red meat

# Proportional change in intake under the tax
prop_change <- elasticity * tax_rate  # e.g. -0.14 for ε = -0.7 and 20% tax

merged <- merged %>%
  mutate(
    red_meat_g_day_baseline = red_meat_g_day,
    red_meat_g_day_posttax  = red_meat_g_day * (1 + prop_change)
  )

## 4. Relative risk model and Potential Impact Fraction ----

# Dose–response assumption:
# RR per +50 g/day red/processed meat (from meta-analyses; e.g. WCRF/AICR)
RR50  <- 1.18
beta  <- log(RR50) / 50    # RR(x) = exp(beta * x), x in grams/day

# Theoretical minimum risk exposure level (TMREL) for red meat (g/day)
tmrel <- 0   # can be updated if using GBD-specific TMREL

merged <- merged %>%
  mutate(
    # Exposure above TMREL at baseline and under the tax
    x0 = pmax(red_meat_g_day_baseline - tmrel, 0),
    x1 = pmax(red_meat_g_day_posttax  - tmrel, 0),
    # Relative risks at each exposure level
    RR0 = exp(beta * x0),
    RR1 = exp(beta * x1),
    # Potential Impact Fraction (single-exposure approximation)
    PIF = (RR0 - RR1) / RR0
  )


## 5. DALYs: baseline, post-tax, and averted ----

merged <- merged %>%
  mutate(
    # From GBD: DALYs per 100,000 attributable to diet high in red meat
    DALYs_attr_baseline_per100k = Value,
    # DALYs per 100,000 averted under the tax scenario
    DALYs_attr_averted_per100k  = PIF * DALYs_attr_baseline_per100k,
    # Post-tax attributable DALYs per 100,000
    DALYs_attr_posttax_per100k  = DALYs_attr_baseline_per100k - DALYs_attr_averted_per100k,
    # Convert to absolute counts using annual population
    DALYs_attr_baseline_total   = DALYs_attr_baseline_per100k * population / 100000,
    DALYs_attr_averted_total    = DALYs_attr_averted_per100k  * population / 100000,
    DALYs_attr_posttax_total    = DALYs_attr_posttax_per100k  * population / 100000
  )

head(merged)

## 6. Visualisation ----

# 6.1 DALYs averted per 100k (Figure A)
p1 <- ggplot(merged, aes(x = Year, y = DALYs_attr_averted_per100k)) +
  geom_line(linewidth = 0.6, colour = "#222222") +
  geom_point(size = 1.8) +
  labs(
    title    = "DALYs averted under a 20% red meat tax",
    subtitle = "United Kingdom, 1990–2022",
    x        = "Year",
    y        = "DALYs averted per 100,000 population"
  ) +
  theme_minimal(base_size = 13)

# 6.2 Red meat intake at baseline vs post-tax (Figure B)
p2 <- ggplot(merged, aes(x = Year)) +
  geom_line(aes(y = red_meat_g_day_baseline, colour = "Baseline"), linewidth = 0.7) +
  geom_point(aes(y = red_meat_g_day_baseline, colour = "Baseline"), size = 1.8) +
  geom_line(aes(y = red_meat_g_day_posttax,  colour = "Post-tax"), linewidth = 0.7) +
  geom_point(aes(y = red_meat_g_day_posttax,  colour = "Post-tax"), size = 1.8) +
  scale_color_manual(
    values = c("Baseline" = "#222222", "Post-tax" = "#666666")
  ) +
  labs(
    title    = "Red meat intake at baseline vs tax scenario",
    subtitle = "United Kingdom, 1990–2022",
    x        = "Year",
    y        = "Grams per person per day",
    colour   = "Scenario"
  ) +
  theme_minimal(base_size = 13)

# 6.3 PIF over time (Figure C)
p3 <- ggplot(merged, aes(x = Year, y = PIF)) +
  geom_line(linewidth = 0.7, colour = "#0A0F14") +
  geom_point(size = 1.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title    = "Potential Impact Fraction (PIF) of red meat tax",
    subtitle = "United Kingdom, 1990–2022",
    x        = "Year",
    y        = "PIF (%)"
  ) +
  theme_minimal(base_size = 13)

# 6.4 Baseline vs post-tax attributable DALYs (Figure D)
p4 <- ggplot(merged, aes(x = Year)) +
  geom_line(aes(y = DALYs_attr_baseline_total, colour = "Baseline"), linewidth = 0.7) +
  geom_point(aes(y = DALYs_attr_baseline_total, colour = "Baseline"), size = 1.8) +
  geom_line(aes(y = DALYs_attr_posttax_total,  colour = "Post-tax"), linewidth = 0.7) +
  geom_point(aes(y = DALYs_attr_posttax_total,  colour = "Post-tax"), size = 1.8) +
  scale_color_manual(
    values = c("Baseline" = "#222222", "Post-tax" = "#666666")
  ) +
  labs(
    title    = "Total red-meat–attributable DALYs: baseline vs tax scenario",
    subtitle = "United Kingdom, 1990–2022",
    x        = "Year",
    y        = "Total DALYs per year",
    colour   = "Scenario"
  ) +
  theme_minimal(base_size = 13)

# 6.5 Combined multi-panel figure
combined <- (p2 | p3) / (p4 | p1)

combined +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold")
    )
  )

Sys.info()


## End analysis---------------------