install.packages("dplyr")
library(dplyr)
install.packages("purrr")
library(purrr)
library(tidyr)

elasticity_values <- seq(-0.3, -1.2, by = -0.1)
RR50_values       <- c( 1.08, 1.12, 1.15, 1.18, 1.22)


param_grid <- expand_grid(
  elasticity = seq(-0.3, -1.2, by = -0.1),
  RR50 = c(1.08, 1.12, 1.15, 1.18, 1.22)
)

run_scenario <- function(data, elasticity, RR50,
                         tax_rate = 0.20,
                         tmrel = 0) {
  
  beta <- log(RR50) / 50
  prop_change <- elasticity * tax_rate
  
  data %>%
    mutate(
      red_meat_g_day = total.red.meat * 1000 / 365,
      baseline = red_meat_g_day,
      posttax = red_meat_g_day * (1 + prop_change),
      
      x0 = pmax(baseline - tmrel, 0),
      x1 = pmax(posttax - tmrel, 0),
      
      RR0 = exp(beta * x0),
      RR1 = exp(beta * x1),
      
      PIF = (RR0 - RR1) / RR0,
      
      DALYs_attr_averted_per100k =
        PIF * Value,
      
      DALYs_attr_averted_total =
        DALYs_attr_averted_per100k *
        population_count / 100000
    ) %>%
    summarise(
      mean_PIF = mean(PIF, na.rm = TRUE),
      DALYs_averted_per100k =
        mean(DALYs_attr_averted_per100k, na.rm = TRUE),
      DALYs_total_averted =
        sum(DALYs_attr_averted_total, na.rm = TRUE)
    )
}

sensitivity_results <-
  param_grid %>%
  mutate(
    results = pmap(
      list(elasticity, RR50),
      ~ run_scenario(merged, ..1, ..2)
    )
  ) %>%
  unnest(results)


head(sensitivity_results, 50)
View(sensitivity_results)


library(ggplot2)

ggplot(
  sensitivity_results,
  aes(x = elasticity,
      y = DALYs_total_averted,
      color = factor(RR50))
) +
  geom_line(size = 1.1) +
  labs(
    x = "Price Elasticity of Demand",
    y = "Total DALYs Averted",
    color = "RR per 50 g/day"
  ) +
  theme_minimal()

#Verification with the scenario where elasticity=-0.7 and RR50=1.18 , Total averted for all years =  414370, which is the same when we run the simulation using those parameters
DALYs_total_averted=sum(merged$DALYs_attr_averted_total)
DALYs_total_averted

write.csv( sensitivity_results, file = "sensitivity_analysis.csv", row.names = FALSE)
getwd()

















