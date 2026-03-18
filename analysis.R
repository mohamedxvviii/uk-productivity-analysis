# UK Productivity Analysis
# Mohamed Mahamud
# March 14th 2026

library(tidyverse)

install.packages(c("tidyverse", "readr", "ggplot2", "dplyr"))

install.packages("WDI")
library(WDI)

# Download productivity data directly from World Bank API

productivity <- WDI(
  country = c("GB", "US", "DE", "FR", "JP"),
  indicator = "SL.GDP.PCAP.EM.KD",
  start = 2000,
  end = 2023
)

investment <- WDI(
  country = c("GB", "US", "DE", "FR", "JP"),
  indicator = "NE.GDI.TOTL.ZS",
  start = 2000,
  end = 2023
)

names(productivity)[names(productivity) == "SL.GDP.PCAP.EM.KD"] <- "gdp_per_worker"
names(investment)[names(investment) == "NE.GDI.TOTL.ZS"] <- "inv_pct_gdp"

head(productivity)
head(investment)

library(ggplot2)

# Rename the productivity column to something cleaner.
names(productivity) [names(productivity) == "SL.GDP.PCAP.EM.KD"] <- "gdp_per_worker"

# Chart 1 - Line chart showing productivity trends
ggplot(productivity, aes(x = year, y = gdp_per_worker, colour = country)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Labour Productivity: UK vs Peer economies 2000-2023",
    subtitle = "GDP per person employed (constant 2021 USD)",
    x = "Year",
    y = "GDP per person employed (USD)",
    colour = "Country",
    caption = "Source : World Bank WDI"
  )

ggsave("chart1_productivity_trends.png", width = 9, height = 6, dpi = 300)

# Chart 2 - Average annual productivity growth by country 2000-2023
productivity_growth <- productivity %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(growth = (gdp_per_worker - lag(gdp_per_worker)) / lag(gdp_per_worker) * 100) %>%
  summarise(avg_growth = mean(growth, na.rm = TRUE))

ggplot(productivity_growth,
       aes( x = reorder(country, avg_growth),
            y = avg_growth,
            fill = country)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Average Annual Productivity Growth 2000-2023",
    subtitle = "Which economies grew fastest?",
  x = "Country",
  y = "Average annual growth (%)",
  caption = "Source: World Bank WDI"
  )

ggsave("chart2_growth_rates.png", width = 9, height = 6, dpi = 300)

# Chart 3 - Relationship between investment and productivity across peer economies
combined <- merge(productivity, investment,
          by = c("country", "year"))

ggplot(combined, aes(x = inv_pct_gdp, y = gdp_per_worker)) +
  geom_point(aes(colour = country), size = 3) +
  geom_smooth(method = "lm", se = TRUE, colour = "black") +
  theme_minimal() +
  labs(
    title = "Investment and Productivity Across Peer Economies",
    subtitle = "Does higher investment predict higher output per worker?",
    x = "Gross Capital Formation (% of GDP)",
    y = "GDP per person employed (USD)",
    color = "Country",
    caption = "Source: World Bank WDI"
  )
    

  
        
         
    

