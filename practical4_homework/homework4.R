# 1. Load required packages

library(tidyverse)
library(sf)
library(countrycode)
library(janitor)
library(readr)

# 2. Import UNDP Gender Inequality Index data

gii_data <- read_csv(
  "HDR25_Composite_indices_complete_time_series.csv"
)

# Examine the dataset
glimpse(gii_data)

# 3. Identify GII variables

# Find all columns related to GII
gii_columns <- names(gii_data)[
  grepl("^gii_", names(gii_data))
]

# Display GII column names
print(gii_columns)

# 4. Check GII values for 2010 and 2019

gii_check <- gii_data %>%
  select(iso3, country, gii_2010, gii_2019)

# View the first 10 countries
head(gii_check, 10)

# Check summary statistics
summary(gii_check)

# Count missing values
colSums(is.na(gii_check))

# 5. Identify countries with complete GII data

gii_complete <- gii_check %>%
  filter(!is.na(gii_2010), !is.na(gii_2019))

# Count countries with complete data
nrow(gii_complete)

# Count countries with missing data
nrow(gii_check) - nrow(gii_complete)

# 6. Calculate GII change between 2010 and 2019

gii_complete <- gii_complete %>%
  mutate(
    GII_change = gii_2019 - gii_2010
  )

# View the first 10 countries
head(gii_complete, 10)

# Summary statistics of GII change
summary(gii_complete$GII_change)

# 7. Classify GII changes

gii_complete <- gii_complete %>%
  mutate(
    GII_status = case_when(
      GII_change < 0 ~ "Improvement",
      GII_change > 0 ~ "Worsening",
      GII_change == 0 ~ "No change"
    )
  )

# Count countries in each category
table(gii_complete$GII_status)

# 8. Identify the World GeoJSON file

list.files(pattern = "\\.geojson$")

# 9. Read the World GeoJSON file

world <- st_read(
  "World_Countries_(Generalized)_2173680399808997149.geojson"
)

# Check the spatial data
print(world)

# Check column names
names(world)

# Check coordinate reference system
st_crs(world)

# 10. Convert ISO-2 codes to ISO-3 codes

world$iso3 <- countrycode(
  world$ISO,
  origin = "iso2c",
  destination = "iso3c"
)

# Check the converted country codes
head(world[, c("COUNTRY", "ISO", "iso3")], 15)

# Count missing ISO-3 codes
sum(is.na(world$iso3))

# 11. Check country code matching

# Countries in GII data but not in World spatial data
gii_not_in_world <- anti_join(
  gii_complete,
  st_drop_geometry(world),
  by = "iso3"
)

# Countries in World spatial data but not in GII data
world_not_in_gii <- anti_join(
  st_drop_geometry(world),
  gii_complete,
  by = "iso3"
)

# Count unmatched countries
nrow(gii_not_in_world)
nrow(world_not_in_gii)

# View unmatched GII countries
gii_not_in_world %>%
  select(iso3, country)

# View unmatched World countries
world_not_in_gii %>%
  select(iso3, COUNTRY)

# 12. Check GII country records

# Check unique ISO-3 codes in GII data
n_distinct(gii_data$iso3)

# Check records with non-standard ISO codes
gii_data %>%
  filter(!grepl("^[A-Z]{3}$", iso3)) %>%
  select(iso3, country)

# 13. Keep country records only

gii_countries <- gii_complete %>%
  filter(grepl("^[A-Z]{3}$", iso3))

# Count countries with complete GII data
nrow(gii_countries)

# Check the first 10 countries
head(gii_countries, 10)

# Check whether any non-country records remain
gii_countries %>%
  filter(!grepl("^[A-Z]{3}$", iso3))

# 14. Recalculate GII status for countries only

table(gii_countries$GII_status)

# Calculate the percentage of each category
prop.table(table(gii_countries$GII_status)) * 100

# 15. Check duplicate ISO-3 codes in World data

# Count the number of unique ISO-3 codes
n_distinct(world$iso3)

# Find duplicated ISO-3 codes
world %>%
  st_drop_geometry() %>%
  count(iso3) %>%
  filter(n > 1)

# Check whether all GII countries can be matched
gii_not_in_world <- anti_join(
  gii_countries,
  st_drop_geometry(world),
  by = "iso3"
)

nrow(gii_not_in_world)

# 16. Join GII data to World spatial data

world_gii <- world %>%
  left_join(gii_countries, by = "iso3")

# Check the merged spatial data
print(world_gii)

# Check the number of spatial features
nrow(world_gii)

# Count missing GII values
sum(is.na(world_gii$GII_change))

# View selected columns
world_gii %>%
  st_drop_geometry() %>%
  select(COUNTRY, iso3, gii_2010, gii_2019, GII_change, GII_status) %>%
  head(15)

# 17. Create World GII Change Map

gii_map <- ggplot(world_gii) +
  geom_sf(aes(fill = GII_change), colour = "white", linewidth = 0.1) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    na.value = "grey80",
    name = "GII change"
  ) +
  labs(
    title = "Change in Gender Inequality Index (2010–2019)",
    subtitle = "Negative values indicate improvement; positive values indicate worsening",
    caption = "Source: UNDP Human Development Reports, HDR25"
  ) +
  theme_minimal()

# Display the map
print(gii_map)

# 18. Identify countries with the greatest GII changes

# Countries with the greatest worsening
gii_countries %>%
  arrange(desc(GII_change)) %>%
  select(country, iso3, gii_2010, gii_2019, GII_change) %>%
  head(10)

# Countries with the greatest improvement
gii_countries %>%
  arrange(GII_change) %>%
  select(country, iso3, gii_2010, gii_2019, GII_change) %>%
  head(10)

# 19. Check selected countries

gii_countries %>%
  filter(country %in% c("Russia", "Vanuatu", "Papua New Guinea")) %>%
  select(country, iso3, gii_2010, gii_2019, GII_change, GII_status)


# 20. Save the GII change map

ggsave(
  "GII_change_2010_2019.png",
  plot = gii_map,
  width = 12,
  height = 7,
  dpi = 300
)

# 21. Improve the map legend

gii_map_final <- gii_map_final +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "#F7F7F7",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-0.5, 0.5),
    breaks = c(-0.5, -0.25, 0, 0.25, 0.5),
    na.value = "grey85",
    name = "Change in GII"
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 18,
      barheight = 0.7,
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom"
    )
  )

print(gii_map_final)

# 22. Save the final improved map

ggsave(
  "GII_change_2010_2019_final.png",
  plot = gii_map_final,
  width = 14,
  height = 8,
  dpi = 300
)

# 23. Export merged spatial data

st_write(
  world_gii,
  "World_GII_Change_2010_2019.geojson",
  delete_dsn = TRUE
)
# 24. Check duplicate column names

names(world_gii)

# Identify duplicated column names
names(world_gii)[duplicated(names(world_gii))]

# 25. Rename the conflicting field

world_gii_export <- world_gii %>%
  rename(GII_country = country)

# Check the new field names
names(world_gii_export)

# Export the merged spatial data
st_write(
  world_gii_export,
  "World_GII_Change_2010_2019.geojson",
  delete_dsn = TRUE
)
