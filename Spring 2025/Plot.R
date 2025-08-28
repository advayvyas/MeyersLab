library(dplyr)
library(ggplot2)

facility <- read.csv("thcic-ids_city_county_crosswalk.csv")
facility_filtered <- facility %>%
  group_by(THCIC_ID) %>%
  filter(YEAR == max(YEAR)) %>%
  ungroup()

TX_zip_zcta_pop_city <- read.csv("TX_zip_zcta_pop_city.csv")
TX_zcta_pop_city <- TX_zip_zcta_pop_city %>%
  select(zcta, City, population) %>%
  distinct() %>%
  group_by(City) %>%
  summarise(population = sum(population))

County <- read.csv("county_fips_us.csv")

flu22_3 <- read.csv("FLU_ALL_PAT_CAT_2022Q3.csv")
flu22_4 <- read.csv("FLU_ALL_PAT_CAT_2022Q4.csv")
flu23_1 <- read.csv("FLU_ALL_PAT_CAT_2023Q1.csv")
flu23_2 <- read.csv("FLU_ALL_PAT_CAT_2023Q2.csv")
flu23_3 <- read.csv("FLU_ALL_PAT_CAT_2023Q3.csv")
flu23_4 <- read.csv("FLU_ALL_PAT_CAT_2023Q4.csv")
flu24_1 <- read.csv("FLU_ALL_PAT_CAT_2024Q1.csv")
flu24_2 <- read.csv("FLU_ALL_PAT_CAT_2024Q2.csv")

flu_all_pat_filter <- function(flu1){
  flu11 <- flu1 %>%
    select(THCIC_ID, PAT_STATE, PAT_ZIP, PAT_COUNTRY, PAT_COUNTY) %>%
    mutate(PAT_ZIP = ifelse(nchar(PAT_ZIP) < 5 | grepl("\\D", PAT_ZIP), NA, PAT_ZIP)) %>%
    mutate(
      PAT_COUNTY_FIPS = str_pad(PAT_COUNTY, width = 3, side = "left", pad = "0"),
      PAT_COUNTY_FIPS = paste0("48", PAT_COUNTY_FIPS)
    ) %>%
    mutate(PAT_COUNTY_FIPS = as.integer(PAT_COUNTY_FIPS),
           PAT_ZIP = as.integer(PAT_ZIP)) %>%
    left_join(County, by = c("PAT_COUNTY_FIPS" = "FIPS")) %>%
    rename(PAT_County = County) %>%
    select(-State.Abbreviation, -State.Name, -FIPS_int, -PAT_COUNTRY) %>%
    left_join(TX_zip_zcta_pop_city, by = c("PAT_ZIP" = "ZIP_CODE", "PAT_STATE" = "STATE")) %>%
    rename(PAT_zcta = zcta, 
           PAT_City = City, 
           PAT_PO_NAME = PO_NAME,
           PAT_population = population) 
  
  flu12 <- flu11 %>%
    left_join(facility_filtered, by = "THCIC_ID") %>%
    select(-HOSP_NAME, -HOSP_NAME_clean) %>%
    filter(!is.na(HOSP_CITY) & !is.na(PAT_City)) 
  return(flu12)
}

flu22_3_prop <- flu_all_pat_filter(flu22_3)
flu22_4_prop <- flu_all_pat_filter(flu22_4)
flu23_1_prop <- flu_all_pat_filter(flu23_1)
flu23_2_prop <- flu_all_pat_filter(flu23_2)
flu23_3_prop <- flu_all_pat_filter(flu23_3)
flu23_4_prop <- flu_all_pat_filter(flu23_4)
flu24_1_prop <- flu_all_pat_filter(flu24_1)
flu24_2_prop <- flu_all_pat_filter(flu24_2)

# Add quarter tag and combine
flu22_3_prop$quarter <- "2022Q3"
flu22_4_prop$quarter <- "2022Q4"
flu23_1_prop$quarter <- "2023Q1"
flu23_2_prop$quarter <- "2023Q2"
flu23_3_prop$quarter <- "2023Q3"
flu23_4_prop$quarter <- "2023Q4"
flu24_1_prop$quarter <- "2024Q1"

flu24_2_prop$quarter <- "2024Q2"

flu_all_labeled <- bind_rows(
  flu22_3_prop, flu22_4_prop,
  flu23_1_prop, flu23_2_prop,
  flu23_3_prop, flu23_4_prop,
  flu24_1_prop, flu24_2_prop
)


city_trend <- flu_all_labeled %>%
  filter(PAT_STATE == "TX") %>%
  group_by(quarter, PAT_City) %>%
  summarise(total_patients = n(), .groups = "drop")

# Focus on major cities
major_cities <- c("Houston", "Dallas", "Austin", "San Antonio", "Fort Worth", "El Paso")

city_trend_major <- city_trend %>%
  filter(PAT_City %in% major_cities)

ggplot(city_trend_major, aes(x = quarter, y = total_patients, color = PAT_City, group = PAT_City)) +
  geom_line(size = 1.2) +
  geom_point() +
  theme_minimal(base_size = 14) +
  labs(title = "Flu Patient Trends Over Time by City", x = "Quarter", y = "Number of Patients", color = "City") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~PAT_City)

