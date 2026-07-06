library(dplyr)
library(readxl)
library(janitor)

lsoa_lookup <- read_excel("data/lsoa21-lookup.xlsx")

# From https://deprivation.communities.gov.uk/
imd_lookup <- read.csv("data/IoD-2025-LSOA.csv") %>%
  clean_names() %>%
  rename(imd_rank = index_of_multiple_deprivation_imd_rank) %>%
  select(lsoa_code_2021, contains("rank")) %>%
  tidyr::pivot_longer(
    cols = contains("rank"),
    names_to = "imd_domain",
    values_to = "domain_rank"
  )

# From https://www.gov.uk/government/statistics/english-indices-of-deprivation-2025
pop_denominators <- read_excel(
  "data/File_6_IoD2025_Population_Denominators.xlsx",
  sheet = "ID 2025 Population Denominators") %>%
  clean_names() %>%
  select(
    lsoa_code_2021, total_population_mid_2022
  ) 

all_lsoa_data <- lsoa_lookup %>%
  left_join(
    pop_denominators,
    by = join_by("lsoa21cd" == "lsoa_code_2021"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    imd_lookup,
    by = join_by("lsoa21cd" == "lsoa_code_2021"),
    relationship = "one-to-many"
  ) %>%
  filter(
    !is.na(domain_rank)
  )

# Ward 
ward_imd <- all_lsoa_data %>%
  group_by(wd25cd, wd25nm, lad25cd, lad25nm, imd_domain) %>%
  summarise(
    pop_weighted_rank = sum(domain_rank * total_population_mid_2022) / sum(total_population_mid_2022),
    .groups = "drop"
  ) %>%
  mutate(
    agg_imd25_rank = rank(pop_weighted_rank),
    imd25_domain_decile = floor( 10 * (agg_imd25_rank - 1) / max(agg_imd25_rank) + 1),
    imd25_domain_quintile = floor( 5 * (agg_imd25_rank - 1) / max(agg_imd25_rank) + 1)
  ) %>% 
  select(-pop_weighted_rank)

# 2022 Constituency 
cons22_imd <- all_lsoa_data %>%
  group_by(pcon22cd, pcon22nm, imd_domain) %>%
  summarise(
    pop_weighted_rank = sum(domain_rank * total_population_mid_2022) / sum(total_population_mid_2022),
    .groups = "drop"
  ) %>%
  mutate(
    agg_imd25_rank = rank(pop_weighted_rank),
    imd25_domain_decile = floor( 10 * (agg_imd25_rank - 1) / max(agg_imd25_rank) + 1),
    imd25_domain_quintile = floor( 5 * (agg_imd25_rank - 1) / max(agg_imd25_rank) + 1)
  )  %>% 
  select(-pop_weighted_rank)
  

# 2024 Constituency 
cons24_imd <- all_lsoa_data %>%
  group_by(pcon24cd, pcon24nm, imd_domain) %>%
  summarise(
    pop_weighted_rank = sum(domain_rank * total_population_mid_2022) / sum(total_population_mid_2022),
    .groups = "drop"
  ) %>%
  mutate(
    agg_imd25_rank = rank(pop_weighted_rank),
    imd25_domain_decile = floor( 10 * (agg_imd25_rank - 1) / max(agg_imd25_rank) + 1),
    imd25_domain_quintile = floor( 5 * (agg_imd25_rank - 1) / max(agg_imd25_rank) + 1)
  )  %>% 
  select(-pop_weighted_rank)

# Local authority
la_imd <- all_lsoa_data %>%
  group_by(lad25cd, lad25nm, imd_domain) %>%
  summarise(
    pop_weighted_rank = sum(domain_rank * total_population_mid_2022) / sum(total_population_mid_2022),
    .groups = "drop"
  ) %>%
  mutate(
    agg_imd25_rank = rank(pop_weighted_rank),
    imd25_domain_decile = floor( 10 * (agg_imd25_rank - 1) / max(agg_imd25_rank) + 1),
    imd25_domain_quintile = floor( 5 * (agg_imd25_rank - 1) / max(agg_imd25_rank) + 1)
  )  %>% 
  select(-pop_weighted_rank)

output <- list(
  "Ward" = ward_imd,
  "Const 2022" = cons22_imd,
  "Const 2024" = cons24_imd,
  "LA" = la_imd
)

writexl::write_xlsx(output, "data/aggregate-imd25-lookup.xlsx")