library(sf)
library(dplyr)
library(janitor)

lsoa21  <- st_read(
  "data/shape-files/lsoa-2021/LSOA_2021_EW_BSC_V4.shp"
  ) %>%
  select(
    LSOA21CD,
    LSOA21NM
  )

# Load aggregate shapes

# 2022 Constituencies
const22  <- st_read(
  "data/shape-files/constituencies-2022/PCON_DEC_2022_UK_BFC.shp"
) %>%
  select(
    PCON22CD,
    PCON22NM
  )

# 2024 Constituencies
const24  <- st_read(
  "data/shape-files/constituencies-2024/PCON_JULY_2024_UK_BFC.shp"
) %>%
  select(
    PCON24CD,
    PCON24NM
  )

# Ward
ward25 <- st_read(
  "data/shape-files/ward-2025/WD_DEC_2025_UK_BSC.shp"
) %>%
  select(
    "WD25CD","WD25NM", "LAD25CD","LAD25NM"
  )

areas <- list(
  "Const22" = const22,
  "Const24" = const24,
  "Ward25" = ward25
)

lookup_list <- list()

for (area_i in names(areas)) {
  # Calculate all intersections
  overlaps <- st_intersection(
    lsoa21,
    areas[[area_i]]
  )
  
  # Calculate overlap area
  overlaps$overlap_area <- st_area(overlaps)
  
  # For each LSOA, keep the area with the largest overlap
  lsoa_lookup <- overlaps %>%
    st_drop_geometry() %>%
    group_by(LSOA21CD) %>%
    slice_max(overlap_area, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-c(LSOA21NM, overlap_area))
  
  lookup_list[[area_i]] <- lsoa_lookup
}

final_lookup <- purrr::reduce(lookup_list, dplyr::left_join, by = 'LSOA21CD') %>%
  clean_names()

writexl::write_xlsx(final_lookup, "data/lsoa21-lookup.xlsx")