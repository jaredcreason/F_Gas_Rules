#### Setup

rm(list = ls())
gc()

# Load libraries
source("scripts/packages.R")

# Load ACS data, pull from API if .Rdata file doesnt exist
#source("acs_api_query.R")
load("data/acs_data/acs_data_2019_block group.Rdata")

data_ct <- data %>% mutate(Tract=substr(GEOID,1,11))

shp = data_ct %>%
  filter(variable=="pop") %>%
  select(GEOID,Tract) %>%
  arrange(GEOID) %>%
  st_transform(3488)

tr <- readRDS("data/tr.rds")
tr_pts <- tr %>% st_centroid()

urban_tracts <- readRDS("data/urban_tracts.rds")

shp_rural <- shp %>% mutate(rural = fifelse(Tract %in% urban_tracts$Tract,0,1))

sq_miles <- shp %>% mutate(sq_miles = units::set_units(st_area(shp),"mi^2")) %>%
  select(GEOID,sq_miles)

units(sq_miles$sq_miles) <- NULL

sq_miles %<>% st_set_geometry(NULL) %>% as.data.table() %>% setkey('GEOID')

# Load NATA data
source("scripts/nata_data_pull.R")

## get urban areas for determining rural status
urban_areas <- urban_areas()
uac <- urban_areas %>% st_transform(3488)

#### Manual Facilities Workbook

allocation_facilities <- read_excel("data/Allocation_Final_list production facilities 2022.xls") %>%
  rename(Latitude = LATITUDE,
         Longitude = LONGITUDE,
         GHG_co2e = `GHG QUANTITY (METRIC TONS CO2e)`,
         City = `CITY NAME`,
         State = STATE) %>%
  mutate(Label = `FACILITY NAME`) %>%
  select(Longitude,Latitude,everything()) %>%
  filter(Label != "CHEMOURS CHAMBERS WORKS")

#### Facilities to SF Format

facilities_to_map <- function(facilities, uac) {
  
  facilities_sf = st_as_sf(facilities, 
                           coords=c(x="Longitude",y="Latitude"), 
                           crs=4326) %>%
    st_transform(3488) 
  
  facilities_sf_urban <- st_intersection(facilities_sf,uac) %>%
    mutate(rural = 0)
  
  facilities_sf_rural <- facilities_sf %>%
    mutate(rural = fifelse(Label %in% unique(facilities_sf_urban$Label),0,1)) %>%
    as.data.frame() %>%
    select(rural,Label)
  
  facilities_lat_lon <- facilities %>% 
    select(Longitude,Latitude,Label) %>%
    rename(lon = Longitude,
           lat = Latitude)
  
  facilities_map <- facilities_sf %>%
    left_join(facilities_sf_rural, by = "Label") %>%
    left_join(facilities_lat_lon, by = "Label")
  
}
  
facilities_map <- facilities_to_map(allocation_facilities, uac) 

#### Make the Map

make_map <- function(facilities_map, regions, variable, variable_label, filename) {
  
  facilities_map_t <- usmap_transform(facilities_map)
  
  fac_map <- plot_usmap(include=c(regions),
                        labels=TRUE, 
                        fill = "#C5CFE3", 
                        alpha = 0.5) +
    ggrepel::geom_label_repel(data = facilities_map_t,
                              aes(x = x, y = y, 
                                  label = Label),
                              size = 5, alpha = 0.8,
                              label.r = unit(0.5, "lines"), label.size = 0.5,
                              segment.color = "#C60404", segment.size = 1,
                              seed = 1002) +
    geom_point(data = facilities_map_t,
               aes(x = x, y = y),
               color = "#C60404") +
    geom_point(data = facilities_map_t,
               aes(x = x, y = y, size = .data[[variable]]),
               color = "purple", alpha = 0.5) +
    scale_size_continuous(range = c(1, 16),
                          label = scales::comma) +
    labs(size = variable_label) +
    theme(legend.position = c(0.85, 0.1))
  
  ggsave(filename, fac_map, width = 10, height = 6)
  
  fac_map
  
}

allocation_map <- make_map(
  facilities_map, 
  regions = c(.northeast_region,.south_region,.north_central_region),
  variable = "GHG_co2e",
  variable_label = expression("GHG Releases (mt CO"[2]*"e)"),
  filename = "output/Allocation Rule/allocation_rule_facilities_map.png"
)


#### Buffer Calculations

buffer_communities <- function(facilities_map, radius) {
  
  communities = st_buffer(facilities_map, dist=radius*1609.34) 
  
  buffer = st_intersection(communities,shp) %>%
    select(GEOID,Tract,Label)
  
  facility_buffer <- st_intersection(facilities_map, buffer) %>% select(Label, GEOID) %>% st_set_geometry(NULL)
  
  
  
}

communities <- buffer_communities(facilities_map, 1)



















