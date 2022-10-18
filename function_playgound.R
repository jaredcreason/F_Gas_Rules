#### Setup

rm(list = ls())
gc()

# Load libraries
source("scripts/packages.R")

# Load NATA data
source("scripts/nata_data_pull.R")

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

table_full <- data_ct %>% 
  st_set_geometry(NULL) %>%
  as.data.table() %>% 
  setkey('GEOID')

table_1 <- table_full[sq_miles]

table_2 <- table_1 %>%
  pivot_wider(names_from=variable,values_from=estimate) %>%
  mutate(white_pct=(white/pop)*100,
         minority_black=(black/pop)*100,
         minority_other=((pop-(white + black))/pop)*100,
         minority_hispanic=(hispanic/hispanic_denominator)*100,
         pov99=pov99/pop*100,
         pov50=pov50/pop*100,
         income=income/1000,
         rural = fifelse(Tract %in% urban_tracts$Tract,0,1)) %>%
  left_join(nata_data,by=c("Tract"="Tract")) %>%
  left_join(nata_data_resp,by=c("Tract"="Tract")) %>%
  as.data.table() %>%
  setkey('GEOID')

table <- as.data.frame(table_2)

comparison_vars = c("white_pct",'minority_black','minority_other','minority_hispanic',
                    "income",
                    "pov99","pov50",
                    "total_risk","total_risk_resp")

desc_vars = c("% White","% Black or African American ","% Other","% Hispanic",
              "Median Income [1,000 2019$]",
              "% Below Poverty Line","% Below Half the Poverty Line",
              "Total Cancer Risk (per million)",
              'Total Respiratory (hazard quotient)')

summary_table = data.frame(Variable=desc_vars)
summary_table_sd = data.frame(Variable=desc_vars)

for (v in 1:length(comparison_vars)) {
  summary_table[v,"Overall (National Average)"] = sum(table$pop*table[,comparison_vars[v]],na.rm=T)/sum(table$pop,na.rm=T)
  a = (table$pop*table[,comparison_vars[v]])/table$pop
  summary_table_sd[v,"Overall (National Average) SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

rural <- table %>% filter(rural==1)
for (v in 1:length(comparison_vars)) {
  summary_table[v,"Rural Areas (National Average)"] = sum(rural$pop*rural[,comparison_vars[v]],na.rm=T)/sum(rural$pop,na.rm=T)
  a = (rural$pop*rural[,comparison_vars[v]])/rural$pop
  summary_table_sd[v,"Rural Areas (National Average) SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

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

buffer_communities <- function(facilities_map, radius, pollutant, filename) {
  
  communities = st_buffer(facilities_map, dist=radius*1609.34) 
  
  buffer = st_intersection(communities,shp) %>%
    select(GEOID,Tract,Label)
  
  facility_buffer <- st_intersection(facilities_map, buffer) %>% select(Label, GEOID) %>% st_set_geometry(NULL)
  
  facility_demographics_pre <- merge(as.data.table(facilities_map), as.data.table(buffer),by="Label")
  
  facility_demographics_mid <- merge(facility_demographics_pre, table_2, by="GEOID") %>% 
    select(Label,City,.data[[pollutant]],GEOID,sq_miles,rural.x,rural.y,pop,
           white,black,indian,asian,hispanic,income,pov50,pov99,
           total_risk,total_risk_resp) %>%
    rename(rural_facility = rural.x, rural_blockgroup = rural.y)
  
  facility_demographics <- facility_demographics_mid %>%
    group_by(Label,City,.data[[pollutant]]) %>%
    mutate(
      blockgroups_n = n(), 
      sq_miles = sum(sq_miles, na.rm=TRUE), 
      pop = sum(pop, na.rm=TRUE),
      white = sum(white, na.rm=TRUE),
      black = sum(black, na.rm=TRUE),
      indian = sum(indian, na.rm=TRUE),
      asian = sum(asian, na.rm=TRUE),
      hispanic = sum(hispanic, na.rm=TRUE),
      income = mean(income, na.rm=TRUE),
      pov50 = mean(pov50, na.rm=TRUE), 
      pov99 = mean(pov99, na.rm=TRUE), 
      total_risk = mean(total_risk, na.rm=TRUE), 
      total_risk_resp = mean(total_risk_resp, na.rm=TRUE)) %>%
    mutate(pop_sq_mile_1mi = pop/sq_miles,
           rural_bg_pct = signif(sum(rural_blockgroup/blockgroups_n, na.rm=TRUE),2)) %>% 
    ungroup() %>%
    select(Label,City,.data[[pollutant]],blockgroups_n,sq_miles,pop,pop_sq_mile_1mi,
           rural_facility,rural_bg_pct,white,black,indian,asian,hispanic,
           income,pov50,pov99,total_risk,total_risk_resp) %>% 
    distinct()
  
  write.xlsx(x = facility_demographics,
             file = paste(filename,"_",radius,"mi.xlsx", sep = ""), 
             overwrite = TRUE)
}

buffer_list = c(1,3,5,10)

for(i in buffer_list) {
  buffer_communities(
    facilities_map = facilities_map, 
    radius = i,
    pollutant = "GHG_co2e",
    filename = "output/Allocation Rule/facility_data/allocation_rule_facility_demographics"
  )
}

pop_weighted_avg <- function(table, summary_table, summary_table_sd, facilities_map, radius, comparison_vars) {
  
  communities = st_buffer(facilities_map, dist=radius*1609.34) 
  
  buffer = st_intersection(communities,shp) %>%
    select(GEOID,Tract,Label)
  
  local = table$GEOID %in% unique(buffer$GEOID)
  
  for (v in 1:length(comparison_vars)) {
    summary_table[v,paste("Within",radius,"mile of HFC production facility")] = sum(table$pop[local]*table[local,comparison_vars[v]],na.rm=T)/sum(table$pop[local],na.rm=T)
    a = (table$pop[local]*table[local,comparison_vars[v]])/table$pop[local]
    summary_table_sd[v,"Within 1 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
  }
  
}

pop_weighted_avg(table, summary_table, summary_table_sd, facilities_map, 1, comparison_vars)

for(i in buffer_list) {
  pop_weighted_avg(
    table = table,
    summary_table = summary_table,
    summary_table_sd = summary_table_sd,
    facilities_map = facilities_map,
    radius = i,
    comparison_vars = comparison_vars
  )
}












