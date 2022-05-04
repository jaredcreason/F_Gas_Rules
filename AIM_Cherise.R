## Written by: US EPA National Center for Environmental Economics; March 2021

###################################################################################
##############################     AIM EJ Analysis       ##########################
###################################################################################

####################################################
##########################################  PREAMBLE
####################################################

## Clear worksace
rm(list = ls())
gc()

## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

## These lines load the required packages
packages <- c('tidycensus','tigris','tidyverse','magrittr','data.table','sf','foreach','doSNOW','scales','odbc','colorspace','openxlsx','here','readxl','openxlsx') ## you can add more packages here
lapply(packages, pkgTest)


####################################################
#################################  WORKING DIRECTORY
####################################################

## SET WORKING DIRECTORY
setwd(here())

###################################################
#################################  GET CENSUS - ACS
###################################################

## This will take approximately 1-2 hours if ACS data are not already downloaded

## census api key
## get one at: https://api.census.gov/data/key_signup.html
#  census_api_key("73898e028f52e49f9e5fbb0ca774d91d38c1672d", install=T,overwrite=T)
#readRenviron("~/.Renviron")

# # geography at which to draw data
 geography = "block group"
# 
# # year of ACS to draw data
 year = 2019
# 
# ## Get dictionary of ACS variables
 acs_variables <- load_variables(year, "acs5", cache = TRUE)
# # write.xlsx(acs_variables,"data\\acs_variables_2019.xlsx") # write to optionally search spreadsheet
# 
# ## Define variables of interest from dictionary
variables = c(pop="B02001_001",white="B02001_002",black="B02001_003",indian="B02001_004",asian="B02001_005",hispanic="B03003_003",hispanic_denominator="B03003_001",
              pov50="C17002_002",pov99="C17002_003",deficit="B17011_001",income="B19013_001")
# 
# # cache the geometries downloaded from census
 options(tigris_use_cache=TRUE)
# 
# # file name for the stored acs data
 acs_file = paste0("/acs_data_",year,"_",geography,".Rdata")
# 
# # download the acs data if it doesn't exist
 if (file.exists(file.path("data\\acs_data\\acs_data",acs_file))) {
   load(file.path("data\\acs_data\\acs_data",acs_file))
   
   } else {
     
   # setup the cluster to download and process the data
   cl = makeCluster(5,outfile="")
   registerDoSNOW(cl)
   
   # list of state abbreviations 
   states = c(state.abb,"DC")  #state abb is a vector of state abbrev.
   #states = c("AL","AR","CA","IL","IN","IA","LA","TX","WV")
   
   
  # tidycensus will only return tract or block group data for a single state, so
   # we need to loop through each state and combine the results
   data = foreach (i=1:length(states),.combine=rbind,
                   .packages=c("tidycensus","tidyverse")) %dopar% {
                     print(paste("starting state:",states[i]))
                     get_acs(geography=geography,
                             state=states[i],
                             variables=variables, 
                             year=year,
                             geometry=TRUE) %>%
                       select(-NAME,-moe)
                     }
   
#   # save the acs data
   if (!dir.exists("data\\acs_data"))
     dir.create("data\\acs_data")
   save(file=file.path("data\\acs_data",acs_file),list="data")
   
   stopCluster(cl)
   
 }

load("data\\acs_data\\acs_data_2019_block group.RData")

## get urban areas for determining rural status
urban_areas <- urban_areas()

####################################################
############   Supporting function to download files
####################################################

## downloads a file if not present locally and extracts the contents if it is a
## compressed archive

## inputs:
##   url: url where the remote file resides
##   file_name: name of the remote file
##   dir: local directory to save the file [defulat is "."]
##   check_file: optional name of local file, where if present the download will
##              not occur. useful if the remote file is a compressed archive and
##               it shouldn't be downloaded if its contents is already present
##   tolower: covnert file names in a zip file to lower case [default is FALSE]
# 
 download_file = function(url,file_name,dir=".",check_file=NA,tolower=F) {
   
   library(httr)
   
   # create the directory if needed
   if (!dir.exists(dir))
     dir.create(dir)
   
   # if no check file name is provided use the remote file name
   if (is.na(check_file))
     check_file = file_name
   
   # if the check file is present then do nothing further
   if (file.exists(file.path(dir,check_file)))
     return()
   
   # download the file
   cat(paste0("downloading ",file_name,"... \n"))
   GET(paste0(url,file_name),
       write_disk(file.path(dir,file_name),overwrite=TRUE),
       progress())
   
   # if the file is a zip archive extract the contents and delete the archive
   if (tools::file_ext(file_name)=="zip") {
     cat("extracting zip file...\n")
     unzip(file.path(dir,file_name),exdir=dir)
     if (tolower) {
       files = unzip(file.path(dir,file_name),list=T)
       for (file in files)
         file.rename(file.path(dir,file),file.path(dir,tolower(file)))
     }
     file.remove(file.path(dir,file_name))
   }
   
   cat("\n")
#   
 }


####################################################
############   NATA cancer and respiratory risk data
####################################################

# directory to store the nata data
nata_dir = "data\\nata_data"

# 2014 nata file containing national cancer risks by toxic
nata_file = "nata2014v2_national_cancerrisk_by_tract_poll.xlsx"

# # download the nata data if it doesn't already exist
# download_file("https://www.epa.gov/sites/production/files/2018-08/",
#               nata_file,
#               dir=nata_dir)
 ##load the nata data
 nata_data = read_excel(file.path(nata_dir,nata_file)) %>%
   rename(total_risk='Total Cancer Risk (per million)') 


# # Respiratory 
nata_file_resp = "nata2014v2_national_resphi_by_tract_poll.xlsx"

 download_file("https://www.epa.gov/sites/production/files/2018-08/",
             nata_file_resp,
               dir=nata_dir)

# load the nata data
nata_data = read_excel(file.path(nata_dir,nata_file)) %>%
 rename(total_risk='Total Cancer Risk (per million)')

nata_data_resp = read_excel(file.path(nata_dir,nata_file_resp)) %>%
 rename(total_risk_resp='Total Respiratory (hazard quotient)') %>%
 select(Tract, total_risk_resp)


####################################################
#########################   Production facility data
####################################################

facilities <- read_excel('data\\hfc_facilities-4-8-22_w_additions.xls') %>%
  select(Longitude,Latitude,everything())
  #rename(facility_id='GHGRP ID',
  #       GHG_co2e = 'GHG QUANTITY (METRIC TONS CO2e)') 


facilities_map <- facilities %>% relocate(Longitude,Latitude)

facilities_sf = st_as_sf(facilities, 
                      coords=c(x="Longitude",y="Latitude"), 
                      crs=4326) %>%
  st_transform(3488) 

uac <- urban_areas %>% st_transform(3488)

urban_facilities <- st_intersection(facilities_sf,uac)

facilities_rural %<>% mutate(rural = fifelse(geometry %in% urban_facilities$geometry,0,1))# %>%
  #as.data.frame() %>%
  #select(Label, rural)

facilities_map <- facilities %>%
  left_join(facilities_rural, by = "Label")

####################################################
#########################   Plot Production facility
####################################################
#
library(usmap)
library(ggplot2)
library(stringr)

facilities_map_t <- usmap_transform(facilities_map) #%>% mutate(FACILITY.NAME = str_to_title(Facility Name))

map <- plot_usmap(include=c(.northeast_region,.south_region,.north_central_region,.west_north_central,.west_region,.west_south_central),
                  labels=TRUE, 
                  fill = "#C5CFE3", 
                  alpha = 0.5) +
  ggrepel::geom_label_repel(data = facilities_map_t,
                            aes(x = Longitude.1, y = Latitude.1, 
                                label = Label),
                            size = 5, alpha = 0.8,
                            label.r = unit(0.5, "lines"), label.size = 0.5,
                            segment.color = "#C60404", segment.size = 1,
                            seed = 1002) +
  geom_point(data = facilities_map_t,
             aes(x = Longitude.1, y = Latitude.1, size = GHG_co2e),
             color = "purple", alpha = 0.5) +
  geom_point(data = facilities_map_t,
             aes(x = Longitude.1, y = Latitude.1),
             color = "#C60404") +
  scale_size_continuous(range = c(1, 16),
                        label = scales::comma) +
  labs(size = expression("GHG Releases (mt CO"[2]*"e)")) +
  theme(legend.position = c(0.85, 0.1))
map
#
#

 
 # # Respiratory 
 #gas_plant_file_resp = "gas_plant2014v2_national_resphi_by_tract_poll.xlsx"
 
 # download_file("https://www.epa.gov/sites/production/files/2018-08/",
 #               gas_plant_file_resp,
 #               dir=gas_plant_dir)
 
 ## load the gas_plant data
 #gas_plant_data = read_excel(file.path(gas_plant_dir,gas_plant_file)) %>%
 #  rename(total_risk='Total Cancer Risk (per million)') 
 
 #gas_plant_data_resp = read_excel(file.path(gas_plant_dir,gas_plant_file_resp)) %>%
 #  rename(total_risk_resp='Total Respiratory (hazard quotient)') %>%
 #  select(Tract, total_risk_resp)
 
 
 
 
####################################################
############  Prep facilities for proximity analysis
####################################################

# add the census tract to the data in case the geography is a lower resolution
data %<>% mutate(Tract=substr(GEOID,1,11))

# get the geography geometry from the acs data
shp = data %>%
  filter(variable=="pop") %>%
  select(GEOID,Tract) %>%
  arrange(GEOID) %>%
  st_transform(3488)

# get the geography geometry from the acs data  **THIS STEP TAKES TIME
tr = shp %>% group_by(Tract) %>% summarize(geometry=st_union(geometry))

tr_pts <- tr %>% st_centroid()

## identify rural and urban geoid **THIS STEP TAKES TIME
urban_tracts <- st_intersection(tr_pts,uac)
#saveRDS(urban_tracts,"data\\urban_tracts.rds")
##rural_tracts <- st_intersection(tr_pts,uac) (Note this is the same as urban?)

# shp %<>% mutate(rural = fifelse(Tract %in% urban_tracts$Tract,0,1))

# identify rural and urban census blocks
sq_miles <- shp %>% mutate(sq_miles = units::set_units(st_area(shp),"mi^2")) %>%
  select(GEOID,sq_miles)
units(sq_miles$sq_miles) <- NULL

# prepare for merge with data
sq_miles %<>% st_set_geometry(NULL) %>% as.data.table() %>% setkey('GEOID')

# draw a buffer around the facilities
# buffer_dist is in miles so we need to multiply by 1609.34 meters/mile
communities = st_buffer(facilities_rural,dist=1*1609.34) 
communities_3mi = st_buffer(facilities_sf_rural,dist=3*1609.34)

# find the census geographies within the buffer around the facilities
buffer = st_intersection(communities,shp) %>%
  select(GEOID,Tract,Label)

buffer_3mi = st_intersection(communities_3mi,shp) %>%
  select(GEOID,Tract,Label)

# # get GEOID to facility list
# facility_geoid <- st_intersection(facilities,buffer) %>% select(FACILITY.NAME,GEOID) %>% st_set_geometry(NULL)
# facility_buffer_3mi <- st_intersection(facilities,buffer_3mi) %>% select(FACILITY.NAME,GEOID) %>% st_set_geometry(NULL)

# drop the geometry to work with the data alone
table <- data %>% st_set_geometry(NULL) %>%  as.data.table() %>% setkey('GEOID')
table <- table[sq_miles]

# merge the acs and nata data
table %<>%
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


# merge the acs and facility data
facility_demographics_1mi_pre <- merge(as.data.table(facilities_map), as.data.table(buffer),by="Label")
facility_demographics_3mi_pre <- merge(as.data.table(facilities_map), as.data.table(buffer_3mi),by="Label")

facility_demographics_1mi_mid <- merge(facility_demographics_1mi_pre, table, by="GEOID") %>% 
              select(Label,City,GHG_co2e,GEOID,sq_miles,rural.x,rural.y,pop,white,black,indian,asian,hispanic,income,pov50,pov99,total_risk,total_risk_resp) %>%
              rename(rural_facility = rural.x, rural_blockgroup = rural.y)

facility_demographics_3mi_mid <- merge(facility_demographics_3mi_pre, table, by="GEOID") %>% 
  select(Label,City,GHG_co2e,GEOID,sq_miles,rural.x,rural.y,pop,white,black,indian,asian,hispanic,income,pov50,pov99,total_risk,total_risk_resp) %>%
  rename(rural_facility = rural.x, rural_blockgroup = rural.y)

facility_demographics_1mi <- facility_demographics_1mi_mid %>%
  group_by(Label,City,GHG_co2e) %>%
  mutate(blockgroups_n = n(), sq_miles = sum(sq_miles, na.rm=TRUE), pop = sum(pop, na.rm=TRUE),white = sum(white, na.rm=TRUE),black = sum(black, na.rm=TRUE),indian = sum(indian, na.rm=TRUE),asian = sum(asian, na.rm=TRUE),hispanic = sum(hispanic, na.rm=TRUE),income = mean(income, na.rm=TRUE),pov50 = mean(pov50, na.rm=TRUE), pov99 = mean(pov99, na.rm=TRUE), total_risk = mean(total_risk, na.rm=TRUE), total_risk_resp = mean(total_risk_resp, na.rm=TRUE)) %>%
  mutate(pop_sq_mile_1mi = pop/sq_miles,
         rural_bg_pct = signif(sum(rural_blockgroup/blockgroups_n, na.rm=TRUE),2)) %>% ungroup() %>%
  select(Label,City,GHG_co2e,blockgroups_n,sq_miles,pop,pop_sq_mile_1mi,rural_facility,rural_bg_pct,white,black,indian,asian,hispanic,income,pov50,pov99,total_risk,total_risk_resp) %>% distinct()

  write.xlsx(facility_demographics_1mi,"data\\facility_data\\facility_demographics_1mi.xlsx")

facility_demographics_3mi <- facility_demographics_3mi_mid %>%
  group_by(Label,City,GHG_co2e) %>%
  mutate(blockgroups_n = n(), sq_miles = sum(sq_miles, na.rm=TRUE), pop = sum(pop, na.rm=TRUE),white = sum(white, na.rm=TRUE),black = sum(black, na.rm=TRUE),indian = sum(indian, na.rm=TRUE),asian = sum(asian, na.rm=TRUE),hispanic = sum(hispanic, na.rm=TRUE),income = mean(income, na.rm=TRUE),pov50 = mean(pov50, na.rm=TRUE), pov99 = mean(pov99, na.rm=TRUE), total_risk = mean(total_risk, na.rm=TRUE), total_risk_resp = mean(total_risk_resp, na.rm=TRUE)) %>%
  mutate(pop_sq_mile_3mi = pop/sq_miles,
         rural_bg_pct = signif(sum(rural_blockgroup/blockgroups_n, na.rm=TRUE),2)) %>% ungroup() %>%
  select(Label,City,GHG_co2e,blockgroups_n,sq_miles,pop,pop_sq_mile_3mi,rural_facility,rural_bg_pct,white,black,indian,asian,hispanic,income,pov50,pov99,total_risk,total_risk_resp) %>% distinct()

write.xlsx(facility_demographics_3mi,"data\\facility_data\\facility_demographics_3mi.xlsx")


####################################################
########################  Conduct proximity analysis
####################################################

table <- as.data.frame(table)

# variables along which the comparisons should be made for the tables
comparison_vars = c("white_pct",'minority_black','minority_other','minority_hispanic',"income","pov99","pov50","total_risk","total_risk_resp")

# descriptions of the comparison variables to be included in the tables
desc_vars = c("% White","% Black or African American ","% Other","% Hispanic",
              "Median Income [1,000 2019$]",
              "% Below Poverty Line","% Below Half the Poverty Line",
              "Total Cancer Risk (per million)",
              'Total Respiratory (hazard quotient)')

# get the national level averages
summary_table = data.frame(Variable=desc_vars)
summary_table_sd = data.frame(Variable=desc_vars)

for (v in 1:length(comparison_vars)) {
  summary_table[v,"Overall (National Average)"] = sum(table$pop*table[,comparison_vars[v]],na.rm=T)/sum(table$pop,na.rm=T)
  a = (table$pop*table[,comparison_vars[v]])/table$pop
  summary_table_sd[v,"Overall (National Average) SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# get the rural area level averages
rural <- table %>% filter(rural==1)
for (v in 1:length(comparison_vars)) {
  summary_table[v,"Rural Areas (National Average)"] = sum(rural$pop*rural[,comparison_vars[v]],na.rm=T)/sum(rural$pop,na.rm=T)
  a = (rural$pop*rural[,comparison_vars[v]])/rural$pop
  summary_table_sd[v,"Rural Areas (National Average) SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# get the population weighted averages around the production facilities
local = table$GEOID %in% unique(buffer$GEOID)
for (v in 1:length(comparison_vars)) {
  summary_table[v,"Within 1 mile of HFC production facility"] = sum(table$pop[local]*table[local,comparison_vars[v]],na.rm=T)/sum(table$pop[local],na.rm=T)
  a = (table$pop[local]*table[local,comparison_vars[v]])/table$pop[local]
  summary_table_sd[v,"Within 1 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# get the population weighted averages around the production facilities
local_3mi = table$GEOID %in% unique(buffer_3mi$GEOID)
for (v in 1:length(comparison_vars))  {
  summary_table[v,"Within 3 miles of HFC production facility"] = sum(table$pop[local_3mi]*table[local_3mi,comparison_vars[v]],na.rm=T)/sum(table$pop[local_3mi],na.rm=T)
  a = (table$pop[local_3mi]*table[local_3mi,comparison_vars[v]])/table$pop[local_3mi]
  summary_table_sd[v,"Within 3 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# only include two significant figures in the summary table
summary_table[,2:5] = signif(summary_table[,2:5],2)
summary_table_sd[,2:5] = signif(summary_table_sd[,2:5],2)

summary_table_all <- summary_table
summary_table_all_sd <- summary_table_sd

# export
list_of_datasets <- list("Means" = summary_table_all, "Standard Deviations" = summary_table_all_sd)
write.xlsx(list_of_datasets,"summary_tables_national.xlsx")

####################################################
###############  Conduct proximity analysis by plant - rural
####################################################

# facilities_rural <- facilities[facilities$Label!="CHEMOURS LOUISVILLE WORKS" & facilities$Label!="DAIKIN AMERICA INC.",]
facilities_rural <- facilities_sf_rural %>% filter(rural == 1)

for (i in 1:length(facilities_rural)){
  
facility <- paste0(facilities_rural[i,]$Label)
communities = st_buffer(facilities_rural[i,],dist=1*1609.34) 
communities_3mi = st_buffer(facilities_rural[i,],dist=3*1609.34) 

# find the census geographies within the buffer around the facilities
buffer = st_intersection(communities,shp) %>%
  select(GEOID,Tract,Label)

buffer_3mi = st_intersection(communities_3mi,shp) %>%
  select(GEOID,Tract,Label)

# get the national level averages
summary_table = data.frame(Variable=desc_vars)
summary_table_sd = data.frame(Variable=desc_vars)

# for (v in 1:length(comparison_vars)) {
#   summary_table[v,"Overall (National Average)"] = sum(table$pop*table[,comparison_vars[v]],na.rm=T)/sum(table$pop,na.rm=T)
#   a = (table$pop*table[,comparison_vars[v]])/table$pop
#   summary_table_sd[v,"Overall (National Average) SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
# }

# # get the state-level averages 
# state <- table %>% filter(State==facilities[i,]$`STATE`)
# for (v in 1:length(comparison_vars)) {
#   summary_table[v,"State Average"] = sum(state$pop*state[,comparison_vars[v]],na.rm=T)/sum(state$pop,na.rm=T)
#   a = (state$pop*state[,comparison_vars[v]])/state$pop
#   summary_table_sd[v,"State Average SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
# }

# get the national rural area level averages
rural <- table %>% filter(rural==1)
for (v in 1:length(comparison_vars)) {
  summary_table[v,"Rural Areas (National Average)"] = sum(rural$pop*rural[,comparison_vars[v]],na.rm=T)/sum(rural$pop,na.rm=T)
  a = (rural$pop*rural[,comparison_vars[v]])/rural$pop
  summary_table_sd[v,"Rural Areas (National Average) SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# get the rural area level averages in that state
state <- table %>% filter(rural==1 & State==facilities_rural[i,]$`State`)
for (v in 1:length(comparison_vars)) {
  summary_table[v,"Rural Areas (State Average)"] = sum(state$pop*state[,comparison_vars[v]],na.rm=T)/sum(state$pop,na.rm=T)
  a = (state$pop*state[,comparison_vars[v]])/state$pop
  summary_table_sd[v,"Rural Areas (State Average) SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# get the population weighted averages around the production facilities
local = table$GEOID %in% unique(buffer$GEOID)
for (v in 1:length(comparison_vars)) {
  summary_table[v,"Within 1 mile of HFC production facility"] = sum(table$pop[local]*table[local,comparison_vars[v]],na.rm=T)/sum(table$pop[local],na.rm=T)
  a = (table$pop[local]*table[local,comparison_vars[v]])/table$pop[local]
  summary_table_sd[v,"Within 1 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# get the population weighted averages around the production facilities
local_3mi = table$GEOID %in% unique(buffer_3mi$GEOID)
for (v in 1:length(comparison_vars))  {
  summary_table[v,"Within 3 miles of HFC production facility"] = sum(table$pop[local_3mi]*table[local_3mi,comparison_vars[v]],na.rm=T)/sum(table$pop[local_3mi],na.rm=T)
  a = (table$pop[local_3mi]*table[local_3mi,comparison_vars[v]])/table$pop[local_3mi]
  summary_table_sd[v,"Within 3 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# only include two significant figures in the summary table
summary_table[,2:5] = signif(summary_table[,2:5],2)
summary_table_sd[,2:5] = signif(summary_table_sd[,2:5],2)

# export
list_of_datasets <- list("Means" = summary_table, "Standard Deviations" = summary_table_sd)
write.xlsx(list_of_datasets,paste0("summary_tables_",facility,".xlsx"),overwrite = TRUE)

}

####################################################
###############  Conduct proximity analysis by plant- URBAN
####################################################

# facilities_urban <- facilities[facilities$Label=="CHEMOURS LOUISVILLE WORKS" | facilities$Label=="DAIKIN AMERICA INC.",]
facilities_urban <- facilities_sf_rural %>% filter(rural == 0)

for (i in 1:length(facilities_urban)){
  
  facility <- paste0(facilities_urban[i,]$Label)
  communities = st_buffer(facilities_urban[i,],dist=1*1609.34) 
  communities_3mi = st_buffer(facilities_urban[i,],dist=3*1609.34) 
  
  # find the census geographies within the buffer around the facilities
  buffer = st_intersection(communities,shp) %>%
    select(GEOID,Tract,Label)
  
  buffer_3mi = st_intersection(communities_3mi,shp) %>%
    select(GEOID,Tract,Label)
  
  # get the national level averages
  summary_table = data.frame(Variable=desc_vars)
  summary_table_sd = data.frame(Variable=desc_vars)
  
  # for (v in 1:length(comparison_vars)) {
  #   summary_table[v,"Overall (National Average)"] = sum(table$pop*table[,comparison_vars[v]],na.rm=T)/sum(table$pop,na.rm=T)
  #   a = (table$pop*table[,comparison_vars[v]])/table$pop
  #   summary_table_sd[v,"Overall (National Average) SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
  # }
  
  # # get the state-level averages 
  # state <- table %>% filter(State==facilities[i,]$`STATE`)
  # for (v in 1:length(comparison_vars)) {
  #   summary_table[v,"State Average"] = sum(state$pop*state[,comparison_vars[v]],na.rm=T)/sum(state$pop,na.rm=T)
  #   a = (state$pop*state[,comparison_vars[v]])/state$pop
  #   summary_table_sd[v,"State Average SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
  # }
  
  # get the national rural area level averages
  for (v in 1:length(comparison_vars)) {
    summary_table[v,"National Average"] = sum(table$pop*table[,comparison_vars[v]],na.rm=T)/sum(table$pop,na.rm=T)
    a = (table$pop*table[,comparison_vars[v]])/table$pop
    summary_table_sd[v,"National Average SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
  }
  
  # get the rural area level averages in that state
  state <- table %>% filter(State==facilities_urban[i,]$`State`)
  for (v in 1:length(comparison_vars)) {
    summary_table[v,"State Average"] = sum(state$pop*state[,comparison_vars[v]],na.rm=T)/sum(state$pop,na.rm=T)
    a = (state$pop*state[,comparison_vars[v]])/state$pop
    summary_table_sd[v,"State Average SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
  }
  
  # get the population weighted averages around the production facilities
  local = table$GEOID %in% unique(buffer$GEOID)
  for (v in 1:length(comparison_vars)) {
    summary_table[v,"Within 1 mile of HFC production facility"] = sum(table$pop[local]*table[local,comparison_vars[v]],na.rm=T)/sum(table$pop[local],na.rm=T)
    a = (table$pop[local]*table[local,comparison_vars[v]])/table$pop[local]
    summary_table_sd[v,"Within 1 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
  }
  
  # get the population weighted averages around the production facilities
  local_3mi = table$GEOID %in% unique(buffer_3mi$GEOID)
  for (v in 1:length(comparison_vars))  {
    summary_table[v,"Within 3 miles of HFC production facility"] = sum(table$pop[local_3mi]*table[local_3mi,comparison_vars[v]],na.rm=T)/sum(table$pop[local_3mi],na.rm=T)
    a = (table$pop[local_3mi]*table[local_3mi,comparison_vars[v]])/table$pop[local_3mi]
    summary_table_sd[v,"Within 3 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
  }
  
  # only include two significant figures in the summary table
  summary_table[,2:5] = signif(summary_table[,2:5],2)
  summary_table_sd[,2:5] = signif(summary_table_sd[,2:5],2)
  
  # export
  list_of_datasets <- list("Means" = summary_table, "Standard Deviations" = summary_table_sd)
  write.xlsx(list_of_datasets,paste0("summary_tables_",facility,".xlsx"), overwrite = TRUE)
  
}

########################################################
######  Plot Production facility differences in toxicity
########################################################


#### ALL data for new columns needs to be replaced with data for the new sites (14)
facilities_map_tox <- facilities_map_t %>% 
  mutate(total_risk_state_rural = c(28,36,30,52,33,52,28,30,42),
         total_risk_1mi = c(40,54,34,130,18,180,22,36,52),
         total_risk_resp_state_rural = c(.37,.51,.42,.61,.4,.61,.35,.42,.65),
         total_risk_resp_1mi = c(.46,.68,2.2,.77,.22,.94,.29,.46,.69)) %>%
  mutate(total_risk_diff = signif(total_risk_1mi/total_risk_state_rural,2),
         total_risk_diff_resp = signif(total_risk_resp_1mi/total_risk_resp_state_rural,2))

facilities_t <- usmap_transform(facilities_map) %>% mutate(Label = str_to_title(Label))

map <- plot_usmap(include=c(.northeast_region,.south_region,.north_central_region,.west_north_central,.west_region,.west_south_central), 
                  labels=TRUE, fill = "yellow", alpha = 0.05) +
  ggrepel::geom_label_repel(data = facilities_map_tox,
                            aes(x = LONGITUDE.1 , y = LATITUDE.1 , 
                                label = paste0(Label,' - ', total_risk_diff )),
                            size = 5, alpha = 0.8,
                            label.r = unit(0.5, "lines"), label.size = 0.5,
                            segment.color = "red", segment.size = 1,
                            seed = 1002) +
  geom_point(data = facilities_map_tox,
             aes(x = LONGITUDE.1, y = LATITUDE.1, size = total_risk_diff),
             color = "purple", alpha = 0.5) +
  scale_size_continuous(range = c(1, 16),
                        label = scales::comma) +
  labs(size = expression("  Cancer Risk Relative \n to State Rural Average")) +
  theme(legend.position = c(0.85, 0.1))
map

map_resp <- plot_usmap(include=c(.northeast_region,.south_region,.north_central_region,.west_north_central,.west_region,.west_south_central), 
                       labels=TRUE, fill = "yellow", alpha = 0.05) +
  ggrepel::geom_label_repel(data = facilities_map_tox,
                            aes(x = LONGITUDE.1 , y = LATITUDE.1 , 
                                label = paste0(Label,' - ', total_risk_diff_resp)),
                            size = 5, alpha = 0.8,
                            label.r = unit(0.5, "lines"), label.size = 0.5,
                            segment.color = "red", segment.size = 1,
                            seed = 1002) +
  geom_point(data = facilities_map_tox,
             aes(x = LONGITUDE.1, y = LATITUDE.1, size = total_risk_diff_resp),
             color = "purple", alpha = 0.5) +
  scale_size_continuous(range = c(1, 16),
                        label = scales::comma) +
  labs(size = expression("Respiratory Risk Relative \n to State Rural Average")) +
  theme(legend.position = c(0.85, 0.1))
map_resp

## END OF SCRIPT. Have a nice day! 