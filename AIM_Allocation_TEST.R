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

# get the population weighted averages around the production facilities
local_5mi = table$GEOID %in% unique(buffer_5mi$GEOID)
for (v in 1:length(comparison_vars))  {
  summary_table[v,"Within 5 miles of HFC production facility"] = sum(table$pop[local_5mi]*table[local_5mi,comparison_vars[v]],na.rm=T)/sum(table$pop[local_5mi],na.rm=T)
  a = (table$pop[local_5mi]*table[local_5mi,comparison_vars[v]])/table$pop[local_5mi]
  summary_table_sd[v,"Within 5 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# get the population weighted averages around the production facilities
local_10mi = table$GEOID %in% unique(buffer_10mi$GEOID)
for (v in 1:length(comparison_vars))  {
  summary_table[v,"Within 10 miles of HFC production facility"] = sum(table$pop[local_10mi]*table[local_10mi,comparison_vars[v]],na.rm=T)/sum(table$pop[local_10mi],na.rm=T)
  a = (table$pop[local_10mi]*table[local_10mi,comparison_vars[v]])/table$pop[local_10mi]
  summary_table_sd[v,"Within 10 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# only include two significant figures in the summary table
summary_table[,2:7] = signif(summary_table[,2:7],2)
summary_table_sd[,2:7] = signif(summary_table_sd[,2:7],2)

summary_table_all <- summary_table
summary_table_all_sd <- summary_table_sd

# export
list_of_datasets <- list("Means" = summary_table_all, "Standard Deviations" = summary_table_all_sd)
write.xlsx(list_of_datasets,"output/Allocation Rule/summary_tables/allocation_rule_summary_tables_national.xlsx", overwrite = TRUE)

####################################################
###############  Conduct proximity analysis by plant - rural
####################################################

facilities_rural <- facilities_map %>% filter(rural == 1)

for (i in 1:length(facilities_rural)){
  
facility <- paste0(facilities_rural[i,]$Label)
communities = st_buffer(facilities_rural[i,],dist=1*1609.34) 
communities_3mi = st_buffer(facilities_rural[i,],dist=3*1609.34) 
communities_5mi = st_buffer(facilities_rural[i,],dist=5*1609.34) 
communities_10mi = st_buffer(facilities_rural[i,],dist=10*1609.34) 

# find the census geographies within the buffer around the facilities
buffer = st_intersection(communities,shp) %>%
  select(GEOID,Tract,Label)

buffer_3mi = st_intersection(communities_3mi,shp) %>%
  select(GEOID,Tract,Label)

buffer_5mi = st_intersection(communities_5mi,shp) %>%
  select(GEOID,Tract,Label)

buffer_10mi = st_intersection(communities_10mi,shp) %>%
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

# get the population weighted averages around the production facilities
local_5mi = table$GEOID %in% unique(buffer_5mi$GEOID)
for (v in 1:length(comparison_vars))  {
  summary_table[v,"Within 5 miles of HFC production facility"] = sum(table$pop[local_5mi]*table[local_5mi,comparison_vars[v]],na.rm=T)/sum(table$pop[local_5mi],na.rm=T)
  a = (table$pop[local_5mi]*table[local_5mi,comparison_vars[v]])/table$pop[local_5mi]
  summary_table_sd[v,"Within 5 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# get the population weighted averages around the production facilities
local_10mi = table$GEOID %in% unique(buffer_10mi$GEOID)
for (v in 1:length(comparison_vars))  {
  summary_table[v,"Within 10 miles of HFC production facility"] = sum(table$pop[local_10mi]*table[local_10mi,comparison_vars[v]],na.rm=T)/sum(table$pop[local_10mi],na.rm=T)
  a = (table$pop[local_10mi]*table[local_10mi,comparison_vars[v]])/table$pop[local_10mi]
  summary_table_sd[v,"Within 10 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# only include two significant figures in the summary table
summary_table[,2:7] = signif(summary_table[,2:7],2)
summary_table_sd[,2:7] = signif(summary_table_sd[,2:7],2)

# export
list_of_datasets <- list("Means" = summary_table, "Standard Deviations" = summary_table_sd)
write.xlsx(list_of_datasets,paste0("output/Allocation Rule/summary_tables/allocation_rule_summary_tables_",facility,".xlsx"), overwrite = TRUE)

}

####################################################
###############  Conduct proximity analysis by plant- URBAN
####################################################

# facilities_urban <- facilities[facilities$Label=="CHEMOURS LOUISVILLE WORKS" | facilities$Label=="DAIKIN AMERICA INC.",]
facilities_urban <- facilities_map %>% filter(rural == 0)

for (i in 1:length(facilities_urban)){
  
  facility <- paste0(facilities_urban[i,]$Label)
  communities = st_buffer(facilities_urban[i,],dist=1*1609.34) 
  communities_3mi = st_buffer(facilities_urban[i,],dist=3*1609.34) 
  communities_5mi = st_buffer(facilities_urban[i,],dist=5*1609.34) 
  communities_10mi = st_buffer(facilities_urban[i,],dist=10*1609.34) 
  
  # find the census geographies within the buffer around the facilities
  buffer = st_intersection(communities,shp) %>%
    select(GEOID,Tract,Label)
  
  buffer_3mi = st_intersection(communities_3mi,shp) %>%
    select(GEOID,Tract,Label)
  
  buffer_5mi = st_intersection(communities_5mi,shp) %>%
    select(GEOID,Tract,Label)
  
  buffer_10mi = st_intersection(communities_10mi,shp) %>%
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
  
  # get the population weighted averages around the production facilities
  local_5mi = table$GEOID %in% unique(buffer_5mi$GEOID)
  for (v in 1:length(comparison_vars))  {
    summary_table[v,"Within 5 miles of HFC production facility"] = sum(table$pop[local_5mi]*table[local_5mi,comparison_vars[v]],na.rm=T)/sum(table$pop[local_5mi],na.rm=T)
    a = (table$pop[local_5mi]*table[local_5mi,comparison_vars[v]])/table$pop[local_5mi]
    summary_table_sd[v,"Within 5 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
  }
  
  # get the population weighted averages around the production facilities
  local_10mi = table$GEOID %in% unique(buffer_10mi$GEOID)
  for (v in 1:length(comparison_vars))  {
    summary_table[v,"Within 10 miles of HFC production facility"] = sum(table$pop[local_10mi]*table[local_10mi,comparison_vars[v]],na.rm=T)/sum(table$pop[local_10mi],na.rm=T)
    a = (table$pop[local_10mi]*table[local_10mi,comparison_vars[v]])/table$pop[local_10mi]
    summary_table_sd[v,"Within 10 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
  }
  
  # only include two significant figures in the summary table
  summary_table[,2:7] = signif(summary_table[,2:7],2)
  summary_table_sd[,2:7] = signif(summary_table_sd[,2:7],2)
  
  # export
  list_of_datasets <- list("Means" = summary_table, "Standard Deviations" = summary_table_sd)
  write.xlsx(list_of_datasets,paste0("output/Allocation Rule/summary_tables/allocation_rule_summary_tables_",facility,".xlsx"), overwrite = TRUE)
  
}
