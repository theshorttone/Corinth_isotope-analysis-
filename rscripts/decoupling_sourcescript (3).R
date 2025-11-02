# Amelia Fitch
# created 5/23/24
# data sourced from the HJ Andrews LTER and NOAA online weather data repository

library(sf)
library(tmap)
library(tidyverse)


#### microclimate decoupling daily temp data info -----------------------------------

# source script for longterm microclimate data at the HJ Andrews and nearby weather stations

# Andrews data MS005 is air and soil temperature from reference stand network, from Mar 17 1971 - Oct 1 2019
# https://doi.org/10.6073/pasta/d0abe716146004268bb5f876ee42c992


### noaa weather station data 
# National Spatial Reference System, epsg code 1116, represents the NAD83 epsg 4269



# # Read in and process noaa data   ---------------------------------------

# noaa station data 
list.files("decoupling_data/noaa_data")

# make 3 separate files of the noaa, benchmark, and reference stand stations
  noaa_data <-
  list.files("decoupling_data/noaa_data", full.names = TRUE) %>%
  
  # pipe each file in folder through processing steps
  
  map(
    ~ read_csv(.x) %>%
      
      # set all column names to lower case
      
      rename_all(.funs = tolower) %>%
      
      # select columns of interest
      
      select(name, latitude, longitude, date, elevation, tmax, tmin) %>%
      na.omit() %>%
      mutate(date = lubridate::parse_date_time(date, orders = c("ymd", "mdy", "dmy")),
             date = lubridate::ymd(date)) ) %>%
      #filter(name != "ROSEBURG REGIONAL AIRPORT, OR US")) %>%
  
  # bind rows of each tibble sequentially 
  
  reduce(bind_rows) %>%
  
  # modify, then remove the name column
  
    separate(name, c("sitecode","other"), extra = "merge") %>%
    
    mutate(
      tmean = 0.5*(tmax + tmin) + (tmax - tmin)/(3*pi),
      HJA = "no",
      lat = latitude,
      long = longitude,
      year = lubridate::year(date) ) %>%
    
    select(-c(other)) %>%
    
    # replace the truncated coordinates from four stations 
    # with the correct version from NOAA metadata
    
    mutate(
      latitude = case_when(
        sitecode == "BIG"  ~ 42.052539,
        sitecode == "BLAZED" ~ 45.428673,
        sitecode == "BILLIE" ~ 42.406921,
        sitecode == "SUGARLOAF" ~ 43.663611,
        sitecode == "GOODWIN" ~  43.927891,
        sitecode == "DORENA" ~ 43.77956389,
        sitecode == "HOLLAND" ~  43.669132, 
        TRUE ~ latitude  # Keep original if not listed
      ),
      longitude = case_when(
        sitecode == "BIG"  ~ -122.266336,
        sitecode == "BLAZED" ~ -121.856469,
        sitecode == "BILLIE" ~ -122.266336,
        sitecode == "SUGARLOAF" ~ -122.629167,
        sitecode == "GOODWIN" ~ -123.891065,
        sitecode == "DORENA" ~ -122.96132500,
        sitecode == "HOLLAND" ~ -122.568819, 
        TRUE ~ longitude  # Keep original if not listed
      )
    ) %>%
    
    filter(sitecode != "BADGER" & # not supported by GNN data
             sitecode != "PEBBLE") %>% # not located on google earth , incorrect cooridinates
  
  # identify stations with the same shortened sitecode name
  
  mutate(sitecode = paste(sitecode, elevation, sep = "_"))


# # Benchmark station data processing  ------------------------------------


# meta data for stations

  # CENMET (air) clearcut 1986, natural regen, 1995 15 min air temp

  # CS2MET (air) old growth canopy, 1958
      # from 1998 15 min air temp

  # H15MET (air) old-growth opening / road junction, 1992 15 min air

  # PRIMET (air) maintained clearning, 1972
      # from 1972 mean hourly temp

  # UPLMET (air) - clearcut 1965, 1994 daily mean

  # VANMET (air) clearcut 1985, 1987 daily mean
      # from 1990 mean hourly air
  

benchmark_data <-

 bind_rows( read_csv("decoupling_data/HJA_data/MS001_benchmark_airtemp_1958_daily.csv") %>%
              mutate(QC_LEVEL = as.character(QC_LEVEL)), 
            read_csv("decoupling_data/HJA_data/MS001_20191001_daily.csv") ) %>%
  
  rename_all(.funs = tolower) %>%
  
  #rename some columns
  
  rename(tmin = airtemp_min_day,
         tmax = airtemp_max_day, 
         tmean = airtemp_mean_day) %>%
  
  # redo the tmean calculation 
  
  mutate(tmean = 0.5*(tmax + tmin) + (tmax - tmin)/(3*pi)) %>%
  
  # keep some columns
  
  select(tmin, 
         tmax, 
         tmean, 
         sitecode, 
         date,
         height) %>%
  mutate(year = lubridate::year(date))  %>%
  
  # filter to a specific height for probe code
  
  group_by(sitecode, date) %>%
  slice_min(abs(height - 150), with_ties = FALSE) %>%
  ungroup() %>%
  select(-height) %>%
  
  na.omit() %>%
  
  # add on the locations and other data by coding in the meta data
  
  left_join(
    tibble(
      latitude = c("44-14-36",  "44-12-54", "44-15-51", "44-12-43", "44-12-26", "44-16-18","44-16-24", "42-9-6.8"),
      longitude = c("122-8-30", "122-14-57",  "122-10-26", "122-15-21", "122-7-10", "122-8-58","122-8-58.8", "122-10-35.64") ) %>%
      
      # convert degrees-minutes-seconds to decimal degrees
      
      separate(latitude, paste("lat",c("d","m","s"), sep="_") ) %>%
      separate(longitude, paste("long",c("d","m","s"), sep="_" ) ) %>%
      mutate_if(is.character, as.numeric) %>%
      transmute(latitude = lat_d + lat_m/60 + lat_s/60^2,
                longitude = (long_d + long_m/60 + long_s/60^2)*-1) %>%
      mutate(lat = latitude,
             long = longitude) %>%
      
      # add the other columns of meta data to the tibble
      
      add_column(
        sitecode = c("CENMET", "CS2MET", "H15MET", "PRIMET", "UPLMET", "VANMET" ,"VARMET", "WS7MET"),
        elevation = c(1018, 485, 922, 430, 1294, 1273, 1315, 988),
        HJA = "yes") , 
    by = "sitecode") %>%
  
  # VARMET and WS7MET start in 2009 and 2006 respectively, so remove those stations
  
  filter(sitecode != "VARMET" & sitecode != "WS7MET")




# # HJA reference station data processing  --------------------------------


ref_data <-
  
  bind_rows(read_csv("decoupling_data/HJA_data/MS005_canopy_airtemp_daily.csv")%>%
              mutate(QC_LEVEL = as.character(QC_LEVEL)),
            read_csv("decoupling_data/HJA_data/MS005_20191001_daily.csv")) %>%
  
  # set all column names to lower case
  
  rename_all(.funs = tolower) %>%
  
  # select columns of interest
  
  select(sitecode, 
         date, 
         airtemp_mean_day, 
         airtemp_max_day, 
         airtemp_min_day,
         height) %>%
  
  rename(tmean = airtemp_mean_day,
         tmax = airtemp_max_day, 
         tmin = airtemp_min_day) %>%
  
  # redo the tmean calculation 
  mutate(tmean = 0.5*(tmax + tmin) + (tmax - tmin)/(3*pi)) %>%
  
  # after this point processing is done on the combined data frame
  
  # filter the probe to the lowest to the ground - 1.5m per date
  
  group_by(sitecode, date) %>%
  arrange(abs(height - 150)) %>% # Sort by proximity to 150
  slice(1) %>% # Retain only the closest height
  ungroup() %>%
  select(-height) %>%
  
  mutate(year = lubridate::year(date)) %>%
  na.omit() %>%
  
  # rename RS38 to TS38 to match the metadata
  
  mutate(sitecode = ifelse(sitecode == "RS38__", "TS38__", sitecode)) %>%
  
  # filter to the active temperature sites
  
  filter(sitecode %in% 
           c("RS02__", "RS04__", "RS05__", "RS10__", "RS12__", "RS20__", "RS26__", "TS38__", "RS86__", "RS89__")) %>%
  
  # add the meta data
  
  left_join(
    read_csv("decoupling_data/HJA_data/MS005_site_metadata.csv") %>%
      
      rename_all(.funs = tolower) %>%
      
      rename(longitude = west_bound_coord_decdeg, 
             # aspect = aspect_degrees, 
             latitude = north_bound_coord_decdeg,
             elevation = elevation_max_meters) %>%
      
      select(longitude, 
             latitude, 
             # aspect, 
             location_code, 
             elevation)  %>%
      
      # filter to the 10 stations that currently active as temp sites 
      # communication with Mark Shultz
      
      filter(location_code %in% 
               c("RS02__", "RS04__", "RS05__", "RS10__", "RS12__", "RS20__", "RS26__", "TS38__", "RS86__", "RS89__")),
    
    by = c("sitecode" = "location_code") ) %>%
  
  mutate(
         HJA = as.factor("yes"),
         lat = latitude,
         long = longitude)

  

  
  ####

# function to check the data coverage of each set -------------------------

  
  # Assuming each dataset has columns: station_id, date, and temperature
  calculate_station_summary <- function(df) {
    df %>%
      mutate(year = lubridate::year(date), month = lubridate::month(date)) %>%
      group_by(sitecode) %>%
      summarise(
        start_date = min(date, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(
        df %>%
          mutate(year = lubridate::year(date), 
                 month = lubridate::month(date)) %>%
          filter(month > 6 & month < 9) %>%  # Filter for July and August
          group_by(sitecode, year) %>%
          summarise(days = n_distinct(date), .groups = "drop"),
        by = "sitecode" ) %>%
      filter(days < 62)
  }
  
  # Combine summaries  
  combined_summary <- 
    list(benchmark_data, noaa_data, ref_data) %>%
    map(~ calculate_station_summary(.x)) %>%
    reduce(bind_rows) %>%
    arrange(days)
    
    bind_rows(summary1, summary2, summary3)
  

# Turn climate data into sf points and buffer ---------------------------
  

  # separate file for just the locations
  
  location_data <- 
    
    bind_rows(
      noaa_data %>%
        select(sitecode,  latitude, longitude, elevation) %>%
        unique(), 
      benchmark_data %>%
        select(sitecode,  latitude, longitude, elevation) %>%
        unique(),
      ref_data %>%
        select(sitecode,latitude, longitude, elevation) %>%
        unique())
    
    location_buffer <- 
      location_data %>%
      sf::st_as_sf(coords = c("longitude", "latitude"),
                 # data are in WGS 84
                 crs = 4326) %>%
  
  # match the raster epsg
  
  sf::st_transform(5070) %>%
    
    # create a buffer 
    
    sf::st_buffer(dist = 30)
  
  write_csv(location_data %>% 
              as_tibble(), 
            file = "decoupling_data/processed_data/decoupling_locations.csv")


# Raw GNN data processing - only run on cluster ---------------------------

# The following data are raster files for canopy cover data in region from gradient nearest neighbor dataset
  
# Define the paths to the folders and the point shapefile
  
  # following code was fun on the cluster for processing 
  # un-hashtag from this sentence to run code
  
# folder1 <- "decoupling_data/GNN_1/rasters"
# folder2 <- "decoupling_data/GNN_2/rasters"
# folder3 <- "decoupling_data/GNN_3/rasters"
# 
# # Merge rasters
# 
# gnn_data <- 
#   terra::mosaic(
#   list.files(folder1, full.names = TRUE) %>%
#   sort() %>%
#   terra::rast(),
#   list.files(folder2, full.names = TRUE) %>%
#     sort() %>%
#     terra::rast(),
#   list.files(folder3, full.names = TRUE) %>%
#     sort() %>%
#     terra::rast() , overwrite= TRUE) 
# 
# terra::writeRaster(gnn_data, "decoupling_data/processed_data/gnn_data.tif")



# canopy cover data unmasked for UPLMET ----------------------------------

# 

  gnn_unmasked_point <- 
    
    location_buffer %>%
    
    filter(sitecode == "UPLMET") %>%
    
    terra::vect() %>%
    terra::extract(
      list.files("decoupling_data/data/cancov_unmasked_1986_2021", full.names = TRUE) %>%
        sort() %>%
        terra::rast(),
      .,
      mean) %>%
    
    # bind values from each year together 
    
    bind_cols() %>%
    as_tibble() %>%
    
    # remove the "cancov_" string from the column names
    
    rename_with(~ str_remove(., "cancov_"), everything()) %>%
    
    # pivot longer so each year is it's own row per station
    pivot_longer(-ID, names_to = "Year", values_to = "canopy_cover") %>%
    
    # remove the extra stuff on year
    
    mutate_all(~ str_remove_all(., "_k07_mean")) %>%
    mutate(
      Year = as.integer(Year),
      ID = 31,
      ID2 = paste(ID, Year, sep = "_"),
      canopy_cover = as.numeric(canopy_cover) )%>%
    select(ID, Year, canopy_cover, ID2)
  

# Compile canopy, aspect, and climate data into one file ------------------

# Extract values from 30 m buffer of points

gnn_buffer <-
  
  location_buffer %>%
  # turn to a raster object to extract points
  
  terra::vect() %>%
  terra::extract(
    terra::rast("decoupling_data/processed_data/gnn_data.tif"),
    .,
    mean) %>%

  # bind values from each year together 
  
  bind_cols() %>%
  as_tibble() %>%
  
  # remove the "cancov_" string from the column names
  
  rename_with(~ str_remove(., "cancov_"), everything()) %>%
  
  # pivot longer so each year is it's own row per station
  pivot_longer(-ID, names_to = "Year", values_to = "canopy_cover") %>%
  
  # replace the UPLMET values with the ones from the unmasked data
  
  mutate(ID2 = paste(ID, Year, sep = "_"), 
         year = as.integer(Year)) %>%
    
# remove the UPLEMET and bind the unmasked version 
    filter(ID != 31) %>%

  bind_rows(gnn_unmasked_point) %>%
  
  
  # make the -1 gnn data 0, and NA data 0 for now 
  
  mutate(canopy_new = if_else(canopy_cover < 0, 0, canopy_cover)/100) %>%
  
  # merge to add site code info 
  
  full_join(
    location_data %>%
      as_tibble() %>%
      select(sitecode, elevation) %>%
      unique() %>%
      
      # the output will be in the same order as the location data, so numbering is a fine way to merge
      add_column(ID = 1:49),
    by = "ID") %>%
  
  
  #make year and site code ID
  
    mutate(join = paste(sitecode, Year, sep = "_"), 
           canopy_new = replace_na(canopy_new, 0)) %>%
    select(-c( canopy_cover, Year))
  
  # write file to processed data
  
  write_csv(gnn_buffer, file = "decoupling_data/processed_data/gnn_buffer.csv")
  
  # plot all stations
  
  gnn_buffer %>%
    ggplot(aes(x = year, y = canopy_new)) + 
    geom_point() + 
    theme_classic()+
    facet_wrap(~sitecode)
    

# Station filtering based on canopy cover change --------------------------

  stations_filtered <- 
  gnn_buffer %>%
    
  select(sitecode, canopy_new, year) %>%
    unique() %>%
    group_by(sitecode) %>%
    nest() %>%
    
    # calculate the slope in change over time for stations
    
    mutate(
      model = map(data, ~ lm(canopy_new/100 ~ year, data = .x)),  
      
      # Fit model inside summarise
      # Extract slope
      
      slope = map(model, broom::tidy),  
      
      # Extract R²
      
      r2 = map(model, broom::glance )) %>%
    
    # Unnest model coefficients  
    unnest(slope) %>%    
    
    # keep only the slope term
    filter(term == "year") %>%
    
    # Unnest model statistics (preventing column conflicts)  
    unnest(r2, names_sep = "_") %>%
    
    # select variables
    
    select(sitecode, 
           estimate, 
           r2_r.squared) %>%
    
    # canopy average and variance
    
    left_join(
      gnn_buffer %>%
        group_by(sitecode, elevation) %>%
        summarise(
          canopy_avg = mean(canopy_new),
          canopy_var = var(canopy_new)), 
      by = "sitecode") %>%
    
    # classify sites by the average canopy cover over time 
    
    mutate(site_class = if_else(canopy_avg < 40, "open", "closed")) %>%
    
    arrange(canopy_avg) %>%
    ungroup() %>%
    
    # STATION FILTERING STEPS
    
    # 1. Add a flag for canopy variance > 100, slope > 0.8
    
    mutate( flag = case_when(
      canopy_var > 100 ~ 1, # flags with 1 will be removed
      estimate > 0.8 ~ 1, 
      canopy_var > 50 & canopy_var <= 100 ~ 0.5, # canopy variance greater 
      TRUE ~ 0  )) %>%
    
    # remove all severely variable canopy stations 
    
    filter(flag != 1) %>%
    
    # Calimus is too far east
    # Holland I can't find and verify on google earth, and the coordinates provided are too coarse
    # high has clear veg change on google earth
    
    filter(sitecode %in% c("CALIMUS_2020.5", "HOLLAND_1502.7", "HIGH_589.8", "UPLMET") == F ) %>%
    
    # remove the lowest elevation stations to even profiles between station types
    # these stations are regional airports, which strengthens the comparisons
    # Oregon, Roseburg, Eugene are airports and lowest elevation - out
    # fern ridge are Ashland are water treatment plants - out
    # medford is next to a small airport on grass
    # ashland is on concrete - remove
    # oak ride is on concrete - remove
    # higher elevation cottage is likely on concrete
    # dorena is on grass
    # Agness and Corvallis_1 are the lowest elevation - out
    # uplmet is more of an open station with clear changes on google earth - out
  # TS38 is clearly young forest in 1994
  # RS89 has a younger regrowing stand very close by - OK
    
    # filter by name and google earth cross reference (above)
    
  filter(sitecode %in% c("FERN_147.8", "EUGENE_109", "ROSEBURG_146.3", "OREGON_50.9", "COTTAGE_253.3", "ASHLAND_527","OAKRIDGE_388.6","CORVALLIS_68.6","MCKENZIE_450.5") == F) %>%
    
    arrange(site_class, elevation) %>% print(n = 30)
  
  # explort list of stations
    
  stations_filtered %>% 
    left_join(location_data %>%
                separate(sitecode, into = c("sitecode","elevation_2")) %>%
                select(sitecode, longitude, latitude), by = "sitecode") %>%
   
    select(-c(elevation_2, r2_r.squared, estimate, flag, clear_sky) ) %>%
    mutate(new_coords = case_match(sitecode, 
                                   c("GOODWIN",
                                     "TS38", 
                                     "CORVALLIS",
                                     "COTTAGE",
                                     "BLAZED",
                                     "DORENA",
                                     "OAKRIDGE",
                                     "BIG",
                                     "MCKENZIE",
                                     "BILLIE") ~ 1, .default = 0) ) %>% print(n = 30) %>%
   
    write_csv("decoupling_data/decoupling_stations_updated_2.25.25.csv")
    
  ###

  
# Adjust temperature data based on selected stations and remove outliers ----------

  
  decoupling_data <- 
    
    bind_rows(noaa_data, benchmark_data, ref_data) %>%
    
    # filter the stations based on stations_filtered
    
    filter(sitecode %in% stations_filtered$sitecode) %>%
    
    # filter by year
    
    filter(year > 1985) %>%
    
    # filter by the highest temperature ever recorded in Oregon
    
    filter(tmax <= 48) %>%
#     
    # remove outliers with median 
    
    group_by(sitecode) %>%

    # Scale factor = 1 for direct comparison

    mutate(mad_val = mad(tmax, constant = 1, na.rm = TRUE),  
           median_val = median(tmax, na.rm = TRUE),
           
           # Adjusted threshold for 4*MAD should remove outliers that are machine error and not warmer days
           
           upper_bound = median_val + 3.75 * mad_val,  
           lower_bound = median_val - 3.75 * mad_val) %>%

    # remove tmax values outside the median

    filter( tmax < upper_bound & tmax > lower_bound) %>%
    # 406 points are removed that are extreme 
    
    #remove the elevation values from sitecode
    
    separate(sitecode, into = c("sitecode", "elevation_2")) %>%
  
    # select(-c(median_val, upper_bound, elevation_2) ) %>%
    ungroup() %>%
    
  # create a new column with the date_time named more specific
  
  mutate(year_month_day = date) %>%
    separate(year_month_day, 
             into = c("year","month", "day")) %>%
    mutate(year_month = lubridate::ym(paste(year, month, sep = "-"))) %>%
    select(-elevation_2)
  
  
# check for duplicated data
  
  decoupling_data  %>%
    select(date, sitecode, latitude, elevation) %>%
    group_by(sitecode, date) %>%  # Group by site
    
    # Check for duplicates across all columns
    
    mutate(duplicated_data = duplicated(across(everything()))) %>%  
    
    # Summarize if any duplicates exist for each site
    
    summarize(duplicates_exist = any(duplicated_data), .groups = "drop") %>%
    
    filter(duplicates_exist == "TRUE") %>%
  
    
    print(n = 40)
    
  # save file

write_csv(decoupling_data, file = "processed_data/decoupling_data.csv")
  # 


# list of sites and their variables
table1 <- decoupling_data %>%
  select(sitecode, 
         site_class, 
         latitude,
         elevation, 
         canopy_avg, 
         canopy_var, 
         estimate,
         flag,
         clear_sky) %>%
  unique() %>%

arrange(site_class, elevation ) %>%
  
  print(n = 45)
  



# plot a reduced dataset so each station for each year has a value 

can_cov_plot <- 
  gnn_buffer %>%
  
  # add a column for stations that are in this dataset
  
 mutate(flag = case_when(sitecode %in% stations_filtered$sitecode ~ 1, 
                         .default = 0)) %>%
  
  group_by(year, sitecode) %>%
  summarise_if(is.numeric, mean) %>%
    ggplot(aes(x = year, y = canopy_new, color = as.factor(flag))) + 
    geom_point() + 
    facet_wrap(~ sitecode) +
   scale_color_manual(name = "Selection", 
                      values = c("grey", "black"),
                      labels = c("Removed", "Retained")) +
  theme_classic() +
  labs(y = "Canopy cover (%)", x = "Year") + 
  scale_x_discrete(breaks = c("1986","2003","2021") ) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) 
  
ggsave(can_cov_plot, file = "plots/can_cov_supplement.pdf")



# Summarized data for summer -------------------------------------

# summer 

summer_cldata <- 
  
  decoupling_data %>%
  
  # filter by months of choice
  
  filter( month == "07" | month == "08") %>%
  filter(tmax > 15) %>%
  
  # group by 
  
  group_by(sitecode, year) %>%
  summarise_at(vars(tmax : tmean),
               mean, 
               na.rm = TRUE) %>%
  
  ungroup() %>%
  
  # calculate the number of days over 40 C 
  
  left_join(
    decoupling_data %>%
      filter( month == "07" | month == "08") %>%
      
      # create a new column that marks a day as 1 if the max temp was over 40 C
      
      mutate(days_35C = if_else(tmax > 35, 1, 0),
             days_30C = if_else(tmax > 30, 1, 0),
             days_40C = if_else(tmax > 40, 1, 0)) %>%
      
      # sum these days by year
      
      group_by(sitecode, year) %>%
      summarise(days_35C = sum(days_35C),
                days_30C = sum(days_30C),
                days_40C = sum(days_40C)),
    by = c("sitecode","year")) %>%
  
  # calculate variance and max - min difference for each day, then summarize the mean
  
  left_join(
    decoupling_data %>%
      filter( month == "07" | month == "08") %>%
      group_by(sitecode, year, latitude, longitude) %>%
      summarise(
        variance_day = sum((tmax - tmean)^2)/(length(tmax)-1),
        daily_difference = mean(tmax - tmin),
        standard_error = sqrt(variance_day)/sqrt(length(tmax))),
    by = c("sitecode", "year")) %>%
  ungroup() %>%
  
  # add on site data
  
  left_join(stations_filtered %>%
              separate(sitecode, into = c("sitecode", NA)) %>%
              select(sitecode, 
                     elevation,
                     site_class,
                     canopy_var,
                     canopy_avg), 
            by = c("sitecode")) %>%
  
  mutate(year = as.integer(year)) %>%
  left_join(gnn_buffer %>%
              filter(sitecode != "CORVALLIS_68.6" &
                       sitecode != "COTTAGE_253.3")%>%
              select(sitecode, canopy_new, year) %>%
              separate(sitecode, into = c("sitecode",NA)),
            by = c("year","sitecode")
              )

# write csv

write_csv(summer_cldata, file = "processed_data/summer_cldata.csv")


