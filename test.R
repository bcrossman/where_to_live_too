
# function to obtain US county shape
get_US_county_2010_shape <- function() {
  dir <- tempdir()
  download.file("http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_20m.zip", destfile = file.path(dir, "gz_2010_us_050_00_20m.zip"))
  unzip(file.path(dir, "gz_2010_us_050_00_20m.zip"), exdir = dir)
  read_shape(file.path(dir, "gz_2010_us_050_00_20m.shp"))
}

US <- append_data(US, select(presidential, per_dem, combined_fips), key.shp = "FIPS", key.data = "combined_fips", ignore.duplicates = TRUE)
US[under_coverage()$id,]
presidential[over_coverage()$id,]

US <- append_data(US, select(education_rural, FIPS, Rural_Urban, Rural_Urban_2, Percent_College), key.shp = "FIPS", key.data = "FIPS", ignore.duplicates = TRUE, ignore.na=T)
US[under_coverage()$id,]
education_rural[over_coverage()$id,]

US <- append_data(US, select(employment, 
                             FIP, 
                             Median_Household_Income_2016, 
                             Med_HH_Income_Percent_of_State_Total_2016), 
                  key.shp = "FIPS", 
                  key.data = "FIP", ignore.duplicates = TRUE, ignore.na=T)
US[under_coverage()$id,]
employment[over_coverage()$id,]

US <- append_data(US, select(home_price, 
                             combined_fips, 
                             listing_price_per_sf), 
                  key.shp = "FIPS", 
                  key.data = "combined_fips", ignore.duplicates = TRUE, ignore.na=T)
US[under_coverage()$id,]
home_price[over_coverage()$id,]

#Temperature
US <- append_data(US, temperature, 
                  key.shp = "FIPS", 
                  key.data = "FIPS", ignore.duplicates = TRUE, ignore.na=T)
US[under_coverage()$id,]

View(presidential[presidential$combined_fips %in% under_coverage()$value,])

home_price[over_coverage()$id,]

#Sunlight
#Temperature
US <- append_data(US, sunlight, 
                  key.shp = "FIPS", 
                  key.data = "FIPS", ignore.duplicates = TRUE, ignore.na=T)
View(presidential[presidential$combined_fips %in% under_coverage()$value,])

sunlight[over_coverage()$id,]


#US <- get_US_county_2010_shape()
#save(US, file = "us.rdata")
load("us.rdata")  

US$FIPS <- paste0(US$STATE, US$COUNTY)

US <- append_data(US, select(presidential, per_dem, combined_fips), key.shp = "FIPS", key.data = "combined_fips", ignore.duplicates = TRUE)
US[under_coverage()$id,]
presidential[over_coverage()$id,]

US <- append_data(US, select(education_rural, FIPS, Rural_Urban, Rural_Urban_2, Percent_College), key.shp = "FIPS", key.data = "FIPS", ignore.duplicates = TRUE, ignore.na=T)
US[under_coverage()$id,]
education_rural[over_coverage()$id,]

US <- append_data(US, select(employment, 
                             FIP, 
                             Median_Household_Income_2016, 
                             Med_HH_Income_Percent_of_State_Total_2016), 
                  key.shp = "FIPS", 
                  key.data = "FIP", ignore.duplicates = TRUE, ignore.na=T)
US[under_coverage()$id,]
employment[over_coverage()$id,]

US <- append_data(US, select(home_price, 
                             combined_fips, 
                             listing_price_per_sf), 
                  key.shp = "FIPS", 
                  key.data = "combined_fips", ignore.duplicates = TRUE, ignore.na=T)
US[under_coverage()$id,]
home_price[over_coverage()$id,]

#Temperature
US <- append_data(US, temperature, 
                  key.shp = "FIPS", 
                  key.data = "FIPS", ignore.duplicates = TRUE, ignore.na=T)
US[under_coverage()$id,]

View(presidential[presidential$combined_fips %in% under_coverage()$value,])

home_price[over_coverage()$id,]

#Sunlight
#Temperature
US <- append_data(US, sunlight, 
                  key.shp = "FIPS", 
                  key.data = "FIPS", ignore.duplicates = TRUE, ignore.na=T)
View(presidential[presidential$combined_fips %in% under_coverage()$value,])

sunlight[over_coverage()$id,]

ttm()
qtm(US, fill = "per_dem")


#Not sure what this does


US_cont <- US %>% 
  subset(!STATE %in% c("02","15","72"))

US_AK <- US %>% 
  subset(STATE == "02") 
  #simplify_shape(0.2) 

US_HI <- US %>% 
  subset(STATE == "15")

# create state boundaries
US_states <- US_cont %>% 
  aggregate_map(by = "STATE")


m_cont <- tm_shape(US_cont, projection=2163) +
  tm_polygons("per_dem", border.col = "gray50", border.alpha = .5, title = "", showNA = TRUE) +
  tm_shape(US_states) +
  tm_borders(lwd=1, col = "black", alpha = .5) #+
  # tm_layout(title="2010 Adult Obesity by County, percent", 
  #           title.position = c("center", "top"), 
  #           legend.position = c("right", "bottom"), 
  #           frame = FALSE, 
  #           inner.margins = c(0.1, 0.1, 0.05, 0.05))
  
  m_AK <- tm_shape(US_AK, projection = 3338) +
    tm_polygons("per_dem", border.col = "gray50", border.alpha = .5) +
    tm_layout("Alaska", legend.show = FALSE, bg.color = NA, title.size = 0.8, frame = FALSE)
  
  # Hawaii inset
  m_HI <- tm_shape(US_HI, projection = 3759) +
    tm_polygons("per_dem", border.col = "gray50", border.alpha = .5) +
    tm_layout("Hawaii", legend.show = FALSE, bg.color=NA, title.position = c("LEFT", "BOTTOM"), title.size = 0.8, frame=FALSE)
  
  # specify viewports for Alaska and Hawaii
  vp_AK <- viewport(x = 0.15, y = 0.15, width = 0.3, height = 0.3)
  vp_HI <- viewport(x = 0.4, y = 0.1, width = 0.2, height = 0.1)
  

  tmap_mode("plot")
  
  # print map
  print(m_cont)
  print(m_AK, vp = vp_AK)
  print(m_HI, vp = vp_HI)
  