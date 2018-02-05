#us_geo <- tigris::counties(class = "sf")
#save(us_geo, file = "us_geo.rdata")
library(tigris)
library(tidyverse)
library(tmap)
library(tmaptools)
library(grid)
library(stringr)
library(splines)
library(DT)

a <- dir(path = "./data/", full.names = T, pattern = "csv")

presidential <- read_csv(file = "./data/2016_US_County_Level_Presidential_Results.csv")

presidential$FIPS <- as.character(presidential$combined_fips)
presidential$FIPS[str_length(presidential$FIPS)==4] <- paste0(0,presidential$FIPS[str_length(presidential$FIPS)==4])

education_rural <- read_csv(file = "./data/Education_Rural.csv")

names(education_rural) <- c("FIPS","State","County","Rural_Urban","Rural_Urban_2","Percent_College","x1")

employment <- read_csv(file = "./data/15zpallagi.csv")

employment_zip <- 
  employment %>% 
  group_by(STATE, zipcode) %>% 
  summarise(Median_Household_Income_2016 = sum(A02650)/sum(N1))

employment_state <- 
  employment %>% 
  group_by(STATE) %>% 
  summarise(state_agi = sum(A02650)/sum(N1)) 

employment <- 
  employment_zip %>% 
  left_join(employment_state) %>% 
  mutate(Med_HH_Income_Percent_of_State_Total_2016 = Median_Household_Income_2016 / state_agi) %>% 
  ungroup() %>% 
  select(zipcode,Median_Household_Income_2016, Med_HH_Income_Percent_of_State_Total_2016)

home_price <- read_csv(file = "./data/Zip_MedianListingPricePerSqft_AllHomes.csv")
home_price <- 
  home_price %>%
  rename(zipcode = RegionName) %>% 
  mutate(listing_price_per_sf  = rowMeans(select(., contains("2017"))))

temperature <- read_csv(file = "./data/Temperature.csv")

temperature <- 
  temperature %>% 
  rename(FIPS = `County Code`) %>% 
  filter(!is.na(FIPS)) %>% 
  group_by(FIPS) %>% 
  summarize(days_degrees_from_perfect = sum(30 * abs(76-`Avg Daily Max Air Temperature (F)`)),
            avg_jan = mean(`Avg Daily Max Air Temperature (F)`[Month=="Jan"]),
            avg_jul = mean(`Avg Daily Max Air Temperature (F)`[Month=="Jul"]))


temperature$FIPS <- as.character(temperature$FIPS)
temperature$FIPS[str_length(temperature$FIPS)==4] <- paste0(0,temperature$FIPS[str_length(temperature$FIPS)==4])    

#Sunlight
sunlight <- read_csv(file = "./data/sunlight.csv")

sunlight <- 
  sunlight %>% 
  rename(FIPS = `County Code`) %>% 
  filter(!is.na(FIPS)) %>% 
  group_by(FIPS) %>% 
  summarize(avg_sunlight = mean(AvgSunlight))


sunlight$FIPS <- as.character(sunlight$FIPS)
sunlight$FIPS[str_length(sunlight$FIPS)==4] <- paste0(0,sunlight$FIPS[str_length(sunlight$FIPS)==4])    

#Education (Separate analysis to isolate income effect)
reading <- read_csv(file = "./data/rla-achievement-sch-sy2015-16.csv")
math <- read_csv(file = "./data/math-achievement-sch-sy2015-16.csv")
lea_id_key <- read_csv(file = "./data/school_leaid_data.csv")

lea_id_key$LZIP <- as.character(lea_id_key$LZIP)
lea_id_key$LZIP[str_length(lea_id_key$LZIP)==4] <- paste0(0,lea_id_key$LZIP[str_length(lea_id_key$LZIP)==4])    

education_perf <-
  reading %>% 
  select(LEAID,ALL_RLA00PCTPROF_1516) %>% 
  left_join(math %>% 
              select(LEAID, ALL_MTH00PCTPROF_1516)) %>% 
  left_join(lea_id_key %>% 
              select(LEAID, LZIP, CONUM))

numericize_data<- function(df){
  #df <- education_perf$ALL_RLA00PCTPROF_1516
  df <- str_sub(df, start= -2)
  df <- gsub(pattern = "-",replacement = "",x = df)
  df <- gsub(pattern = "E",replacement = "",x = df)
  df[df=="PS"] <- NA
  df <- as.numeric(df)
}

education_perf$ALL_RLA00PCTPROF_1516 <- numericize_data(education_perf$ALL_RLA00PCTPROF_1516)
education_perf$ALL_MTH00PCTPROF_1516 <- numericize_data(education_perf$ALL_MTH00PCTPROF_1516)

sum_education_perf <- 
  education_perf %>%
  select(-LEAID) %>% 
  rename(FIPS = CONUM,
         zipcode = LZIP) %>% 
  group_by(FIPS, zipcode) %>% 
  summarise(math_avg = mean(ALL_MTH00PCTPROF_1516, na.rm = T),
            reading_avg = mean(ALL_RLA00PCTPROF_1516, na.rm = T),
            math_max = max(ALL_MTH00PCTPROF_1516, na.rm = T),
            reading_max = max(ALL_RLA00PCTPROF_1516, na.rm = T)) 

sum_education_perf[is.infinite(sum_education_perf$math_max),"math_avg"] <- NA
sum_education_perf[is.infinite(sum_education_perf$math_max),"math_max"] <- NA
sum_education_perf[is.infinite(sum_education_perf$reading_max),"reading_avg"] <- NA
sum_education_perf[is.infinite(sum_education_perf$reading_max),"reading_max"] <- NA


sum_education_perf <- 
  sum_education_perf %>% 
  ungroup() %>% 
  mutate(math_z_avg = (math_avg-mean(math_avg, na.rm = T))/sd(math_avg, na.rm = T),
         math_z_max = (math_max-mean(math_max, na.rm = T))/sd(math_max, na.rm = T),
         reading_z_avg = (reading_avg-mean(reading_avg, na.rm = T))/sd(reading_avg, na.rm = T),
         reading_z_max = (reading_max-mean(reading_max, na.rm = T))/sd(reading_max, na.rm = T),
         comb_z_avg = (math_z_avg + reading_z_avg)/2,
         comb_z_max = (math_z_max + reading_z_max)/2)


education_income <- left_join(sum_education_perf, employment, by="zipcode")

ggplot(education_income,
       aes(x=Median_Household_Income_2016,
           y=math_avg )) +
  geom_point()+
  geom_smooth()

ggplot(education_income,
       aes(x=Median_Household_Income_2016,
           y=reading_avg )) +
  geom_point()+
  geom_smooth()

qplot(education_income$comb_z_avg)
qplot(education_income$comb_z_max)

fit_avg <- lm(comb_z_avg ~ ns(Median_Household_Income_2016,2) + ns(Med_HH_Income_Percent_of_State_Total_2016,2), 
              data = education_income)

summary(fit_avg)

education_income$edu_avg_vs_income <- NA

education_income$edu_avg_vs_income[-fit_avg$na.action] <- fit_avg$residuals
qplot(education_income$edu_avg_vs_income)

fit_max <- lm(comb_z_max ~ ns(Median_Household_Income_2016,3) + ns(Med_HH_Income_Percent_of_State_Total_2016,2), 
              data = education_income)

summary(fit_max)

education_income$edu_max_vs_income <- NA

education_income$edu_max_vs_income[-fit_max$na.action] <- fit_max$residuals
qplot(education_income$edu_max_vs_income)

##Combined Data
dim(education_income)

one_data_set <-
  education_income %>% 
  select(FIPS,
         zipcode,
         math_avg, 
         math_max, 
         reading_avg, 
         reading_max,
         comb_z_avg, 
         comb_z_max,
         Median_Household_Income_2016,
         Med_HH_Income_Percent_of_State_Total_2016,
         edu_avg_vs_income, 
         edu_max_vs_income) %>% 
  left_join(presidential %>%
              select(FIPS, per_dem)) %>% 
  left_join(education_rural %>% 
              select(FIPS,
                     State,
                     County,
                     Rural_Urban,
                     Rural_Urban_2,
                     Percent_College)) %>%
  left_join(home_price %>% 
              select(zipcode,
                     listing_price_per_sf,
                     City,
                     Metro)) %>% 
  left_join(temperature) %>% 
  left_join(sunlight)

dim(one_data_set)

#Get county level listing price to fill in for missing zipcodes
home_price_county <- read_csv(file = "./data/County_MedianListingPricePerSqft_AllHomes.csv")
home_price_county$FIPS <- paste0(home_price_county$StateCodeFIPS, home_price_county$MunicipalCodeFIPS)
home_price_county <- 
  home_price_county %>% 
  mutate(listing_price_per_sf_county  = rowMeans(select(., contains("2017")))) %>% 
  select(FIPS, listing_price_per_sf_county)

one_data_set <- left_join(one_data_set, home_price_county)

one_data_set$listing_price_per_sf[is.na(one_data_set$listing_price_per_sf)] <- 
  one_data_set$listing_price_per_sf_county[is.na(one_data_set$listing_price_per_sf)] 

#Fill in some empty cities
temp <- 
  lea_id_key %>% 
  rename(zipcode = LZIP,
         city_edu = LCITY,
         state_edu = LSTATE) %>% 
  distinct(zipcode, .keep_all = TRUE) %>%
  select(zipcode, city_edu, state_edu)

one_data_set <- left_join(one_data_set, temp)

one_data_set$City[is.na(one_data_set$City)] <- 
  one_data_set$city_edu[is.na(one_data_set$City)] 


one_data_set$State[is.na(one_data_set$State)] <- 
  one_data_set$state_edu[is.na(one_data_set$State)] 



##Multiply Weights and Combine into 1 Metric
one_data_set_z <- 
  one_data_set %>% 
  distinct(zipcode, .keep_all = TRUE) %>% 
  mutate(avg_sunlight_z = (avg_sunlight-mean(avg_sunlight, 
                                             na.rm=T))/sd(avg_sunlight,
                                                          na.rm = T),
         days_degrees_from_perfect_z = -(days_degrees_from_perfect-mean(days_degrees_from_perfect, 
                                                                        na.rm=T))/sd(days_degrees_from_perfect,
                                                                                     na.rm = T),
         listing_price_per_sf_z = -(listing_price_per_sf-mean(listing_price_per_sf, 
                                                              na.rm=T)) / sd(listing_price_per_sf,
                                                                             na.rm = T),
         schools_avg_z = (comb_z_avg-mean(comb_z_avg,
                                         na.rm=T)) / sd(comb_z_avg,
                                                        na.rm = T),
         schools_max_z = (comb_z_max-mean(comb_z_max,
                                         na.rm=T)) / sd(comb_z_max,
                                                        na.rm = T),
         Percent_College_z = (Percent_College-mean(Percent_College,
                                                   na.rm=T)) / sd(Percent_College,
                                                                  na.rm = T),
         per_dem_z = (per_dem-mean(per_dem,
                                   na.rm=T)) / sd(per_dem,
                                                  na.rm = T)) %>% 
  mutate(combined_z_score = rowMeans(select(., ends_with("_z")),na.rm = T))

save(one_data_set_z, file = "one_data_set_zipcode_z.rdata")
