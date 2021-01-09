#Load packages
library(readr)
library(rgdal)
library(sf)
library(tmap) 
library(tmaptools)
library(tidyverse)
library(readxl)
library(janitor)
library(dplyr)
library(stringr)
library(ggplot2)
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(sf)
library(highcharter)
library(tidyverse)
library(downloader)
library(rgdal)
library(sf)
library(ggplot2)
library(reshape2)
library(plotly)
library(raster)
library(downloader)
library(rgdal)

#Read in population data LSOA
pop_lsoa <- read_csv("ons-mye-LSOA11-custom-age-tool.csv",
                     locale = locale(encoding = "latin1"),
                     na = "n/a",
                     skip = 13)
class(pop_lsoa)
summary(pop_lsoa)
#We want all the rows - no need to select 
#Select columns
pop_london <- pop_lsoa[,c(2,4,3,6,7,8)]
#Rename Columns
colnames(pop_london)
names(pop_london)[names(pop_london) == "LSOA name"] <- "area_name"
pop_london
#Clean names
pop_london <- clean_names(pop_london)
#Making tidy data (in longer version)
#Pivot pop_london wider
pop_london <- pop_london%>%
  pivot_wider(
    id_cols = c(1,2),
    names_from = year,
    values_from = c(persons))
#Select pop columns again
pop_london <- pop_london[,c(1,2,6,7,8,9,10,11,12,13,14,15,16)]

#Read in retail floorspace data LSOA
retail_floor <- read_csv("FS_OA2.0.csv",
                         locale = locale(encoding = "latin1"))
class(retail_floor)
summary(retail_floor)
#Select London lsoa (rows)
retail_floor_londonall <- retail_floor[c(21057:25864),]
retail_floor_lonlsoa <- retail_floor_londonall %>% 
  filter(str_detect(`area_code`, "^E01"))
#Select Years (columns)
retail_floor_london_years <- retail_floor_lonlsoa[,c(2,3,8,9,10,11,12,13,14,15,16,17,18)]
#Rename Columns
colnames(retail_floor_london_years)
names(retail_floor_london_years)[names(retail_floor_london_years) == "Floorspace_2004 05_Total"] <- "r2004"
names(retail_floor_london_years)[names(retail_floor_london_years) == "Floorspace_2005 06_Total"] <- "r2005"
names(retail_floor_london_years)[names(retail_floor_london_years) == "Floorspace_2006 07_Total"] <- "r2006"
names(retail_floor_london_years)[names(retail_floor_london_years) == "Floorspace_2007 08_Total"] <- "r2007"
names(retail_floor_london_years)[names(retail_floor_london_years) == "Floorspace_2008 09_Total"] <- "r2008"
names(retail_floor_london_years)[names(retail_floor_london_years) == "Floorspace_2009 10_Total"] <- "r2009"
names(retail_floor_london_years)[names(retail_floor_london_years) == "Floorspace_2010 11_Total"] <- "r2010"
names(retail_floor_london_years)[names(retail_floor_london_years) == "Floorspace_2011 12_Total"] <- "r2011"
names(retail_floor_london_years)[names(retail_floor_london_years) == "Floorspace_2012 13_Total"] <- "r2012"
names(retail_floor_london_years)[names(retail_floor_london_years) == "Floorspace_2013 14_Total"] <- "r2013"
names(retail_floor_london_years)[names(retail_floor_london_years) == "Floorspace_2014 15_Total"] <- "r2014"
retail_floor_london_years
#Clean names
retail_floor_lon <- clean_names(retail_floor_london_years)
#Retail data already in tidy wider format

#Merge pop_london with retail_floor_lon (not the wide versions)
pop_retail_london <- merge(pop_london, retail_floor_lon, by = c("area_code","area_name"), all.x = TRUE, all.y = FALSE)
#Clean names
pop_retail_london <- clean_names(pop_retail_london)

#Compare retail floorspace per head (population) (density/concentration)
#Add column for retail floorspace per head (population)
pop_retail_london <- pop_retail_london %>% 
  mutate(rph2004= ((r2004*1000)/x2004)) %>% 
  mutate(rph2005= ((r2005*1000)/x2005)) %>% 
  mutate(rph2006= ((r2006*1000)/x2006)) %>% 
  mutate(rph2007= ((r2007*1000)/x2007)) %>% 
  mutate(rph2008= ((r2008*1000)/x2008)) %>% 
  mutate(rph2009= ((r2009*1000)/x2009)) %>% 
  mutate(rph2010= ((r2010*1000)/x2010)) %>% 
  mutate(rph2011= ((r2011*1000)/x2011)) %>% 
  mutate(rph2012= ((r2012*1000)/x2012)) %>% 
  mutate(rph2013= ((r2013*1000)/x2013)) %>% 
  mutate(rph2014= ((r2014*1000)/x2014))
rph_lon <- pop_retail_london[,c(1,2,25:35)]

#NOTE - have the data for population, retail and population and retail, now to make the maps
#Names of the relevant datasets
##Population = pop_london
##Retail floorspace = retail_floor_lon
##Population and retail = pop_retail_london
##retail per head = rph_lon

###MAPPING DATA
#Import London Boundary LSOA data (lb_lsoa) and Borough data (lb_borough)
lb_lsoa <- st_read(here::here("ESRI",
                              "LSOA_2011_London_gen_MHW.shp"))
lb_borough <- st_read(here::here("ESRI",
                                 "London_Borough_Excluding_MHW.shp"))
#Plot lb_lsoa and lb_borough using the qtm function
qtm(lb_lsoa)
summary(lb_lsoa)
qtm(lb_borough)
summary(lb_borough)

#Code for base map (run only if want osm base map)
lb_lsoa <- lb_lsoa %>% 
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "osm", zoom = NULL)
#include following code prior to mapping pop or retail code
tm_shape(lb_lsoa)+
  tm_rgb()+
  
  #Mapping retail per head rph_lon
  #Merge lb_lsoa data with rph_lon
  mergelbrph <- lb_lsoa%>%
  filter(str_detect(LSOA11CD, "^E01"))%>%
  merge(.,
        rph_lon, 
        by.x=c("LSOA11CD","LSOA11NM"), 
        by.y=c("area_code","area_name"),
        no.dups = TRUE)%>%
  distinct(.,LSOA11CD, 
           .keep_all = TRUE)
#Map mergelbrph
#Search for colour palette want to use
palette_explorer()
#Set mode to plotting
tmap_mode("plot")
#Define the map elements
rph_map <- tm_shape(mergelbrph)+
  #Toggle year of interest (data ranges from 2004 to 2014)
  tm_polygons("rph2014",
              style="jenks",
              palette="Set1",
              midpoint=NA,
              alpha = 1,
              border.alpha = 0) +
  tm_compass(position = c("left","bottom"),type = "arrow")+
  tm_scale_bar(position = c("left","bottom"))+
  tm_layout(legend.position = c("right", "bottom"), frame = FALSE)+
  #Layer borough outlines on top
  tm_shape(lb_borough)+
  tm_borders(col = "black",
             alpha = 0.4)
#Print map
rph_map
#Export map to PNG (toggle the year in the file name)
tmap_save(rph_map, filename = "rph_map2014_map.png")

###STATISTICAL ANALYSIS 
##Population = pop_london
##Retail floorspace = retail_floor_lon
##Population and retail = pop_retail_london
##retail per head = rph_lon
#Summary Statistics 
#Mean, median, mode, max, min, standard deviation  
install.packages('stargazer')
library(stargazer)
dfpop <- data.frame(pop_london)
cols <- c('2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014')
stargazer(
  dfpop, type = "text", 
  summary.stat = c("min", "p25", "median", "p75", "max", "mean", "sd")
)

dfret <- data.frame(retail_floor_lon)
cols <- c('2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014')
stargazer(
  dfret, type = "text", 
  summary.stat = c("min", "p25", "median", "p75", "max", "mean", "sd")
)

dfpopret <- data.frame(rph_lon)
stargazer(
  rph_lon, type = "text", 
  summary.stat = c("min", "p25", "median", "p75", "max", "mean", "sd")
)

#Plot line graph for population (borough)
pop_borough <- read_csv("population_borough_level.csv",
                        locale = locale(encoding = "latin1"),
                        na = "n/a",
                        skip = 4)
#Select London Boroughs (rows)
london_pop <- pop_borough[220:253,]
london_pop <- london_pop[1,]
#Select Years (columns)
london_pop <- london_pop[,c(1,2,35,33,31,29,27,25,23,21,19,17,15)]
#Clean names
london_pop <- clean_names(london_pop)
#Rename columns
colnames(london_pop)
names(london_pop)[names(london_pop) == "name"] <- "area_name"
names(london_pop)[names(london_pop) == "code"] <- "area_code"
names(london_pop)[names(london_pop) == "estmated_population_mid_2004"] <- "2004"
names(london_pop)[names(london_pop) == "estmated_population_mid_2005"] <- "2005"
names(london_pop)[names(london_pop) == "estmated_population_mid_2006"] <- "2006"
names(london_pop)[names(london_pop) == "estmated_population_mid_2007"] <- "2007"
names(london_pop)[names(london_pop) == "estmated_population_mid_2008"] <- "2008"
names(london_pop)[names(london_pop) == "estmated_population_mid_2009"] <- "2009"
names(london_pop)[names(london_pop) == "estmated_population_mid_2010"] <- "2010"
names(london_pop)[names(london_pop) == "estmated_population_mid_2011"] <- "2011"
names(london_pop)[names(london_pop) == "estmated_population_mid_2012"] <- "2012"
names(london_pop)[names(london_pop) == "estmated_population_mid_2013"] <- "2013"
names(london_pop)[names(london_pop) == "estmated_population_mid_2014"] <- "2014"
london_pop
#Pivot pop_london longer
london_pop <- london_pop%>% 
  pivot_longer(
    cols = 3:13,
    names_to = "year",
    values_to = "population"
  )
#Isolate the columns
london_pop <- london_pop[,c(3,4)]
#Create variable values
yearpop <- london_pop$year
population <- london_pop$population
#Plot the line graph
# Plot with main and axis titles
plot(yearpop, population, main = "London Population",
     xlab = "Year", ylab = "Population", frame = FALSE)

#Plot line graph for retail floorspace (borough)
#Read in data for retail (borough)
ret_borough <- read_csv("NDR_Business_Floorspace_tables.csv",
                        locale = locale(encoding = "latin1"),
                        na = "n/a",
                        skip = 8)
#Select London Boroughs (rows)
london_ret <- ret_borough[220:253,]
london_ret <- london_ret[1,]
#Select Years (columns)
london_ret <- london_ret[,c(4,5,10:20)]
#Clean names
london_ret <- clean_names(london_ret)
#Rename columns
colnames(london_ret)
names(london_ret)[names(london_ret) == "area"] <- "area_name"
names(london_ret)[names(london_ret) == "area_code5"] <- "area_code"
names(london_ret)[names(london_ret) == "x2004_05"] <- "2004"
names(london_ret)[names(london_ret) == "x2005_06"] <- "2005"
names(london_ret)[names(london_ret) == "x2006_07"] <- "2006"
names(london_ret)[names(london_ret) == "x2007_08"] <- "2007"
names(london_ret)[names(london_ret) == "x2008_09"] <- "2008"
names(london_ret)[names(london_ret) == "x2009_10"] <- "2009"
names(london_ret)[names(london_ret) == "x2010_11"] <- "2010"
names(london_ret)[names(london_ret) == "x2011_12"] <- "2011"
names(london_ret)[names(london_ret) == "x2012_13"] <- "2012"
names(london_ret)[names(london_ret) == "x2013_14"] <- "2013"
names(london_ret)[names(london_ret) == "x2014_15"] <- "2014"
london_ret
#Pivot pop_london longer
london_ret <- london_ret%>% 
  pivot_longer(
    cols = 3:13,
    names_to = "year",
    values_to = "retail_floor"
  )
#Isolate the columns
london_ret <- london_ret[,c(3,4)]
#Create variable values
yearret <- london_ret$year
ret <- london_ret$retail_floor
#Plot the line graph
# Plot with main and axis titles
plot(yearret, ret, main = "London Retail Floorspace",
     xlab = "Year", ylab = "Retail Floorspace", frame = FALSE)

#Plot a scatter graph for retail and population 
#Create variable values
x <- london_pop$population
y <- london_ret$retail_floor
# Plot with main and axis titles
# Change point shape (pch = 19) and remove frame.
plot(x, y, main = "Population v Retail",
     xlab = "Population", ylab = "Retail Floorspace",
     pch = 19, frame = FALSE)
# Add regression line
plot(x, y, main = "Population v Retail",
     xlab = "Population", ylab = "Retail Floorspace",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = pop_retail_london), col = "blue")

#Location Quotient 
#Read in all business floorspace (lsoa level)
allbus_floor <- read_csv("FS_OA1.0.CSV",
                         locale = locale(encoding = "latin1",))
#Select the rows and columns 
allbus_floor_londonall <- allbus_floor[c(23418:29207),]
allbus_floor_lonlsoa <- allbus_floor_londonall %>% 
  filter(str_detect(`area_code`, "^E01"))
allbus_floor_lonbor <- allbus_floor_lonlsoa[,c(2,3,8:18)]
#Rename columns 
colnames(allbus_floor_lonbor)
names(allbus_floor_lonbor)[names(allbus_floor_lonbor) == "Floorspace_2004-05_Total"] <- "2004"
names(allbus_floor_lonbor)[names(allbus_floor_lonbor) == "Floorspace_2005-06_Total"] <- "2005"
names(allbus_floor_lonbor)[names(allbus_floor_lonbor) == "Floorspace_2006-07_Total"] <- "2006"
names(allbus_floor_lonbor)[names(allbus_floor_lonbor) == "Floorspace_2007-08_Total"] <- "2007"
names(allbus_floor_lonbor)[names(allbus_floor_lonbor) == "Floorspace_2008-09_Total"] <- "2008"
names(allbus_floor_lonbor)[names(allbus_floor_lonbor) == "Floorspace_2009-10_Total"] <- "2009"
names(allbus_floor_lonbor)[names(allbus_floor_lonbor) == "Floorspace_2010-11_Total"] <- "2010"
names(allbus_floor_lonbor)[names(allbus_floor_lonbor) == "Floorspace_2011-12_Total"] <- "2011"
names(allbus_floor_lonbor)[names(allbus_floor_lonbor) == "Floorspace_2012-13_Total"] <- "2012"
names(allbus_floor_lonbor)[names(allbus_floor_lonbor) == "Floorspace_2013-14_Total"] <- "2013"
names(allbus_floor_lonbor)[names(allbus_floor_lonbor) == "Floorspace_2014-15_Total"] <- "2014"
allbus_floor_lonbor
#Clean names
allbus_floor_lonbor <- clean_names(allbus_floor_lonbor)
#Merge retail_floor_lon with allbus_floor_lonbor (not the wide versions)
allbus_v_ret <- merge(retail_floor_lon, allbus_floor_lonbor, by = c("area_code","area_name"), all.x = TRUE, all.y = FALSE)
#Make data all numeric
allbus_v_ret <- mutate(allbus_v_ret, across(everything(), ~replace_na(.x, 0)))
#Add total rows 
allbus_v_ret %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total")))
#Make data all numeric
allbus_v_ret <- mutate(allbus_v_ret, across(everything(), ~replace_na(.x, 0)))
#Set to show 2dp
options(digits = 2)
#Add columns for calculations
allbus_v_ret <- allbus_v_ret %>% 
  mutate(allbusvtotbus04= (x2004/sum(x2004))) %>% 
  mutate(allbusvtotbus09= (x2009/sum(x2009))) %>% 
  mutate(allbusvtotbus14= (x2014/sum(x2014))) %>% 
  mutate(retvtotret04= (r2004/sum(r2004))) %>% 
  mutate(retvtotret09= (r2009/sum(r2009))) %>% 
  mutate(retvtotret14= (r2014/sum(r2014))) 
#Add LQ columns
allbus_v_ret <- allbus_v_ret %>% 
  mutate(lq04= (allbusvtotbus04/retvtotret04)) %>% 
  mutate(lq09= (allbusvtotbus09/retvtotret09)) %>% 
  mutate(lq14= (allbusvtotbus14/retvtotret14)) 
#Select the percentages columns 
allbus_v_retlq <- allbus_v_ret[,c(1,2,31:33)] 
#Remove NaN values 
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
allbus_v_retlq[is.nan(allbus_v_retlq)] <- 0

#Map LQ for 2004,2009,2014
#Merge lb_lsoa data with allbus_v_retlq
mergelblq <- lb_lsoa%>%
  filter(str_detect(LSOA11CD, "^E01"))%>%
  merge(.,
        allbus_v_retlq, 
        by.x=c("LSOA11CD","LSOA11NM"), 
        by.y=c("area_code","area_name"),
        no.dups = TRUE)%>%
  distinct(.,LSOA11CD, 
           .keep_all = TRUE)
#Map mergelblq
#Search for colour palette want to use
palette_explorer()
#Set mode to plotting
tmap_mode("plot")
#Define the map elements
lq_map <- tm_shape(mergelblq)+
  #Toggle year of interest (data ranges from 2004 to 2014)
  tm_polygons("lq04",
              style="jenks",
              palette="Paired",
              midpoint=NA,
              alpha = 1,
              border.alpha = 0) +
  tm_compass(position = c("left","bottom"),type = "arrow")+
  tm_scale_bar(position = c("left","bottom"))+
  tm_layout(legend.position = c("right", "bottom"), frame = FALSE)+
  #Layer borough outlines on top
  tm_shape(lb_borough)+
  tm_borders(col = "black",
             alpha = 0.4)
#Print map
lq_map
#Export map to PNG (toggle the year in the file name)
tmap_save(lq_map, filename = "lq_map2004_map.png")

###Create Flow Chart
grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Calibri, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']
      tab7 [label = '@@7']
      tab8 [label = '@@8']
      tab9 [label = '@@9']
      tab10 [label = '@@10']
      
      # edge definitions with the node IDs
      tab1 -> tab2;
      tab2 -> tab3;
      tab3 -> tab4; 
      tab4 -> tab5;
      tab5 -> tab6;
      tab6 -> tab7;
      tab7 -> tab8;
      tab8 -> tab9;
      tab9 -> tab10
      }

      [1]: 'Start'
      [2]: 'Find Data'
      [3]: 'Population (LSOA and Borough), Retail Floorspace (LSOA and Borough), London Boundary (LSOA and Borough)'
      [4]: 'Import Data into R'
      [5]: 'Plot Population and Retail Floorspace Data into Graphs'
      [6]: 'Plot Population against Retail Floorspace'
      [7]: 'Map Retail Floorspace per Head (Population)'
      [8]: 'Calculate and Map Location Quotient'
      [9]: 'Deduce Observations'
      [10]: 'End'
      ")
