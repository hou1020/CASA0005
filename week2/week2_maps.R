#Load Packages (ignore any error messages about being built under a 
#different R version):
library(tmap)
library(tmaptools)
library(sf)
library(stringr)
library(dplyr)
library(janitor)

# this will take a few minutes
# geojson in local folder
# EW <- st_read(here::here("prac2_data",
#                          "LAD_Dec_2015_FCB_GB.geojson"))
# shapefile in local folder
EW <- st_read(here::here("week2","LAD_Dec_2015_FCB_GB_2022_-1836983239597816196",
                         "LAD_Dec_2015_FCB_GB.shp"))

# 静态
# tmap_mode("plot")

LondonMap<- EW %>%
  filter(str_detect(lad15cd, "^E09"))
#plot it using the qtm function
qtm(LondonMap)

LondonData <- clean_names(LondonData)

#EW is the data we read in straight from the web
BoroughDataMap <- EW %>%
  clean_names()%>%
  # the . here just means use the data already loaded
  filter(str_detect(lad15cd, "^E09"))%>%
  merge(.,
        LondonData, 
        by.x="lad15cd", 
        by.y="new_code",
        no.dups = TRUE)%>%
  distinct(.,lad15cd,
           .keep_all = TRUE)

BoroughDataMap2 <- EW %>% 
  clean_names() %>%
  filter(str_detect(lad15cd, "^E09"))%>%
  left_join(., 
            LondonData,
            by = c("lad15cd" = "new_code"))

tmap_mode("plot")
qtm(BoroughDataMap, 
    fill = "rate_of_job_seekers_allowance_jsa_claimants_2015")

# 添加底图
library(OpenStreetMap)
tmaplondon <- BoroughDataMap %>%
  # st_bbox gives the bounding x and y coordinates 
  st_bbox(.) %>% 
  #note type="osm" gives error atm - issue raised on github: https://github.com/r-tmap/tmaptools/issues/41
  tmaptools::read_osm(., type = "esri", zoom = NULL)

tmap_mode("plot")
tm_shape(tmaplondon)+
  # add basemap as Red Green Blue raster
  tm_rgb()+
  # add sf data
  tm_shape(BoroughDataMap) + 
  # add polygon layer
  tm_polygons(fill="rate_of_job_seekers_allowance_jsa_claimants_2015",
              fill.scale= tm_scale_intervals(values="brewer.bu_pu",
                                             style="jenks"),
              fill_alpha=0.5,
              fill.legend = tm_legend(title = "rate of claimants 2015", 
                                      size = 0.8))+
  tm_compass(type = "arrow", position = c("left", "bottom")) + 
  tm_scalebar(position = c("left", "bottom"))+
  tm_title("Job seekers' Allowance Claimants", 
           size = 2,
           position = c("center", "top"))

BoroughDataMap84 <- BoroughDataMap %>%
  st_transform(.,4326)

tmap_mode("view")

tm_shape(BoroughDataMap84) + 
  # add polygon layer
  tm_polygons(fill="rate_of_job_seekers_allowance_jsa_claimants_2015",
              fill.scale= tm_scale_intervals(values="brewer.bu_pu",
                                             style="jenks"),
              fill_alpha=0.5,
              fill.legend = tm_legend(title = "Job seekers' Allowance Claimants", 
                                      size = 0.8))+
  tm_basemap(server = "OpenStreetMap") +
  tm_compass(type = "arrow", position = c("left", "bottom")) + 
  tm_scalebar(position = c("left", "bottom"))+
  tm_title("Job seekers' Allowance Claimants", 
           size = 2,
           position = c("center", "top"))

# 合并
Life_expectancy4map <- EW %>%
  inner_join(., 
             Life_expectancy4,
             by = c("lad15cd" = "new_code"))%>%
  distinct(.,lad15cd, 
           .keep_all = TRUE)

tmap_mode("plot")

tm_shape(tmaplondon)+
  # add basemap as Red Green Blue raster
  tm_rgb()+
  # add sf data
  tm_shape(Life_expectancy4map) + 
  # add polygon layer
  tm_polygons(fill="UKdiff",
              fill.scale= tm_scale_intervals(values="brewer.bu_pu",
                                             style="jenks"),
              fill_alpha=0.5,
              fill.legend = tm_legend(title = "Number of years", 
                                      size = 0.8))+
  tm_compass(type = "arrow", position = c("left", "bottom")) + 
  tm_scalebar(position = c("left", "bottom"))+
  tm_title("Difference in life expectancy", 
           size = 2,
           position = c("center", "top"))
