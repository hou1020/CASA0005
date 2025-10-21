library(sf)
library(tidyverse)

shape <- st_read('/Users/hou/Documents/CASA/CASA0005/week1/statsnz-territorial-authority-2018-generalised-SHP/territorial-authority-2018-generalised.shp')

summary(shape)

mycsv <- read_csv("/Users/hou/Documents/CASA/CASA0005/week1/2018_paid_employee.csv")  

shape_simple <- st_simplify(shape, dTolerance = 1000)
shape_simple %>%
 st_geometry() %>%
 plot()

# shape <- shape%>%
#  merge(.,
#        mycsv,
#        by.x="TA2018_V_1",
#        by.y="Territorial authority description")

shape <- merge(shape, mycsv,
               by.x="TA2018_V_1",
               by.y="Territorial authority description",
               all.x=TRUE)

class(shape)

shape_simple <- st_simplify(shape, dTolerance = 1000)

plot(shape_simple)