library(tidyverse)
library(sf)
library(tmap)

if(False){
?plot

#create some datasets, first a vector of 1-100 and 101-200
Data1 <- c(1:100)
Data2 <- c(101:200)
#Plot the data
plot(Data1, Data2, col="red")

#just for fun, create some more, this time some normally distributed
#vectors of 100 numbers
Data3 <- rnorm(100, mean = 53, sd=34)
Data4 <- rnorm(100, mean = 64, sd=14)
#plot
plot(Data3, Data4, col="blue")

df <- data.frame(Data1, Data2)
plot(df, col="green")

#show the first 10 and then last 10 rows of data in df...
df %>%
  head()
df %>%
  tail()

# data.frame[row,column]

df[1:10, 1]
df[5:15,]
df[c(2,3,6),2]
df[,1]

df <- df %>%
  dplyr::rename(column1 = Data1, column2=Data2)

df %>% 
  dplyr::select(column1)

df[["column1"]]
}

LondonDataOSK<- read.csv("ward-profiles-excel-version.csv", 
                           header = TRUE, 
                           sep = ",",  
                           encoding = "latin1")

#wang the data in straight from the web using read_csv, 
#skipping over the 'n/a' entries as you go...
LondonData <- read_csv("https://data.london.gov.uk/download/f33fb38c-cb37-48e3-8298-84c0d3cc5a6c/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a")
# LondonData <- read_csv("https://data.london.gov.uk/download/f33fb38c-cb37-48e3-8298-84c0d3cc5a6c/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", 
#                        locale = locale(encoding = "latin1"))

class(LondonData)
# or, if you have your old skool data
class(LondonDataOSK)

Datatypelist <- LondonData %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")
Datatypelist

# 需要XQuartz
# LondonData <- edit(LondonData)

summary(df)

LondonData%>%
  colnames()%>%
  # just look at the head, top5
  head()

library(dplyr)
# 提取行政区数据
LondonBoroughs<-LondonData[626:658,]
# 或者
LondonBoroughs<-LondonData%>%
  dplyr::slice(626:658)

# 提取女性预期寿命大于 90 岁的所有病房
Femalelifeexp<- LondonData %>% 
  filter(`Female life expectancy -2009-13`>90)

library(stringr)
LondonBoroughs<- LondonData %>% 
  filter(str_detect(`New code`, "^E09"))

LondonBoroughs$`Ward name`
# 或者
LondonBoroughs %>% 
  dplyr::select(`Ward name`) %>%
  print()

# 仅提取唯一行
LondonBoroughs<-LondonBoroughs %>%
  distinct()

#select columns 1,19,20 and 21
LondonBoroughs_manualcols<-LondonBoroughs[,c(1,19,20,21)]
# 或者
#select columns 1,19,20 and 21
LondonBoroughs_dplyrcols<-LondonBoroughs %>%
  dplyr::select(c(1,19,20,21))

LondonBoroughs_contains<-LondonBoroughs %>% 
  dplyr::select(contains("expectancy"), 
                contains("obese - 2011/12 to 2013/14"),
                contains("Ward name")) 

library(janitor)
LondonBoroughs <- LondonBoroughs %>%
  dplyr::rename(Borough=`Ward name`)%>%
  clean_names()

# 改为大写字母
# LondonBoroughs <- LondonBoroughs %>%
#   #here the ., means all data
#   clean_names(., case="big_camel")

Life_expectancy <- LondonBoroughs %>% 
  #new column with average of male and female life expectancy
  mutate(averagelifeexpectancy= (female_life_expectancy_2009_13 +
                                   male_life_expectancy_2009_13)/2)%>%
  #new column with normalised life expectancy
  mutate(normalisedlifeepectancy= averagelifeexpectancy /
           mean(averagelifeexpectancy))%>%
  #select only columns we want
  dplyr::select(new_code,
                borough,
                averagelifeexpectancy, 
                normalisedlifeepectancy)%>%
  #arrange in descending order
  #ascending is the default and would be
  #arrange(normalisedlifeepectancy)
  arrange(desc(normalisedlifeepectancy))

#top of data
slice_head(Life_expectancy, n=5)

#bottom of data
slice_tail(Life_expectancy,n=5)

Life_expectancy2 <- Life_expectancy %>%
  mutate(UKcompare = case_when(averagelifeexpectancy>81.16 ~ "above UK average",
                               TRUE ~ "below UK average"))
Life_expectancy2

# 高于全国平均水平的伦敦自治市的预期寿命范围
Life_expectancy2_group <- Life_expectancy2 %>%
  mutate(UKdiff = averagelifeexpectancy-81.16) %>%
  group_by(UKcompare)%>%
  summarise(range=max(UKdiff)-min(UKdiff), count=n(), Average=mean(UKdiff))
Life_expectancy2_group

# 将列四舍五入UKdiff到小数点后 0 位（不添加新列）
# 用于case_when()查找平均年龄等于或超过 81 岁的行政区，并创建一个新列，其中包含基于文本的数据，equal or above UK average by这些数据与 UKdiff 中创建的年份数据相结合。我们通过包中str_c()的函数来实现这一点stringr，该函数允许我们将两个或多个向量元素合并为一个字符向量。这里sep确定了如何分离这两个向量。
# 然后按UKcompare列分组
# 最后统计每组的数量。
Life_expectancy3 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(where(is.numeric), round, 3))%>%
  mutate(across(UKdiff, round, 0))%>%
  mutate(UKcompare = case_when(averagelifeexpectancy >= 81 ~ 
                                 str_c("equal or above UK average by",
                                       UKdiff, 
                                       "years", 
                                       sep=" "), 
                               TRUE ~ str_c("below UK average by",
                                            UKdiff,
                                            "years",
                                            sep=" ")))%>%
  group_by(UKcompare)%>%
  summarise(count=n())
Life_expectancy3

# 伦敦各行政区平均预期寿命与英国平均水平的差异
Life_expectancy4 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(is.numeric, round, 3))%>%
  mutate(across(UKdiff, round, 0))

plot(LondonBoroughs$male_life_expectancy_2009_13,
       LondonBoroughs$percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14)

library(plotly)
plot_ly(LondonBoroughs, 
        #data for x axis
        x = ~male_life_expectancy_2009_13, 
        #data for y axis
        y = ~percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14, 
        #attribute to display when hovering 
        text = ~borough, 
        type = "scatter", 
        mode = "markers")

