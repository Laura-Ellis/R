# Script to import data for public library e-book capstone project
# UMGC, DATA 670
# Prof. Jon McKeeby
# written January 2021 by Laura Ellis

# SETUP
# Set working directory
setwd("C:/Users/Laura/OneDrive/Libraries/Import_Files")
library(dplyr)
library(plyr)
library(readr)

# IMPORT DATASET
# Public Library Survey from Institute of Museum and Library Services
URLs <- read.table("C:/Users/Laura/OneDrive/Libraries/Import_Files/URLs.txt", quote="\"", comment.char="")

#** NOTE: 2014 file had wrong data. SAS version was downloaded and converted to csv.

pls_files <- list.files(pattern="*.csv", full.names=TRUE)
pls_files
df <- ldply(pls_files, read_csv)
dim(df) # 92850 x 262
# Assign year
df$SURVEY_YEAR <- as.factor(str_sub(df$F_POPLSA, start=-2))
summary(df$SURVEY_YEAR)
# Remove survey coding variables
df <- select(df, c(`STABR`,`FSCSKEY`,`LIBID`,`LIBNAME`,`ADDRESS`,`CITY`,`ZIP`,`CNTY`,`WEB_ADDR`,`C_RELATN`,
                   `C_LEGBAS`,`C_ADMIN`,`C_FSCS`,`GEOCODE`,`LSABOUND`,`POPU_LSA`,`POPU_UND`,`CENTLIB`,
                   `BRANLIB`,`BKMOB`,`MASTER`,`LIBRARIA`,`OTHPAID`,`TOTSTAFF`,`LOCGVT`,`STGVT`,`FEDGVT`,
                   `OTHINCM`,`TOTINCM`,`SALARIES`,`BENEFIT`,`STAFFEXP`,`PRMATEXP`,`ELMATEXP`,`OTHMATEX`,
                   `TOTEXPCO`,`OTHOPEXP`,`TOTOPEXP`,`LCAP_REV`,`SCAP_REV`,`FCAP_REV`,`OCAP_REV`,`CAP_REV`,
                   `CAPITAL`,`BKVOL`,`EBOOK`,`AUDIO`,`VIDEO`,`DB_LOC`,`DB_ST`,`DB_OTH`,`DATABASE`,`SUBSCRIP`,
                   `ESUBSCRP`,`HRS_OPEN`,`VISITS`,`REFERENC`,`REGBOR`,`TOTCIR`,`KIDCIRCL`,`LOANTO`,`LOANFM`,
                   `TOTPRO`,`KIDPRO`,`YAPRO`,`TOTATTEN`,`KIDATTEN`,`YAATTEN`,`GPTERMS`,`PITUSR`,`YR_SUB`,
                   `OBEREG`,`RSTATUS`,`STATSTRU`,`STATNAME`,`STATADDR`,`LONGITUD`,`LATITUDE`,`FIPSST`,`FIPSCO`,
                   `FIPSPLAC`,`CNTYPOP`,`LOCALE`,`CENTRACT`,`CENBLOCK`,`CDCODE`,`MAT_CENT`,`SURVEY_YEAR`))
dim(df) #92850 x 88


# IMPORT DATASET HIFLD
# Homeland Infrastructure Foundation-Level Data
# Number and type of schools by county
library(tidyverse)
library(httr)
library(jsonlite)
path <- "https://opendata.arcgis.com/datasets/0d7bedf9d582472e9ff7a6874589b545_0.geojson"
request <- GET(url = path)
request$status_code
response <- content(request, as = "text", encoding = "UTF-8")
schools_univ <- fromJSON(response, flatten = TRUE) %>% 
  data.frame()
schools_univ$type <- "College/University"

path <- "https://opendata.arcgis.com/datasets/284d5c00b0d046e18eddff4017927dd1_0.geojson"
request <- GET(url = path)
response <- content(request, as = "text", encoding = "UTF-8")
schools_supp <- fromJSON(response, flatten = TRUE) %>% 
  data.frame()
schools_supp$type <- "Supplemental College"

path <- "https://opendata.arcgis.com/datasets/0dfe37d2a68545a699b999804354dacf_0.geojson"
request <- GET(url = path)
response <- content(request, as = "text", encoding = "UTF-8")
schools_priv <- fromJSON(response, flatten = TRUE) %>% 
  data.frame()
schools_priv$type <- "Private School"

path <- "https://opendata.arcgis.com/datasets/87376bdb0cb3490cbda39935626f6604_0.geojson"
request <- GET(url = path)
response <- content(request, as = "text", encoding = "UTF-8")
schools_pub <- fromJSON(response, flatten = TRUE) %>% 
  data.frame()
schools_pub$type <- "Public School"

schools1 <- select(schools_univ, c('type', 'features.properties.COUNTY', 'features.properties.STATE', 'features.properties.COUNTYFIPS'))
schools2 <- select(schools_supp, c('type', 'features.properties.COUNTY', 'features.properties.STATE', 'features.properties.COUNTYFIPS'))
schools3 <- select(schools_priv, c('type', 'features.properties.COUNTY', 'features.properties.STATE', 'features.properties.COUNTYFIPS'))
schools4 <- select(schools_pub, c('type', 'features.properties.COUNTY', 'features.properties.STATE', 'features.properties.COUNTYFIPS'))

schools <- rbind(schools1, schools2, schools3, schools4)

# IMPORT FIPS CODES
# US Census Bureau
# https://www.census.gov/geographies/reference-files/2018/demo/popest/2018-fips.html
library(readxl)
temp <- tempfile()
download.file("https://www2.census.gov/programs-surveys/popest/geographies/2018/all-geocodes-v2018.xlsx",temp,mode="wb")
fips <- read_excel(path=temp, sheet=1)
colnames(fips) <- c("SUMMARY_LEVEL", "STATE_CODE", "COUNTY_CODE", "COUNTY_SUBDIV_CODE", "PLACE_CODE", "CITY_CODE", "NAME")
fips <- fips %>% slice(-c(1:4))
fips$SUMMARY_LEVEL <- as.numeric(fips$SUMMARY_LEVEL)
# SUMMARY_LEVEL 40 = STATE NAME
# SUMMARY_LEVEL 50 = COUNTY NAME
fips_state <- subset(fips, SUMMARY_LEVEL==40, select = c("STATE_CODE", "NAME"))
fips_county <- subset(fips, SUMMARY_LEVEL==50, select = c("STATE_CODE", "COUNTY_CODE", "NAME"))
fips_county$COUNTY_FIPS <- paste(fips_county$STATE_CODE,fips_county$COUNTY_CODE,sep='')
library(stringr)
fips_county$NAME_SHORT <- fips_county$NAME %>% str_replace(" County", "")



# IMPORT CENSUS DATA
# American Community Survey
# https://data.census.gov/cedsci/all?q=internet%20use

# Types of internet subscriptions
# https://data.census.gov/cedsci/table?q=internet&g=0100000US.050000&tid=ACSDT5Y2019.B28002&tp=true&hidePreview=true
x

# Get list of apis
apis <- listCensusApis()
View(apis)

# Poverty by County from Small Area and Income Poverty Estimates
saipe_county <- getCensus(
  name="timeseries/poverty/saipe",
  vars=c("NAME", "SAEMHI_PT", "SAEPOVALL_PT", "GEOID", "YEAR"),
  region="COUNTY",
  key="09da5b3090a049875130be75751e1c7910d57100")
head(saipe_county)

# Health Insurance from Small Area Health Insurance Estimates
## Number Insured
sahie_county <- getCensus(name="timeseries/healthins/sahie",
                                   vars=c("NIC_PT", "NUI_PT", "GEOID", "YEAR"),
                                   region="county",
                                   key="09da5b3090a049875130be75751e1c7910d57100")
head(sahie_county)
listCensusMetadata(name="timeseries/healthins/sahie",
                   type="variables")

# 
sahie_county <- getCensus(
  name="timeseries/healthins/sahie",
  vars=c("NAME", "IPRCAT", "IPR_DESC", "PCTUI_PT"),
  region="county",
  time="from 2008 to 2019", key="09da5b3090a049875130be75751e1c7910d57100")
head(sahie_county)
  
acs_income <- getCensus(
  name = "acs/acs1",
  vintage = 2017, 
  vars = c("NAME", "B19013_001E", "B19013_001EA", "B19013_001M", "B19013_001MA"), 
  region = "county")
head(acs_income)

listCensusMetadata(name="acs/acs1")

2019/acs/acs1
df2019 <- getCensus(
  name="2019/acs/acs1",
  vars=c("B01001_001E", "B15001_002E",
         "B17001_001E", "B22001_001E", "B23001_001E"),
  region="COUNTY",
  key="09da5b3090a049875130be75751e1c7910d57100")
df2019 <- as.data.frame(df2019)
head(df2019)

names[df2019] <- c("state1", "county1", "Population","Education", "Poverty", "SNAP", "Employment")


api.census.gov/data/2019/acs/acs1/subject?get=NAME,group(S0101)&for=us:1&key=YOUR_KEY_GOES_HERE

"GEO_ID", "S0101_C01_001E", 

df2019 <- getCensus(
  name="2019/acs/acs1/subject",
  vars=c("S0101_C01_001E", "S0101_C05_001E", "S1501_C02_001E", "S1701_C02_001E"),
  region="COUNTY",
  key="09da5b3090a049875130be75751e1c7910d57100")
  
df2019 <- as.data.frame(df2019)
head(df2019)

"Population", "Female_Population", "Poverty", 


df2019 <- getCensus(
  name="2019/acs/acs1?get=NAME,group(B01001)&for=us:1",
  vars=c("S0101_C01_001E", "S0101_C05_001E", "S1501_C02_001E", "S1701_C02_001E"),
  region="COUNTY",
  key="09da5b3090a049875130be75751e1c7910d57100")




# Health Insurance 
# https://www.census.gov/data-tools/demo/sahie/#/?s_year=2018,2017,2016,2015,2014,2013,2012,2011,2010,2009&map_yearSelector=2012
