setwd("C:\\Users\\12408\\Documents ")
getwd()
library(tidyverse)
library(readxl)
library(reader)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(pander)
library(gt)

#Lawful Permanent Residents
lpr_genX <- read_excel("fy2019_table1.xlsx")
lpr_ByLastRegionResidenceX <- read_excel("fy2019_table2.xlsx")
lpr_ByRegionBirthX <- read_excel("fy2019_table3d.xlsx")
lpr_ByRegionResidence <- read_excel("fy2019_table4.xlsx")
lpr_ByCBSAREsidence <- read_excel("fy2019_table5.xlsx")
lpr_ByMajorClass <- read_excel("fy2019_table6.xlsx")
lpr_ByDetailledClass <- read_excel("fy2019_table7d.xlsx")
lpr_ByS.A.MS.O <- read_excel("fy2019_table8.xlsx")
lpr_ByBC_Demographics <- read_excel("fy2019_table9d..xlsx")
lpr_ByBC_RegionBirth <- read_excel("fy2019_table10d.xlsx")
lpr_ByBC_RegionResidence <- read_excel("fy2019_table11d.xlsx")
lpr_Orphans <- read_excel("fy2019_table12d.xlsx")

#Refugees and Asylees
ref_arrivals <- read_excel("fy2019_table13.xlsx")
ref_arr_ByNationality <- read_excel("fy2019_table14d.xlsx")
ref_arr_ByRelation_S.A.MS <- read_excel("fy2019_table15d.xlsx")
ref_asyl_grantedAD <- read_excel("fy2019_table16.xlsx")
ref_asyl_grantedA_ByNationality <- read_excel("fy2019_table17d.xlsx")
ref_asyl_grantedA_ByRelation_S.A.MS <- read_excel("fy2019_table18d.xlsx")
ref_asyl_grantedD_ByNationality <- read_excel("fy2019_table19d.xlsx")

#Naturalizations
nat_gen <- read_excel("fy2019_table20.xlsx")
nat_ByRegionBirth <- read_excel("fy2019_table21d.xlsx")
nat_ByRegionResidence <- read_excel("fy2019_table22.xlsx")
nat_ByCBSAResidence <- read_excel("fy2019_table23.xlsx")
nat_ByS.A.MS.O <- read_excel("fy2019_table24.xlsx")

#Nonimmigrants
nonimg_ByClass <- read_excel("fy2019_table25d.xlsx")
nonimg_ByRegionCitizenship <- read_excel("fy2019_table26.xlsx")
nonimg_ByRegionResidence <- read_excel("fy2019_table27d.xlsx")
nonimg_BySelCat_RegionCit <- read_excel("fy2019_table28d.xlsx")
nonimg_BySelCat_A.S <- read_excel("fy2019_table29.xlsx")
nonimg_BySelCat_Destination <- read_excel("fy2019_table30.xlsx")
nonimg_BySelCat_Month <- read_excel("fy2019_table31.xlsx")
nonimg_Work_ByRegionCit <- read_excel("fy2019_table32d.xlsx")

#Enforcement and "Aliens"
aliens_appreh <- aliens_gen <- read_excel("fy2019_table33.xlsx")
aliens_appreh_ByRegionNat  <- read_excel("fy2019_table34d.xlsx")
aliens_appreh_ByJuris <- read_excel("fy2019_table35.xlsx")
aliens_inadmin <- read_excel("fy2019_table36.xlsx")
aliens_inadmin_ByRegionNat <- read_excel("fy2019_table37d.xlsx")
aliens_inadmin_ByFieldOffice <- read_excel("fy2019_table38.xlsx")
aliens_remov <- read_excel("fy2019_table39.xlsx")
aliens_remov_ByRegionNat <- read_excel("fy2019_table40d.xlsx")
aliens_remov_byCrimS_RegionNat <- read_excel("fy2019_table41d.xlsx")
#########################################################Pull ing the ones I want ------------------------------------------
lpr_genX <- read_excel("fy2019_table1.xlsx")
lpr_ByLastRegionResidenceX <- read_excel("fy2019_table2.xlsx")
lpr_ByRegionBirthX <- read_excel("fy2019_table3d.xlsx")
lpr_ByRegionResidence <- read_excel("fy2019_table4.xlsx")
lpr_ByCBSAREsidence <- read_excel("fy2019_table5.xlsx")
lpr_ByMajorClass <- read_excel("fy2019_table6.xlsx")
####
ref_arrivals <- read_excel("fy2019_table13.xlsx")
ref_arr_ByNationality <- read_excel("fy2019_table14d.xlsx")
ref_asyl_grantedAD <- read_excel("fy2019_table16.xlsx")
ref_asyl_grantedA_ByNationality <- read_excel("fy2019_table17d.xlsx")
ref_asyl_grantedD_ByNationality <- read_excel("fy2019_table19d.xlsx")
####
nat_gen <- read_excel("fy2019_table20.xlsx")
nat_ByRegionBirth <- read_excel("fy2019_table21d.xlsx")
nat_ByRegionResidence <- read_excel("fy2019_table22.xlsx")
nat_ByCBSAResidence <- read_excel("fy2019_table23.xlsx")
####
nonimg_ByClass <- read_excel("fy2019_table25d.xlsx")
nonimg_ByRegionCitizenship <- read_excel("fy2019_table26.xlsx")
nonimg_ByRegionResidence <- read_excel("fy2019_table27d.xlsx")
###
aliens_appreh <- aliens_gen <- read_excel("fy2019_table33.xlsx")
aliens_appreh_ByRegionNat  <- read_excel("fy2019_table34d.xlsx")
aliens_appreh_ByJuris <- read_excel("fy2019_table35.xlsx")
aliens_inadmin <- read_excel("fy2019_table36.xlsx")
aliens_inadmin_ByRegionNat <- read_excel("fy2019_table37d.xlsx")
aliens_inadmin_ByFieldOffice <- read_excel("fy2019_table38.xlsx")
aliens_remov <- read_excel("fy2019_table39.xlsx")
aliens_remov_ByRegionNat <- read_excel("fy2019_table40d.xlsx")
aliens_remov_byCrimS_RegionNat <- read_excel("fy2019_table41d.xlsx")
#-------------------------------------------------------------------------------Putting then in lists-------------------

lpr_genX <- read_excel("fy2019_table1.xlsx")
lpr1 <- lpr_genX
lpr1 <- lpr1[-c(1:33,54,55), -c(1:6)]
colnames(lpr1) <- c("Year","People Obtaining LPR Status")
lpr1$`People Obtaining LPR Status` <- as.numeric(lpr1$`People Obtaining LPR Status`)
LPR_years <- lpr1
ggplot(data=LPR_years, aes(x=Year,y=`People Obtaining LPR Status`)) + 
  geom_point(color="yellow", size=3) +
  xlab("Year") +
  ylab("People Obtaining LPR Status") +
  ggtitle("PERSONS OBTAINING LAWFUL PERMANENT RESIDENT STATUS: FISCAL YEARS 2000 TO 2019") +
  theme_dark() 
  
  
  

lpr_ByLastRegionResidenceX <- read_excel("fy2019_table2.xlsx")
lpr2x <- lpr_ByLastRegionResidenceX
lpr2x <- lpr2x[-c(1:2,92:115), -c(2:20)]
names <- lpr2x$`Table 2.`
lpr2x <- lpr2x[,-c(1)]
lpr2x <- t(lpr2x)
colnames(lpr2x) <- names
colnames(lpr2x)[1] <- "Year"
lpr2x <- lpr2x[,-c(4,18,62)]
colnames(lpr2x)[4] <- "Austria"
colnames(lpr2x)[5] <- "Hungary"
colnames(lpr2x)[17] <- "Norway"
colnames(lpr2x)[18] <- "Sweden"
lpr2x[lpr2x=="-"]<-0

lpr_ByRegionBirthX <- read_excel("fy2019_table3d.xlsx")
lpr3x <- lpr_ByRegionBirthX
lpr3x <- lpr3x[-c(1:2,219:224),]
names3 <- lpr3x$`Table 3.`
lpr3x <- lpr3x[,-c(1)]
lpr3x <- t(lpr3x)
colnames(lpr3x) <- names3
colnames(lpr3x)[1] <- "Year"
lpr3x <- lpr3x[,-c(2,11)]
lpr3x[lpr3x=="X"]<-"NA"
lpr3x[lpr3x=="D"]<-"witheld"
lpr3x[lpr3x=="-"]<-0

lpr_ByRegionResidence <- read_excel("fy2019_table4.xlsx")
lpr4x <- lpr_ByRegionResidence
lpr4x <- lpr4x[-c(1,2,60:62), ]
names1 <- lpr4x$`Table 4.`
lpr4x <- lpr4x[,-c(1)]
lpr4x <- t(lpr4x)
colnames(lpr4x) <- names1
colnames(lpr4x)[1] <- "Year"
colnames(lpr4x)[56] <- "Other"
lpr4x[lpr4x=="-"]<-0
#########


ref_arrivals <- read_excel("fy2019_table13.xlsx")
ref1 <- ref_arrivals
colnames(ref1) <- c("Year","Refugee Arrivals")
ref1$`Refugee Arrivals` <- as.numeric(ref1$`Refugee Arrivals`)
ref1 <- ref1[-c(1:23,44,45), ]

ref_arr_ByNationality <- read_excel("fy2019_table14d.xlsx")
ref2x <- ref_arr_ByNationality
ref2x <- ref2x[-c(1,2,4,10,76:82), ]
names_ref2x <- ref2x$`Table 14.`
ref2x <- ref2x[,-c(1)]
ref2x <- t(ref2x)
colnames(ref2x) <- names_ref2x
colnames(ref2x)[1] <- "Year"
colnames(ref2x)[70] <- "All Other Countries"
colnames(ref2x)[71] <- "Unknown"
ref2x[ref2x=="X"]<-"NA"
ref2x[ref2x=="D"]<-"witheld"
ref2x[ref2x=="-"]<-0


ref_asyl_grantedAD <- read_excel("fy2019_table16.xlsx")
ref3 <- ref_asyl_grantedAD
colnames(ref3) <- c("Year","Total Asylums Granted","Asylums Granted Affirmatvely","Asylums Granted Defensively")
ref3$`Total Asylums Granted` <- as.numeric(ref3$`Total Asylums Granted`)
ref3$`Asylums Granted Affirmatvely` <- as.numeric(ref3$`Asylums Granted Affirmatvely`)
ref3$`Asylums Granted Defensively` <- as.numeric(ref3$`Asylums Granted Defensively`)
ref3 <- ref3[-c(1:13,34),]

ref_asyl_grantedA_ByNationality <- read_excel("fy2019_table17d.xlsx")
ref4x <- ref_asyl_grantedA_ByNationality
ref4x <- ref4x[-c(1,2,4,13,121:125), ]
names_ref4x <- ref4x$`Table 17.`
ref4x <- ref4x[,-c(1)]
ref4x <- t(ref4x)
colnames(ref4x) <- names_ref4x
colnames(ref4x)[1] <- "Year"
colnames(ref4x)[115] <- "All Other Countries"
ref4x[ref4x=="X"]<-"NA"
ref4x[ref4x=="D"]<-"witheld"
ref4x[ref4x=="-"]<-0

ref_asyl_grantedD_ByNationality <- read_excel("fy2019_table19d.xlsx")
ref5x <- ref_asyl_grantedD_ByNationality
ref5x <- ref5x[-c(1,2,4,13,104:109), ]
names_ref5x <- ref5x$`Table 19.`
ref5x <- ref5x[,-c(1)]
ref5x <- t(ref5x)
colnames(ref5x) <- names_ref5x
colnames(ref5x)[1] <- "Year"
colnames(ref5x)[98] <- "All Other Countries"
colnames(ref5x)[99] <- "Unknown"
ref5x[ref5x=="X"]<-"NA"
ref5x[ref5x=="D"]<-"witheld"
ref5x[ref5x=="-"]<-0

REF <- merge(ref1, ref3, by = "Year")
REF <- REF%>% dplyr::arrange(Year)
ggplot(data=REF, aes(x=Year,y=`Refugee Arrivals`)) + 
  geom_point(color="yellow", size=3) +
  xlab("Year") +
  ylab("Refugee Arrivals") +
  ggtitle("REFUGEE ARRIVALS: FISCAL YEARS 2000 TO 2019") +
  theme_dark() 

ggplot(data=REF, aes(x=Year,y=`Total Asylums Granted`)) + 
  geom_point(color="yellow", size=3) +
  xlab("Year") +
  ylab("Total Asylums Granted") +
  ggtitle("ASYLUM STATUS ARRIVALS: FISCAL YEARS 2000 TO 2019") +
  theme_dark()
########

nat_gen <- read_excel("fy2019_table20.xlsx")
nat1 <- nat_gen
colnames(nat1) <- c("Year","Petitions Filled"," Total Persons Naturalized","Civilians Naturalized","Military Naturalized", "Not Reported","Petitions Denied")
nat1 <- nat1[-c(1:98,119:124),]

nat_ByRegionBirth <- read_excel("fy2019_table21d.xlsx")
nat2x <- nat_ByRegionBirth
nat2x <- nat2x[-c(1,2,4,13,216:222), ]
names_nat2x <- nat2x$`Table 21.`
nat2x <- nat2x[,-c(1)]
nat2x <- t(nat2x)
colnames(nat2x) <- names_nat2x
colnames(nat2x)[1] <- "Year"
colnames(nat2x)[98] <- "All Other Countries"
nat2x[nat2x=="X"]<-"NA"
nat2x[nat2x=="D"]<-"witheld"
nat2x[nat2x=="-"]<-0

nat_ByRegionResidence <- read_excel("fy2019_table22.xlsx")
nat3x <- nat_ByRegionResidence
nat3x <- nat3[-c(1,2,60:62), ]
names2 <- nat3x$`Table 22.`
nat3x <- nat3x[,-c(1)]
nat3x <- t(nat3x)
colnames(nat3x) <- names2
colnames(nat3x)[1] <- "Year"
colnames(lpr4x)[56] <- "Other"

########
nonimg_ByRegionCitizenship <- read_excel("fy2019_table26.xlsx")
nim1x <- nonimg_ByRegionCitizenship
nim1x <- nim1x[-c(1,2,4,13,210:225), ]
names_nim1x <- nim1x$`Table 26.`
nim1x <- nim1x[,-c(1)]
nim1x <- t(nim1x)
colnames(nim1x) <- names_nim1x
colnames(nim1x)[1] <- "Year"
nim1x[nim1x=="X"]<-"NA"
nim1x[nim1x=="D"]<-"witheld"
nim1x[nim1x=="-"]<-0

nonimg_ByRegionResidence <- read_excel("fy2019_table27d.xlsx")
nim2x <- nonimg_ByRegionResidence
nim2x <- nim2x[-c(1,2,4,13,255:264), ]
names_nim2x <- nim2x$`Table 27.`
nim2x <- nim2x[,-c(1)]
nim2x <- t(nim2x)
colnames(nim2x) <- names_nim2x
colnames(nim2x)[1] <- "Year"
nim2x[nim2x=="X"]<-"NA"
nim2x[nim2x=="D"]<-"witheld"
nim2x[1,1]=2010


########


aliens_appreh <- aliens_gen <- read_excel("fy2019_table33.xlsx")
enf1 <- aliens_appreh
colnames(enf1) <- c("X","Y","Year","Aliens Apprehended")
enf1 <- enf1[-c(1:22,43:65), -c(1,2)]
enf1$Year[9]=2008
enf1$Year[10]=2009
enf1$Year[17]=2016


aliens_appreh_ByRegionNat  <- read_excel("fy2019_table34d.xlsx")
enf2x <- aliens_appreh_ByRegionNat
enf2x <- enf2x[-c(1,2,4,13,180:187), ]
names_enf2x <- enf2x$`Table 34.`
enf2x <- enf2x[,-c(1)]
enf2x <- t(enf2x)
colnames(enf2x) <- names_enf2x
colnames(enf2x)[1] <- "Year"
enf2x[1,1]=2010
enf2x[8,1]=2016
colnames(enf2x)[174] <- "All Other Countries"
enf2x[enf2x=="X"]<-"NA"
enf2x[enf2x=="D"]<-"witheld"
enf2x[enf2x=="-"]<-0

aliens_inadmin <- read_excel("fy2019_table36.xlsx")
enf4 <- aliens_inadmin
colnames(enf4) <- c("Year","Aliens Determined Inadmissible")
enf4 <- enf4[-c(1:3,19,20),]
enf4.new <- data.frame(c(2000, 2001, 2002, 2003, 2004),c("NA", "NA", "NA", "NA","NA"))
colnames(enf4.new) <- c("Year","Aliens Determined Inadmissible")
enf4 <- rbind(enf4,enf4.new)
enf4 <- enf4 %>% dplyr::arrange(Year)

aliens_inadmin_ByRegionNat <- read_excel("fy2019_table37d.xlsx")
enf5x <- aliens_inadmin_ByRegionNat
enf5x <- enf5x[-c(1,2,4,13,197:201), ]
names_enf5x <- enf5x$`Table 37.`
enf5x <- enf5x[,-c(1)]
enf5x <- t(enf5x)
colnames(enf5x) <- names_enf5x
colnames(enf5x)[1] <- "Year"
colnames(enf5x)[191] <- "All Other Countries"
enf5x[enf5x=="D"]<-"witheld"
enf5x[enf5x=="-"]<-0

aliens_remov <- read_excel("fy2019_table39.xlsx")
enf7 <-aliens_remov
colnames(enf7) <- c("Year","Aliens Removals","Aliens Returned")
enf7 <- enf7[-c(1:111,132:137),]
enf7$Year[17]=2016

aliens_remov_ByRegionNat <- read_excel("fy2019_table40d.xlsx")
enf8x <- aliens_remov_ByRegionNat
enf8x <- enf8x[-c(1,2,4,13,188:193), ]
names_enf8x <- enf8x$`Table 40.`
enf8x <- enf8x[,-c(1)]
enf8x <- t(enf8x)
colnames(enf8x) <- names_enf8x
colnames(enf8x)[1] <- "Year"
colnames(enf8x)[182] <- "All Other Countries"
enf8x[enf8x=="D"]<-"witheld"
enf8x[enf8x=="-"]<-0
enf8x[7,1]=2016

aliens_remov_byCrimS_RegionNat <- read_excel("fy2019_table41d.xlsx")
enf9 <- aliens_remov_byCrimS_RegionNat

enf9_2010 <- enf9[1:4]
enf9_2010 <- enf9_2010[-c(1:5,14,176:181), ]
colnames(enf9_2010) <- c("Region/Country","Total Alien Removals","Criminal Alien Removed","Non-Criminal Alien Removed")
enf9_2010[enf9_2010=="D"]<-"witheld"
enf9_2010[enf9_2010=="-"]<-"0"
enf9_2010[168,1] <- "All Other Countries"


enf9_2011 <- enf9[1:7]
enf9_2011 <- enf9_2011[-c(1:5,14,176:181),-c(2:4)]
colnames(enf9_2011) <- c("Region/Country","Total Alien Removals","Criminal Alien Removed","Non-Criminal Alien Removed")
enf9_2011[enf9_2011=="D"]<-"witheld"
enf9_2011[enf9_2011=="-"]<-"0"
enf9_2011[168,1] <- "All Other Countries"

enf9_2012 <- enf9[1:10]
enf9_2012 <- enf9_2012[-c(1:5,14,176:181),-c(2:7)]
colnames(enf9_2012) <- c("Region/Country","Total Alien Removals","Criminal Alien Removed","Non-Criminal Alien Removed")
enf9_2012[enf9_2012=="D"]<-"witheld"
enf9_2012[enf9_2012=="-"]<-"0"
enf9_2012[168,1] <- "All Other Countries"

enf9_2013 <- enf9[1:13]
enf9_2013 <- enf9_2013[-c(1:5,14,176:181),-c(2:10)]
colnames(enf9_2013) <- c("Region/Country","Total Alien Removals","Criminal Alien Removed","Non-Criminal Alien Removed")
enf9_2013[enf9_2013=="D"]<-"witheld"
enf9_2013[enf9_2013=="-"]<-"0"
enf9_2013[168,1] <- "All Other Countries"

enf9_2014 <- enf9[1:16]
enf9_2014 <- enf9_2014[-c(1:5,14,176:181),-c(2:13)]
colnames(enf9_2014) <- c("Region/Country","Total Alien Removals","Criminal Alien Removed","Non-Criminal Alien Removed")
enf9_2014[enf9_2014=="D"]<-"witheld"
enf9_2014[enf9_2014=="-"]<-"0"
enf9_2014[168,1] <- "All Other Countries"

enf9_2015 <- enf9[1:19]
enf9_2015 <- enf9_2015[-c(1:5,14,176:181),-c(2:16)]
colnames(enf9_2015) <- c("Region/Country","Total Alien Removals","Criminal Alien Removed","Non-Criminal Alien Removed")
enf9_2015[enf9_2015=="D"]<-"witheld"
enf9_2015[enf9_2015=="-"]<-"0"
enf9_2015[168,1] <- "All Other Countries"


enf9_2016 <- enf9[1:22]
enf9_2016 <- enf9_2016[-c(1:5,14,176:181),-c(2:19)]
colnames(enf9_2016) <- c("Region/Country","Total Alien Removals","Criminal Alien Removed","Non-Criminal Alien Removed")
enf9_2016[enf9_2016=="D"]<-"witheld"
enf9_2016[enf9_2016=="-"]<-"0"
enf9_2016[168,1] <- "All Other Countries"

enf9_2017 <- enf9[1:25]
enf9_2017 <- enf9_2017[-c(1:5,14,176:181),-c(2:21)]
colnames(enf9_2017) <- c("Region/Country","Total Alien Removals","Criminal Alien Removed","Non-Criminal Alien Removed")
enf9_2017[enf9_2017=="D"]<-"witheld"
enf9_2017[enf9_2017=="-"]<-"0"
enf9_2017[168,1] <- "All Other Countries"

enf9_2018 <- enf9[1:28]
enf9_2018 <- enf9_2018[-c(1:5,14,176:181),-c(2:24)]
colnames(enf9_2018) <- c("Region/Country","Total Alien Removals","Criminal Alien Removed","Non-Criminal Alien Removed")
enf9_2018[enf9_2018=="D"]<-"witheld"
enf9_2018[enf9_2018=="-"]<-"0"
enf9_2018[168,1] <- "All Other Countries"


enf9_2019 <- enf9[1:31]
enf9_2019 <- enf9_2019[-c(1:5,14,176:181),-c(2:27)]
colnames(enf9_2011) <- c("Region/Country","Total Alien Removals","Criminal Alien Removed","Non-Criminal Alien Removed")
enf9_2019[enf9_2019=="D"]<-"witheld"
enf9_2019[enf9_2019=="-"]<-"0"
enf9_2019[168,1] <- "All Other Countries"



ENF_years <- merge(enf1, enf4,by = "Year")
ENF_years <- merge(ENF_years, enf7, by = "Year")
ENF_years$`Aliens Apprehended` <- as.numeric(ENF_years$`Aliens Apprehended`)
ENF_years$`Aliens Determined Inadmissible` <- as.numeric(ENF_years$`Aliens Determined Inadmissible`)
ENF_years$`Aliens Removals` <- as.numeric(ENF_years$`Aliens Removals`)
ENF_years$`Aliens Returned` <- as.numeric(ENF_years$`Aliens Returned`)

ggplot(data=ENF_years, aes(x=Year,y=`Aliens Apprehended`)) + 
  geom_point(color="yellow", size=3) +
  xlab("Year") +
  ylab("Total") +
  ggtitle("ALIENS APPREHENDED: FISCAL YEARS 2000 TO 2019") +
  theme_dark() 
ggplot(data=ENF_yeat, aes(x=Year,y=`Aliens Determined Inadmissible`)) + 
  geom_point(color="yellow", size=3) +
  xlab("Year") +
  ylab("Total") +
  ggtitle("ALIENS DETERMINED INADMISSIBLE: FISCAL YEARS 2000 TO 2019") +
  theme_dark() 
ggplot(data=ENF, aes(x=Year,y=`Aliens Removals`)) + 
  geom_point(color="yellow", size=3) +
  xlab("Year") +
  ylab("Total") +
  ggtitle("ALIEN REMOVALS: FISCAL YEARS 2000 TO 2019") +
  theme_dark() 
ggplot(data=ENF, aes(x=Year,y=`Aliens Returned`)) + 
  geom_point(color="yellow", size=3) +
  xlab("Year") +
  ylab("Total") +
  ggtitle("ALIEN RETURNS: FISCAL YEARS 2000 TO 2019") +
  theme_dark() 
