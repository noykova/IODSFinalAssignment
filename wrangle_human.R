#Here is a Data wrangling of human data
#The differences comparing to earlier data modifications are 
#separated by using capital letters. 
# The data are used for analysis in Final assignment exercise.

# Author: Neli Noykova
# e-mail: nnoykova'at'yahoo.com
# Date: 07.03.2017

#Summary of changes comparing data wringling for Week 5:
#1. Labour Force Participation Rate for both male and female 
#are not included in the analysis for this assignment
#2. Populations with secondary education are taken separately for
# male and females, as it was in the original data
#3. The average of (Math.Mor + Ado.birth)/2 = FRHI is 
#taken instead of individual columns Math.Mor and Ado.birth. 
#4. Edu.mean is taken instead edu.exp. 

# I would like to add more variables (for example related to Gender Development Index) 
# and investigate them, but I have noticed 
# that on the original publishing site it is almost impossible for me to do it. 
# All other data are with different dimensions, for different years, etc. 
# I would need more information, butr the time is very limited. 

#This analysis ends up with 155 observations of 7 vaiables. 


#The same two human datasets are loaded. 
# The data are joined on "country" column as before. 

library(dplyr)

#HD - HUMAN DEVELOPMENT DATA, USED FOR CALCULATING HUMAN DECELOPMENT INDEX

hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)

# GII - GENDER INEQUALITY DATA, USED FOR CALCULATION OF GENDER INEQUALITY INDEX. 
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

# look at the dimension, structure and column names of the data 
#195 observations of 8 variables 
dim(hd)
str(hd)
colnames(hd)
summary(hd)

#gii - data
#195 observations of 10 variables
dim(gii)
str(gii)
colnames(gii)
summary(gii)

# change the column names of hd:
colnames(hd)[1] <- "HDIrank"
colnames(hd)[2] <- "Country"
colnames(hd)[3] <- "HDI"
colnames(hd)[4] <- "Life.Exp"
colnames(hd)[5] <- "Edu.Exp"
colnames(hd)[6] <- "Edu.Mean"
colnames(hd)[7] <- "GNI"
colnames(hd)[8] <- "GNIcapitaRankMinusHDIrank"

colnames(hd)

# change the column names of gii:
colnames(gii)[1] <- "GIIrank"
colnames(gii)[2] <- "Country"
colnames(gii)[3] <- "GII"
colnames(gii)[4] <- "Mat.Mor"
colnames(gii)[5] <- "Ado.Birth"
colnames(gii)[6] <- "Parli.F"
colnames(gii)[7] <- "Edu2.F"
colnames(gii)[8] <- "Edu2.M"
colnames(gii)[9] <- "labF"
colnames(gii)[10] <- "labM"

colnames(gii)


# HERE WE DO NOT MUTATE GII DATA 
#AND DO NOT ADD VAIABLES edu2.FM AND labo.FM.
#INSTEAD WE FORM FRHI = (Mat.Mor + Ado.Birth)/2
#HRHI = Female reproductive health index

gii <- mutate(gii, FRHI = (Mat.Mor + Ado.Birth)/2)


glimpse(gii)

#Join the two data sets 

#hd_gii <- merge(hd, gii, by = "country")
hd_gii <-inner_join(x=hd, y=gii, by = "Country", suffix = c(".hd",".gii"))


# see the new column names
colnames(hd_gii)

# glimpse at the data
glimpse(hd_gii)


#WEEK 5: data wringling exercise Part 2. 
library(stringr)

#1. Transform the Gross National Income (GNI) variable to numeric

# remove the commas from GNI and print out a numeric version of it
GNI_num <- str_replace(hd_gii$GNI, pattern=",", replace ="")%>%as.numeric()
# add GNI_num to dataset using mutate() function 
hd_gii <- mutate(hd_gii, GNI_num)
colnames(hd_gii)


#HERE WE KEEP DIFFERENT COLUMNS, NEED FOR THE WEEK 6 ANALYSIS
#keep only the columns matching the following variable names: 
# "Country", "Edu2.F", "Edu2.M", "Edu.Mean", "Life.Exp", "GNI_num", "FRHI", "GII"

# columns to keep
keep <- c("Country", "Edu2.F", "Edu2.M", "Edu.Mean", "Life.Exp", "GNI_num", "FRHI", "GII")

# select the 'keep' columns
human <- select(hd_gii, one_of(keep))

#rename GNI_num to GNI
colnames(human)[6] <- "GNI"
colnames(human)

# Remove all rows with missing values 

# print out a completeness indicator of the 'human' data
complete.cases(human)

# print out the data along with a completeness indicator as the last column
data.frame(human[-1], comp = complete.cases(human))

# filter out all rows with NA values
human_ <- filter(human, complete.cases(human)==TRUE)
str(human_)

#Remove the observations which relate to regions instead of countries.
# These are last 10 observations(rows)

# look at the last 10 observations of human
tail(human_, n=10L)

#In this case we see that the last 10 observations are related to countries, 
#not to regions. Therefore we do not delete them as in DataCamp exercise. 
# define the last indice we want to keep
last <- nrow(human_) - 7

# choose everything until the last 7 observations
humann <- human_[1:last, ]


#Define the row names of the data by the country names 
# and remove the country name column from the data.

# add countries as rownames
rownames(humann) <- humann$Country

# remove the Country variable
human <- select(humann, -Country)
str(human)
#THE TRANSFORMED DATA INVOLVES 155 OBSERVATIONS OF 7 VARIABLES. 

#Save the human data in the project data folder including the row names
# export data as *txt file. 
write.table(human, file="human1.txt", quote=F)
# export data as *csv file.
write.csv(human, file = "human1.csv")


