#Here is a Data wrangling of human data
#The differences comparing to earlier data modifications are 
#separated by using capital letters. 
# The data are used for analysis in Final assignment exercise.

# Author: Neli Noykova
# e-mail: nnoykova'at'yahoo.com
# Date: 07.03.2017

#SUMMARY OF THE CHANGES COMPARING DATA WRINGLING FOR WEEK 5:

#1. Labour Force Participation Rate for both male and female 
#are not included in the analysis for this assignment

#2 (it is the same as before): Since the goal includes investigating the influence of gender inequality
# the populations with secondary education are taken as before: 
# only the ratio between female and male with secondary education is taken. 

#3. In accordance with the original article explainin HDI, 
#all data are rescaled between 0 and 1. They are not standardized, 
# which will not affect the outcome of linear regression. 

#3.1 In the original paper: Health index Health.I = ("Life.Exp" -20)/(85 -20) = ("Life.Exp" -20)/65
# Here we use standard normalozation formula Health.i = (x - min(x))/(max(x) -min(x))
# The reason: After checking both distribution I conclude that is more correct not to loose 
# informatoin by truncating minimum or maximum values. 

#3.2 Education Index EduI is obtained as simple arithmetic average of
# Edu.MeanI and Edu.ExpI are normalized using sdandard formula for Edu.Mean and Edu.Exp
# EduI = (Edu.MeanI + Edu.ExpI) / 2

#3.3 For the normalization of IncomeI in the original paper logarithmic function 
#(natural logarithm) is used in order to truncate too large 
#incomes. 
#Here we normalize this variable using standard normalization formula 
#IncomeI = (GNI - min(GNi))/(max(GNI) - min(GNI)). We make this choice 
#because we do not want to loose informations as it happen during truncating. 

#3.4 For normalization of other columns ("Mat.Mor", "Ado.Birth" and "Edu2.FM")
#the following common formula is used:
#xnor = (x -min(x))/(max(x) - min(x))

#3.5 Since we are interested in 
#the influence of the gender inefuality, we transform the variable
# Edu2.FM as factorial. 
#When the ratio (edu2.F/edu2.M >= 0.5), we assign to it "F", 
# otherwise - level "M".

# NOTE: I would like to add more variables 
#(for example related to Gender Development Index) 
# and investigate them, but I have noticed 
# that it is almost impossible for me to do it from the site 
# where the data are published. 
# All other data are in different dimensions, for different years, etc. 
# I would need more information, but the time is very limited. 

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


#HERE DO NOT ADD THE VAIABLE labo.FM

#take only the female/mail proportion of peiople with secondary education.
gii <- mutate(gii, Edu2.FM = Edu2.F / Edu2.M)



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
keep <- c("Country", "Edu2.FM", "Edu.Mean", "Edu.Exp", "Life.Exp", "GNI_num", "Mat.Mor", "Ado.Birth", "GII")

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

#FROM HERE BELLOW ALL VARIABLES ARE RESCALED, WHICH IS DIFFERENT 
#THAN IN PREVIOUS DATA WRINGLING EXERCISE 5. 

#rescaling all variables between 0 and 1 according the original article about HDI
#add new column Health.I = (Life.Exp -20)/65
human <- mutate(human, Life.I = (Life.Exp -min(Life.Exp))/(max(Life.Exp)-min(Life.Exp)))

#add new column Edu.MeanI = Edu.Mean/15
human <- mutate(human, Edu.MeanI = (Edu.Mean - min(Edu.Mean))/(max(Edu.Mean)- min(Edu.Mean)))

#add new column Edu.ExpI = Edu.Exp/18
human <- mutate(human, Edu.ExpI = (Edu.Exp - min(Edu.Exp))/(max(Edu.Exp)- min(Edu.Exp)))

#add new column EduI = (Edu.MeanI + Edu.ExpI)/2
human <- mutate(human, EduI = (Edu.MeanI + Edu.ExpI)/2)

#add new column IncomeI = (log(GNI) -log(100))/(log(75000) - log(100))
#human <- mutate(human, IncomeI = (log(GNI) -log(100))/(log(75000) - log(100)))
human <- mutate(human, IncomeI = (GNI -min(GNI))/(max(GNI) - min(GNI)))

#and finally columns "Mat.Mor" and "Ado.Birth" are rescaled using their minimum 
# and maximum values

human$Mat.Mor <- (human$Mat.Mor - min(human$Mat.Mor)) / (max(human$Mat.Mor) - min(human$Mat.Mor))
human$Ado.Birth <- (human$Ado.Birth - min(human$Ado.Birth)) / (max(human$Ado.Birth) - min(human$Ado.Birth))
human$Edu2.FM <- (human$Edu2.FM - min(human$Edu2.FM)) / (max(human$Edu2.FM) - min(human$Edu2.FM))
human$Edu2.FM

human$Edu2.FM <- round(human$Edu2.FM, digits = 0)
human$Edu2.FM <-as.factor(human$Edu2.FM)


levels(human$Edu2.FM) <- c("M","F")

human$Edu2.FM




#select only rescaled columns
# columns to keep
keep <- c("Edu2.FM", "Life.I",  "EduI", "IncomeI", "GII", "Mat.Mor", "Ado.Birth")

# select the 'keep' columns
human <- select(human, one_of(keep))
dim(human)
summary(human)


#Save the human data in the project data folder including the row names
# export data as *txt file. 
write.table(human, file="human.txt", quote=F)
# export data as *csv file.
write.csv(human, file = "human.csv")


