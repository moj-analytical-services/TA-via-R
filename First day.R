
  jan_18_csv <- s3tools::read_using(
    FUN = readr::read_csv,
    s3_path = "alpha-ccma-2018/01_Jan_2018 - Copy.csv",
    guess_max = 1000000
  )
  #the above didnt work actually
  
  jul_17 <- s3tools::read_using(
    FUN = readxl::read_xlsx,
    s3_path = "alpha-ccma-2018/07_Jul_2017.xlsx",
    guess_max = 1000000
  )
  
  aug_17 <- s3tools::read_using(
    FUN = readxl::read_xlsx,
    s3_path = "alpha-ccma-2018/08_Aug_2017.xlsx",
    guess_max = 1000000
  )
  
  
  sep_17 <- s3tools::read_using(
    FUN = readxl::read_xlsx,
    s3_path = "alpha-ccma-2018/09_Sep_2017.xlsx",
    guess_max = 1000000
  )
  
  
  oct_17 <- s3tools::read_using(
    FUN = readxl::read_xlsx,
    s3_path = "alpha-ccma-2018/10_Oct_2017.xlsx",
    guess_max = 1000000
  )
  
  
  nov_17 <- s3tools::read_using(
    FUN = readxl::read_xlsx,
    s3_path = "alpha-ccma-2018/11_Nov_2017.xlsx",
    guess_max = 1000000
  )
  
  dec_17 <- s3tools::read_using(
    FUN = readxl::read_xlsx,
    s3_path = "alpha-ccma-2018/12_Dec_2017.xlsx",
    guess_max = 1000000
  )
  
#---------------------------------------------------------data upload complete------------------------------------------------------------------------------
  
  
  ?rm
  rm(Jan_19)
  #the above removes objects!
  
  jan_18 <- s3tools::read_using(
    FUN = readxl::read_xlsx,
    s3_path = "alpha-ccma-2018/01_Jan_2018.xlsx",
    guess_max = 1000000
  )
  # the above worked
  
  
  ?xts
  
#----------------------data tricks and tips ends here-----------------------------------------------------------------------------------------------------------  
  
#---------------------------------Freestyle Exploration Begins Here  ---------------------------------------------------------------------------------------  
  
  216436 + 159699 + 155402 + 141569 + 186181 + 205643 + 154503 + 161644 + 47841 + 203540 + 166670 + 147723 + 179750 
  # 2,126,601 Observations of 28 variables
  #you can save it on the environment tab if you wanted to save but it takes to much space
  # R-markdown is similar to latex because you are able to write and have code on it
  #I think r-markdown might be better for your purposes! 
  
  # dude where is my car 
  
  jan_feb <- merge(jan_18, feb_18, by = c('PO Matched'))
  
  # union command
  ?union
# thomas sugguests to have a unique ID that seperates feb from the jan, what would make it a separate 
  
  may_17 <- union(May_17_N, May_17_Y)
  
  may_17 <- union(May_17_N, May_17_Y, by = c('PO Matched'))
  #---------------------------------------------------------the above two don't work------------------------
  #----------------------------------Humbugs so far-----------------------------------
  # using merger becasue it gives negative vectors, long story short, it's two big so I took Thomas's sugguestion of stacking one on the other with rows 
  # union didnt allow me to do that, it just created a list but rbind2 did that!! 
  
  
  
  may_17_1 <- rbind2(May_17_N, May_17_Y)
  
  # the above worked!!
  
  ?cbind
  
  #attempt the following once you find a way to create a time variable 
  
 # year_prova <- rbind2(apr_17, may_17_1, jun_17, jul_17, aug_17, sep_17, oct_17, nov_17, dec_17,)
  
 # may_17_1 <- time()
  
 #time(may_17_1)
 # 
# may_17_1$mese <- seq(may, )
 
 
 install.packages("tidyverse")
 library("tidyverse")
 
 #may_17_1$mese <- may_17_1$`Supplier ID`*May
 
 #?ts
 #may_17_1 <- ts(data = 'Supplier ID', start =c(05, 12), end=c(05, 12), frequency=12)
 
 #summary(may_17_1)
 
 
 #myts <- ts(may_17_1[, 1:28], start = c(2017,5), frequency = 12)
 
 # thre is some progress here but tge output is just a large mts 
 
 
 #myts_2 <- ts(may_17_1[, 1:28], start = c(2017,5), frequency = 0 )
 
 #myts2 <- window(may_17_1[, 1:28], start=c(2017, 5), end=c(2017, 5)) 
# may_17_1 <- seq(as.Date("2000/1/1"), by = "month", length.out = 12
# the above doesn't work
 

 
 
# warnings()
 
#?as.Date
 
 #may_17_1$dates <- c("2017-05-01") 
# the above creates a vector with the above date and the info is found https://www.stat.berkeley.edu/~s133/dates.html
 
 apr_17$dates <- c("2017-04-01")
 may_17_1$dates <- c("2017-05-01")
 jun_17$dates <- c("2017-06-01")
 jul_17$dates <- c("2017-07-01")
 aug_17$dates <- c("2017-08-01")
 sep_17$dates <- c("2017-09-01")
 oct_17$dates <- c("2017-10-01")
 nov_17$dates <- c("2017-11-01")
 dec_17$dates <- c("2017-12-01")
 jan_18$dates <- c("2018-01-01")
 feb_18$dates <- c("2018-02-01")
 mar_18$dates <- c("2018-03-01")
 # to get rid of x infront of mar_18 it requires a comma and I want to know why 
 # from 139 to 144 seems to have worked but I don't know if it is a legit time vector
 #----------------------Combining them all--------------------------------------------------#
may_jun <- rbind2(may_17_1, jun_17)

Q2_17 <- rbind2(apr_17, may_jun)

jul_aug_17 <- rbind2(jul_17, aug_17)
 Q3_17 <- rbind2(jul_aug_17, sep_17)
# the above presents the solution, rbind2 can only do two objects at a time
 
 oct_nov_17 <- rbind2(oct_17, nov_17)
Q4_17 <- rbind2(oct_nov_17, dec_17)


jan_feb_18 <- rbind2(jan_18, feb_18)

Q1_18 <- rbind2(jan_feb_18, mar_18)


Q2_17_Q3_17<- rbind2(Q2_17, Q3_17)

Q4_17_Q1_18 <- rbind2(Q4_17, Q1_18)


# you've made a mess here by the way, if you can be bothered make it tidy although R would probably sort it out itself

FY_17_18 <- rbind2(Q2_17_Q3_17, Q4_17_Q1_18)

#I will create another one that I can experiment with

FY_Exp <- rbind2(Q2_17_Q3_17, Q4_17_Q1_18)

#TS_17_18 <- ts(FY_17_18[, 1:28], start = c(2017,5), frequency = 12) - some warning 

#TS_17_18 <- window(FY_17_18[, 1:28], start=c(2017, 5), end=c(2018, 3)) - invalid time series


df$new_date_col <- as.Date(df$date)


#apr_17$dates_2 <- c("01-05-2017"),
#apr_17$new_date <- as.Date(apr_17$dates_2)

may_17_1$new_date <- as.Date(may_17_1$dates)
# I think it has worked for may because I can clearly see it, now time to see the type
class(may_17_1$new_date)
# you can see from the above that you have managed to change it into a date type 



##--------------------differenet rbind procedure as I suspect there might be an error with the one I already did-----------------------------------------#

apr_may_17 <- rbind2(apr_17, may_17_1)
jun_jul_17 <- rbind2(jun_17, jul_17) 
aug_sep_17 <-rbind2(aug_17, sep_17)
oct_nov_17 <-rbind2(oct_17, nov_17)
dec_jan_ny <-rbind2(dec_17, jan_18)
feb_mar_18<-rbind2(feb_18, mar_18)


apr_jul_17 <- rbind2(apr_may_17, jun_jul_17)
aug_nov_17 <- rbind2(aug_sep_17,oct_nov_17)
dec_mar_18 <- rbind2(dec_jan_ny,feb_mar_18)

aug_mar_18 <- rbind2(aug_nov_17, dec_mar_18)

FY_New <- rbind2(apr_jul_17, aug_mar_18)

# I think this one might be the correct one 
# ACTUALLY ITS ALL GOOD IN THE HOOD NOW, YOU HAVE MADE THE CHANGES.....
#----------------------------- that competes the combination of the century---------------------------------------------#

# i wonder if I remove any of my data fields will it get rid of the final combinations
# so your data is a character and to change it to a date type, you need to use as.Date
# if you have the date vector as a date type then you don't neccessarily need it to be time series

summarise("FY_17_18$Payment Amount")

class(FY_17_18$Payment Amount)

# I am faced with issues so I might have to change the name of certain vectors and change the type of certain vectors from character to integer or something

#you might
head(FY_17_18)


#------------R can't read columns, I suspect it is because of the spaces-----------------

colnames(FY_17_18)[colnames(FY_17_18)=="Catalogue Item"] <- "Catalogue_Item"
colnames(FY_17_18)[colnames(FY_17_18)=="GL Business Unit Name"] <- "GL_Business_Unit_Name"
colnames(FY_17_18)[colnames(FY_17_18)=="Account Code Description"] <- "Account_Code_Description"
colnames(FY_17_18)[colnames(FY_17_18)=="Account Code"] <- "Account_Code"

colnames(FY_17_18)[colnames(FY_17_18)=="GL Business Unit ID"] <- "GL_Business_Unit_ID"
colnames(FY_17_18)[colnames(FY_17_18)=="GL Operating Unit ID"] <- "GL_Operating_Unit_ID"
colnames(FY_17_18)[colnames(FY_17_18)=="GL Operating Unit Name"] <- "GL Operating_Unit_Name"
colnames(FY_17_18)[colnames(FY_17_18)=="Invoice Line Description"] <- "Invoice_Line_Description"

colnames(FY_17_18)[colnames(FY_17_18)=="Net Value"] <- "Net_Value"
colnames(FY_17_18)[colnames(FY_17_18)=="Invoice VAT Line Amount"] <- "Invoice_VAT_Line_Amount"
colnames(FY_17_18)[colnames(FY_17_18)=="Invoice Number"] <- "Invoice_Number"

colnames(FY_17_18)[colnames(FY_17_18)=="GPS Category L1"] <- "GPS_Category_L1"
colnames(FY_17_18)[colnames(FY_17_18)=="GPS Category L2"] <- "GPS_Category_L2"
colnames(FY_17_18)[colnames(FY_17_18)=="GPS Category L3"] <- "GPS_Category_L3"
colnames(FY_17_18)[colnames(FY_17_18)=="GPS Category L4"] <- "GPS_Category_L4"

colnames(FY_17_18)[colnames(FY_17_18)=="UNSPSC Category L1"] <- "UNSPSC_Category_L1"
colnames(FY_17_18)[colnames(FY_17_18)=="UNSPSC Category L2"] <- "UNSPSC_Category_L2"
colnames(FY_17_18)[colnames(FY_17_18)=="UNSPSC Category L3"] <- "UNSPSC_Category_L3"
colnames(FY_17_18)[colnames(FY_17_18)=="UNSPSC Category L4"] <- "UNSPSC_Category_L4"
colnames(FY_17_18)[colnames(FY_17_18)=="UNSPSC Code"] <- "UNSPSC_Code"

colnames(FY_17_18)[colnames(FY_17_18)=="Payment Amount"] <- "Payment_Amount"
colnames(FY_17_18)[colnames(FY_17_18)=="Payment Date"] <- "Payment_Date"

colnames(FY_17_18)[colnames(FY_17_18)=="PO Matched"] <- "PO_Matched"
colnames(FY_17_18)[colnames(FY_17_18)=="SME Flag"] <- "SME_Flag"
colnames(FY_17_18)[colnames(FY_17_18)=="Source System"] <- "Source_System"


colnames(FY_17_18)[colnames(FY_17_18)=="DUNS Number"] <- "DUNS_Number"
colnames(FY_17_18)[colnames(FY_17_18)=="Supplier ID"] <- "Supplier_ID"

#-----------------------------------------------------------------Renaming the experimental dataset------------------------------------------------------
colnames(FY_Exp)[colnames(FY_Exp)=="Catalogue Item"] <- "Catalogue_Item"
colnames(FY_Exp)[colnames(FY_Exp)=="GL Business Unit Name"] <- "GL_Business_Unit_Name"
colnames(FY_Exp)[colnames(FY_Exp)=="Account Code Description"] <- "Account_Code_Description"
colnames(FY_Exp)[colnames(FY_Exp)=="Account Code"] <- "Account_Code"

colnames(FY_Exp)[colnames(FY_Exp)=="GL Business Unit ID"] <- "GL_Business_Unit_ID"
colnames(FY_Exp)[colnames(FY_Exp)=="GL Operating Unit ID"] <- "GL_Operating_Unit_ID"
colnames(FY_Exp)[colnames(FY_Exp)=="GL Operating Unit Name"] <- "GL Operating_Unit_Name"
colnames(FY_Exp)[colnames(FY_Exp)=="Invoice Line Description"] <- "Invoice_Line_Description"

colnames(FY_Exp)[colnames(FY_Exp)=="Net Value"] <- "Net_Value"
colnames(FY_Exp)[colnames(FY_Exp)=="Invoice VAT Line Amount"] <- "Invoice_VAT_Line_Amount"
colnames(FY_Exp)[colnames(FY_Exp)=="Invoice Number"] <- "Invoice_Number"

colnames(FY_Exp)[colnames(FY_Exp)=="GPS Category L1"] <- "GPS_Category_L1"
colnames(FY_Exp)[colnames(FY_Exp)=="GPS Category L2"] <- "GPS_Category_L2"
colnames(FY_Exp)[colnames(FY_Exp)=="GPS Category L3"] <- "GPS_Category_L3"
colnames(FY_Exp)[colnames(FY_Exp)=="GPS Category L4"] <- "GPS_Category_L4"

colnames(FY_Exp)[colnames(FY_Exp)=="UNSPSC Category L1"] <- "UNSPSC_Category_L1"
colnames(FY_Exp)[colnames(FY_Exp)=="UNSPSC Category L2"] <- "UNSPSC_Category_L2"
colnames(FY_Exp)[colnames(FY_Exp)=="UNSPSC Category L3"] <- "UNSPSC_Category_L3"
colnames(FY_Exp)[colnames(FY_Exp)=="UNSPSC Category L4"] <- "UNSPSC_Category_L4"
colnames(FY_Exp)[colnames(FY_Exp)=="UNSPSC Code"] <- "UNSPSC_Code"

colnames(FY_Exp)[colnames(FY_Exp)=="Payment Amount"] <- "Payment_Amount"
colnames(FY_Exp)[colnames(FY_Exp)=="Payment Date"] <- "Payment_Date"

colnames(FY_Exp)[colnames(FY_Exp)=="PO Matched"] <- "PO_Matched"
colnames(FY_Exp)[colnames(FY_Exp)=="SME Flag"] <- "SME_Flag"
colnames(FY_Exp)[colnames(FY_Exp)=="Source System"] <- "Source_System"


colnames(FY_Exp)[colnames(FY_Exp)=="DUNS Number"] <- "DUNS_Number"
colnames(FY_Exp)[colnames(FY_Exp)=="Supplier ID"] <- "Supplier_ID"


#------------Too many useless columns for initial excercise, droping them temporarily---------------------
#I think you can drop DUNS numner and 
FY_Exp$dates <- NULL
FY_Exp$DUNS_Number <- NULL
FY_Exp$Source_System <- NULL
FY_Exp$Catalogue_Item <- NULL
FY_Exp$GPS_Category_L1 <- NULL
FY_Exp$GPS_Category_L2 <- NULL
FY_Exp$GPS_Category_L3 <- NULL
FY_Exp$GPS_Category_L4 <- NULL

FY_Exp$UNSPSC_Category_L1 <- NULL
FY_Exp$UNSPSC_Category_L2 <- NULL
FY_Exp$UNSPSC_Category_L3 <- NULL
FY_Exp$UNSPSC_Category_L4 <- NULL

FY_Exp$SME_Flag <- NULL
FY_Exp$Invoice_Line_Description <- NULL
FY_Exp$Account_Code_Description <- NULL

FY_Exp$Invoice_Line_Description <- NULL
FY_Exp$Account_Code_Description <- NULL

#------------------Understanding the nature of the vectors and perhaps considering converting them into integers

class(FY_Exp$Payment_Date)
# Date seems fine as it is POSTIXct

class(FY_Exp$Net_Value)
# Net_value is numeric so we can tango

#operating unit id 230 is MoJ_OU
subset(FY_Exp, GL_Operating_Unit_ID == 230)
#the above is a way to subset the dataset and another way is the following 
#FY_Exp[Rows we want to keep, columns we want to keep]
# maybe you can code your table in a way that has a list of variables on the rows as well, instead of having a dataset with pure vectors, you can have something that is more like a matrix
FY_Exp[ FY_Exp$GL_Operating_Unit_ID == 230, ]
# we can save this into new varibles

#------------------------Creation of Operating Units--------------------------------------# 
table(FY_Exp$GL_Operating_Unit_ID)


moj_ou <- FY_Exp[ FY_Exp$GL_Operating_Unit_ID == 230, ]
#the above created a subset which is MOJ operating unit only 

# the supplier ID for Redfern is 2875856
# the supplier ID for Enterprise rent a c ar is 3839975

Redfern <- subset(FY_Exp, Supplier_ID == 2875856)

# after this point you can ge rid of the vendor group name, UNSPSC code and supplier ID as the are just taking space and not doing anything ellse
# enterprise is for enterprise rent a car
Redferm_moj <- subset(FY_Exp, Supplier_ID == 2875856 & GL_Operating_Unit_ID == 230)
#after this point you can also get rid of GL operating unit ID and gl operating unit name 
#I think you have succesfully isolated redfern spend that is specifically MoJ with the above code, you just need to make things more presentable

# the following isolates redfern non po spend
#Redferm_moj_no <- subset(Redferm_moj, PO_Matched == "Yes")
#rm(Redferm_moj_no)
#The above gives me the exact same as Redferm_moj, therefore it must be the case that they are all PO matched
head(moj_ou, 2)
dim(moj_ou)

str(Redferm_moj)
# str is very nice, it tells you everthing you need to know. 

summary(Redferm_moj)
# when 

R_moj_Net_V <- Redferm_moj$Net_Value


summarise(R_moj_Net_V)

?ggplot

table(Redferm_moj$Payment_Amount)
str(Redferm_moj)
levels(Redferm_moj$Net_Value)
# the above gives you NULL, not sure if this is a bad sign 
install.packages('ggplot2')
library(ggplot2)
qplot(data = Redferm_moj, x = Payment_Amount)
qplot(data = Redferm_moj, x = Payment_Date)



#Redferm_moj$Payment_Amount <- ordered(Redferm_moj$Payment_Amount = c("0"))

# idead- what you can and should do is 


#-----------------------------------Redfern ends here and Enterprise Begins here------------------------------#
Enterprise <- subset(FY_Exp, Supplier_ID == 3839975)


names(Redferm_moj)

qplot(x = GL_Operating_Unit_ID, data = FY_Exp)
# the above is a good measure actually, it can tell you were total spend is concentrated among the different operating units, it might be a good idea to trip it a bit 

qplot(x = Supplier_ID, data = FY_Exp)
# the other captures which suppliers have paid the most essentially
qplot(x = Payment_Date, data = FY_Exp)

#tp adjust the x_axis we do the following

qplot(x = Payment_Date, data = FY_Exp) +
  scale_x_discrete(breaks = 1:4)
# the above doesnt work 
qplot(x = Payment_Date, data = FY_Exp) +
  facet_wrap(~Payment_Amount, ncol=3)

