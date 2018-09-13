#-------------------------Installations-------------------------------
install.packages("tidyverse")
library("tidyverse")
install.packages('gridExtra')
library(gridExtra)
#-------------------------------Data mounts------------------------------------------------------------------------------  
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
  
#---------------------------------------------------------Tricks------------------------------------------------------------------------------
  
  
  ?rm
  rm(Jan_19)
  #the above removes objects!
  
  jan_18 <- s3tools::read_using(
    FUN = readxl::read_xlsx,
    s3_path = "alpha-ccma-2018/01_Jan_2018.xlsx",
    guess_max = 1000000
  )
  # the above worked
  
  
  
#---------------------------------Notes  ---------------------------------------------------------------------------------------  
  
  216436 + 159699 + 155402 + 141569 + 186181 + 205643 + 154503 + 161644 + 47841 + 203540 + 166670 + 147723 + 179750 
  # 2,126,601 Observations of 28 variables
 
  str(Redferm_moj)
  # str is very nice, it tells you everthing you need to know. 
  table(Redferm_moj$Payment_Amount)
  
#-------------------------------------------------Combining Objects----------------------------------------------------------
  may_17_1 <- rbind2(May_17_N, May_17_Y)
  
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
 # the above might not be soo neccessary but it might be used as some sort of Unique identifier
 
may_jun <- rbind2(may_17_1, jun_17)
Q2_17 <- rbind2(apr_17, may_jun)

jul_aug_17 <- rbind2(jul_17, aug_17)
 Q3_17 <- rbind2(jul_aug_17, sep_17)

oct_nov_17 <- rbind2(oct_17, nov_17)
Q4_17 <- rbind2(oct_nov_17, dec_17)


jan_feb_18 <- rbind2(jan_18, feb_18)
Q1_18 <- rbind2(jan_feb_18, mar_18)


Q2_17_Q3_17<- rbind2(Q2_17, Q3_17)
Q4_17_Q1_18 <- rbind2(Q4_17, Q1_18)


FY_17_18 <- rbind2(Q2_17_Q3_17, Q4_17_Q1_18)
FY_Exp <- rbind2(Q2_17_Q3_17, Q4_17_Q1_18)



##--------------------Differenet rbind procedure-----------------------------------------

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

#-----------------------------------Removing Certain Objects--------------------------------------------------

rm(apr_may_17)
rm(jun_jul_17)
rm(aug_sep_17) 
rm(oct_nov_17)
rm(dec_jan_ny)
rm(feb_mar_18)
rm(apr_jul_17)
rm(aug_nov_17)
rm(dec_mar_18)
rm(aug_mar_18)
# Maybe consider getting rid of month specific ones as well

rm(may_jun)
rm(Q2_17)
rm(jul_aug_17)
rm(Q3_17) 

rm(FY_O)
rm(Q2_17_Q3_17_Q4_17)
rm(Q3_17_Q1_18)

rm(apr_17)
rm(May_17_Y)
rm(May_17_N) 
rm(may_17_1)
rm(jun_17)
rm(jul_17)
rm(aug_17)
rm(sep_17)
rm(oct_17)
rm(nov_17)
rm(dec_17)
rm(jan_18)
rm(feb_18)
rm(mar_18)


# I got rid of all the ones that created FY_New but it still Exists therefore it is not neccessary to have them lying around


FY_New <- rbind2(apr_jul_17, aug_mar_18)

#--------------------Removing the dreaded 0 in Net value------------------------------------

myvars <- names(mydata) %in% c("v1", "v2", "v3")
newdata <- mydata[!myvars]

newnet <- names(FY_Exp) %in% c("0")

zeros <- FY_Exp %>%
  
filter(Net_Value == 0)

# the above choose only zeros

FY_Ref <- FY_Exp %>%
  
  filter(Net_Value != 0)
# I think the above got rid of all the zeros in Net value bro 

#The following will be used as a check to see where the 0.01 pence and 0.02 pence is concentrated

FY_Ref2 <- FY_Ref %>%
  
  filter(Net_Value <= 0.05 & Net_Value >= -0.05 )


Vat_test <- FY_Exp %>%
  
  filter(Invoice_VAT_Line_Amount != 0)

nrow(Vat_test)
nrow(FY_Exp) - nrow(FY_Ref)
# the above two lines are tests to see if taking out all the zeroes in Net_Value amount is chill. 


table(FY_Ref2$Vendor_Group_Name_L1)

table(FY_Ref2$Invoice_VAT_Line_Amount) - nrow(FY_Ref2)
# Second Query of Nisha is not of concern none of the really small transactions are VAT 

#------------Renaming Vectors-----------------

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

#----------------------------------------------Renaming the experimental dataset------------------------------------------------------
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

#----------------------------Understanding the nature of the vectors and perhaps considering converting them into integers-------------------------

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

#------------------------Creation of Operating Units--------------------------------------------------------- 

#CTS for HMCTS and NMS for HMPPS


moj_ou <- FY_Exp[ FY_Exp$GL_Operating_Unit_ID == 230, ]
#the above created a subset which is MOJ operating unit only 

cts_ou <- FY_Exp[ FY_Exp$GL_Operating_Unit_ID == 250, ]

nms_ou <- FY_Exp[ FY_Exp$GL_Operating_Unit_ID == 210, ]

opg_ou <- FY_Exp[ FY_Exp$GL_Operating_Unit_ID == 470, ]



#---------------------Overall picture---------------------------------------------------
table(FY_Exp$GL_Operating_Unit_ID)
by(FY_Exp$Supplier_ID,FY_Exp$PO_Matched, summary)
by(FY_Exp$GL_Operating_Unit_ID,FY_Exp$PO_Matched, summary)

qplot(x = Supplier_ID, data = FY_Exp)
# the other captures which suppliers have paid the most essentially
#------------Without Seperating Operating units----------------------------------

qplot(x = Payment_Date, data = FY_Exp)
qplot(data = FY_Exp, x = Payment_Date)

qplot(data = FY_Exp, x = Payment_Amount)



qplot(data = FY_Exp, x = Payment_Amount, binwidth = 30) +
  scale_x_continuous(limits = c(-10000, 10000), breaks = seq(-10000, 10000, 2500), color = I('black'), fill = I('#099DD9'))) 


qplot(data = FY_Exp, x = Payment_Amount, binwidth = 1) +
  scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, 10)) 

qplot(data = FY_Exp, x = Net_Value)
table(FY_Exp$Net_Value)



qplot(data = FY_Exp, x = Net_Value, binwidth = 30) +
  scale_x_continuous(limits = c(100000, 200000), breaks = seq(100000, 200000, 25000))


qplot(x = Net_Value, data = FY_Exp, 
      xlab = 'Net Value Last Financial Year', 
      ylab = 'Number of Transactions Across all Units', 
      color = I('black'), fill = I('#F79420')) +
  scale_x_continuous(limits = c(100000, 200000), breaks = seq(100000, 200000, 25000))


qplot(x = Net_Value, data = FY_Exp, 
      xlab = 'Net Value Last Financial Year', 
      ylab = 'Number of Transactions Across all Units', 
      color = I('black'), fill = I('#F79420')) +
  scale_x_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 1))



qplot(x = Payment_Amount, data = FY_Exp, 
      xlab = 'Payment Value Last Financial Year', 
      ylab = 'Number of Transactions Across all Units', 
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 1))


qplot(x = Net_Value, data = FY_Ref, 
      xlab = 'Payment Value Last Financial Year', 
      ylab = 'Number of Transactions Across all Units', 
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 0.5))
# I an still see around 7000 which are zero values




qplot(x = Net_Value, data = FY_Exp, binwidth = 1,
      geom = 'freqpoly', colour = PO_Matched ) +
  scale_x_continuous(lim = c(-10, 10), breaks = seq(-10, 10, 1)) 
 

qplot(x = Net_Value, data = FY_Exp,
      geom = 'freqpoly', colour = PO_Matched ) +
  scale_x_continuous(lim = c(-5, 5), breaks = seq(-5, 5, 1)) 



qplot(x = Net_Value, data = FY_Exp, binwidth = 100,
      xlab = 'Net Value Last Financial Year', 
      ylab = 'Number of Transactions Across all Units', 
      color = I('black'), fill = I('#F79420')) +
  scale_x_continuous(limits = c(100000, 200000), breaks = seq(100000, 200000, 25000))
# the above is ugly



qplot(data = FY_Exp, x = Payment_Amount, binwidth = 30) +  scale_x_continuous(limits = c(-10000, 10000), breaks = seq(-10000, 10000, 2500))

qplot(data = FY_Exp, x = Payment_Amount, binwidth = 30) +
  scale_x_continuous(limits = c(-500, 1500), breaks = seq(-500, 1500, 200))

qplot(data = FY_Exp, x = Payment_Amount, binwidth = 30) +  scale_x_continuous(limits = c(-150, 600), breaks = seq(-150, 600, 150))

qplot(data = FY_Exp, x = Payment_Amount, binwidth = 10) +
  scale_x_continuous(limits = c(-150, 600), breaks = seq(-150, 600, 150))


qplot(data = FY_Exp, x = Payment_Amount, binwidth = 1) +
  scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, 10))


qplot(x = GL_Operating_Unit_ID, data = FY_Exp)

qplot(x = Invoice_VAT_Line_Amount, data = FY_Exp) +
  scale_x_continuous(limits = c(-20, 100), breaks = seq(-20, 100, 10))

# 

table(Invoice_Number)

# loops, iflese skipping o's


qplot(x = Payment_Date, data = FY_Exp) +
  scale_x_discrete(breaks = 1:4)
# the above doesnt work 
qplot(x = Payment_Date, data = FY_Exp) +
  facet_wrap(~GL_Operating_Unit_ID, ncol=3)

qplot(x = Payment_Date, data = FY_Exp) +
  facet_wrap(~PO_Matched, ncol=3)


qplot(x = Payment_Date, data = FY_Exp) +
  facet_wrap(~PO_Matched, ncol=3)

sp <- ggplot(FY_Exp, aes(x= Payment_Date, y= Payment_Amount )) + geom_point()

sp + facet_grid(GL_Operating_Unit_ID ~  ., scales= "free", space = "free")
# the above code is not soo bad but it doesnt show me the reslts 

sp + facet_grid(Supplier_ID ~  ., scales= "free", space = "free")
#-----------
a[!(a %in% b)]

FY_Exp$Net_Value[!(FY_EXP$Net_Value %in% FY_Exp$Payment_Amount)]


#-------------------------------Redfern-------------------------------------------------

# the supplier ID for Redfern is 2875856

Redfern <- subset(FY_Exp, Supplier_ID == 2875856)

Redferm_moj <- subset(FY_Exp, Supplier_ID == 2875856 & GL_Operating_Unit_ID == 230)
Redfern_nms <- subset(FY_Exp, Supplier_ID == 2875856 & GL_Operating_Unit_ID == 210)
Redfern_cts <- subset(FY_Exp, Supplier_ID == 2875856 & GL_Operating_Unit_ID == 250)
Redfern_opg <- subset(FY_Exp, Supplier_ID == 2875856 & GL_Operating_Unit_ID == 470)

qplot(data = Redferm_moj, x = Payment_Amount)
qplot(x = Payment_Amount, data = Redferm_moj, xlim = c(0, 700))

qplot(data = Redferm_moj, x = Payment_Amount, binwidth = 25) +
  scale_x_continuous(limits = c(0, 700), breaks = seq(0, 700, 50)) 


qplot(data = Redfern, x = Payment_Amount, binwidth = 25) +
  scale_x_continuous(limits = c(0, 700), breaks = seq(0, 700, 50)) + 
  facet_wrap(~GL_Operating_Unit_ID)

qplot(data = Redfern, x = Payment_Amount, binwidth = 25) +
  scale_x_continuous(limits = c(0, 700), breaks = seq(0, 700, 50)) + 
  facet_wrap(~PO_Matched)

qplot (x = Payment_Amount, data = subset(Redfern, Invoice_Number== 3651), binwidth = 25) +
  scale_x_continuous(limits = c(0, 700), breaks = seq(0, 700, 50)) + 
  facet_wrap(~GL_Operating_Unit_ID)

qplot (x = Payment_Amount, data =Redfern, binwidth = 25) +
  scale_x_continuous(limits = c(0, 700), breaks = seq(0, 700, 50)) + 
  facet_wrap(~Invoice_Number)

qplot(x = Payment_Amount, data = Redferm_moj, 
      color = I('black'), fill = I('#099DD9'))






# the above is pretty decent , I think Nisha will like it. 

gpc <-  qplot(data = Redfern, x = Payment_Amount, binwidth = 25, geom = 'freqpoly', color = GL_Operating_Unit_ID ) +
  scale_x_continuous(limits = c(-10000, 0,), breaks = seq(-10000, 0, 50))
table(FY_Exp$GL_Operating_Unit_ID)


by(FY_Exp$Supplier_ID,FY_Exp$PO_Matched, summary)

ggplot(aes(x = Payment_Amount), data = Redferm_moj) +
  geom_histogram()
  
qplot(data = Redferm_moj, x = Payment_Date)


table(Redferm_moj$Invoice_Number)
table(Redfern$GL_Operating_Unit_ID)
by(Redfern$Payment_Amount, Redfern$PO_Matched, summary)
by(Redferm_moj$Payment_Amount, Redferm_moj$PO_Matched, summary)

by(Redfern$Payment_Amount, Redfern$GL_Operating_Unit_ID, summary)
#you might have to repeat the above taking into consideration outliers, so you need to find a way of droping values

Redfern_lim <- Redfern$Payment_Amount < 1000

summary(Redfern_lim)

summary(Redfern$Invoice_Number)

qplot(x = Payment_Date, data = Redferm_moj, 
      xlab = 'Last Financial Year', 
      ylab = 'Number of Redfern transactions at MoJ', 
      color = I('black'), fill = I('#F79420'))

qplot(x = Payment_Amount, data = Redferm_moj, bindwidth = 1, 
      color = I('black'), fill = I('#5760AB'))

summary(log10(Redferm_moj$Payment_Amount))

p1 <- qplot(x = Payment_Amount, data = Redferm_moj)
p2 <- qplot(x = log10(Payment_Amount + 1), data = Redferm_moj)
p3 <- qplot(x = sqrt(Payment_Amount), data = Redferm_moj)

grid.arrange(p1, p2, p3, ncol = 1)

 #ggplot(aes(x = Payment_Date, y = Payment_Amount, fill = Payment_Date, data = Redferm_moj)) + geom_line

ggplot(data = Redfern, aes(x = Payment_Date, y = Payment_Amount)) +
  geom_line() + scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, 50))

# the above looks ok

qplot(data = Redfern, x = Payment_Amount, binwidth = 25) +
  scale_x_continuous(limits = c(0, 700), breaks = seq(0, 700, 50)) + 
  facet_wrap(~GL_Operating_Unit_ID)

qplot(data = Redfern, x = Payment_Amount, binwidth = 25, geom = 'freqpoly', colour = GL_Operating_Unit_ID ) +
  scale_x_continuous(lim = c(0, 700), breaks = seq(0, 700, 50)) 

qplot(data = Redfern, x = Payment_Date, y = ..count../sum(..count..), 
      binwidth = 25, geom = 'freqpoly', colour = GL_Operating_Unit_ID )

qplot(x = Payment_Amount, data = Redfern, binwidth = 25,
      geom = 'freqpoly', colour = PO_Matched ) +
  scale_x_continuous(lim = c(0, 700), breaks = seq(0, 700, 50)) 

by(Redfern$Payment_Amount, Redfern$GL_Operating_Unit_ID, sum)
by(Redfern$Payment_Amount, Redfern$PO_Matched, sum)  
 
#Box-plots son-----------#

qplot(x = PO_Matched, y = Payment_Amount,
       data = Redfern, geom ='boxplot', ylim = c(0, 1000))

qplot(x = PO_Matched, y = Payment_Amount,
      data = Redfern, geom ='boxplot', ylim = c(0, 1000))

  
#-----------------------------------Enterprise Rent a Car------------------------------
# the supplier ID for Enterprise rent a c ar is 3839975

Enterprise <- subset(FY_Exp, Supplier_ID == 3839975)
Enterprise_moj <- subset(FY_Exp, Supplier_ID == 3839975 & GL_Operating_Unit_ID == 230)

qplot(data = Enterprise_moj, x = Payment_Amount)
qplot(x = Payment_Amount, data = Enterprise_moj, xlim = c(0, 700))

qplot(data = Enterprise_moj, x = Payment_Date)




# the above is a good measure actually, it can tell you were total spend is concentrated among the different operating units, it might be a good idea to trip it a bit 

qplot(x = Supplier_ID, data =)
# the other captures which suppliers have paid the most essentially
qplot(x = Payment_Date, data = )

#tp adjust the x_axis we do the following















