#-------------------------Installations-------------------------------
install.packages("tidyverse")
library("tidyverse")
install.packages('gridExtra')
library(gridExtra)
library(ggplot2)


rm(acc)
rm(fidy2)
rm(netx)
rm(NMS)
rm(ss)
rm(tble)
#-------------------------------Data mounts------------------------------------------------------------------------------  
#jan_18_csv <- s3tools::read_using(
    #FUN = readr::read_csv,
    #s3_path = "alpha-ccma-2018/01_Jan_2018 - Copy.csv",
    #guess_max = 1000000
  #)
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
 
  
  
  df %>%
    
    group_by(v2) %>% # group by the actual value
    
    mutate(cs = row_number()) %>% # count how many times the actual value appeared so far
    
    mutate(abs = abs(v2)) %>% # get the absolute value
    
    group_by(abs, cs) %>% # group by the absolute value and how many times each actual number appears
    
    mutate(rs = n()) %>% # count how many rows there are in that group
    
    filter(rs == 1) %>% # if cumulative freuqency and absolute value are equal, they are a duplicate
    
    View()
  
  
  
#---------------------------------Notes  ---------------------------------------------------------------------------------------  
  
  216436 + 159699 + 155402 + 141569 + 186181 + 205643 + 154503 + 161644 + 47841 + 203540 + 166670 + 147723 + 179750 
  # 2,126,601 Observations of 28 variables
 
  str(Redferm_moj)
  # str is very nice, it tells you everthing you need to know. 

  
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



##--------------------Different rbind procedure-----------------------------------------

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
# I got rid of all the ones that created FY_New but it still Exists therefore it is not neccessary to have them lying around


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



rm(FY_17_18)
rm(FY_New)
rm(Vat_test)
rm(FY_Exp)
rm(Enterprise)
rm(Enterprise_moj)
rm(Test_Jul)
rm(Test_Julb)
rm(zeros)
rm(Redfern)
rm(Redfern_nms)
rm(FY_Ref2)
rm(sapplier3)
rm(sapplier4)
rm(saplier_m1)
rm(saplier_m2)
rm(Redferm_moj)
rm(Redfern_opg)
rm(moj_ou)
rm(opg_ou)
rm(Jul_Cinb)
rm(Jul_Cinb2)
rm(Jul_Cinb3)
#--------------------Removing the dreaded 0 in Net value------------------------------------

#myvars <- names(mydata) %in% c("v1", "v2", "v3")
#newdata <- mydata[!myvars]

#newnet <- names(FY_Exp) %in% c("0")



#---------------------------------Solution of the Century---------------------------------


FY_Ref <- FY_Exp %>%
  
  filter(Net_Value != 0)

#Jul_Ref <- jul_17 %>%
  
 # filter(Net_Value != 0)

#oct_ref <- oct_17 %>%
  
  #filter(Net_Value != 0)

# Further Refinement in July to study Supplier ID = 3844494

#Jul_CinQ <- jul_17 %>%
  
 # filter(Supplier_ID == 3844494)



#Jul_Cin <- Jul_Ref %>%
  
 # filter(Supplier_ID == 3844494)

#Jul_Cin2 <- Jul_Cin %>%
 # filter(Net_Value <= 702362)

#Jul_Cin3 <- Jul_Cin2 %>%
 # filter(Net_Value >= -702362)


#Jul_Cinb <-  Jul_Ref %>%
 #filter(Supplier_ID == 3844504)

#Jul_Cinb2 <- Jul_Cinb %>%
 # filter(Net_Value <= 29000000)

#Jul_Cinb3 <- Jul_Cinb2 %>%
 # filter(Net_Value >= -29000000)

# Jul_Cin3 is fixed it doesn't contain any of the things 
# the following will be without are infamous supplier then I will try to reconect them with Jul_Cin3

#Jul_Ref2 <- Jul_Ref %>%
 # filter(Supplier_ID != 3844504)

#Jul_Ref3 <- rbind2(Jul_Ref2, Jul_Cin3)


# jul_ref3 is  completely clean from that crazy anomalie, lets doubl check

#Jul_Ref4 <- Jul_Ref3 %>%
 # filter(Supplier_ID != 3844504)
#Jul_Ref5 <- rbind2(Jul_Ref4, Jul_Cinb3)

#rm(Jul_Ref)
#rm(Jul_Ref2)
#rm(Jul_Cin)
#rm(Jul_Cin2)
#rm(Jul_Cin3)
#rm(Jul_Ref3)
#rm(Jul_Ref4)


#rm(newnet)
#rm(Redfern_lim)
#rm(cts_ou)
#rm(nms_ou)


# After studying this supplier it has become known to me that I should ignore anything above and below this value 702363.4 for this supplier is simply a correction
# because of the nature of distirbution lines which are cost allocations.

#Jul_Cin <- Jul_Ref %>%
  
  #filter(Supplier_ID == 3844494)

# I guest what I can do is remove this supplier completely for the month of july then bring them back in


# I think the above got rid of all the zeros in Net value  

#The following will be used as a check to see where the 0.01 pence and 0.02 pence is concentrated

#FY_Ref2 <- FY_Ref %>%
  
  #filter(Net_Value <= 0.05 & Net_Value >= -0.05 )



Vat_test <- FY_Exp %>%
  
  filter(Invoice_VAT_Line_Amount != 0)

Vat_test2 <- FY_Exp %>%
  
  filter(Net_Value != 0)

nrow(Vat_test)
nrow(FY_Exp) - nrow(FY_Ref)
# the above two lines are tests to see if taking out all the zeroes in Net_Value amount is chill. 


table(FY_Ref2$Vendor_Group_Name_L1)

table(FY_Ref2$Invoice_VAT_Line_Amount) 



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


# Addressing the July issues
colnames(oct_17)[colnames(oct_17)=="Payment Date"] <- "Payment_Date"
colnames(oct_17)[colnames(oct_17)=="Net Value"] <- "Net_Value"
colnames(oct_17)[colnames(oct_17)=="PO Matched"] <- "PO_Matched"
colnames(oct_17)[colnames(oct_17)=="GL Operating Unit ID"] <- "GL_Operating_Unit_ID"
colnames(oct_17)[colnames(oct_17)=="Supplier ID"] <- "Supplier_ID"

colnames(oct_17)[colnames(oct_17)=="Catalogue Item"] <- "Catalogue_Item"
colnames(oct_17)[colnames(oct_17)=="GL Business Unit Name"] <- "GL_Business_Unit_Name"
colnames(oct_17)[colnames(oct_17)=="Account Code Description"] <- "Account_Code_Description"
colnames(oct_17)[colnames(oct_17)=="Account Code"] <- "Account_Code"

colnames(oct_17)[colnames(oct_17)=="GL Business Unit ID"] <- "GL_Business_Unit_ID"
colnames(oct_17)[colnames(oct_17)=="GL Operating Unit ID"] <- "GL_Operating_Unit_ID"
colnames(oct_17)[colnames(oct_17)=="GL Operating Unit Name"] <- "GL Operating_Unit_Name"
colnames(oct_17)[colnames(oct_17)=="Invoice Line Description"] <- "Invoice_Line_Description"

colnames(oct_17)[colnames(oct_17)=="Net Value"] <- "Net_Value"
colnames(oct_17)[colnames(oct_17)=="Invoice VAT Line Amount"] <- "Invoice_VAT_Line_Amount"
colnames(oct_17)[colnames(oct_17)=="Invoice Number"] <- "Invoice_Number"

colnames(oct_17)[colnames(oct_17)=="GPS Category L1"] <- "GPS_Category_L1"
colnames(oct_17)[colnames(oct_17)=="GPS Category L2"] <- "GPS_Category_L2"
colnames(oct_17)[colnames(oct_17)=="GPS Category L3"] <- "GPS_Category_L3"
colnames(oct_17)[colnames(oct_17)=="GPS Category L4"] <- "GPS_Category_L4"

colnames(oct_17)[colnames(oct_17)=="UNSPSC Category L1"] <- "UNSPSC_Category_L1"
colnames(oct_17)[colnames(oct_17)=="UNSPSC Category L2"] <- "UNSPSC_Category_L2"
colnames(oct_17)[colnames(oct_17)=="UNSPSC Category L3"] <- "UNSPSC_Category_L3"
colnames(oct_17)[colnames(oct_17)=="UNSPSC Category L4"] <- "UNSPSC_Category_L4"
colnames(oct_17)[colnames(oct_17)=="UNSPSC Code"] <- "UNSPSC_Code"

colnames(oct_17)[colnames(oct_17)=="Payment Amount"] <- "Payment_Amount"
colnames(oct_17)[colnames(oct_17)=="Payment Date"] <- "Payment_Date"

colnames(oct_17)[colnames(oct_17)=="PO Matched"] <- "PO_Matched"
colnames(oct_17)[colnames(oct_17)=="SME Flag"] <- "SME_Flag"
colnames(oct_17)[colnames(oct_17)=="Source System"] <- "Source_System"


colnames(oct_17)[colnames(oct_17)=="DUNS Number"] <- "DUNS_Number"
colnames(oct_17)[colnames(oct_17)=="Supplier ID"] <- "Supplier_ID"


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


oct_17$dates <- NULL
oct_17$DUNS_Number <- NULL
oct_17$Source_System <- NULL
oct_17$Catalogue_Item <- NULL
oct_17$GPS_Category_L1 <- NULL
oct_17$GPS_Category_L2 <- NULL
oct_17$GPS_Category_L3 <- NULL
oct_17$GPS_Category_L4 <- NULL

oct_17$UNSPSC_Category_L1 <- NULL
oct_17$UNSPSC_Category_L2 <- NULL
oct_17$UNSPSC_Category_L3 <- NULL
oct_17$UNSPSC_Category_L4 <- NULL

oct_17$SME_Flag <- NULL
oct_17$Invoice_Line_Description <- NULL
oct_17$Account_Code_Description <- NULL

oct_17$Invoice_Line_Description <- NULL
oct_17$Account_Code_Description <- NULL
#-----------------------Consider Adding the next few removals to the bigger dataset-------------------------------
Jul_Ref$Invoice_VAT_Line_Amount <- NULL
Jul_Ref$Invoice_Number <- NULL
Jul_Ref$UNSPSC_Code <- NULL

Jul_Ref$Payment_Amount <- NULL
Jul_Ref$Account_Code <- NULL


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


moj_ou <- FY_Ref[ FY_Ref$GL_Operating_Unit_ID == 230, ]
#the above created a subset which is MOJ operating unit only 

cts_ou <- FY_Ref[ FY_Ref$GL_Operating_Unit_ID == 250, ]

nms_ou <- FY_Ref[ FY_Ref$GL_Operating_Unit_ID == 210, ]

opg_ou <- FY_Ref[ FY_Ref$GL_Operating_Unit_ID == 470, ]



#---------------------Overall picture---------------------------------------------------
table(FY_Ref$GL_Operating_Unit_ID)

by(FY_Ref$Supplier_ID,FY_Ref$PO_Matched, summary)

by(FY_Ref$Vendor_Group_Name_L1,FY_Ref$PO_Matched, summary)


by(FY_Ref$GL_,FY_Ref$PO_Matched, summary)

summarise_if()
#-----GET BACK TO THIS IMMEDIATELY SON ---------------------------------Checking which Supplier is the highest -------------------------------------------

saplier_m1 <- FY_Ref[FY_Ref$Supplier_ID == 3840019, ]
#  4232949 has only one observation

saplier_m2 <- FY_Ref[FY_Ref$Supplier_ID == 3840019, ]

sapplier3 <- FY_Ref %>%
  group_by(Supplier_ID) %>%
  summarise("PO_Matched == No")

sapplier4 <- FY_Ref %>%
  filter(GL_Operating_Unit_ID == 230) %>%
  group_by("PO_Matched == No")
  summarise("PO_Matched == No")
  # think carefullly about the above son 


# the above just has all the supplier ID wih non po mathed
table(sapplier3$Supplier_ID)
?table
 


by(FY_Ref$GL_Operating_Unit_ID,FY_Ref$PO_Matched, summary)

qplot(x = Supplier_ID, data = FY_Ref)
# the other captures which suppliers have paid the most essentially
#------------Without Seperating Operating units----------------------------------



#-----------------Net_Value qplots-----------------------------------


qplot(data = FY_Ref, x = Net_Value, color = I('black'), fill = I('#CC0000')) 


qplot(data = FY_Ref, x = Net_Value, binwidth = 100, color = I('black'), fill = I('#CC0000'), 
      xlab = 'Net Value Last Financial Year', 
      ylab = 'Number of Transactions Across all Units', 
      color = I('black'), fill = I('#F79420')) +
   scale_x_continuous(limits = c(-200000, 200000), breaks = seq(-200000, 200000, 50000))

qplot(data = FY_Ref, x = Net_Value, color = I('black'), fill = I('#CC0000'), 
      xlab = 'Net Value Last Financial Year', 
      ylab = 'Number of Transactions Across all Units', 
      color = I('black'), fill = I('#F79420')) +
  scale_x_continuous(limits = c(-100000, 100000), breaks = seq(-100000, 100000, 25000))


qplot(x = Net_Value, data = FY_Ref,
      xlab = 'Net Value Last Financial Year', 
      ylab = 'Number of Transactions Across all Units', 
      color = I('black'), fill = I('#F79420')) +
  scale_x_continuous(limits = c(-500, 1500), breaks = seq(-500, 1500, 200))


qplot(data = FY_Ref, x = Net_Value, binwidth = 10,
      xlab = 'Net Value Last Financial Year', 
      ylab = 'Number of Transactions Across all Units', 
      color = I('black'), fill = I('#F79420')) +
  scale_x_continuous(limits = c(-150, 600), breaks = seq(-150, 600, 150))

qplot(data = FY_Ref, x = Net_Value, binwidth = 2.5,
      xlab = 'Net Value Last Financial Year', 
      ylab = 'Number of Transactions Across all Units', 
      color = I('black'), fill = I('#F79420')) +
  scale_x_continuous(limits = c(-50, 150), breaks = seq(-50, 150, 10))


qplot(data = FY_Ref, x = Net_Value, binwidth = 1,
      xlab = 'Net Value Last Financial Year', 
      ylab = 'Number of Transactions Across all Units', 
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(limits = c(-10, 55), breaks = seq(-10, 55, 5))


qplot(data = FY_Ref, x = Net_Value, binwidth = 0.5,
      xlab = 'Net Value Last Financial Year', 
      ylab = 'Number of Transactions Across all Units', 
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(limits = c(-10, 20), breaks = seq(-10, 20, 1))

qplot(data = FY_Ref, x = Net_Value, binwidth = 0.25,
      xlab = 'Net Value Last Financial Year', 
      ylab = 'Number of Transactions Across all Units', 
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(limits = c(-2, 10), breaks = seq(-2, 10, 1))


qplot(data = FY_Ref, x = Net_Value, binwidth = 1000,
      xlab = 'Net Value Last Financial Year', 
      ylab = 'Number of Transactions Across all Units', 
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(limits = c(100000, 150000), breaks = seq(100000, 150000, 25000))

qplot(data = FY_Ref, x = Net_Value, binwidth = 1000,
      xlab = 'Net Value Last Financial Year', 
      ylab = 'Number of Transactions Across all Units', 
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(limits = c(-10000, 15000), breaks = seq(-10000, 25000, 5000))

qplot(data = FY_Ref, x = Net_Value, binwidth = 25, geom = 'freqpoly', color = PO_Matched ) +
  scale_x_continuous(limits = c(-1000, 2500), breaks = seq(-1000, 2500, 500))

qplot(data = FY_Ref, x = Net_Value, binwidth = 25, geom = 'freqpoly', color = PO_Matched ) +
  scale_x_continuous(limits = c(-500, 1200), breaks = seq(-500, 1200, 150))

#-----------------------------------------



qplot(x = GL_Operating_Unit_ID, data = FY_Ref)

qplot(x = Invoice_VAT_Line_Amount, data = FY_Ref) +
  scale_x_continuous(limits = c(-20, 100), breaks = seq(-20, 100, 10))

# 

table(Invoice_Number)

#------------------------Payment_Date--------------------

qplot(x = Payment_Date, data = FY_Ref,  color = I('antiquewhite'), fill = I('#099DD9')) 

ggplot(FY_Ref, aes(x= Payment_Date, y= Net_Value )) + geom_point()

ggplot(FY_Ref, aes(x= Payment_Date, y= Net_Value )) + geom_line()

ggplot(FY_Ref, aes(x= Payment_Date, y= Net_Value )) + geom_bar()

qplot(x = Payment_Date, data = FY_Ref,  color = I('antiquewhite'), fill = I('#099DD9')) +

# Addressing issues with the month July----------------------------------------

ggplot(Jul_Ref, aes(x= Payment_Date, y= Net_Value )) + geom_point()

# checking if your Jul ref 3 is working

ggplot(Jul_Ref3, aes(x= Payment_Date, y= Net_Value )) + geom_point()

ggplot(Jul_Ref5, aes(x= Payment_Date, y= Net_Value )) + geom_point()

# it fixed most of the problem but I realised there is some other outlier at around jul 15. 

  qplot(x = Payment_Date, data = Jul_Ref,  color = I('antiquewhite'), fill = I('#099DD9')) 

qplot(x = Payment_Date, data = Jul_Ref,  color = I('antiquewhite'), fill = I('#FFFF33')) 

qplot(x = Net_Value, data = Jul_Ref, 
      xlab = 'Net Value July 17', 
      ylab = 'Number of Transactions Across all Units',
      color = I('antiquewhite'), fill = I('#FFFF33'))  +
  scale_x_continuous(limits = c(200000, 650000), breaks = seq(200000, 650000, 150000))

qplot(x = Net_Value, data = Jul_Ref, 
      xlab = 'Net Value July 17', 
      ylab = 'Number of Transactions Across all Units',
      color = I('antiquewhite'), fill = I('#FFFF33'))  +
  scale_x_continuous(limits = c(200000, 650000), breaks = seq(200000, 650000, 150000))



#qplot(x = Net_Value, data = Jul_Ref, bindwith = 500000,
      #xlab = 'Net Value July 17', 
      #ylab = 'Number of Transactions Across all Units',
      #color = I('antiquewhite'), fill = I('#FFFF33'))  +
  #scale_x_continuous(limits = c(200000, 650000), breaks = seq(200000, 650000, 150000))


# I will reproduce the exact same thing on the negative side and if they seem to replicate each other 
# then I will create them as actual varibales and then test if those variables are identical and if they are 
# it means that it is cancelled out, which means that these are payments that are implemented in order to adjust for errors

qplot(x = Net_Value, data = Jul_Ref,
      xlab = 'Net Value July 17', 
      ylab = 'Number of Transactions Across all Units',
      color = I('antiquewhite'), fill = I('#FFFF33'))  +
  scale_x_continuous(limits = c(-650000, -200000), breaks = seq(-650000, -200000, 150000))





Test_Jul <- qplot(x = Net_Value, data = Jul_Ref, 
      xlab = 'Net Value July 17', 
      ylab = 'Number of Transactions Across all Units',
      color = I('antiquewhite'), fill = I('#FFFF33'))  +
  scale_x_continuous(limits = c(200000, 650000), breaks = seq(200000, 650000, 150000))



Test_Julb <- qplot(x = Net_Value, data = Jul_Ref, 
                  xlab = 'Net Value July 17', 
                  ylab = 'Number of Transactions Across all Units',
                  color = I('antiquewhite'), fill = I('#FFFF33'))  +
  scale_x_continuous(limits = c(-650000, -200000), breaks = seq(-650000, -200000, 150000))




by(Jul_Cin$Net_Value,Jul_Cin$Payment_Date, summary)
# the above van be used to find any issues with any particular month. 


by(Jul_Cin3$Net_Value,Jul_Cin3$Payment_Date, summary)

#---------------Supplier Problem

qplot(x = Net_Value, data = Jul_Cin,
      xlab = 'Net Value July 17', 
      ylab = 'Jiansu Sunshine',
      color = I('antiquewhite'), fill = I('#FFFF33'))


# the soltuion is anythng aboe and below this number 702363.4 for supplier_ID= 3844494 is an error and should be removed

#qplot(data = FY_Ref, x = Net_Value, binwidth = 100, color = I('black'), fill = I('#CC0000'), 
     # xlab = 'Net Value Last Financial Year', 
     # ylab = 'Number of Transactions Across all Units', 
     # color = I('black'), fill = I('#F79420')) +
  #scale_x_continuous(limits = c(-200000, 200000), breaks = seq(-200000, 200000, 50000))




breaks = seq(0, 700, 50)

ggplot(FY_Ref)

qplot(x = Payment_Date, data = FY_Ref) +
  scale_x_discrete(breaks = 1:4)
# the above doesnt work 
qplot(x = Payment_Date, data = FY_Ref) +
  facet_wrap(~GL_Operating_Unit_ID, ncol=3)

qplot(x = Payment_Date, data = FY_Ref) +
  facet_wrap(~PO_Matched, ncol=3)


qplot(x = Payment_Date, data = FY_Ref) +
  facet_wrap(~PO_Matched, ncol=3)

sp <- ggplot(FY_Ref, aes(x= Payment_Date, y= Net_Value )) + geom_point()

sp + facet_grid(GL_Operating_Unit_ID ~  ., scales= "free", space = "free")
# the above code is not soo bad but it doesnt show me the reslts 

sp + facet_grid(Supplier_ID ~  ., scales= "free", space = "free")
#-----------
a[!(a %in% b)]

FY_Ref$Net_Value[!(FY_Ref$Net_Value %in% FY_Ref$Net_Value)]


#-------------------------------Redfern-------------------------------------------------

# the supplier ID for Redfern is 2875856

Redfern <- subset(FY_Ref, Supplier_ID == 2875856)

Redferm_moj <- subset(FY_Ref, Supplier_ID == 2875856 & GL_Operating_Unit_ID == 230)
Redfern_nms <- subset(FY_Ref, Supplier_ID == 2875856 & GL_Operating_Unit_ID == 210)
Redfern_cts <- subset(FY_Ref, Supplier_ID == 2875856 & GL_Operating_Unit_ID == 250)
Redfern_opg <- subset(FY_Ref, Supplier_ID == 2875856 & GL_Operating_Unit_ID == 470)



qplot(data = Redferm_moj, x = Net_Value)
qplot(x = Net_Value, data = Redferm_moj, xlim = c(0, 700))

qplot(data = Redferm_moj, x = Net_Value, binwidth = 25) +
  scale_x_continuous(limits = c(0, 700), breaks = seq(0, 700, 50)) 


qplot(data = Redfern, x = Net_Value, binwidth = 25) +
  scale_x_continuous(limits = c(0, 700), breaks = seq(0, 700, 50)) + 
  facet_wrap(~GL_Operating_Unit_ID)

qplot(data = Redfern, x = Net_Value, binwidth = 25) +
  scale_x_continuous(limits = c(0, 700), breaks = seq(0, 700, 50)) + 
  facet_wrap(~PO_Matched)

qplot (x = Net_Value, data = subset(Redfern, Invoice_Number== 3651), binwidth = 25) +
  scale_x_continuous(limits = c(0, 700), breaks = seq(0, 700, 50)) + 
  facet_wrap(~GL_Operating_Unit_ID)

qplot (x = Net_Value, data =Redfern, binwidth = 25) +
  scale_x_continuous(limits = c(0, 700), breaks = seq(0, 700, 50)) + 
  facet_wrap(~Invoice_Number)

qplot(x = Net_Value, data = Redferm_moj, 
      color = I('black'), fill = I('#099DD9'))






# the above is pretty decent , I think Nisha will like it. 

gpc <-  qplot(data = Redfern, x = Net_Value, binwidth = 25, geom = 'freqpoly', color = GL_Operating_Unit_ID ) +
  scale_x_continuous(limits = c(-10000, 0,), breaks = seq(-10000, 0, 50))
table(FY_Ref$GL_Operating_Unit_ID)


by(FY_Ref$Supplier_ID,FY_Ref$PO_Matched, summary)

ggplot(aes(x = Net_Value), data = Redferm_moj) +
  geom_histogram()
  
qplot(data = Redferm_moj, x = Payment_Date)


table(Redferm_moj$Invoice_Number)
table(Redfern$GL_Operating_Unit_ID)
by(Redfern$Net_Value, Redfern$PO_Matched, summary)
by(Redferm_moj$Net_Value, Redferm_moj$PO_Matched, summary)

by(Redfern$Net_Value, Redfern$GL_Operating_Unit_ID, summary)
#you might have to repeat the above taking into consideration outliers, so you need to find a way of droping values

Redfern_lim <- Redfern$Net_Value < 1000

summary(Redfern_lim)

summary(Redfern$Invoice_Number)

qplot(x = Payment_Date, data = Redferm_moj, 
      xlab = 'Last Financial Year', 
      ylab = 'Number of Redfern transactions at MoJ', 
      color = I('black'), fill = I('#F79420'))

qplot(x = Net_Value, data = Redferm_moj, bindwidth = 1, 
      color = I('black'), fill = I('#5760AB'))

summary(log10(Redferm_moj$Net_Value))

p1 <- qplot(x = Net_Value, data = Redferm_moj)
p2 <- qplot(x = log10(Net_Value + 1), data = Redferm_moj)
p3 <- qplot(x = sqrt(Net_Value), data = Redferm_moj)

grid.arrange(p1, p2, p3, ncol = 1)

 #ggplot(aes(x = Payment_Date, y = Net_Value, fill = Payment_Date, data = Redferm_moj)) + geom_line

ggplot(data = Redfern, aes(x = Payment_Date, y = Net_Value)) +
  geom_line() + scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, 50))

# the above looks ok

qplot(data = Redfern, x = Net_Value, binwidth = 25) +
  scale_x_continuous(limits = c(0, 700), breaks = seq(0, 700, 50)) + 
  facet_wrap(~GL_Operating_Unit_ID)

qplot(data = Redfern, x = Net_Value, binwidth = 25, geom = 'freqpoly', colour = GL_Operating_Unit_ID ) +
  scale_x_continuous(lim = c(0, 700), breaks = seq(0, 700, 50)) 

qplot(data = Redfern, x = Payment_Date, y = ..count../sum(..count..), 
      binwidth = 25, geom = 'freqpoly', colour = GL_Operating_Unit_ID )

qplot(x = Net_Value, data = Redfern, binwidth = 25,
      geom = 'freqpoly', colour = PO_Matched ) +
  scale_x_continuous(lim = c(0, 700), breaks = seq(0, 700, 50)) 

by(Redfern$Net_Value, Redfern$GL_Operating_Unit_ID, sum)
by(Redfern$Net_Value, Redfern$PO_Matched, sum)  
 
#Box-plots son-----------#

qplot(x = PO_Matched, y = Net_Value,
       data = Redfern, geom ='boxplot', ylim = c(0, 1000))

qplot(x = PO_Matched, y = Net_Value,
      data = Redfern, geom ='boxplot', ylim = c(0, 1000))


#-----------------------------------Enterprise Rent a Car------------------------------
# the supplier ID for Enterprise rent a c ar is 3839975

Enterprise <- subset(FY_Ref, Supplier_ID == 3839975)
Enterprise_moj <- subset(FY_Ref, Supplier_ID == 3839975 & GL_Operating_Unit_ID == 230)

qplot(data = Enterprise_moj, x = Net_Value)
qplot(x = Net_Value, data = Enterprise_moj, xlim = c(0, 700))

qplot(data = Enterprise_moj, x = Payment_Date)




# the above is a good measure actually, it can tell you were total spend is concentrated among the different operating units, it might be a good idea to trip it a bit 

qplot(x = Supplier_ID, data =)
# the other captures which suppliers have paid the most essentially
qplot(x = Payment_Date, data = )

#tp adjust the x_axis we do the following

#----Clearing Environment-run this every time---------
rm(cts_ou)
rm(Enterprise)
rm(Enterprise_moj)
rm(FY_17_18)
rm(FY_New)
rm(FY_Ref2)
rm(jul_17)
rm(moj_ou)
rm(nms_ou)
rm(opg_ou)
rm(Redferm_moj)
rm(Redfern)
rm(Redfern_cts)
rm(Redfern_opg)
rm(sapplier3)
  rm(Redfern_nms)
  rm(saplier_m1)
  rm(saplier_m2)
  rm(sapplier4)
  rm(Test_Jul)
    rm(Jul_Ref)
    rm(Jul_Cin)
    rm(Vat_test)
    rm(zeros)
    rm(Test_Julb)
    
    rm(finale2_yes_no)
      rm(FY_Exp)
  
#------------GPC card question Can you weed out GPC cards on the non po spend!? 
#GPC cards haev GL_business_Unit ID = 10220003

# Investigations--------------------
# instead of going month for month, I will just remove any outliers generically without diving into who the suppliers are 

FY_Ref1 <- FY_Ref %>%
  filter(Net_Value <= 500000)
FY_Ref2 <- FY_Ref1 %>%
  filter(Net_Value >= -500000)
ggplot(FY_Ref2, aes(x= Payment_Date, y= Net_Value )) + geom_point()

rm(FY_Ref2)


FY_GPC <- FY_Ref %>%
  filter(GL_Business_Unit_ID == 10220003)




table(FY_GPC$GL_Op)

k

ggplot(FY_GPC, aes(x= Payment_Date, y= Net_Value )) + geom_point()


qplot(data = FY_GPC, x = Net_Value, binwidth = 25, geom = 'freqpoly', color = PO_Matched ) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))


qplot(data = FY_GPC, x = Net_Value, y = ..count../sum(..count..), 
      binwidth = 25, geom = 'freqpoly', colour = PO_Matched) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))


FY_GPC$SNet_Value <- sum(FY_GPC$Net_Value)

qplot( data = FY_GPC, x = Net_Value, y = Net_Value/sum(SNet_Value), 
      binwidth = 25, geom = 'freqpoly', colour = PO_Matched) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))



qplot( data = FY_GPC, x = Net_Value/SNet_Value, y = ..count../sum(..count..),
       geom = 'freqpoly', colour = PO_Matched) 


qplot( data = FY_GPCA, x = Net_Value, y = Proportion,
       geom = 'freqpoly', colour = PO_Matched) 



ggplot(FY_GPC, aes(x= Payment_Date, y= Net_Value/SNet_Value )) + geom_point()
# the above wors really well whereas the propriotn business below doesnt work so well...

ggplot(FY_GPCA, aes(x= Payment_Date, y= Proportion )) + geom_point()

 FY_GPCA <- FY_GPC %>%
  group_by(Net_Value) %>%
 mutate(Count = n()) %>%
  mutate(Proportion = Count/sum(Count)) 
   

 FY_Ref <- FY_Ref %>%
   group_by(Net_Value) %>%
   mutate(Count = n()) %>%
   mutate(Proportion = Count/sum(Count)) 
 
 
 
 qplot(data = FY_GPCA, x = Net_Value, y = Proportion, 
       binwidth = 25, geom = 'freqpoly', colour = PO_Matched) +
   scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))
 
 
 
rbind2()
 
 qplot(data = FY_GPCA, x = Net_Value, y = Proportion, 
       binwidth = 25, geom = 'freqpoly', colour = PO_Matched) +
   scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))
  
   
 FY_GPC2 <- subset(FY_GPC, !is.na(Net_Value))
 
 nrow(FY_GPC) - nrow(FY_GPCA)

 zeros <- FY_Exp %>%
   
   filter(Net_Value == 0)
 
 FY_GPC$proportion <- 0
 
 FY_GPC$proportion <- FY_GPC$Net_Value/FY_GPC$SNet_value


# Solution to the negatives---------------
 rm()

v1 <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10")

v2 <- c(-1000, -200, -100, 100, 100, 300, 1000, 1000, -100, 100)

delete <- c("yes", "no", "yes", "yes", "yes", "no", "yes", "no", "yes", "no")



df <- data.frame(v1 = v1, v2 = v2, delete = delete)


rm(df2)

df2 <- df %>%
  
  group_by(v2) %>%
  
  mutate(cs = row_number())%>%
  
  mutate(abs = abs(v2)) %>%
  
  group_by(abs, cs)%>%
  
  mutate(rs = n()) 

%>%
  
  filter(rs == 1) %>%
  
  View()

df %>%
  
  group_by(v2) %>% # group by the actual value
  
  mutate(cs = row_number()) %>% # count how many times the actual value appeared so far
  
  mutate(abs = abs(v2)) %>% # get the absolute value
  
  group_by(abs, cs) %>% # group by the absolute value and how many times each actual number appears
  
  mutate(rs = n()) %>% # count how many rows there are in that group
  
  filter(rs == 1) %>% # if cumulative freuqency and absolute value are equal, they are a duplicate
  
  View()

Jul_Sam <- Jul_Ref %>%
  
  group_by(Net_Value) %>%
  
  mutate(cs = row_number()) %>%
  
  mutate(abs = abs(Net_Value)) %>%
  
  group_by(abs, cs) %>%
  
  mutate(rs = n()) %>%
  filter(rs == 1)


Jul_Sam3 <- Jul_Ref5 %>%
  group_by(Net_Value) %>%
  mutate(cs = row_number()) %>%
  mutate(abs = abs(Net_Value)) %>%
  group_by(abs, cs) %>%
  mutate(rs = n()) %>%
  filter(rs == 1)



# I think the above code works son!  


 
    qplot(data = Jul_Sam, x = Net_Value, geom = 'freqpoly', colour = PO_Matched) 
    
    ggplot(Jul_Sam, aes(x= Payment_Date, y= Net_Value)) + geom_point()
    
    ggplot(Jul_Ref5, aes(x= Payment_Date, y= Net_Value)) + geom_point()
    

    scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))
  
  
    julSamNet <- data.frame(Net_Value = Jul_Sam$Net_Value,datez = Jul_Sam$Payment_Date)

    julRefNet  <- data.frame(Net_Value = Jul_Ref5$Net_Value, datez = Jul_Ref5$Payment_Date)
    
    rm(Jul_Ref)

    discrep <- mapply(setdiff, Jul_Ref5, Jul_Sam)
    
    
  commonID <- intersect(Jul_Sam$Net_Value, Jul_Ref5$Net_Value)
  
  things_not_acounted <- Jul_Ref5[!Jul_Ref5$Net_Value %in% commonID, ]
  
  
  # you can use the above to tell them which are the things that you took out
  
  
  #ggplot(Jul_Sam, aes(x= Payment_Date, y= Net_Value)) + geom_point() + 
  #  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100, 100, 25))
  
  
  #ggplot(Jul_Ref5, aes(x= Payment_Date, y= Net_Value)) + geom_point() + 
    #scale_y_continuous(limits = c(-100, 100), breaks = seq(-100, 100, 25))
  
  
  
  

 
  things_not_acounted2 <- things_not_acounted %>%
    group_by(Net_Value) %>%
    mutate(cs = row_number()) %>%
    mutate(abs = abs(Net_Value))  %>%
    group_by(abs, cs) %>%
    mutate(rs = n())  %>%
    filter(rs == 1)
  
  Hays_acc <- Jul_Sam3 %>%
    filter(Supplier_ID == 3842259)
  
  table(Hays_acc$`GL Operating_Unit_Name`)

# add supplier_ID to CS
  
  #--------------------------Massive Revelation Getting Rid of duplicates on opposite spectrums of Cartesian graph
  
  # the following is a technique to get rid of the repetitions
  2+2
  FY_NRef <- FY_Ref %>%
    
    group_by(Net_Value) %>%
    
    mutate(cs = row_number()) %>%
    
    mutate(abs = abs(Net_Value)) %>%
    
    group_by(abs, cs) %>%
    
    mutate(rs = n()) %>%
    filter(rs == 1)
  
  commonID <- intersect(Jul_Sam2$Net_Value, Jul_Sam3$Net_Value)
  
  things_not_acounted <- Jul_Sam2[!Jul_Sam2$Net_Value %in% commonID, ]
  
  rm(things_not_acounted2)
  
  #-------------From now one, you will have to work with FY_NRef only
  ggplot(FY_NRef, aes(x= Payment_Date, y= Net_Value)) + geom_point()
  
  # when doing the above, I discovered that there was something going on in October
  
#-------------October investiation0-------------------
  ggplot(zoomococt4, aes(x= Payment_Date, y= Net_Value)) + geom_point()
  
  zoomococt <- oct_ref %>%
    filter(Net_Value > 1000000)
  zoomococt2 <- oct_ref %>%
    filter(Net_Value < -1000000)
  zoomococt3 <- rbind2(zoomococt2, zoomococt)
  
  installed.packages("lubridate")
  library(lubridate)
  
  ggplot(zoomococt3, aes(x= Payment_Date, y= Net_Value)) + geom_point()
  
  # the following is technique to see what you have taken out
  
  zoomococt4 <- oct_ref %>%
    
    group_by(Net_Value) %>%
    
    mutate(cs = row_number()) %>%
    
    mutate(abs = abs(Net_Value)) %>%
    
    group_by(abs, cs) %>%
    
    mutate(rs = n()) %>%
    filter(rs == 1)
  
  
  
  #---------------------Midway date exraction technique------------------------------
  dudness <- FY_Ref %>% select(Payment_Date, Net_Value, GL_Operating_Unit_ID) %>%
    filter(Payment_Date >= as.Date("2017-10-02") )    
  
  rm(Jul_Cin)
  # the above works, it lets you extract dates from whichever facking time you want son!!
table(dudness$Payment_Date)
dudness <- FY_Ref %>% select(Payment_Date, Net_Value, GL_Operating_Unit_ID) %>%
  filter(Payment_Date >= as.Date("2017-10-02")) %>% 
           filter(Payment_Date <= as.Date("2017-10-31"))    
# the above code gets you precisely the dates in between dates, so the above got you precisely the month of October, Michael you are a beast!!
# inside the select are the things ou want to keep as well heeheh man this is exciting 

# I need to go home and really think about my codes!   



tableplot(FY_Ref, select = c(Supplier_ID, PO_Matched, GL_Operating_Unit_ID))





#-------------I will use the 75 Percentile rule or less to investigate

ggplot(data = FY_NRef, mapping = aes(x = PO_Matched, y= Net_Value)) + geom_boxplot()

ggplot(data = FY_NRef) +
  geom_boxplot(mapping = aes(x = reorder(PO_Matched, Net_Value, FUN = median), y = Net_Value))
# it still looks dirty 

ggplot(FY_NRef, aes(x= Payment_Date, y= Net_Value)) + geom_point()
# ---- I am just going to filter these out and then I will create a dataset that contains all of the ones that have been filtered out #

FY_NRef2 <- FY_NRef %>%
  
  filter(Net_Value <= 10000000)


FY_NRef3 <- FY_NRef2 %>%
  
  filter(Net_Value >= -10000000)

rm(FY_NRef)
rm(FY_NRef2)

ggplot(FY_NRef3, aes(x= Payment_Date, y= Net_Value)) + geom_point()

Sample1 <-  FY_NRef3 %>%
  
  filter(Supplier_ID ==  3840029)

2+2

#I took a supplier that does not have such a large expenditure in order to see whether my hunch of money 
#being returned is a little less and that seems to be the general trend here. 
#Maybe they have some sort of prior arrangement but it needs to get investigated. 

# Maybe you can create some type of algorithm that groups by invoice number and sees by how much they are losing money 
# so you need to first filter out all the negatives then see where they correspiond 
# I thik I will stop the detting rid of things now.. 

#----------------Process of creating percentages-----------------------------

ggplot(FY_NRef3, aes(x= Payment_Date, y= Net_Value/SNet )) + geom_point()

qplot(data = FY_NRef3, x = Net_Value, binwidth = 25, geom = 'freqpoly', color = PO_Matched ) +
  scale_x_continuous(limits = c(-1000, 2500), breaks = seq(-1000, 2500, 500))


qplot(data = FY_NRef3, x = Net_Value, binwidth = 25, geom = 'freqpoly', color = PO_Matched ) +
  scale_x_continuous(limits = c(-10, 50), breaks = seq(-10, 50, 5))

qplot(data = FY_NRef3, x = Perc, geom = 'freqpoly', color = PO_Matched )  +
  scale_x_continuous(limits = c(0, 0.009), breaks = seq(0, 0.009, 0.002))



FY_NRef3$SNet <- sum(FY_NRef3$Net_Value)

FY_NRef3$Perc <- FY_NRef3$Net_Value/FY_NRef3$SNet

qplot(x = PO_Matched, y = Net_Value,
      data = FY_NRef3, geom ='boxplot', ylim = c(0, 1000))
#------ Limiting dataset to less than 600 because of the above histogram 



FY_Extr <-  FY_NRef3 %>%
  
  filter(Net_value <= 600)


#---------- Abit of clearing ----------------

FY_NRef3$UNSPSC_Code <- NULL
FY_NRef3$Payment_Amount <- NULL




#------------------BY OPERATING UNIT------------------

#------Moj--------------
moj_ou <- finale2[ finale2$GL_Operating_Unit_ID == 230, ]


#FY_Extr <-  FY_NRef3 %>%
  
  #filter(Net_value <= 600)

#the above created a subset which is MOJ operating unit only 
# mOJ HAS ONLY 20474 OBSEVATIONS 

moj_ou$SNet <- sum(moj_ou$Net_Value)

moj_ou$Perc <- moj_ou$Net_Value/FY_NRef3$SNet


moj_ou_no <-  moj_ou %>%
  
  filter(PO_Matched == 'No')

ggplot(moj_ou, aes(x= Payment_Date, y= Perc )) + geom_point()

rm(moj_ou_no)

moj_ou_no <-  moj_ou %>%
  
  filter(PO_Matched == 'No')
#here are only 3844 Moj observations that are non po mathced which is 18.78 % of entire moj spend

moj_ou_yes <-  moj_ou %>%
  
  filter(PO_Matched == 'Yes')
#there are 

rm(moj_ou_yes)
rm(moj_ou)

#dudness <- FY_Ref %>% select(Payment_Date, Net_Value, GL_Operating_Unit_ID) %>%
 # filter(Payment_Date >= as.Date("2017-10-02")) %>% 
  #filter(Payment_Date <= as.Date("2017-10-31")) 

ggplot(moj_ou, aes(x= Payment_Date, y= Net_Value/SNet )) + geom_bar()

#--------------------------------CTS-------------------------


cts_ou <- FY_NRef3[ FY_NRef3$GL_Operating_Unit_ID == 250, ]

ggplot(cts_ou, aes(x= Payment_Date, y= Perc )) + geom_point()


cts_ou_no <-  cts_ou %>%
  
  filter(PO_Matched == 'No')
rm(cts_ou_no)

ggplot(cts_ou_no, aes(x= Payment_Date, y= Perc )) + geom_point()


# there are 85763 observatiosn which are NO PO for HMCTS which is 36.6% of total CTS spend
85763/234579
rm(cts_ou_no)


cts_ou_yes <-  cts_ou %>%
  
  filter(PO_Matched == 'Yes')
148816/234579

85763/234579 +148816/234579

rm(cts_ou_yes)
rm(cts_ou)
#-------------------------NMS--------------------

nms_ou <- FY_NRef3[ FY_NRef3$GL_Operating_Unit_ID == 210, ]

ggplot(nms_ou, aes(x= Payment_Date, y= Perc )) + geom_point()

# noms is the biggest
576980/926175
179176/576980
nms_ou_no <-  nms_ou %>%
  
  filter(PO_Matched == 'No')
rm(nms_ou_no)

ggplot(nms_ou_no, aes(x= Payment_Date, y= Perc )) + geom_point()


# there are 85763 observatiosn which are NO PO for HMCTS which is 36.6% of total CTS spend
85763/234579
rm(nms_ou_no)


nms_ou_yes <-  nms_ou %>%
  
  filter(PO_Matched == 'Yes')


rm(nms_ou_yes)
rm(nms_ou)

#---------------------OPG-----------------------------------


opg_ou <- FY_NRef3[ FY_NRef3$GL_Operating_Unit_ID == 470, ]

# OPG is quite small with only 11840 observations of which only 2.89% are Non PO

342/11840

ggplot(opg_ou, aes(x= Payment_Date, y= Perc )) + geom_point()


opg_ou_no <- opg_ou %>%
  
  filter(PO_Matched == 'No')

rm(opg_ou_no)

ggplot(opg_ou_no, aes(x= Payment_Date, y= Perc )) + geom_point()


# there are 85763 observatiosn which are NO PO for HMCTS which is 36.6% of total CTS spend
85763/234579
rm(opg_ou_no)


opg_ou_yes <-  opg_ou %>%
  
  filter(PO_Matched == 'Yes')


rm(opg_ou_yes)
rm(opg_ou)


#------------------The Other Operating Units

laa_ou <- FY_NRef3[ FY_NRef3$GL_Operating_Unit_ID == 290, ]


wls_ou <- FY_NRef3[ FY_NRef3$GL_Operating_Unit_ID == 350, ]

yjb_ou <- FY_NRef3[ FY_NRef3$GL_Operating_Unit_ID == 490, ]

pbd_ou <- FY_NRef3[ FY_NRef3$GL_Operating_Unit_ID == 510, ]

jac_ou <- FY_NRef3[ FY_NRef3$GL_Operating_Unit_ID == 330, ]



# OPG is quite small with only 11840 observations of which only 2.89% are Non PO



laa_ou_no <- laa_ou %>%
  
  filter(PO_Matched == 'No')

# The majority of LAA is non Po around 91.3% 
73288/80301

rm(laa_ou_no)


laa_ou_yes <-  laa_ou %>%
  
  filter(PO_Matched == 'Yes')


rm(laa_ou_yes)
rm(laa_ou)

#------------------------------------#




wls_ou_no <- laa_ou %>%
  
  filter(PO_Matched == 'No')

    rm(wls_ou_no)

    wls_ou_yes <-  wls_ou %>%
  
  filter(PO_Matched == 'Yes')

      rm(wls_ou_yes)
        rm(wls_ou)

#-----------------------------------#
        
        yjb_ou_no <- yjb_ou %>%
          
          filter(PO_Matched == 'No')
        
        rm(yjb_ou_no)
        
        yjb_ou_yes <-  yjb_ou %>%
          
          filter(PO_Matched == 'Yes')
        
        rm(yjb_ou_yes)
        rm(yjb_ou)
        

#-----------------------------------------#
        
        pbd_ou_no <- pbd_ou %>%
          
          filter(PO_Matched == 'No')
        
        rm(pbd_ou_no)
        
        pbd_ou_yes <-  pbd_ou %>%
          
          filter(PO_Matched == 'Yes')
        
        rm(pbd_ou_yes)
        rm(pbd_ou)
        
#------------------------------------------#

        
        jac_ou_no <-   jac_ou_ou %>%
          
          filter(PO_Matched == 'No')
        
        rm(jac_ou_no)
        
        jac_ou_yes <-  jac_ou %>%
          
          filter(PO_Matched == 'Yes')
        
        rm(jac_ou_yes)
        rm(jac_ou)
        
#-------------------------Change of Direction-------------------
        rm(FY_Ref2)
        FY_NEG <- FY_NRef %>%
          filter(Net_Value <= 0)
        # there are only 3575 repetitions 
        
        commonID <- intersect(FY_NEG$Invoice_Number, FY_NRef$Invoice_Number)
        
        FY_Cl <- FY_NRef[!FY_NRef$Invoice_Number %in% commonID, ]
        
        FY_Cl$cs <- NULL
        FY_Cl$abs <- NULL
        FY_Cl$rs <- NULL
        
        finale2$Payment_Amount <- NULL
        FY_nuovo$UNSPSC_Code <- NULL
        FY_nuovo$Account_Code <- NULL
        FY_nuovo$Invoice_VAT_Line_Amount <- NULL
        
        rm(FY_NRef)
        rm(FY_Ref)
        
        FY_Cl$SNet <- sum(FY_Cl$Net_Value)
        
        FY_Cl$Perc <- FY_Cl$Net_Value/FY_Cl$SNet
        
    
       ggplot(data = FY_Cl, mapping = aes(x = Payment_Date, y = Net_Value)) + geom_point()
 
       
       FY_Cl_no <-  FY_Cl %>%
         
         filter(PO_Matched == 'No')
      
    2+2
     
       moj_ou_no <-  moj_ou %>%
         
         filter(PO_Matched == 'No')
       #here are only 3844 Moj observations that are non po mathced which is 18.78 % of entire moj spend
       
       moj_ou_yes <-  moj_ou %>%
         
         filter(PO_Matched == 'Yes')
       #there are 
       
       rm(moj_ou_yes)
       rm(moj_ou)
       
       # try to find a way to create tha most frequent suppliers
       
       hh
       
       
       
       
#---------------------Getting rid of only negatives--------------------


#-------------------Resolving negatives and those with different negative values------------------


# Clearing repetions with identical values
FY_Adis<- FY_Ref %>%
  group_by(Net_Value) %>%
  mutate(cs = row_number()) %>%
  mutate(abs = abs(Net_Value)) %>%
  group_by(abs, cs) %>%
  mutate(rs = n()) %>%
  filter(rs == 1)

#isolation process
Negativs <- FY_Adis %>%
  filter(Net_Value <= 0)

FY_pos<- FY_Adis %>%
  filter(Net_Value >= 0)


# this is the process of finding common invoices number 
commonID <- intersect(FY_pos$Invoice_Number, Negativs$Invoice_Number)

commonID2 <- intersect(FY_Adis$Invoice_Number, Negativs$Invoice_Number)


# the following are pure postives that is those who do have any contamination of corrections
Pure_pos <- FY_pos[!FY_pos$Invoice_Number %in% commonID, ]

# process explained:
# one find the invoiec numbers that intersect between only positives and only negatives, drop those invoice numbers
# form the only postive sections. Extract from the holistic dataset using those invoice numbers to craete a dataset
# that has both positives and negatives, group by invoice numbers and sum them, then you join 
# the only postives with the new aggregated dataset. 

841515+81087

hje <- FY_Adis[FY_Adis$Invoice_Number %in% commonID, ]

#hjen <- FY_Adis[FY_Adis$Invoice_Number %in% commonID2, ]



#x <- 0
#if (x < 0) {
 # print("Negative number")
#} else if (x > 0) {
 # print("Positive number")
#} else
 # print("Zero")

#ss <- aggregate(hje$Net_Value, by = list(Net_Value = hje$Invoice_Number, FUN = sum)                                                                             

       

 hje2 <- tbl_df(hje) 
    rm(hjen)
#summed <- hje2[, list(sm =sum(Net_Value)), by="Invoice_number"]
      
  
    hje3 <-  hje2 %>% 
      group_by(Invoice_Number) %>%
     mutate(Net_sum = sum(Net_Value, na.rm = TRUE)) 
    hje3[, c("Net_Value","Net_sum")] <-  hje3[, c("Net_sum","Net_Value")] 
    
    lela <-  hje2 %>% 
      group_by(Invoice_Number, Payment_Date) %>%
      mutate(Net_sum = sum(Net_Value, na.rm = TRUE)) 
    
    lela1 <- lela  %>%
      group_by(Net_sum) %>%
      mutate(cs2 = n())    %>%
      group_by(Invoice_Number, cs2) %>%
      filter(cs2 == 1)
    
    lela1[, c("Net_Value","Net_sum")] <-  lela1[, c("Net_sum","Net_Value")] 
    

 # Spring cleaning----------------------------   
    Pure_pos$Payment_Amount <- NULL
    Pure_pos$abs <- NULL
    Pure_pos$cs2 <- NULL
    Pure_pos$cs<- NULL
    Pure_pos$rs <- NULL
    hje6$cs2 <- NULL
    hje6$rs <- NULL
    hje6$Net_sum <- NULL
    
    lela1$cs2 <- NULL
    lela1$Net_sum <- NULL
    finale <- rbind2(Pure_pos, lela1)
  
    #hje6 <- hje3  %>%
     # group_by(Invoice_Number) %>%
     # mutate(cs2 = n()) %>%
     # group_by(Invoice_Number, cs2) %>%
     # filter(cs2 == 1)
    
    # the above was wrong.
    
    #aggregate(formula,  data, function())
    
# bla<- aggregate(Net_Value~Invoice_Number, hje, FUN = sum)
 
# bl2<- aggregate(Net_Value~Invoice_Number, FY_Adis, FUN = sum)
 #bl2 kind of educates of the existence of invoice numbers that are purely negative, they might be corrections done from the previous year's spend
# rm(bl2)
#rm(bla) 
#rm(comoinvo)
#rm(FY_Adis)


finale2 <- rbind2(Pure_pos, lela1)
rm(FY_Adis)
rm(FY_pos)
rm(hje)
rm(hje2)
rm(hje3)
rm(hje5)

rm(Negativs)
rm(Pure_pos2)
rm(Pure_pos)
rm(hje5)

# hje6 was completely wrong and therefore dont use finale
rm(lela)
rm(hje6)
rm(finale2)
rm(commonID2)
rm(commonsq)
rm(newnet)
rm(Redfern_lim)
rm(notcommonID)

rm(lela1)
#finale2$Invoice_VAT_Line_Amount <- NULL
#finale2$UNSPSC_Code <- NULL
#finale2$cs <- NULL
=
#finale2$abs <- NULL
#finale2$rs <- NULL


  
  #----------------General trends-------------------

finale$sm <- sum(finale$Net_Value)

finale$per <- finale$Net_Value/finale$sm


tble <- data.frame(table(finale$GL_Business_Unit_Name))
tble$sum <- sum(tble$Freq)
tble$percent <- tble$Freq/tble$sum
rm(tble)


operatum <- data.frame(table(finale$`GL Operating_Unit_Name`))
# the above code is really awesome, it is a shortcut for getting frequencies by category 
operatum$sum <- sum(operatum$Freq)
operatum$percent <- operatum$Freq/operatum$sum
rm(operatum)

Supplica <- data.frame(table(finale$Vendor_Group_Name_L1))
Supplica$sum <- sum(Supplica$Freq)
Supplica$percent <- Supplica$Freq/Supplica$sum
rm(Supplica)

datess <- data.frame(table(finale$Payment_Date))
datess$sum <- sum(datess$Freq)
datess$percent <- datess$Freq/datess$sum
rm(datess)

netx <- data.frame(table(finale$Net_Value))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum
rm(netx)

netx <- data.frame(table(finale$Invoice_Number))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum

rm(netx)

netx <- data.frame(table(finale$Account_Code))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum

rm(netx)


# I will put redfern on pause for now 
#Redfern <- finale2 %>%
 # filter(Supplier_ID == 2875856 | Supplier_ID == 3841042)
##operatumred <- data.frame(table(Redfern$`GL Operating_Unit_Name`))
#rm(operatumred)
#BusinesRed <- data.frame(table(Redfern$`GL_Business_Unit_Name`))
#rm(BusinesRed)
#Timered <- data.frame(table(Redfern$Payment_Date))
#rm(Redfern)
#rm(Timered)
#rm(Cda)

you need to zoom into Unallocated buggest as a business unit name 

# the following is the code for unallocated bugets GL bussines unit id = 10207990

#rm(Redfern)
#rm(Timered)

Unallocated <- finale %>%
  filter(GL_Business_Unit_Name == 'Unallocated Budgets')

Supplica <- data.frame(table(Unallocated$Vendor_Group_Name_L1))

rm(Supplica)

operatumred <- data.frame(table(Unallocated$`GL Operating_Unit_Name`))

rm(Unallocated)
rm(operatumred)
rm(Supplica)
rm(netx)
#---------Yes PO-------------------------



rm(netx)

finale_yes <- finale %>%
  filter(PO_Matched == 'Yes')

finale_yes$sm <- sum(finale_yes$Net_Value)
finale_yes$per <- finale_yes$Net_Value/finale_yes$sm





FY_GPC <- finale2_yes %>%
  filter(GL_Business_Unit_ID == 10220003)

rm(FY_GPC)

GVA <- finale2_yes %>%
  filter(Supplier_ID == 2088425 | Supplier_ID == 3343912)
# the above code worked beautifully 


tble <- table(finale2_yes$GL_Business_Unit_Name)
tble2 <- data.frame(tble)



rm(GVA)
rm(tble2)
rm(Redfern_lim)
rm(tble)
rm(newnet)
rm(commonID)

netx<- data.frame(table(finale_yes$GL_Business_Unit_Name))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum
rm(netx)

netx <- data.frame(table(finale$`GL Operating_Unit_Name`))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum

rm(netx)


netx <- data.frame(table(finale_yes$Vendor_Group_Name_L1))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum
rm(netx)

netx <- data.frame(table(finale_yes$Payment_Date))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum
rm(netx)

netx <- data.frame(table(finale_yes$Net_Value))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum
rm(netx)

netx <- data.frame(table(finale_yes$Invoice_Number))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum

rm(netx)

netx <- data.frame(table(finale_yes$Account_Code))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum

rm(netx)


#Redfern <- finale_yes %>%
  #filter(Supplier_ID == 2875856 | Supplier_ID == 3841042)

 

# the following is the code for unallocated bugets GL bussines unit id = 10207990








#------Save and export to Excel 

write.csv(lela1, file="lela1.csv")

moj_ou <- finale2[ finale2$GL_Operating_Unit_ID == 230, ]
write.csv(moj_ou, file="lela1.csv")
rm(moj_ou)



  
#The following focuses only on Non PO ----------------------------



finale_no <- finale %>%
  filter(PO_Matched == 'No')


FY_GPC <- FY_nuovo_no %>%
  filter(GL_Business_Unit_ID == 10220003)

rm(FY_GPC)

GVA <- FY_nuovo_no %>%
  filter(Supplier_ID == 2088425 | Supplier_ID == 3343912)


# the above code worked beautifully 

netx<- data.frame(table(NMS$GL_Business_Unit_Name))
netx$sum <- sum(netx$Freq)


netx<- data.frame(table(finale_no$GL_Business_Unit_Name))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum
rm(netx)

netx <- data.frame(table(finale_no$`GL Operating_Unit_Name`))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum
rm(netx)
#
netx <- data.frame(table(finale_yes$`GL Operating_Unit_Name`))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum



netx <- data.frame(table(finale_no$Vendor_Group_Name_L1))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum

rm(netx)

netx <- data.frame(table(finale_no$Payment_Date))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum
rm(netx)

netx <- data.frame(table(finale_no$Net_Value))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum

rm(netx)

netx <- data.frame(table(finale_no$Invoice_Number))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum

rm(netx)

netx <- data.frame(table(finale_no$Account_Code))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum

acc <- finale %>%
  filter(Account_Code == '5224102148 - EXP - PURCHASE OF GOODS/SERVICES - OTHER - Translation / Interpreting Services')

netx <- data.frame(table(acc$`GL Operating_Unit_Name`))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum



netx <- data.frame(table(acc$Vendor_Group_Name_L1))
netx$sum <- sum(netx$Freq)

#Redfern <- FY_nuovo_no %>%
 ## filter(Supplier_ID == 2875856 | Supplier_ID == 3841042)


#Unallocated <- FY_nuovo_no %>%
  #filter(GL_Business_Unit_ID == 10207990)
#------------Comparative PO vs non PO-------------


qplot(data = finale, x = Net_Value, binwidth = 25, color = I('black'), fill = I('#CC0000'), 
      xlab = 'Net Value Last Financial Year', 
      ylab = 'PO vs Non PO volume Across all Units') +
  scale_x_continuous(limits = c(0, 700), breaks = seq(0, 700, 50)) + 
  facet_wrap(~PO_Matched)




qplot(x = Net_Value, data = finale_no, binwidth = 25, 
      color = I('black'), fill = I('#F79420'), 
      xlab = 'Net Value Last Financial Year', 
      ylab = 'Non PO volume Across Operating Units' ) +
scale_x_continuous(limits = c(0, 700), breaks = seq(0, 700, 50)) + 
  facet_wrap(~GL_Operating_Unit_ID, ncol=3) 


ggplot(data = finale_no, aes(x = Payment_Date, y = Net_Value) color = I('black'), fill = I('#F79420'), 
       xlab = 'Net Value Last Financial Year', 
       ylab = 'Non PO volume Across Operating Units') +
  geom_point() + scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, 50))



                     

ggplot(data = finale_no, aes(x = Payment_Date, y = Net_Value)) +
  geom_point()


interestg <- by(finale_no$Net_Value, finale_no$GL_Operating_Unit_ID, sum)

ss <- tableGrob(netx)

sevehana <- finale %>%
  filter(Net_Value <= 700)
(841663-769071)/841663
rm(sevehana_no)

sevehana_no <- finale_no %>%
  filter(Net_Value <= 700)



259809/269664

sevehana_yes <- finale_yes %>%
  filter(Net_Value <= 700)

509262 / 571999

qplot(data = finale, x = Net_Value, binwidth = 1, geom = 'freqpoly', color = PO_Matched, 
      xlab = 'Spend less than £700', 
      ylab = ' Non Po vs PO volume Across Operating Units') +
  scale_x_continuous(limits = c(0, 700), breaks = seq(-0, 700, 25))


8,301,696

qplot(data = finale, x = Net_Value, binwidth = 10000, geom = 'freqpoly', color = PO_Matched, 
      xlab = 'Spend above £700', 
      ylab = ' Non Po vs PO volume Across Operating Units', ylim = c(0, 200)) +
  scale_x_continuous(limits = c(700, 8400000), breaks = seq(700, 8400000, 500000))

rm(sevehana_yes)

fidy2 <- finale %>%
  filter(Net_Value == 378.88)
tble <- data.frame(table(fidy2$Vendor_Group_Name_L1))

NMS <- finale %>%
  filter(GL_Operating_Unit_ID == 210)



£378.88

qplot(x = GL_Operating_Unit_ID, data = finale) +
  facet_wrap(~PO_Matched, ncol=3)


qplot(x = Net_Value, data = acc) +
  facet_wrap(~PO_Matched, ncol=3, 
             xlab = 'PO Match', 
             ylab = ' Non Po vs PO volume Across Operating Units')

qplot(data = acc, x = Net_Value, binwidth = 1, geom = 'freqpoly', color = PO_Matched, 
      xlab = 'Spend less than £700', 
      ylab = ' Non Po vs PO volume Interpreting Services') +
  scale_x_continuous(limits = c(0, 700), breaks = seq(-0, 700, 50))



