#-------------------------Installations-------------------------------
install.packages("tidyverse")
library("tidyverse")
install.packages('gridExtra')
library(gridExtra)
library(ggplot2)

#---------------------------------------------------------Codes used repetetively------------------------------------------------------------------------------
  

  #The following was used to upload data from the amazon cloud to R
  
  jan_18 <- s3tools::read_using(
    FUN = readxl::read_xlsx,
    s3_path = "alpha-ccma-2018/01_Jan_2018.xlsx",
    guess_max = 1000000
  )
 
  #The following technique was used to get rid erroneous invoices and their corrections
  
  df %>%
    
    group_by(v2) %>% # group by the actual value
    
    mutate(cs = row_number()) %>% # count how many times the actual value appeared so far
    
    mutate(abs = abs(v2)) %>% # get the absolute value
    
    group_by(abs, cs) %>% # group by the absolute value and how many times each actual number appears
    
    mutate(rs = n()) %>% # count how many rows there are in that group
    
    filter(rs == 1) %>% # if cumulative freuqency and absolute value are equal, they are a duplicate
    
    View()
  
  # I used the following technique to change the column names because R doesn't read spaces well
  colnames(FY_17_18)[colnames(FY_17_18)=="Catalogue Item"] <- "Catalogue_Item"
  # I used the following technique to drop certain columns because they were not so useful
  FY_Exp$UNSPSC_Category_L1 <- NULL
#-------------------------------------------------Cleaning data shortened----------------------------------------------------------
 
 # Firstly, combined consequtive months together 
may_jun <- rbind2(may_17_1, jun_17)
  # then combined them into quarters
Q2_17 <- rbind2(apr_17, may_jun)

# then combined quarters into semi-annual
Q4_17_Q1_18 <- rbind2(Q4_17, Q1_18)

#then combined the semi-annuals into year

FY_Exp <- rbind2(Q2_17_Q3_17, Q4_17_Q1_18)

# Took out all the zero net values since they were found to be tax rebates
.
FY_Ref <- FY_Exp %>%
  
  filter(Net_Value != 0)



#------------------------Creation of Operating Units--------------------------------------------------------- 

# the following was not used in the analysis but perhaps could be. 

#moj_ou <- FY_Ref[ FY_Ref$GL_Operating_Unit_ID == 230, ]
#the above created a subset which is MOJ operating unit only 
#cts_ou <- FY_Ref[ FY_Ref$GL_Operating_Unit_ID == 250, ]
#nms_ou <- FY_Ref[ FY_Ref$GL_Operating_Unit_ID == 210, ]
#opg_ou <- FY_Ref[ FY_Ref$GL_Operating_Unit_ID == 470, ]



#-----------------Net_Value qplots-----------------------------------


qplot(data = FY_Ref, x = Net_Value, binwidth = 100, color = I('black'), fill = I('#CC0000'), 
      xlab = 'Net Value Last Financial Year', 
      ylab = 'Number of Transactions Across all Units', 
      color = I('black'), fill = I('#F79420')) +
   scale_x_continuous(limits = c(-200000, 200000), breaks = seq(-200000, 200000, 50000))



qplot(data = FY_Ref, x = Net_Value, binwidth = 25, geom = 'freqpoly', color = PO_Matched ) +
  scale_x_continuous(limits = c(-500, 1200), breaks = seq(-500, 1200, 150))


#------------------------Payment_Date--------------------

# variations of the following wqs used to understand outliers
ggplot(FY_Ref, aes(x= Payment_Date, y= Net_Value )) + geom_point()


# facet wraps are an interesting way to isolate by categorically 

qplot(x = Payment_Date, data = FY_Ref) +
  facet_wrap(~GL_Operating_Unit_ID, ncol=3)

qplot(x = Payment_Date, data = FY_Ref) +
  facet_wrap(~PO_Matched, ncol=3)



  
  
  #---------------------Midway date exraction technique------------------------------
  dudness <- FY_Ref %>% select(Payment_Date, Net_Value, GL_Operating_Unit_ID) %>%
    filter(Payment_Date >= as.Date("2017-10-02") )    
 
  # the above works, it lets you extract dates from whichever time period you want


dudness <- FY_Ref %>% select(Payment_Date, Net_Value, GL_Operating_Unit_ID) %>%
  filter(Payment_Date >= as.Date("2017-10-02")) %>% 
           filter(Payment_Date <= as.Date("2017-10-31"))    
# the above code gets you precisely the dates in between dates, so the above got you precisely the month of October
# inside the select are the things ou want to keep as well heeheh man this is exciting 


#-------------I will use the 75 Percentile rule or less to investigate

ggplot(data = FY_Ref, mapping = aes(x = PO_Matched, y= Net_Value)) + geom_boxplot()


#----------------Process of creating percentages-----------------------------

ggplot(FY_NRef3, aes(x= Payment_Date, y= Net_Value/SNet )) + geom_point()

qplot(data = FY_NRef3, x = Net_Value, binwidth = 25, geom = 'freqpoly', color = PO_Matched ) +
  scale_x_continuous(limits = c(-1000, 2500), breaks = seq(-1000, 2500, 500))




FY_NRef3$SNet <- sum(FY_NRef3$Net_Value)

FY_NRef3$Perc <- FY_NRef3$Net_Value/FY_NRef3$SNet

# the following was not used in the analysis but

  
#moj_ou_no <-  moj_ou %>%
  
  #filter(PO_Matched == 'No')
#here are only 3844 Moj observations that are non po mathced which is 18.78 % of entire moj spend

#moj_ou_yes <-  moj_ou %>%
  
  #filter(PO_Matched == 'Yes')
#there are 



#dudness <- FY_Ref %>% select(Payment_Date, Net_Value, GL_Operating_Unit_ID) %>%
 # filter(Payment_Date >= as.Date("2017-10-02")) %>% 
  #filter(Payment_Date <= as.Date("2017-10-31")) 

# the following is not included it does give an interesting graph
ggplot(moj_ou, aes(x= Payment_Date, y= Net_Value/SNet )) + geom_bar()


#------------------The Other Operating Units

#laa_ou <- FY_NRef3[ FY_NRef3$GL_Operating_Unit_ID == 290, ]
#wls_ou <- FY_NRef3[ FY_NRef3$GL_Operating_Unit_ID == 350, ]
#yjb_ou <- FY_NRef3[ FY_NRef3$GL_Operating_Unit_ID == 490, ]
#pbd_ou <- FY_NRef3[ FY_NRef3$GL_Operating_Unit_ID == 510, ]
#jac_ou <- FY_NRef3[ FY_NRef3$GL_Operating_Unit_ID == 330, ]



# OPG is quite small with only 11840 observations of which only 2.89% are Non PO
#laa_ou_no <- laa_ou %>%
 #filter(PO_Matched == 'No')

# The majority of LAA is non Po around 91.3% 
#73288/80301


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


# the following are pure postives that is those who do not  have any contamination of corrections
Pure_pos <- FY_pos[!FY_pos$Invoice_Number %in% commonID, ]

# process explained:
# once you find the invoice numbers that intersect between only positives and only negatives, drop those invoice numbers
# fromm the only postive sections. Extract from the holistic dataset using those invoice numbers to create a dataset
# that has both positives and negatives, group by invoice numbers and sum them, then you join 
# the only postives with the new aggregated dataset. 



hje <- FY_Adis[FY_Adis$Invoice_Number %in% commonID, ]
# the above is all the datasets that have contamination then change it to a table format so that it can resemble some kind of matrix which wil be easier to manipulate
 hje2 <- tbl_df(hje) 

      
  # I sum the contaminated data grouped by invoice number since the negative values are implemented such that in aggregate the error gets cancelled out
    
    lela <-  hje2 %>% 
      group_by(Invoice_Number, Payment_Date) %>%
      mutate(Net_sum = sum(Net_Value, na.rm = TRUE)) 
    
    # I then get rid of repeated values
    lela1 <- lela  %>%
      group_by(Net_sum) %>%
      mutate(cs2 = n())    %>%
      group_by(Invoice_Number, cs2) %>%
      filter(cs2 == 1)
    # the I switch the column, the original one with the adjusted one and then I delete the orginial contaminated column from the dataset
    lela1[, c("Net_Value","Net_sum")] <-  lela1[, c("Net_sum","Net_Value")] 
    

# then combine the fixed dataset with the uncontaminated dataset
    
    finale <- rbind2(Pure_pos, lela1)
  
rm(FY_Adis)
rm(FY_pos)
rm(FY_Ref)
rm(hje)
rm(hje2)
rm(lela)
rm(lela1)
rm(Negativs)
rm(Pure_pos)




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

netx <- data.frame(table(Redfern_no$Net_Value))
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



finale_no$sum <- sum(finale_no$Net_Value)


netx$percent <- netx$Freq/netx$sum


summary(finale_no$Net_Value)


netx$sum_net <- sum(netx$Var1)
 

# the following is the code for unallocated bugets GL bussines unit id = 10207990


#------Save and export to Excel 

write.csv(lela1, file="lela1.csv")

write.csv(netx, file="All_non_po_net.csv")

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





qplot(data = acc, x = Net_Value, binwidth = 1, geom = 'freqpoly', color = PO_Matched, 
      xlab = 'Spend less than £700', 
      ylab = ' Non Po vs PO volume Interpreting Services') +
  scale_x_continuous(limits = c(0, 700), breaks = seq(-0, 700, 50))


finale_no_less <- finale_no %>%
  filter(Net_Value < 400)

157684/269664



netx <- data.frame(table(finale_no$Net_Value))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum
#-------------------Redfern---------------------

Redfern_no <- finale_no %>%
  filter(Supplier_ID == 2875856 | Supplier_ID == 3841042)

Redfern <- finale %>%
  filter(Supplier_ID == 2875856 | Supplier_ID == 3841042)


summary(Redfern_no$Net_Value)

Redfern_yes <- finale_yes %>%
  filter(Supplier_ID == 2875856 | Supplier_ID == 3841042)

Redfern_gen <- finale %>%
  filter(Supplier_ID == 2875856 | Supplier_ID == 3841042)

qplot(data = Redfern_gen, x = Net_Value, binwidth = 25, geom = 'freqpoly', color = PO_Matched )

ggplot(data = Redfern_no, aes(x = Payment_Date, y = Net_Value),
       color = I('black'), fill = I('#F79420'), 
       xlab = 'Redfern FY 17/18', 
       ylab = 'Non PO Volume') 

ggplot(Redfern_no, aes(x= Net_Value, y= Payment_Date, 
       xlab = 'Redfern FY 17/18', 
       ylab = 'PO Match?') ) + geom_point() 



qplot(x = Net_Value, data = Redfern_gen, xlab = 'Redfern Costs FY 17/18', 
      ylab = ' PO match? ', color = I('black'), fill = I('#CC0000')) +
  facet_wrap(~PO_Matched, ncol=3)

qplot(x = Net_Value, data = Enterprise_gen & Redfern_gen, xlab = 'Enterprise Costs FY 17/18', 
      ylab = ' PO match? ', color = I('black'), fill = I('#CC0000')) +
  facet_wrap(~PO_Matched, ncol=3)


two_comp <- rbind2(Enterprise_gen, Redfern_gen)

qplot(x = PO_Matched, data = two_comp, xlab = 'Travel Suppliers FY 17/18', 
      ylab = ' PO match? ', color = I('black'), fill = I('#CC0000')) +
  facet_wrap(~Vendor_Group_Name_L1, ncol=3)


qplot(data = two_comp, x = PO_Matched, geom = 'freqpoly', color = Vendor_Group_Name_L1) 

?count
count(Vendor_Group_Name_L1)

qplot(x = PO_Matched, data = Redfern_gen, xlab = 'Redfern PO Match FY 17/18', 
      ylab = 'Volume by Cost Centres', color = I('black'), fill = I('#CC0000')) +
  facet_wrap(~GL_Business_Unit_Name, ncol=3)

qplot(x = PO_Matched, data = Enterprise_gen, xlab = 'Enterprise Rent a Car PO Match FY 17/18', 
      ylab = 'Volume by Cost Centres', color = I('black'), fill = I('#CC0000')) +
  facet_wrap(~GL_Business_Unit_Name, ncol=3)



# the above is also gold 


qplot(x = PO_Matched , data = Redfern_gen, xlab = 'Redfern Cost Centres', 
      ylab = ' PO match? ', color = I('black'), fill = I('#CC0000')) +
  facet_grid(GL_Business_Unit_Name ~  ., scales= "free", space = "free")

qplot(x = PO_Matched , data = Redfern_gen, xlab = 'Redfern Cost Centres', 
      ylab = ' PO match? ', color = I('black'), fill = I('#CC0000')) +
  facet_grid(GL_Business_Unit_Name ~  ., scales= "free", space = "free")





qplot(x = Net_Value, data = Enterprise_gen, xlab = 'Enterprise Rent a Car Costs FY 17/18', 
      ylab = ' PO match? ', color = I('black'), fill = I('#F79420')) +
  facet_wrap(~PO_Matched, ncol=3)




# above code is gold 


# facet wraps are an interesting way to isolate by categorically 

qplot(x = Payment_Date, data = FY_Ref) +
  facet_wrap(~GL_Operating_Unit_ID, ncol=3)






netx <- data.frame(table(Redfern_gen$Net_Value))
netx$sum <- sum($Freq)
netx$percent <- netx$Freq/netx$sum %>% View()


netx<- data.frame(table(Redfern_yes$GL_Business_Unit_Name))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum


netx <- data.frame(table(Redfern_yes$Payment_Date))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum



netx <- data.frame(table(Redfern_yes$`GL Operating_Unit_Name`))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum


#Redfern Non-PO


netx <- data.frame(table(Redfern_no$`GL Operating_Unit_Name`))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum
rm(netx)


netx<- data.frame(table(Redfern_no$GL_Business_Unit_Name))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum
rm(netx)


netx <- data.frame(table(Redfern_no$Payment_Date))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum
rm(netx)


rm(netx)

netx <- data.frame(table(Redfern_no$Invoice_Number))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum

rm(netx)

netx <- data.frame(table(Redfern_no$Account_Code))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum

qplot(data = Enterprise_gen, x = Payment_Date, binwidth = 25, geom = 'freqpoly', color = PO_Matched )

qplot(data = Redfern_gen, x = Net_Value, binwidth = 1, geom = 'freqpoly', color = PO_Matched, 
      xlab = 'Redfern Costs', 
      ylab = ' Non Po vs PO volume') 


# I will put redfern on pause for now 

##operatumred <- data.frame(table(Redfern$`GL Operating_Unit_Name`))
#rm(operatumred)
#BusinesRed <- data.frame(table(Redfern$`GL_Business_Unit_Name`))
#rm(BusinesRed)
#Timered <- data.frame(table(Redfern$Payment_Date))
#rm(Redfern)
#rm(Timered)
#rm(Cda)

#Redfern <- FY_nuovo_no %>%
## filter(Supplier_ID == 2875856 | Supplier_ID == 3841042)


#Unallocated <- FY_nuovo_no %>%
#filter(GL_Business_Unit_ID == 10207990)


#Redfern <- finale_yes %>%
#filter(Supplier_ID == 2875856 | Supplier_ID == 3841042)

#-------------------------------Redfern-------------------------------------------------
# The analysis of Redfern is currently on pause

# the supplier ID for Redfern is 2875856

#Redfern <- subset(FY_Ref, Supplier_ID == 2875856)

#Redferm_moj <- subset(FY_Ref, Supplier_ID == 2875856 & GL_Operating_Unit_ID == 230)
#Redfern_nms <- subset(FY_Ref, Supplier_ID == 2875856 & GL_Operating_Unit_ID == 210)
#Redfern_cts <- subset(FY_Ref, Supplier_ID == 2875856 & GL_Operating_Unit_ID == 250)
#Redfern_opg <- subset(FY_Ref, Supplier_ID == 2875856 & GL_Operating_Unit_ID == 470)



#gpc <-  qplot(data = Redfern, x = Net_Value, binwidth = 25, geom = 'freqpoly', color = GL_Operating_Unit_ID ) +
#scale_x_continuous(limits = c(-10000, 0,), breaks = seq(-10000, 0, 50))



#you might have to repeat the above taking into consideration outliers, so you need to find a way of droping values



#Box-plots are an excellent way to visually see the percentiles in the data

#qplot(x = PO_Matched, y = Net_Value,
#data = Redfern, geom ='boxplot', ylim = c(0, 1000))

#qplot(x = PO_Matched, y = Net_Value,
#data = Redfern, geom ='boxplot', ylim = c(0, 1000))


#-----------------------------------Enterprise Rent a Car------------------------------
# the supplier ID for Enterprise rent a c ar is 3839975

#Enterprise <- subset(FY_Ref, Supplier_ID == 3839975)
#Enterprise_moj <- subset(FY_Ref, Supplier_ID == 3839975 & GL_Operating_Unit_ID == 230)

Enterprise_no <- finale_no %>%
  filter(Supplier_ID == 3839975 | Supplier_ID == 2904566)

summary(Enterprise_no$Net_Value)


netx<- data.frame(table(Enterprise_no$GL_Business_Unit_Name))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum
rm(netx)


netx<- data.frame(table(Enterprise_no$Payment_Date))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum

netx<- data.frame(table(Enterprise_no$Net_Value))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum


Enterprise_yes <- finale_yes %>%
  filter(Supplier_ID == 3839975 | Supplier_ID == 2904566)


Enterprise_gen <- finale %>%
  filter(Supplier_ID == 3839975 | Supplier_ID == 2904566)


  
netx <- data.frame(table(Enterprise_no$`GL Operating_Unit_Name`))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum


netx <- data.frame(table(Enterprise_yes$Payment_Date))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum



#--------------------Analsying individual net values

finale_52.63 <- finale_no %>%
  filter(Net_Value == 52.63)




finale_378 <- finale_yes %>%
  filter(Net_Value == 53.81)

rm(finale_378)

netx <- data.frame(table(finale_378$`GL Operating_Unit_Name`))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum
rm(netx)


netx<- data.frame(table(finale_378$GL_Business_Unit_Name))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum
rm(netx)


netx <- data.frame(table(finale_378$Payment_Date))
netx$sum <- sum(netx$Freq)
netx$percent <- netx$Freq/netx$sum



sort(table(finale_378$GL_Business_Unit_Name))


# ------------------------Nisha's most recent requests--------


# looking at the top then net values 

netx <- data.frame(table(finale$Net_Value))


finale46 <- finale %>%
  filter(Net_Value == 46)

finale46y <- finale_yes %>% 
  filter(Net_Value == 46) 
rm(finale46)

  sort(table(finale46y$Vendor_Group_Name_L1))


  finale470.64 <- finale %>%
    filter(Net_Value == 470.64)
  
netx <- data.frame(table(finale$Net_Value))
  
  finale52.63 <- finale %>%
    filter(Net_Value == 52.63)
 
  (table(finale52.63$GL_Business_Unit_Name))
  
 data.frame(table(finale52.63$GL_Business_Unit_Name))

  rm(finale52.63)
  finale54.1 <- finale %>%
    filter(Net_Value == 54.1)
  rm(finale54.1)
  
  finale65.79 <- finale %>%
    filter(Net_Value == 65.79)
  
  data.frame(table(finale65.79$GL_Business_Unit_Name))
  

  finaletop10 <- finale %>%
    filter(Net_Value == 52.63 | Net_Value == 54.1  | Net_Value == 53.81  | Net_Value == 378.88
           | Net_Value == 466.57  | Net_Value ==  46 |
             Net_Value == 470.64 | Net_Value == 4.38 | Net_Value == 78.95 | Net_Value == 65.79)
# the above is to study the top 10 most frequented net values 
  
  netx<- data.frame(table(finaletop10$GL_Business_Unit_Name))
  rm(netx)
  
  rm(finale54.1)
  rm(finale65.79)
  rm(finale78.95)
  rm(finaleAcc)
 
  x %>% sort(table(finale470.64$PO_Matched))
  
  
  finale378.88 <- finale %>%
    filter(Net_Value == 378.88)

  finale51.4 <- finale %>%
    filter(Net_Value == 51.4)
  
  finale51.4a <- FY_Ref %>%
    filter(Net_Value == 51.4)

  sort(table(finale51.4a$Vendor_Group_Name_L1))

rm(finale51.4)
rm(finale51.4a)


brook <- finale %>%
  filter(Supplier_ID == 4071336 | Supplier_ID == 1108878)

 data.frame((table(brook$Net_Value)))

sort(table(finale$PO_Matched))
     
    netx <- data.frame(table(finale_no$Vendor_Group_Name_L1))
          
   netx <- data.frame(table(brook$Net_Value))
rm(netx)     
rm(brook)

 # RBS supplier ID is 1026259

RBS <- finale %>%
  filter(Supplier_ID == 1026259)

table(RBS$PO_Matched)

sort(table(RBS$Account_Code))

# now lets investigate the account code that is highly RBs and see if it is only an RBS


PCard <- finale %>%
  filter(Account_Code == '1816900001 - CA - OTHER RECEIVABLES - PURCHASING CARD CONTROL ACCOUNT')

RBS$check <- apply(RBS, 1, function(x) ifelse(any(x[1] == FY_Ref$Net_Value & x[2] == RBS$Net_Value), 'yes', 'no'))

#Investigating if RBS is truly a duplicate#------------------------
commonID4 <- intersect(RBS$Account_Code, FY_Ref$Account_Code)
samples <- FY_Ref[!FY_Ref$Invoice_Number %in% commonID4, ]

FY_Ref_subset <- FY_Ref[FY_Ref$Account_Code %in% commonID4, ]

commonID5 <- intersect(RBS$Net_Value, FY_Ref_subset$Net_Value)

samples <- FY_Ref_subset[FY_Ref_subset$Net_Value %in% commonID5, ]
 
# I get a zero observations i samples

fy52.63 <- finale %>%
  filter(Net_Value == 52.63)
table(fy52.63$Vendor_Group_Name_L1)

rm(commonID4)

data.frame(table(fy52.63$Vendor_Group_Name_L1))
data.frame(table(fy52.63$PO_Matched))


data.frame(table(fy52.63$Vendor_Group_Name_L1$'THEBIGWORD INTERPRETING SERVICES LTD'$PO_Matched))
8292/8399



#fy52.63b <- fy52.63 %>% group_by(Vendor_Group_Name_L1) %>% mutate(rs5a = n()) %>% group_by(Vendor_Group_Name_L1,PO_Matched) %>% mutate(rs5 =n()) %>% mutate(pf = rs5/rs5a)
rm(fy53.81)
#the above code works

fyin <- finale %>%
  filter(Net_Value == 65.79) %>% 
  group_by(Vendor_Group_Name_L1) %>% mutate(rs5a = n()) %>% 
  group_by(Vendor_Group_Name_L1,PO_Matched) %>%
  mutate(rs5 =n()) %>% mutate(pf = (rs5/rs5a)*100)

fycc <- finale %>%
  filter(Net_Value == 46) %>% 
  group_by(GL_Business_Unit_Name) %>% mutate(rs5a = n()) %>% 
  group_by(Vendor_Group_Name_L1,GL_Business_Unit_Name) %>%
  mutate(rs5 =n()) %>% group_by(rs5) %>% mutate(g = rank())

fycc <- finale %>%
  filter(Net_Value == 65.79)

 sort(table(fycc$GL_Business_Unit_Name)) 
 sort(table(fycc$`GL Operating_Unit_Name`))
 unique(fycc$GL_Business_Unit_Name)
 
 
 
rm(acc)
rm(finale65.79)

#top non po suppliers dive 
#RBS
 table(RBS$PO_Matched)
 sort(table(RBS$GL_Business_Unit_Name)) 
 unique(RBS$GL_Business_Unit_Name)
#Bigword------------------------
 bigw_no <- finale_no %>%
   filter(Supplier_ID == 3840019)
 
 finale_no <- finale %>% filter(PO_Matched =='No')
 
 finae_yes <- finale %>% filter(PO_Matched =='Yes') 

 
 bigw <- finae_yes %>%
   filter(Supplier_ID == 3840019)
 
 bigwLAA <-  bigw  %>%
   filter(GL_Operating_Unit_ID == 290)
 
 bigwopg <-  bigw  %>%
   filter(GL_Operating_Unit_ID == 470)
 
 hamsasost <-  bigw  %>%
   filter(Net_Value == 53.81) 
 
 
 netx <- data.frame(table(hamsasost$Payment_Date)) %>% View()
 
 netx <- data.frame(table(hamsasost$GL_Business_Unit_Name))%>% View()
 
 netx <- data.frame(table(hamsasost$`GL Operating_Unit_Name`))%>% View()
 
 
 netx <- data.frame(table(bigw$Account_Code)) %>% View()
 
 bigwLAA <-  bigw  %>%
   filter(GL_Operating_Unit_ID == 290)
 
 
 
 bigw2 <-  bigw %>% group_by(`GL Operating_Unit_Name`, Payment_Date) %>% mutate(ghex = count()) %>% View()
 
 
 table(bigw$PO_Matched)
 sort(table(bigw$GL_Business_Unit_Name)) 
 unique(bigw$GL_Business_Unit_Name)
 214522/217907
 sort(table(bigw$`GL Operating_Unit_Name`))
 sort(table(brk$`GL Operating_Unit_Name`))
 
rm(bigw)
 
 clar <- finale %>%
   filter(Supplier_ID == 3788865) 
 table(clar$PO_Matched)
 sort(table(clar$GL_Business_Unit_Name)) 
 unique(clar$GL_Business_Unit_Name)
 
 3872/3874
 rm(clar)
 
 brk <- finale %>%
   filter(Supplier_ID == 4071336 | Supplier_ID == 1108878 ) 
 table(brk$PO_Matched)
 (2174/(2174+175725))*100
 sort(table(brk$GL_Business_Unit_Name)) 
 unique(brk$GL_Business_Unit_Name)
 rm(brk)
 
 brk <- finale_no %>%
   filter(Supplier_ID == 4071336 | Supplier_ID == 1108878) 
 rm(brk)
 

 
 hays <- finale %>%
   filter(Vendor_Group_Name_L1 == 'HAYS PLC')
 
 table(hays$PO_Matched)
 sort(table(hays$GL_Business_Unit_Name)) 
 unique(hays$GL_Business_Unit_Name)
 
rm(hays)
rm(bigw)
rm(brk)
rm(FY_Exp)
rm(FY_Ref)
rm(FY_Ref_subset)
rm(fy52.63)
rm(fycc)
rm(fyin)
rm(RBS)
rm(PCard)
 
 hays <- finale_no %>%
   filter(Vendor_Group_Name_L1 == 'HAYS PLC')
 #----------------Back to Redfern----------
 
 Redfern_no$sum2 <- sum(Redfern_no$Net_Value) 
 
   Redyin <- Redfern_no %>% 
     group_by(GL_Business_Unit_ID) %>% mutate(rs5a = n()) %>%
     mutate(sg = sum(Net_Value)) %>% group_by(GL_Business_Unit_ID) %>%
     mutate(rs5ab = row_number()) %>% 
     group_by(rs5ab) %>% filter(rs5ab == 1 )%>% View
   
   
   Redyin <- Redfern_no %>% 
     group_by(Net_Value) %>% mutate(rs5a = n()) %>%
     group_by(Net_Value, GL_Business_Unit_Name) %>% mutate()
     
     View
   
  
   
   mutate(rs5ab = row_number())
   
   %>%
     mutate(sg = sum(Net_Value)) %>% group_by(GL_Business_Unit_ID) %>%
     %>% 
     group_by(rs5ab) %>% filter(rs5ab == 1 )%>% View
   
   
   
          # the above is certainly interesting but I am not sure if this is something we are interested in 
          
   table(Redfern_no$GL_Business_Unit_Name)
   
   #---------redfern comparative dive 
   
   sort(table(Redfern_gen$Payment_Date))
   
   unspc9 <- s3tools::read_using(
     FUN = readxl::read_xlsx,
     s3_path = "alpha-ccma-2018/UNSPSC_9000000_FY1718.xlsx",
     guess_max = 1000000
   )
   
   
   UNSPSC_9000000_FY1718
   
   
   # first there is a need to eliminate  duplicates 
   
   colnames(ref17_18)[colnames(ref17_18)=="Catalogue Item"] <- "Catalogue_Item"
   colnames(ref17_18)[colnames(ref17_18)=="GL Business Unit Name"] <- "GL_Business_Unit_Name"
   colnames(ref17_18)[colnames(ref17_18)=="Account Code Description"] <- "Account_Code_Description"
   colnames(ref17_18)[colnames(ref17_18)=="Account Code"] <- "Account_Code"
   
   colnames(ref17_18)[colnames(ref17_18)=="GL Business Unit ID"] <- "GL_Business_Unit_ID"
   colnames(ref17_18)[colnames(ref17_18)=="GL Operating Unit ID"] <- "GL_Operating_Unit_ID"
   colnames(ref17_18)[colnames(ref17_18)=="GL Operating Unit Name"] <- "GL Operating_Unit_Name"
   colnames(ref17_18)[colnames(ref17_18)=="Invoice Line Description"] <- "Invoice_Line_Description"
   
   colnames(ref17_18)[colnames(ref17_18)=="Net Value"] <- "Net_Value"
   colnames(ref17_18)[colnames(ref17_18)=="Invoice VAT Line Amount"] <- "Invoice_VAT_Line_Amount"
   colnames(ref17_18)[colnames(ref17_18)=="Invoice Number"] <- "Invoice_Number"
   
   colnames(ref17_18)[colnames(ref17_18)=="GPS Category L1"] <- "GPS_Category_L1"
   colnames(ref17_18)[colnames(ref17_18)=="GPS Category L2"] <- "GPS_Category_L2"
   colnames(ref17_18)[colnames(ref17_18)=="GPS Category L3"] <- "GPS_Category_L3"
   colnames(ref17_18)[colnames(ref17_18)=="GPS Category L4"] <- "GPS_Category_L4"
   
   colnames(ref17_18)[colnames(ref17_18)=="UNSPSC Category L1"] <- "UNSPSC_Category_L1"
   colnames(ref17_18)[colnames(ref17_18)=="UNSPSC Category L2"] <- "UNSPSC_Category_L2"
   colnames(ref17_18)[colnames(ref17_18)=="UNSPSC Category L3"] <- "UNSPSC_Category_L3"
   colnames(ref17_18)[colnames(ref17_18)=="UNSPSC Category L4"] <- "UNSPSC_Category_L4"
   colnames(ref17_18)[colnames(ref17_18)=="UNSPSC Code"] <- "UNSPSC_Code"
   
   colnames(ref17_18)[colnames(ref17_18)=="Payment Amount"] <- "Payment_Amount"
   colnames(ref17_18)[colnames(ref17_18)=="Payment Date"] <- "Payment_Date"
   
   colnames(ref17_18)[colnames(ref17_18)=="PO Matched"] <- "PO_Matched"
   colnames(ref17_18)[colnames(ref17_18)=="SME Flag"] <- "SME_Flag"
   colnames(ref17_18)[colnames(ref17_18)=="Source System"] <- "Source_System"
   
   
   colnames(ref17_18)[colnames(ref17_18)=="DUNS Number"] <- "DUNS_Number"
   colnames(ref17_18)[colnames(ref17_18)=="Supplier ID"] <- "Supplier_ID"
   
  
   
   ref17_18c <-  ref17_18 %>%
     group_by(Net_Value) %>%
     mutate(cs = row_number()) %>%
     mutate(abs = abs(Net_Value)) %>%
     group_by(abs, cs) %>%
     mutate(rs = n()) %>%
     filter(rs == 1)
   
   ref17_18c$DUNS_Number <- NULL
   ref17_18c$Catalogue_Item <- NULL
   ref17_18c$GPS_Category_L1 <- NULL
   ref17_18c$GPS_Category_L2 <- NULL
   ref17_18c$GPS_Category_L3 <- NULL
   ref17_18c$GPS_Category_L4 <- NULL
   
   ref17_18c$UNSPSC_Category_L1 <- NULL
   ref17_18c$UNSPSC_Category_L2 <- NULL
   ref17_18c$UNSPSC_Category_L3 <- NULL
   ref17_18c$UNSPSC_Category_L4 <- NULL
   
   ref17_18c$SME_Flag <- NULL
   ref17_18c$Invoice_Line_Description <- NULL
   ref17_18c$Account_Code_Description <- NULL
   
   ref17_18c$Invoice_Line_Description <- NULL
   ref17_18c$Account_Code_Description <- NULL
   
   redref <- ref17_18c %>%
     
     filter(Net_Value != 0)
   
   redfref2 <- redref %>% 
     group_by(Invoice_ID) %>% mutate(Net_value2 = if_else((Net_Value <= 0)), sum(Net_Value), (Net_Value >= 0), Net_Value) %>% View()
                                     
   
   colnames(redref)[colnames(redref)=="Invoice ID"] <- "Invoice_ID"
   
   redref$Net_Value2 <- redref$Net_Value   %>% group_by(Invoice_ID)
   
   
   
   Negativs <-  redref %>%
     filter(Net_Value <= 0)
   
   FY_pos<- redref %>%
     filter(Net_Value >= 0)
   
   
   

   
   
   
   # this is the process of finding common invoices number 
   commonID <- intersect(FY_pos$Invoice_ID, Negativs$ID)

   # the following are pure postives that is those who do not  have any contamination of corrections
   Pure_pos <- FY_pos[!FY_pos$Invoice_Number %in% commonID, ]
   
   # process explained:
   # once you find the invoice numbers that intersect between only positives and only negatives, drop those invoice numbers
   # fromm the only postive sections. Extract from the holistic dataset using those invoice numbers to create a dataset
   # that has both positives and negatives, group by invoice numbers and sum them, then you join 
   # the only postives with the new aggregated dataset. 
   
   
   hje <- redref[redref$Invoice_Number %in% commonID, ]
   # the above is all the datasets that have contamination then change it to a table format so that it can resemble some kind of matrix which wil be easier to manipulate
   hje2 <- tbl_df(redref) 
   
   
   # I sum the contaminated data grouped by invoice number since the negative values are implemented such that in aggregate the error gets cancelled out
   
   lela <-  redref  %>% 
     group_by(Invoice_Number, Payment_Date) %>%
     mutate(Net_sum = sum(Net_Value, na.rm = TRUE)) 
   
   # I then get rid of repeated values
   lela1 <- lela  %>%
     group_by(Net_sum) %>%
     mutate(cs2 = n())    %>%
     group_by(Invoice_Number, cs2) %>%
     filter(cs2 == 1)
   # the I switch the column, the original one with the adjusted one and then I delete the orginial contaminated column from the dataset
   lela1[, c("Net_Value","Net_sum")] <-  lela1[, c("Net_sum","Net_Value")] 
   
   Redfern_gentes <- Redfern_gen %>% 
     group_by(Invoice_Number, Payment_Date) %>% mutate(Net_value2 = sum(Net_Value))
   
   #------------UNSPSC----------------------------
   
   
   unspc9 <- s3tools::read_using(
     FUN = readxl::read_xlsx,
     s3_path = "alpha-ccma-2018/UNSPSC_9000000_FY1718.xlsx",
     guess_max = 1000000
   )
   
   colnames(unspc9)[colnames(unspc9)=="Catalogue Item"] <- "Catalogue_Item"
   colnames(unspc9)[colnames(unspc9)=="GL Business Unit Name"] <- "GL_Business_Unit_Name"
   colnames(unspc9)[colnames(unspc9)=="Account Code Description"] <- "Account_Code_Description"
   colnames(unspc9)[colnames(unspc9)=="Account Code"] <- "Account_Code"
   
   colnames(unspc9)[colnames(unspc9)=="GL Business Unit ID"] <- "GL_Business_Unit_ID"
   colnames(unspc9)[colnames(unspc9)=="GL Operating Unit ID"] <- "GL_Operating_Unit_ID"
   colnames(unspc9)[colnames(unspc9)=="GL Operating Unit Name"] <- "GL Operating_Unit_Name"
   colnames(unspc9)[colnames(unspc9)=="Invoice Line Description"] <- "Invoice_Line_Description"
   
   colnames(unspc9)[colnames(unspc9)=="Net Value"] <- "Net_Value"
   colnames(unspc9)[colnames(unspc9)=="Invoice VAT Line Amount"] <- "Invoice_VAT_Line_Amount"
   colnames(unspc9)[colnames(unspc9)=="Invoice Number"] <- "Invoice_Number"
   
   colnames(unspc9)[colnames(unspc9)=="GPS Category L1"] <- "GPS_Category_L1"
   colnames(unspc9)[colnames(unspc9)=="GPS Category L2"] <- "GPS_Category_L2"
   colnames(unspc9)[colnames(unspc9)=="GPS Category L3"] <- "GPS_Category_L3"
   colnames(unspc9)[colnames(unspc9)=="GPS Category L4"] <- "GPS_Category_L4"
   
   colnames(unspc9)[colnames(unspc9)=="UNSPSC Category L1"] <- "UNSPSC_Category_L1"
   colnames(unspc9)[colnames(unspc9)=="UNSPSC Category L2"] <- "UNSPSC_Category_L2"
   colnames(unspc9)[colnames(unspc9)=="UNSPSC Category L3"] <- "UNSPSC_Category_L3"
   colnames(unspc9)[colnames(unspc9)=="UNSPSC Category L4"] <- "UNSPSC_Category_L4"
   colnames(unspc9)[colnames(unspc9)=="UNSPSC Code"] <- "UNSPSC_Code"
   
   colnames(unspc9)[colnames(unspc9)=="Payment Amount"] <- "Payment_Amount"
   colnames(unspc9)[colnames(unspc9)=="Payment Date"] <- "Payment_Date"
   
   colnames(unspc9)[colnames(unspc9)=="PO Matched"] <- "PO_Matched"
   colnames(unspc9)[colnames(unspc9)=="SME Flag"] <- "SME_Flag"
   colnames(unspc9)[colnames(unspc9)=="Source System"] <- "Source_System"
   
   
   colnames(unspc9)[colnames(unspc9)=="DUNS Number"] <- "DUNS_Number"
   colnames(unspc9)[colnames(unspc9)=="Supplier ID"] <- "Supplier_ID"
   
   unspc9r <- unspc9 %>%
     
     filter(Net_Value != 0)
   
   
   unspc9c <-  unspc9r %>%
     group_by(Net_Value) %>%
     mutate(cs = row_number()) %>%
     mutate(abs = abs(Net_Value)) %>%
     group_by(abs, cs) %>%
     mutate(rs = n()) %>%
     filter(rs == 1)
   
   unspc9c$DUNS_Number <- NULL
   unspc9c$Catalogue_Item <- NULL
   unspc9c$GPS_Category_L1 <- NULL
   unspc9c$GPS_Category_L2 <- NULL
   unspc9c$GPS_Category_L3 <- NULL
   unspc9c$GPS_Category_L4 <- NULL
   
   unspc9c$UNSPSC_Category_L1 <- NULL
   unspc9c$UNSPSC_Category_L2 <- NULL
   unspc9c$UNSPSC_Category_L3 <- NULL
   unspc9c$UNSPSC_Category_L4 <- NULL
   
   unspc9c$SME_Flag <- NULL
   unspc9c$Invoice_Line_Description <- NULL
   unspc9c$Account_Code_Description <- NULL
   
   unspc9c$Invoice_Line_Description <- NULL
   unspc9c$Account_Code_Description <- NULL
   
   unsred <- unspc9c %>%
     filter(Supplier_ID == 2875856 | Supplier_ID == 3841042)
   
   #this has worked.
   
   #-------------finally completing Nisha's task
  
   
   unspc18 <- s3tools::read_using(
     FUN = readxl::read_xlsx,
     s3_path = "alpha-ccma-2018/2018q1_q2.xlsx",
     guess_max = 1000000
   )
   
   unspc17 <- s3tools::read_using(
     FUN = readxl::read_xlsx,
     s3_path = "alpha-ccma-2018/2017q1_q2.xlsx",
     guess_max = 1000000
   )
   
   unspc18$Distinction <- 2018
   unspc17$Distinction <- 2017
   rm(unspc9)
   rm(unspc9r)
   rm(Redfern_no)
   rm(Redfern_yes)
   rm(Enterprise_yes)
   rm(Enterprise_no)
   rm(Enterprise_gen)
   rm(finale_no)
   rm(finale_yes)
   rm(ref17_18)
   rm(ref17_18c)
   rm(Redfern_gentes)
   rm(samples)
   rm(two_comp)
   rm(finaletop10)
   rm(netx)
   rm(redref)
  rm(unspc9c)
   
  comparativo <- rbind2(unspc17, unspc18)
  
  colnames(unspc18 )[colnames(unspc18 )=="Catalogue Item"] <- "Catalogue_Item"
  colnames(unspc18 )[colnames(unspc18 )=="GL Business Unit Name"] <- "GL_Business_Unit_Name"
  colnames(unspc18 )[colnames(unspc18 )=="Account Code Description"] <- "Account_Code_Description"
  colnames(unspc18 )[colnames(unspc18 )=="Account Code"] <- "Account_Code"
  
  colnames(unspc18 )[colnames(unspc18 )=="GL Business Unit ID"] <- "GL_Business_Unit_ID"
  colnames(unspc18 )[colnames(unspc18 )=="GL Operating Unit ID"] <- "GL_Operating_Unit_ID"
  colnames(unspc18 )[colnames(unspc18 )=="GL Operating Unit Name"] <- "GL Operating_Unit_Name"
  colnames(unspc18 )[colnames(unspc18 )=="Invoice Line Description"] <- "Invoice_Line_Description"
  
  colnames(unspc18 )[colnames(unspc18 )=="Net Value"] <- "Net_Value"
  colnames(unspc18 )[colnames(unspc18 )=="Invoice VAT Line Amount"] <- "Invoice_VAT_Line_Amount"
  colnames(unspc18 )[colnames(unspc18 )=="Invoice Number"] <- "Invoice_Number"
  
  colnames(unspc18 )[colnames(unspc18 )=="GPS Category L1"] <- "GPS_Category_L1"
  colnames(unspc18 )[colnames(unspc18 )=="GPS Category L2"] <- "GPS_Category_L2"
  colnames(unspc18 )[colnames(unspc18 )=="GPS Category L3"] <- "GPS_Category_L3"
  colnames(unspc18 )[colnames(unspc18 )=="GPS Category L4"] <- "GPS_Category_L4"
  
  colnames(unspc18 )[colnames(unspc18 )=="UNSPSC Category L1"] <- "UNSPSC_Category_L1"
  colnames(unspc18 )[colnames(unspc18 )=="UNSPSC Category L2"] <- "UNSPSC_Category_L2"
  colnames(unspc18 )[colnames(unspc18 )=="UNSPSC Category L3"] <- "UNSPSC_Category_L3"
  colnames(unspc18 )[colnames(unspc18 )=="UNSPSC Category L4"] <- "UNSPSC_Category_L4"
  colnames(unspc18 )[colnames(unspc18 )=="UNSPSC Code"] <- "UNSPSC_Code"
  
  colnames(unspc18 )[colnames(unspc18 )=="Payment Amount"] <- "Payment_Amount"
  colnames(unspc18 )[colnames(unspc18 )=="Payment Date"] <- "Payment_Date"
  
  colnames(unspc18 )[colnames(unspc18 )=="PO Matched"] <- "PO_Matched"
  colnames(unspc18 )[colnames(unspc18 )=="SME Flag"] <- "SME_Flag"
  colnames(unspc18 )[colnames(unspc18 )=="Source System"] <- "Source_System"
  
  
  colnames(unspc18 )[colnames(unspc18 )=="DUNS Number"] <- "DUNS_Number"
  colnames(unspc18 )[colnames(unspc18 )=="Supplier ID"] <- "Supplier_ID"
  
  unspc18c <- unspc18 %>%
    filter(Net_Value != 0) %>%
    group_by(Net_Value) %>%
    mutate(cs = row_number()) %>%
    mutate(abs = abs(Net_Value)) %>%
    group_by(abs, cs) %>%
    mutate(rs = n()) %>%
    filter(rs == 1) %>%
    filter(Supplier_ID == 2875856 | Supplier_ID == 3841042)
  # what I have seen is that 2017 does not survive this filteration process, perhaps it is irrational to correct errenous things from different time lines..
  #just continue on this path again tomorrow and it should be fine... then you can do a little time line and say this is trend and if it continues then this will happen
  #also maybe try to find out where exactly red fern falls in terms of UNSPCC categorisations...
  unspc18c_no <- comparativoc %>% filter(PO_Matched == 'No')
  
  
  #there seems to be no data for Redfern 2017, therefore I think i will try to extract that time frame from a different dataset, I think perhaps there has 
  #been some kind of recoding process where the UNSPC code changed or got relabelled, investigate this further 
  
  
  
  red17 <- Redfern_gen %>%
    filter(Payment_Date >= as.Date("2017-01-05")) %>% 
    filter(Payment_Date <= as.Date("2017-05-31")) 
  
  red17_no <- red17 %>% filter(PO_Matched == 'No') 
  
  
red18x <- sort(table(unspc18c_no$Payment_Date)) %>% View()

red17x <- sort(table(red17_no$Payment_Date)) %>% View()

#--------Yes PO matches--------------------------

unspc18c_yes <- comparativoc %>% filter(PO_Matched == 'Yes')
red17_yes <- red17 %>% filter(PO_Matched == 'Yes') 

red18x <- sort(table(unspc18c_yes$Payment_Date)) %>% View()

red17x <- sort(table(red17_yes$Payment_Date)) %>% View()


redsimz <- Redfern_gen %>% mutate(simz = sum(Net_Value)) %>% View()
Redfern_gen$Sumz <- sum(Redfern_gen$Net_Value)

Redfern_gen2 <- Redfern_gen %>% filter(PO_Matched == 'No')

Redfern_gen3 <-  Redfern_gen2 %>% group_by(GL_Operating_Unit_ID) %>% mutate(snight = sum(Net_Value)) %>% group_by(Payment_Date) %>% mutate(snight2 = sum(Net_Value))
  Redfern_gen2$simzing <- sum(Redfern_gen2$Net_Value) 
  
  write.csv(Redfern_gen3, file="Redfern_son")
  
Redunallocated <- Redfern_gen2  %>% filter(GL_Business_Unit_ID == 10207990)

  
  Enterprise_na <- Enterprise_gen %>% filter(PO_Matched == 'No')
  Enterprise_na$simzing <- sum(Enterprise_na$Net_Value)
  
investog <- finale %>% filter(Account_Code == '5224100011 - EXP - PURCHASE OF GOODS/SERVICES - PURCHASE PRICE VARIANCE') 

#-------------------------Arcadis--------------


arc <- finale %>% filter(Supplier_ID == 1108672)


table(arc$PO_Matched)
23/119
group_by(v2) %>% # group by the actual value
  
  mutate(cs = row_number()) %>% # count how many times the actual value appeared so far
  
  mutate(abs = abs(v2)) %>% # get the absolute value
  
  group_by(abs, cs) %>% # group by the absolute value and how many times each actual number appears
  
  mutate(rs = n()) %>%

arc2 <- arc %>% group_by(PO_Matched, Payment_Date) %>% mutate(d = n()) %>% View()
arc2 <- arc %>% group_by(PO_Matched) %>% mutate(d = sum(Net_Value)) %>% View()
netx<- data.frame()
  arc$sams <- sum(arc$Net_Value)
  
  #-------------Data extraction for Zoey
  
  write.csv(Redfern_gen, file="Redfern data")
  write.csv(Enterprise_gen, file="Enterprise Rent a Car data")
  
  
  
  