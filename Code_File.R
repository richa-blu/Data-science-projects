#libraries required
library(stringi)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(colorspace)
library(stargazer)
library(corrplot)

#setting the working directory
#setwd("C:/IIITB_PGDDA/Course2_Stats_EDA/EDA Group Project")

#reading data
loan <- read.csv("loan.csv")

#Dimensions rows 39717  fields : 111
 
###############################################################################################################
### At this stage it is observed that  some of the field given in the DATA Dictionary are not avaialble in the data.  
###  Examples are FICO rele fico_range_high , fico_range_low ,last_fico_range_high,last_fico_range_low
###############################################################################################################

############################################## DATA CLEANING  ##################################################

#finding unique values in each field . In order to  remove the unnecessary fields with single value/complete na value
as.data.frame(rapply(loan,function(x)length(unique(x))))

#removing empty/NA fields
loan <- Filter(function(x)!all(is.na(x)), loan)

#finding max minimum mean of the field values to see the outliers and general distribution of numerical data.
stargazer(loan, type = "text",  digits=2)

#Obervation:
#1. There are some fields with a single values
#2. fields with outliers: annual_inc, loan_amt

#Removing variables with single value
loan <- Filter(function(x)(length(unique(x))>1), loan)

#removing other fields that not relevant
# id, member_id, desc, url, title, zip_code : will not be used for analysis
# tax_liens, chargeoff_within_12_mth, mths_since_last_delinq, mths_since_last_record, 
# collections_12_mths_ex_med, pub_rec_bankruptcies, pymnt_plan, initial_list_status : 
# is not relevent for analysis as there is only 0/na values
# mths_since_last_delinq, mths_since_last_record: data not sufficient for analysis
# next_pymnt_d, out_prncp, out_prncp_inv: not relevant for analysis 
loan = loan[, !names(loan) %in% c("id","member_id","tax_liens","chargeoff_within_12_mths","mths_since_last_delinq","mths_since_last_record","collections_12_mths_ex_med","pub_rec_bankruptcies","desc","url","title","zip_code","next_pymnt_d","out_prncp","out_prncp_inv","pymnt_plan","initial_list_status")]

#Removing all the columns that will come into picture after the loan has been sanctioned 
#these will be of no use for analysis
loan <-loan[,-c(27:33)]
loan <-loan[,-c(27:29)]

#Identifying outliers in loan amount
ggplot(loan, aes(loan_status, loan_amnt)) +geom_boxplot(aes(fill = loan_status)) + labs(list(title = "Loan amount by status", x = "Status", y = "Loan Amount"))
#removing outlier values of loan amount
loan = loan[loan$loan_amnt <= 30000,]
ggplot(loan, aes(loan_status, loan_amnt)) +geom_boxplot(aes(fill = loan_status)) + labs(list(title = "Loan amount by status", x = "Status", y = "Loan Amount"))


#Identifying outliers in annual income
ggplot(loan, aes(loan_status, annual_inc)) +geom_boxplot(aes(fill = loan_status)) + labs(list(title = "Annual income by status", x = "Status", y = "Annual Income"))
#removing outlier values for annual income
loan = loan[loan$annual_inc <= 130000,]
ggplot(loan, aes(loan_status, annual_inc)) +geom_boxplot(aes(fill = loan_status)) + labs(list(title = "Annual income by status", x = "Status", y = "Annual Income"))

											 
#cleaning emp_title field 
loan$emp_title <- tolower(loan$emp_title) #converting to lowercase
loan$emp_title <- sub("^$","n/a", loan$emp_title) #replacing empty values with 'n/a'

#cleaning home_ownership field
loan$home_ownership <- tolower(loan$home_ownership) #converting to lowercase
loan$home_ownership<-sub("none","other",loan$home_ownership) #replacing 'none' with 'other'

#cleaning verification_status field
loan$verification_status <- tolower(loan$verification_status) #converting to lowercase

#cleaning loan_status field
loan$loan_status <- tolower(loan$loan_status) #converting to lowercase

#cleaning purpose field
loan$purpose <- tolower(loan$purpose)	#converting to lowercase


#Removing records of loans currently under process
loan <- filter(loan, loan_status!="current") #35639 rows and 26 columns

#Creating a variable "default_status" with values 'default' and 'non default'
#Marking 'charged off' as 'default' and 'fully paid' as 'non default'
loan$default_status=ifelse(loan$loan_status=="charged off","default","non default")

#changing term to Numeric
loan$term = as.numeric(sub("months","", loan$term))
sum(is.na(loan$term))

#changing revol_util from % value to numeric field 
loan$revol_util = as.numeric(sub("%","", loan$revol_util)) #removing % sign

#finding na values in revol_util field and replacing with 0
sum(is.na(loan$revol_util))
loan$revol_util[(is.na(loan$revol_util))]<-0

#changing from % value to numeric field int_rate : 
loan$int_rate = as.numeric(sub("%","", loan$int_rate)) #removing % sign
sum(is.na(loan$revol_util))

#employment Length into numeric.    
loan$emp_length =sub("< 1 year","0", loan$emp_length) #converting '< 1 year' to '0'
loan$emp_length =sub(" years","", loan$emp_length)    #removing 'years'
loan$emp_length =sub(" year","", loan$emp_length)     #removing 'year'
loan$emp_length =sub("\\+","", loan$emp_length)       #removing '+'
loan$emp_length =sub("n/a","0", loan$emp_length)      #replacing 'n/a' with '0'
loan$emp_length =as.numeric(loan$emp_length)          #converting to numeric data type

#creating a numeric field 'default_statusN' from 'default_status' in order to do correlation analysis
loan$default_statusN=ifelse(loan$default_status=="default",1,0)

#creating subset of 'Loan' DataFrame by default/non default for different stages of analysis
loandefault <-subset(loan, default_statusN == 1)
loannondefault <-subset(loan, default_statusN == 0)

##Coverting earliest_cr_line, issue_d to appropriate date format
loan$earliest_cr_line <- as.Date(paste("01",loan$earliest_cr_line ,sep="-"),"%d-%b-%y")
loan$issue_d <- as.Date(paste("01",loan$issue_d ,sep="-"),"%d-%b-%y")

#Loan state : 35639 rows and 28 columns

#######################################################################################################
###########################  Run code till this point in a single go  #################################
#######################################################################################################

###########################################Univariate Analysis#########################################

#addr_state 
#Analysing if there are some states with high percentage of defaulters as compared to other states
ggplot(loan ,aes(x=addr_state,fill=default_status )) +geom_bar(stat="count",col="black")+ ggtitle("State vs Loan Status")+ labs(x="State",y="No of Loans")

stats<-as.data.frame.matrix(table(loan$addr_state, loan$default_status ))
stats$percentage_defaults<-(stats[,1]/(stats[,1]+stats[,2])*100)

#sorting and filtering based on states with total number of loans >=1500 and %defaults>=14%
stats <- subset(stats , (stats[,1]+stats[,2]) >=1500)
sort(stats$percentage,decreasing=TRUE)
stats1 <- subset(stats , percentage_defaults >=14)
show(stats1)

#########################################  Significant   #############################################
##It is observed that states like CA, FL, NJ have higher number of defaulters as compared to other states. 
##It can be a case that these states does not have very strict rules for loan defaulters	
##LC must follow strict guidelines for people applying loans from these states
#########################################  

#delinq_2yrs 
ggplot(loan ,aes(x=delinq_2yrs,fill=default_status )) +geom_bar(stat="count",col="black",position="fill")+ ggtitle("Deliq in last 2yrs vs Loan Status")+labs(x="No of times Deliquent",y="Percentage of count of loans")

loan_dellow<-filter(loan,loan$delinq_2yrs==0)
loan_delhigh<-filter(loan,loan$delinq_2yrs>0)
length(which(loan_dellow$default_statusN == "1"))/nrow(loan_dellow)*100
length(which(loan_delhigh$default_statusN == "1"))/nrow(loan_delhigh)*100

stats<-as.data.frame.matrix(table(loan$delinq_2yrs, loan$default_status ))
stats$percentage<-(stats[,1]/(stats[,1]+stats[,2])*100)
show(stats)

#######################################   Significant    ##############################################
##It is observed that only 13.8% people defaulted when delinq_2yrs=0
##whereas 15.8% people defaulted when delinq_2yrs>0
#########################################  

#dti 
#It seems that the lons were only given to people with dti<=0.33 or 33% which is a good startegy
ggplot(loan ,aes(x=dti,fill=default_status )) +geom_histogram(bins=60)+ ggtitle("DTI vs Loan Status")+labs(x="dti values",y="No of Loans")

loan_dtilow<-filter(loan,loan$dti<=12)
loan_dtihigh<-filter(loan,loan$dti>12)
sum(loan_dtilow$default_statusN)/nrow(loan_dtilow)*100
sum(loan_dtihigh$default_statusN)/nrow(loan_dtihigh)*100

#####################################   Significant    ##############################################
##It is observed that 12.4% people defaulted where dti<=12, whereas the default %age shoots up to 15.3% when dti>12. 
##This clearly shows that higher dti means higher chances of defaulting
#########################################  

#inq_last_6mths 
ggplot(loan ,aes(x=inq_last_6mths,fill=default_status )) +geom_bar(stat="count",col="black", position = "fill")+ ggtitle("Inquiries in last 6 months vs Loan Status")+labs(x="No of Inquiries",y="No of Loans")

loan_inqlow<-filter(loan,loan$inq_last_6mths<=3)
loan_inqhigh<-filter(loan,loan$inq_last_6mths>3)
length(which(loan_inqlow$default_statusN == "1"))/nrow(loan_inqlow)*100
length(which(loan_inqhigh$default_statusN == "1"))/nrow(loan_inqhigh)*100

######################################   Significant   ###########################################
##It is observed that 13.9% people defaulted where inquiries in last 6mnth<=3, 
##whereas 17.6% people defaulted where inquiries in last 6mnths>3. 
##It clearly shows that higher number of inquiries maybe evidence of potential “credit seeking behavior” 
##and hence more chances of defaulting
#########################################  

#purpose
#Analysing if any particular loan purpose have high number of defaulters
ggplot(loan ,aes(x=purpose,fill=default_status )) +geom_bar(position="fill",stat="count",col="black")+ ggtitle(" Purpose vs Loan Status")+labs(x="Purpose",y="No of Loans")+ theme(axis.text.x = element_text(size  = 10, angle = 45, hjust = 1, vjust = 1))


#######################################   Significant   #########################################
##It is observed that out of the 14 categories, for small businesses there is the highest % of defaults  more than 25%
#########################################  

#pub_rec
ggplot(loan ,aes(x=pub_rec,fill=default_status )) +geom_bar(stat="count",col="black")+ ggtitle("Number of derogatory public records by Count")+labs(x="Pub Records",y="No of Loans ") +geom_text(stat='count',aes(label= ..count..), position = position_stack(vjust = 0.5))

stats<-as.data.frame.matrix(table(loan$pub_rec, loan$default_status ))
stats$percentage<-(stats[,1]/(stats[,2]+stats[,1])*100)


#######################################   Significant   #########################################
##it is observed that if pub_rec >0, default rate is higher.
#########################################  

#revol_util 
ggplot(loan, aes(default_status, revol_util)) +geom_boxplot(aes(fill = default_status)) +theme(axis.text.x = element_blank()) +   labs(list(title = "revol util by status",    x = "Status",y = "revol_util"))

summary(loan$revol_util)
summary(loandefault$revol_util)
summary(loannondefault$revol_util)

ggplot(loan, aes(revol_util , col=default_status)) + geom_histogram(aes(fill = default_status ),bins = 4,colour="grey20", lwd=0.2)

#######################################   Significant   #########################################
##HIGH influence on defaulting if the revolving utilisation is more that 25%.
##customers are more likely to default if the revol_util>25%
#########################################  


####################################################   Segmented Univariate Analysis   ###################################################    

#experience(emp_length )  vs home ownership segmentation 
ggplot(loan ,aes(x=home_ownership ,fill=default_status )) +geom_bar(stat="count",col="black")+ facet_wrap(~emp_length)+ggtitle("Summarised Data by Employment Length / Home Ownership ")+labs(x="Count",y="Home Ownership ")

loan_emplength =subset(loan, emp_length >=10 )
stats<-as.data.frame.matrix(table(loan_emplength$home_ownership, loan_emplength$default_status  ))
stats$percentage<-(stats[,1]/(stats[,1]+stats[,2])*100)
show(stats)

#######################################   Significant   #########################################
# From the barchart it is obvious that people who are experienced 10 years 
# and more,living in a rented house have more chance of defalting.
# It is observed that 18% of people living in rented houses and experience more than 10 years defaulted their loan
#########################################  


########################################################   Bivariate Analysis   #######################################################

#Extracting all numeric field data from loan dataframe to find correlation
loandata_numeric <- Filter(is.numeric, loan)
loandata_cor<- cor(loandata_numeric)
corrplot(loandata_cor, method = "ellipse")

# int_rate  
ggplot(loan, aes(default_status, int_rate)) +geom_boxplot(aes(fill = default_status)) +theme(axis.text.x = element_blank()) +   labs(list(title = "Interst Rate  by status",    x = "Status",y = "Int Rate"))
#histogram
ggplot(loan, aes(int_rate , col=default_status)) + geom_histogram(aes(fill = default_status ),bins =10,colour="grey20", lwd=0.2)

summary(loan$int_rate)
summary(loandefault$int_rate)
summary(loannondefault$int_rate)

#######################################   Significant   #########################################
##It is observed that the rate of defaulting is directly proportional to interest rate
#########################################  

# Creating a varibales for analysing correlation of Categorical variables 

gradelevel<-sort(unique(loan$grade, incomparables = FALSE),decreasing = FALSE)
loan$gradeN <- as.numeric(factor(loan$grade , levels=gradelevel))

subgradelevel<-sort(unique(loan$sub_grade, incomparables = FALSE),decreasing = FALSE)
loan$sub_gradeN<-as.numeric(factor(loan$sub_grade , levels=subgradelevel))

homelevel<-sort(unique(loan$home_ownership, incomparables = FALSE),decreasing = FALSE)
loan$home_ownershipN<-as.numeric(factor(loan$home_ownership , levels=homelevel))

loanlevel<-sort(unique(loan$loan_status, incomparables = FALSE),decreasing = FALSE)
loan$loan_statusN<-as.numeric(factor(loan$loan_status,  levels=loanlevel))

loandata_category = loan[, names(loan) %in% c("term","gradeN","sub_gradeN","home_ownershipN","loan_statusN")]

# Creating Correlation table for analysing categorical varibales
loandata_cor_category<- cor(loandata_category)
corrplot(loandata_cor_category, type = "lower", order ="hclust" ,tl.col = "black", tl.srt = 30)
### it is observed that term, grade , subgrade has negative correlation with  loand_default


# Grade 
ggplot(loan ,aes(x=grade,fill=default_status )) +geom_bar(stat="count",col="black")+ ggtitle("grade by Count")+labs(x="grade",y="No of Loans ")+geom_text(stat='count',aes(label= ..count..), position = position_stack(vjust = 0.5),check_overlap =TRUE)

loan$grade_type <- factor(loan$grade)
levels(loan$grade_type) <- list(  High = c("A","B","C"),  Low = c("D","E","F","G"))
ggplot(loan ,aes(x=loan$grade_type,fill=factor(loan$default_status) )) +geom_bar(position="fill")+ ggtitle("% of defaults by Grade type")+labs(x="grade level",y="No of Loans ")

#######################################   Significant   #########################################
##It is observed that A-C catergories have defaulting rates of approx 10% and approx 25% for grades above that   
##LC needs to be more vigilant while allocating loan to category D and above
#########################################  

#term
ggplot(loan ,aes(x=term,fill=default_status )) +geom_bar(stat="count",col="black",position="fill")+ ggtitle("Term by Count")+labs(x="Term",y="No of Loans ")
stats<-as.data.frame.matrix(table(loan$term, loan$default_status ))
stats$percentage<-(stats[,1]/(stats[,2]+stats[,1])*100)
show(stats)

#######################################   Significant   #########################################
## it is observed that 25.5% of loans with term 60 have been defaulted whereas only 11.3% defaulted in case of term 36
#######################################   


##################################### Derived Metric #############################################

#derived metric for calculating the number of closed credit lines
loan$closed_acc<-loan$total_acc-loan$open_acc
ggplot(loan ,aes(x=closed_acc,fill=default_status )) +geom_bar()

#plotting the closed_acc against emp_length
ggplot(loan ,aes(x=closed_acc, y=emp_length, color=default_status )) +geom_point() + geom_smooth()

stats<-as.data.frame.matrix(table(loan$closed_acc, loan$default_status))
stats$percentage<-(stats[,1]/(stats[,2]+stats[,1])*100)
show(stats)

###################################################################################################
####################################  Driver Variables  ###########################################

#1. term
#2. Grade
#3. revol_util
#4. addr_state
#5. purpose
#6. pub_rec
#7. dti
#8. inq_last_6mths

## Hence, above is the priority order list of the variables that drive the number of defaulters for LC.
## It is adviced that LC must use these variable to filter out the applications that can be future loan defaulters.
