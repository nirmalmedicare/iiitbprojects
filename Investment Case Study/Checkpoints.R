#Checkpoint 1: Data Cleaning 1==>
#Loading companies data frame with companies.txt

companies<-read.delim("companies.txt",header = TRUE,stringsAsFactors = FALSE)
str(companies)
summary(companies)

#Loading rounds2 data frame with rounds2.csv

rounds2<- read.csv("rounds2.csv",stringsAsFactors = FALSE)
str(rounds2)
summary(rounds2)

#1.1-How many unique companies are present in rounds2?

length(unique(toupper(rounds2$company_permalink)))
#66368

#1.2-How many unique companies are present in companies?

length(unique(toupper(companies$permalink)))
#66368

#1.5-Merge the two data frames so that all variables (columns) in the companies frame are 
#added to the rounds2 data frame.Name the merged frame master_frame.
#How many observations are present in master_frame ?

# For merging we need to convert the unique columns in both the dataframes
#in either lower or upper case and then merge both the frames with left outer join.

companies$permalink <- tolower(companies$permalink)
rounds2$company_permalink <- tolower(rounds2$company_permalink)
master_frame <- merge(rounds2,companies,by.x = "company_permalink",by.y = 
                       "permalink",all.x=TRUE)
summary(master_frame)   #To find total number of observations in master_frame.
#114949


#Checkpoint 2: Data Cleaning 2==>
#Cleaning of the columns which are necessary for analysis.
#2.1-How many NA values are present in the column raised_amount_usd ?
sum(is.na(master_frame$raised_amount_usd))
#19990


#2.2What do you replace NA values of raised_amount_usd with? Enter a numeric value.
#Here replacing the raised_amount_usd with a unique number 999999999999 which is not present in
#dataframe so that it can be filtered out in the further analysis, other option was to impute the data
#but the better option here is not taking into account this data as the data is very skewed
#the percentage is very high(17) for which this value is NA.
master_frame[["raised_amount_usd"]][is.na(master_frame["raised_amount_usd"])] <-999999999999
sum(master_frame$raised_amount_usd == 999999999999)


#Checkpoint 3: Funding Type Analysis==>

#All the below averages are calculated by filtering out the records for which raise_amount_usd was set to a unique value
#as these are taken to be the rounds for which there was no funding
mean(subset(master_frame,master_frame$funding_round_type == "venture"& master_frame$raised_amount_usd != 999999999999)$raised_amount_usd)
#11748949
mean(subset(master_frame,master_frame$funding_round_type == "angel"& master_frame$raised_amount_usd != 999999999999)$raised_amount_usd)
#958694.5
mean(subset(master_frame,master_frame$funding_round_type == "seed"& master_frame$raised_amount_usd != 999999999999)$raised_amount_usd)
#719818
mean(subset(master_frame,master_frame$funding_round_type == "private_equity"& master_frame$raised_amount_usd != 999999999999)$raised_amount_usd)
#73308593
#By the above values we can see that the best funding type where others are investing is Venture



#Checkpoint 4: Country Analysis==>

#Making the dataframes of the name of the english speaking countries
eng_countries<-read.delim("englishspeaking.txt",header = TRUE,stringsAsFactors = FALSE)
str(eng_countries)
install.packages("countrycode")
library(countrycode)
eng_countries$country_code<-countrycode(eng_countries$Name, 'country.name.en', 'wb')
eng_countries$eng_flag <- 1
#Replacing all the country_code which are blank to 'Not Available' so that we can filter them out
master_frame[["country_code"]][master_frame$country_code ==''] <-'Not Available'
sum(master_frame$country_code =='Not Available')
sum(master_frame$country_code == 'USA') #72308
length(unique(toupper(master_frame_cleaned$country_code)))

install.packages("tidyr")
library(tidyr)
install.packages("dplyr")
library(dplyr)

#Filtering the data with the most funded type of investement which was found in the previos checkpoint
master_frame_cleaned<-filter(master_frame,master_frame$raised_amount_usd!=999999999999 & master_frame$funding_round_type=='venture')

#Aggregating the cleaned data frame on the sum of raised_amount_usd and arranging it in descending manner
master_frame_cleaned_aggregate<-arrange(aggregate(raised_amount_usd~country_code,data=master_frame_cleaned,sum),desc(raised_amount_usd))

#Filering the records for which counrty_code was blank
master_frame_cleaned_aggregate<- filter(master_frame_cleaned_aggregate,master_frame_cleaned_aggregate$country_code!='Not Available')
#Adding a flag to identify which of the countries are English speaking or not
master_frame_english<-merge(x=master_frame_cleaned_aggregate,y=eng_countries,by = 'country_code',all.x = TRUE)
master_frame_english[["eng_flag"]][is.na(master_frame_english["eng_flag"])] <-0
master_frame_english<-arrange(master_frame_english,desc(raised_amount_usd))

#Making top9 countries data frame only English and combined
top9<-master_frame_english[1:9,]
top9_english<-filter(master_frame_english,master_frame_english$eng_flag==1)
top9_english<-top9_english[1:9,]
top9$Name <- countrycode(top9$country_code, 'wb', 'country.name.en')

#Checkpoint 5: Sector Analysis 1==>
#Making a new column primary_sector and applying the strsplit function on that
#to take out the primary_sector from it
master_frame$primary_sector<- master_frame$category_list
#lapply return a list so unlist it and making the type of the new column as character
master_frame$primary_sector<-unlist(lapply(master_frame$category_list,function (x) strsplit(x,"|",fixed = TRUE)[[1]][1]))
str(master_frame)
sum(is.na(master_frame$primary_sector))
#Replacing the primary sector with NA values to 'Not Available'
master_frame[["primary_sector"]][is.na(master_frame$primary_sector)] <- "Not Available"
length(unique(master_frame$primary_sector))

#Reading of mapping file and making it clean by removing the 'Blanks' sector
#records as there are only 8 main sectors
mapping<-read.csv("mapping.csv",stringsAsFactors = FALSE)
sum(mapping$Blanks == 1)
mapping <-mapping[-1,]
mapping <- mapping[,-3]
length(unique(mapping$category_list))

#More cleaning of the mapping file as na is 0 in the mapping file
library(stringr)
sum(str_count(mapping$category_list,pattern = "0"))
mapping$category_list<-str_replace(mapping$category_list,pattern = "0",replacement = "na")
mapping$category_list<-str_replace(mapping$category_list,pattern = "Fi0nce",replacement = "Finance")
mapping$category_list<-str_replace(mapping$category_list,pattern = "enterprise 2.na",replacement = "enterprise 2.0")

#Making the wide data format to long data format
mapping_long<-gather(mapping,main_sector,val,Automotive...Sports:Social..Finance..Analytics..Advertising)
mapping_long<-mapping_long[!(mapping_long$val==0),]
mapping_long <-mapping_long[,-3]
length(unique(mapping_long$category_list))

#Converting the common column to lower before merging
master_frame$primary_sector<- tolower(master_frame$primary_sector)
mapping_long$category_list<- tolower(mapping_long$category_list)

#Taking a left outer join between the two dataframes
master_frame <- merge(master_frame,mapping_long,by.x = "primary_sector",by.y ="category_list",all.x=TRUE)

#Replacing NA values of main_sector to 'Not Available'
master_frame[["main_sector"]][is.na(master_frame["main_sector"])] <- "Not Available"



#Checkpoint 6: Sector Analysis 2==>

#Making the top 3 English speaking countries Dataframe keeping in mind the investing condition
#of the spark fund management
#Finding the count and the sum of investments in the top 3 English Speaking countries

DA1<-filter(master_frame,master_frame$country_code=='USA'&master_frame$funding_round_type=='venture'&raised_amount_usd!='999999999999'& between(master_frame$raised_amount_usd,5000000,15000000))
length(DA1$raised_amount_usd)#12150
sum(DA1$raised_amount_usd)#108531347515
DA2<-filter(master_frame,master_frame$country_code=='GBR'&master_frame$funding_round_type=='venture'&raised_amount_usd!='999999999999',between(master_frame$raised_amount_usd,5000000,15000000))
length(DA2$raised_amount_usd)#628
sum(DA2$raised_amount_usd)#5436843539
DA3<-filter(master_frame,master_frame$country_code=='IND'&master_frame$funding_round_type=='venture'&raised_amount_usd!='999999999999',between(master_frame$raised_amount_usd,5000000,15000000))
length(DA3$raised_amount_usd)#330
sum(DA3$raised_amount_usd)#2976543602


DA1_group<- group_by(DA1,main_sector)
DA1_group_summ<-summarise(DA1_group,numbers=length(main_sector),total=sum(raised_amount_usd))
DA1<-merge(x = DA1, y = DA1_group_summ, by = "main_sector", all = TRUE)
DA1 <- arrange(DA1_final,desc(numbers))
DA1[1,]$main_sector#Top Sector of Country 1- Others
DA1_top_sector<-filter(DA1,DA1$main_sector=='Others')
DA1_top_sector_arranged<-arrange(aggregate(raised_amount_usd~company_permalink,data = DA1_top_sector,sum),desc(raised_amount_usd))
length(DA1_top_sector$raised_amount_usd)#2950
DA1[2951,]$main_sector#Second Sector of Country 1- Social..Finance..Analytics..Advertising
DA1_second_sector<-filter(DA1,DA1$main_sector=='Social..Finance..Analytics..Advertising')
DA1_second_sector_arranged<-arrange(aggregate(raised_amount_usd~company_permalink,data = DA1_second_sector,sum),desc(raised_amount_usd))
DA1[5665,]$main_sector#Third sector of DA1

DA2_group<- group_by(DA2,main_sector)
DA2_group_summ<-summarise(DA2_group,numbers=length(main_sector),total=sum(raised_amount_usd))
DA2<-merge(x = DA2, y = DA2_group_summ, by = "main_sector", all = TRUE)
DA2 <- arrange(DA2,desc(numbers))
DA2[1,]$main_sector#Top Sector of Country 2- Others
DA2_top_sector<-filter(DA2,DA2$main_sector=='Others')
DA2_top_sector_arranged<-arrange(aggregate(raised_amount_usd~company_permalink,data = DA2_top_sector,sum),desc(raised_amount_usd))

length(DA2_top_sector$raised_amount_usd)#147
DA2_final[148,]$main_sector#Second Sector of Country 2- Social..Finance..Analytics..Advertising
DA2_second_sector<-filter(DA2_final,DA2_final$main_sector=='Social..Finance..Analytics..Advertising')
DA2_second_sector_arranged<-arrange(aggregate(raised_amount_usd~company_permalink,data = DA2_second_sector,sum),desc(raised_amount_usd))
DA2[281,]$main_sector#Third sector of DA2

DA3_group<- group_by(DA3,main_sector)
DA3_group_summ<-summarise(DA3_group,numbers=length(main_sector),total=sum(raised_amount_usd))
DA3<-merge(x = DA3, y = DA3_group_summ, by = "main_sector", all = TRUE)
DA3 <- arrange(DA3,desc(numbers))
DA3[1,]$main_sector#Top Sector of Country 3- Others
DA3_top_sector<-filter(DA3,DA3$main_sector=='Others')
DA3_top_sector_arranged<-arrange(aggregate(raised_amount_usd~company_permalink,data = DA3_top_sector,sum),desc(raised_amount_usd))
length(DA3_top_sector$raised_amount_usd)#88
DA3[111,]$main_sector#Second Sector of Country 1- Social..Finance..Analytics..Advertising
DA3_second_sector<-filter(DA3,DA3$main_sector=='Social..Finance..Analytics..Advertising')
DA3_second_sector_arranged<-arrange(aggregate(raised_amount_usd~company_permalink,data = DA3_second_sector,sum),desc(raised_amount_usd))
DA3[172,]$main_sector#Third sector of DA3
