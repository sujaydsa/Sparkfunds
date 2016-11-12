##############################################################################################################
# Libraries Used
##############################################################################################################
library(ggplot2)
library(dplyr)
library(plyr)

##############################################################################################################
#
#                                                  PART 1  (Checkpoint 1)
#
##############################################################################################################
#1. Load the two files companies.txt and rounds2.csv into two data frames and name them companies and rounds2 respectively.

companies <- read.delim("downloads/companies.txt",sep = "\t", header = T, stringsAsFactors = F)
rounds2 <- read.csv("downloads/rounds2.csv", header = T, stringsAsFactors = F)

#Check for duplicates
rounds2[which(duplicated(rounds2[,c("company_permalink","funding_round_permalink")])),]

#Table 1.1 : Understand the data set 

#Convert both the keys to same case to prevent issues while merging
companies$permalink <- tolower(companies$permalink)
rounds2$company_permalink <- tolower(rounds2$company_permalink)

# How many unique companies are present in rounds2?
# 66368

length(unique(rounds2$company_permalink))

# How many unique companies are present in companies ?
# 66368 

length(unique(companies$permalink))

#In the companies data frame, which column can be used as the  unique key for each company? Write the name of the column
# permalink

str(companies)
summary(companies)

# Are there any companies in the rounds2 file which are not  present in companies ? Answer Y/N.
# N
#unique(master_frame$funding_round_type)
setdiff(companies$permalink, rounds2$company_permalink)

# Merge the two data frames so that all  variables (columns)  in the companies frame 
# are added to the rounds2 data frame. 
# Name the merged frame master_frame. How many observations are present in master_frame ?
# 114949

master_frame <- merge(companies,rounds2,by.x="permalink",by.y="company_permalink",all=F)
nrow(master_frame)

##############################################################################################################
#
#                                                  PART 1 (Checkpoint 2) 
#
##############################################################################################################

# Question 1: How many NA values are present in the column raised_amount_usd ?
# Answer : 19990

sum(is.na(master_frame$raised_amount_usd))

# Question 2: What do you replace NA values of raised_amount_usd  with? 
# Answer 2: We first use a plot to determine outlier values

plot(density(log(master_frame$raised_amount_usd), na.rm=TRUE))

plot(density(log(master_frame[master_frame$funding_round_type=="angel", "raised_amount_usd"]),na.rm=TRUE))
plot(density(log(master_frame[master_frame$funding_round_type=="venture", "raised_amount_usd"]),na.rm=TRUE))
plot(density(log(master_frame[master_frame$funding_round_type=="seed", "raised_amount_usd"]),na.rm=TRUE))
plot(density(log(master_frame[master_frame$funding_round_type=="private_equity", "raised_amount_usd"]),na.rm=TRUE))

#Replace all the NA values from the raised_amount_usd column of the master frame.

#We make use of the following technique
# Get the mean of each funding type
# Use the mean value to fill the NAs in that type.
str(master_frame)

not_null <- master_frame[which(!is.na(master_frame$raised_amount_usd)),c("funding_round_type","raised_amount_usd")]

mean_by_funding_type <- aggregate(not_null[,"raised_amount_usd"], list(not_null$funding_round_type), mean)
mean_by_funding_type

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
master_frame <- ddply(master_frame, ~ funding_round_type, transform, raised_amount_usd = impute.mean(raised_amount_usd))

sum(is.na(master_frame$raised_amount_usd))

##############################################################################################################
#
#                                                  PART 1 (Checkpoint 3) 
#
##############################################################################################################


str(master_frame)
# Average funding amount of venture type
# 11748949

mean_venture <- mean(subset(master_frame,funding_round_type=="venture")$raised_amount_usd)

# Average funding amount of angel type
# 958694.5

mean_angel <- mean(subset(master_frame,funding_round_type=="angel")$raised_amount_usd)

# Average funding amount of seed type	 
# 719818

mean_seed <- mean(subset(master_frame,funding_round_type=="seed")$raised_amount_usd)

# Average funding amount of private equity type
# 73308593

mean_private_equity <- mean(subset(master_frame, funding_round_type=="private_equity")$raised_amount_usd)

# Question 5 : Considering that Spark Funds wants to invest between 5 to 15 million USD per  investment round, 
#             which investment type is the most suitable for them?
# Answer 5 :  Venture Type

##############################################################################################################
#
#                                                  PART 2 (Checkpoint 4) 
#
##############################################################################################################

# Question 1: Spark Funds wants to see the top 9 countries which have received the highest total 
#             funding (across ALL sectors for the chosen investment type).
# Answer 1:   Chosen investment type is venture capital.

selected_venture_subset <- subset(master_frame, funding_round_type=="venture")

country <- aggregate(raised_amount_usd ~ country_code, data = selected_venture_subset, FUN = sum)

# For the chosen investment type, make a data frame named top9 with top9 countries 
# (based on the total investment amount each country has received).
country_ordered <- country[order(country$raised_amount_usd, decreasing = TRUE),]
top9 <- head(unique(country_ordered$country_code), n = 9)
top9

##############################################################################################################
#
#                                                  PART 2 (Checkpoint 5) 
#
##############################################################################################################

# Use mapping file "mapping.csv" to map each primary sector to one of the 8 main sectors. 
# (Note that ‘Others’ is also considered one of the main sector)

mapping_list <- read.csv("downloads/mapping_file.csv", header = T, stringsAsFactors = F)
str(mapping_list)
mapping_list$category_list <- tolower(mapping_list$category_list)
master_frame$category_list <- tolower(master_frame$category_list)
# Extract the primary sector of each category list from the category_list column

master_frame$primary_sector <- tolower(gsub("\\|.*","", master_frame$category_list))
head(master_frame[,c("category_list","primary_sector")])

master_frame <- merge(master_frame,mapping_list, by.x = "primary_sector", by.y="category_list", all.x=TRUE)


# Check for Null values after Merge
# Assigning the Nulls to the mainsector Others (Not sure about this either)
sum(is.na(master_frame$main_sector))
unique(master_frame[which(is.na(master_frame$main_sector)),"primary_sector"])
master_frame[which(is.na(master_frame$main_sector)),]$main_sector <- "Others"
sum(is.na(master_frame$main_sector))

##############################################################################################################
#
#                                                  PART 2 (Checkpoint 6) 
#
##############################################################################################################
# Question :  Create three separate data frames D1, D2 and D3 for each of the 3 countries containing 
#             the observations of funding type FT  falling between 5 to 15 million USD range. 
#             The three data frames should contain:

#                 All the columns of the master_frame along with the primary sector and the main sector
#                 The total number (or count) of investments for each main sector in a separate column
#                 The total amount invested in each main sector in a separate column
#
# Answer : From previous analysis, the top 3 english countries are USA, IND and GBR

D1 <- subset(master_frame,master_frame$country_code == "USA" & master_frame$funding_round_type=="venture" & master_frame$raised_amount_usd >= 5000000 & master_frame$raised_amount_usd <= 15000000)
D2 <- subset(master_frame,master_frame$country_code == "GBR" & master_frame$funding_round_type=="venture" & master_frame$raised_amount_usd >= 5000000 & master_frame$raised_amount_usd <= 15000000)
D3 <- subset(master_frame,master_frame$country_code == "IND" & master_frame$funding_round_type=="venture" & master_frame$raised_amount_usd >= 5000000 & master_frame$raised_amount_usd <= 15000000)

#Total Number of investments & Total amount of investments
nrow(D1)
total_amt_USA <- sum(D1$raised_amount_usd)

nrow(D2)
total_amt_GBR <- sum(D2$raised_amount_usd)

nrow(D3)
total_amt_IND <- sum(D3$raised_amount_usd)

#Sector Wise Number of investments
freq_counts_USA <- as.data.frame(table(D1$main_sector))
colnames(freq_counts_USA) <- c ("main_sector","No. of Investments")
freq_counts_USA

freq_counts_GBR <- as.data.frame(table(D2$main_sector))
colnames(freq_counts_GBR) <- c ("main_sector","No. of Investments")

freq_counts_IND <- as.data.frame(table(D3$main_sector))
colnames(freq_counts_IND) <- c ("main_sector","No. of Investments")

D1 <- merge(D1,freq_counts_USA,by="main_sector", all.x=T)
D2 <- merge(D2,freq_counts_GBR,by="main_sector", all.x=T)
D3 <- merge(D3,freq_counts_IND,by="main_sector", all.x=T)

#Sector Wise Total amount of investments
sum_USA <- aggregate(raised_amount_usd ~ main_sector, D1, FUN = sum)
colnames(sum_USA) <- c ("main_sector","Total Amount Invested")

sum_GBR <- aggregate(raised_amount_usd ~ main_sector, D2, FUN = sum)
colnames(sum_GBR) <- c ("main_sector","Total Amount Invested")

sum_IND <- aggregate(raised_amount_usd ~ main_sector, D3, FUN = sum)
colnames(sum_IND) <- c ("main_sector","Total Amount Invested")

D1 <- merge(D1,sum_USA,by="main_sector", all.x=T)
D2 <- merge(D2,sum_GBR,by="main_sector", all.x=T)
D3 <- merge(D3,sum_IND,by="main_sector", all.x=T)

#Find the Top 3 sectors based on number of investments
D1 <- D1[order(D1$`No. of Investments`, decreasing = TRUE), ]
D2 <- D2[order(D2$`No. of Investments`, decreasing = TRUE), ]
D3 <- D3[order(D3$`No. of Investments`, decreasing = TRUE), ]

unique(D1$main_sector)
summary_D1 <- merge(freq_counts_USA,sum_USA)
summary_D2 <- merge(freq_counts_GBR,sum_GBR)
summary_D3 <- merge(freq_counts_IND,sum_IND)

summary_D1 <- summary_D1[order(summary_D1$`No. of Investments`, decreasing = TRUE), ]
summary_D2 <- summary_D2[order(summary_D2$`No. of Investments`, decreasing = TRUE), ]
summary_D3 <- summary_D3[order(summary_D3$`No. of Investments`, decreasing = TRUE), ]

str(D1)

#For USA, below section gives the following results

summary_D1
head(D1[D1$main_sector=="Others","name"], n=1)
top_company <- aggregate(raised_amount_usd ~ name, D1[D1$main_sector=="Others",], FUN = sum)
top_company <- top_company[order(top_company$raised_amount_usd, decreasing = TRUE),]
head(top_company)

head(D1[D1$main_sector=="Social, Finance, Analytics, Advertising","name"], n=1)
top_company <- aggregate(raised_amount_usd ~ name, D1[D1$main_sector=="Social, Finance, Analytics, Advertising",], FUN = sum)
top_company <- top_company[order(top_company$raised_amount_usd, decreasing = TRUE),]
head(top_company)

#For GBR, below section gives the following results

summary_D2
head(D2[D2$main_sector=="Others","name"], n=1)
top_company <- aggregate(raised_amount_usd ~ name, D2[D2$main_sector=="Others",], FUN = sum)
top_company <- top_company[order(top_company$raised_amount_usd, decreasing = TRUE),]
head(top_company)
D2[D2$main_sector=="Social, Finance, Analytics, Advertising","name"]
top_company <- aggregate(raised_amount_usd ~ name, D2[D2$main_sector=="Social, Finance, Analytics, Advertising",], FUN = sum)
top_company <- top_company[order(top_company$raised_amount_usd, decreasing = TRUE),]
head(top_company)

#For IND, below section gives the following results
summary_D3
head(D3[D3$main_sector=="Others","name"], n=1)
top_company <- aggregate(raised_amount_usd ~ name, D3[D3$main_sector=="Others",], FUN = sum)
top_company <- top_company[order(top_company$raised_amount_usd, decreasing = TRUE),]
head(top_company)
head(D3[D3$main_sector=="Social, Finance, Analytics, Advertising","name"], n=1)
top_company <- aggregate(raised_amount_usd ~ name, D3[D3$main_sector=="Social, Finance, Analytics, Advertising",], FUN = sum)
top_company <- top_company[order(top_company$raised_amount_usd, decreasing = TRUE),]
head(top_company)

##############################################################################################################
#
#                                                  PART 2 (Checkpoint 7: Plots) 
#
##############################################################################################################
# Question :One pie chart showing the fraction of total investments (globally) 
#           in venture, seed and private equity and the average amount of investment in each funding type. 
#           This chart should make it clear that a certain funding type (FT) is best suited for Spark Funds.
master_frame[master_frame$funding_round_type == "private equity",]
unique(master_frame$funding_round_type)

subset_selected_types <- subset(master_frame,master_frame$funding_round_type %in% c("venture","seed","private_equity"))
sum_venture <- sum(master_frame[master_frame$funding_round_type == "venture","raised_amount_usd"])
sum_seed <- sum(master_frame[master_frame$funding_round_type == "seed","raised_amount_usd"])
sum_private_equity <- sum(master_frame[master_frame$funding_round_type == "private_equity","raised_amount_usd"])
investments <- c(sum_private_equity,sum_seed,sum_venture)
lbls <- c("Private Equity", "Seed", "Venture")
mean_investments <- c(mean_private_equity,mean_seed, mean_venture)
pct <- round(investments/sum(investments)*100)
lbls <- paste(lbls,pct)
lbls <- paste(lbls,"%",sep="")
lbls <- paste(lbls,"\nAverage:")
lbls <- paste(lbls,mean_investments)
pie(investments,labels = lbls,col=rainbow(length(lbls)))

# Question :One bar chart showing top 9 countries against the total amount of investments of funding type FT. 
#           This should make the top 3 countries (Country 1, Country 2 and Country 3) very clear.

top9 <- head(unique(country_ordered), n = 9)
top9
ggplot(data = top9, aes(x=reorder(country_code, -raised_amount_usd), y=(raised_amount_usd/100000000))) +
  labs(x="Countries", y="Total Amount of Investments in 100 Millions") +
  geom_bar(stat="identity")


# Question :Any chart type you think is suitable: This should show the number of investments 
#           in the top 3 sectors of the top 3 countries on one chart (for the chosen investment type FT). 

x <- "USA"
country1 <- data.frame(summary_D1,x)
x <- "GBR"
country2 <- data.frame(summary_D2,x)
x <- "IND"
country3 <- data.frame(summary_D3,x)


colnames(country1) <- c("Sector", "No. of Investments", "Total Amount Invested", "Country")
colnames(country2) <- c("Sector", "No. of Investments", "Total Amount Invested", "Country")
colnames(country3) <- c("Sector", "No. of Investments", "Total Amount Invested", "Country")
country1

top3 <- rbind(ddply(country1, "Country", function(x) head(x[order(x$`No. of Investments`, decreasing = TRUE) , ], 3)))
top3<- rbind(top3,ddply(country2, "Country", function(x) head(x[order(x$`No. of Investments`, decreasing = TRUE) , ], 3)))
top3<- rbind(top3,ddply(country3, "Country", function(x) head(x[order(x$`No. of Investments`, decreasing = TRUE) , ], 3)))

top3



ggplot(data = top3, aes(x = Country, y = (`No. of Investments`), fill=Sector)) + 
  geom_bar(position="dodge", stat="identity") 

