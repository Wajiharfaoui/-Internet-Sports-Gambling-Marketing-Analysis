#install.packages("assertive")
#install.packages("lubridate")
#install.packages(("tidyr"))
#install.packages("ggplot")
library('tidyr')
library('dplyr')
library('assertive')
library('lubridate')
library('readr')
library('stringr')
library('ggplot2')

# Explore the dataset
load('./Data/DataGroupAssignment.rdata')
head(Demographics)
head(PokerChipConversions)
head(UserDailyAggregation)
str(Demographics)
str(PokerChipConversions)
str(UserDailyAggregation)

# Check outliers
assert_all_are_in_closed_range(Demographics$Country, lower=8, upper = 895)
assert_all_are_in_closed_range(Demographics$Language, lower=1, upper = 17)
assert_all_are_in_closed_range(Demographics$Gender, lower=0, upper = 1)
assert_all_are_in_closed_range(Demographics$ApplicationID, lower=1, upper = 42)
assert_all_are_in_closed_range(PokerChipConversions$TransType, lower=24, upper = 124)
assert_all_are_in_closed_range(PokerChipConversions$TransAmount, lower=0.000064437, upper = 15995.53)
assert_all_are_in_closed_range(UserDailyAggregation$ProductID, lower=1, upper = 8)
assert_all_are_in_closed_range(UserDailyAggregation$Stakes, lower=-28.3, upper = 272415.5)
assert_all_are_in_closed_range(UserDailyAggregation$Winnings, lower=-40, upper = 270222.0)
assert_all_are_in_closed_range(UserDailyAggregation$Bets, lower=-1, upper = 6223)

# exclude one row where the gender is missing 
Demographics= Demographics[-c(37744),]

# Check missing values 
sum(is.na(UserDailyAggregation))
sum(is.na(Demographics))
sum(is.na(PokerChipConversions))
# Checking types
for (i in (1:ncol(PokerChipConversions))){
  print(class(PokerChipConversions[[i]])) 
}
for (i in (1:ncol(Demographics))){
  print(class(Demographics[[i]])) 
}
for (i in (1:ncol(UserDailyAggregation))){
 print(class(UserDailyAggregation[[i]])) 
}

# Converting date to date type
UserDailyAggregation$Date <- ymd(UserDailyAggregation$Date)
class(UserDailyAggregation$Date)


PokerChipConversions$TransDateTime <- ymd_hms(PokerChipConversions$TransDateTime)
class(UserDailyAggregation$Date)


Demographics$RegDate <- ymd(Demographics$RegDate)
Demographics[, 5:10] <- lapply(Demographics[,5:10], ymd)

#Merging application, country and language to demographics table
language = read_csv('./Data/language.csv')
head(language)
Demographics <- left_join(Demographics, language, by ='Language')%>%
                  rename_at(vars("Language Description"), ~"LanguageDescription")%>%
                  select(-c("Language"))

country = read_csv('./Data/countries.csv')
head(country)
Demographics <- left_join(Demographics, country, by ='Country')%>%
                  rename_at(vars("Country Name"), ~"CountryName")%>%
                  select(-c("Country"))

application = read_csv('./Data/application.csv')
head(application)
Demographics <- left_join(Demographics, application, by ='ApplicationID')%>%
                  rename_at(vars("Application Description"), ~"ApplicationDescription")%>%
                  select(-c("ApplicationID"))

#Dropping row if gender is NA
Demographics <- Demographics[!is.na(Demographics$Gender), ]
#Checking if NA is dropped
unique(Demographics$Gender)
Demographics$Gender[Demographics$Gender == 0] <- "Female"
Demographics$Gender[Demographics$Gender == 1] <- "Male"



# test whether there is records that took place before the first pay-in 

a <- UserDailyAggregation%>% 
  group_by(UserID)%>% 
  summarise_at("Date",max, na.rm = TRUE)
test <- left_join(Demographics,a,by='UserID')
typeof(test$FirstPay)
typeof(test$Date)
test$diff <- test$FirstPay-test$Date
test$diff <- as.numeric(test$diff)
test[test$diff>0,]

# Aggregation function (PokerChipsConversions)
poker_agg_func <- function (dataset,prefix){
  
  Agg_count <- dataset %>% group_by(UserID, TransType)%>%
    summarise(n = n())%>%spread(key = TransType, value = n)%>%
    rename('BuyFreq' = '124', 'SellFreq' = '24')
  
  Agg_sum <- dataset %>% group_by(UserID, TransType)%>%
    summarise(amount = sum(TransAmount))%>%
    spread(key = TransType, value = amount)%>%
    rename('BuyTotalAmount' = '124', 'SellTotalAmount' = '24')
 
  Agg_min <- dataset %>% group_by(UserID, TransType)%>%
    summarise(min_amount = min(TransAmount))%>%
    spread(key = TransType, value = min_amount)%>%
    rename('BuyMinAmount' = '124', 'SellMinAmount' = '24')
  
  Agg_max <- dataset %>% group_by(UserID, TransType)%>%
    summarise(max_amount = max(TransAmount))%>%
    spread(key = TransType, value = max_amount)%>%
    rename('BuyMaxAmount' = '124', 'SellMaxAmount' = '24')
  
  Agg_mean <- dataset %>% group_by(UserID, TransType)%>%
    summarise(mean_amount = mean(TransAmount))%>%
    spread(key = TransType, value = mean_amount)%>%
    rename('BuyAverageAmount' = '124', 'SellAverageAmount' = '24')
  
  Agg_date_min <- dataset %>% group_by(UserID, TransType)%>%
    summarise(first_date = min(TransDateTime))%>%
    spread(key = TransType, value = first_date)%>%
    rename('FirstBuyDate' = '124', 'FirstSellDate' = '24')

  Agg_date_max <- dataset %>% group_by(UserID, TransType)%>%
    summarise(last_date = max(TransDateTime))%>%
    spread(key = TransType, value = last_date)%>%
    rename('LastBuyDate' = '124', 'LastSellDate' = '24')
  
  PokerAgg <- left_join(Agg_count,Agg_sum, by='UserID') %>% 
  left_join(.,Agg_min, by='UserID')%>% 
  left_join(.,Agg_max, by='UserID')%>% 
  left_join(.,Agg_mean, by='UserID')%>%
  left_join(.,Agg_date_max, by='UserID')%>%
  left_join(.,Agg_date_min, by='UserID')
  
  names(PokerAgg) = paste0(prefix,"_", names(PokerAgg))
  
  return (PokerAgg)
  
}

# Aggregate the pokerchipsconversions
PokerFinal <- poker_agg_func(PokerChipConversions,"Prod3")
names(PokerFinal)[1] <- 'UserID'
# Remove product 3 
UserDaily <- filter(UserDailyAggregation, ProductID != 3)

# product list 
prod_list <- list()
for (i in c(1,2,4,5,6,7,8)){
  prod_list[[i]] = UserDaily %>% filter(ProductID==i)
}

# Overall aggregation(UserDaily)
agg_func <- function(dataset){
  
  
  Agg_Sum <- dataset %>% group_by(UserID)%>% 
    summarise_at(c("Stakes", "Winnings","Bets"),sum, na.rm = TRUE)%>% 
    rename_at(vars(c("Stakes", "Winnings","Bets")), ~ c("sum_Stakes", "sum_Winnings","sum_Bets"))
  
  Agg_min <- dataset %>% group_by(UserID)%>% 
    summarise_at(c("Stakes", "Winnings","Bets"),min, na.rm = TRUE)%>% 
    rename_at(vars(c("Stakes", "Winnings","Bets")), ~ c("min_Stakes", "min_Winnings","min_Bets"))
  
  Agg_max <- dataset %>% group_by(UserID)%>% 
    summarise_at(c("Stakes", "Winnings","Bets"),max, na.rm = TRUE)%>% 
    rename_at(vars(c("Stakes", "Winnings","Bets")), ~ c("max_Stakes", "max_Winnings","max_Bets"))
  
  Agg_mean <- dataset %>% group_by(UserID)%>% 
    summarise_at(c("Stakes", "Winnings","Bets"),mean, na.rm = TRUE)%>% 
    rename_at(vars(c("Stakes", "Winnings","Bets")), ~ c("mean_Stakes", "mean_Winnings","mean_Bets"))
  
  Agg_count <- dataset %>% group_by(UserID)%>% 
    summarise(count = n())
  
  Agg_Date_max <- dataset %>% group_by(UserID)%>% 
    summarise_at("Date",max, na.rm = TRUE)%>% 
    rename_at(vars("Date"), ~"Last_date_played")
  
  Agg_Date_min <- dataset %>% group_by(UserID)%>% 
    summarise_at(("Date"),min, na.rm = TRUE)%>% 
    rename_at(vars("Date"), ~"First_date_played")
  

  
  UserDailyAgg <- left_join(Agg_Sum,Agg_min, by='UserID') %>% 
    left_join(.,Agg_max, by='UserID')%>% 
    left_join(.,Agg_mean, by='UserID')%>% 
    left_join(.,Agg_count, by='UserID')%>%
    left_join(.,Agg_Date_max, by='UserID')%>%
    left_join(.,Agg_Date_min, by='UserID')
  
  UserDailyAgg$Active_Days <- UserDailyAgg$Last_date_played - UserDailyAgg$First_date_played
  
  UserDailyAgg$Total_Diff <- UserDailyAgg$sum_Winnings - UserDailyAgg$sum_Stakes
  
  return(UserDailyAgg)
}

# Product Aggregation (User Daily)
prod_agg_func <- function (dataset,prefix){
  
  
  Agg_Sum_Prod <- dataset %>% group_by(UserID)%>% 
    summarise_at(c("Stakes", "Winnings","Bets"),sum, na.rm = TRUE)%>% 
    rename_at(vars(c("Stakes", "Winnings","Bets")), ~ c("sum_Stakes", "sum_Winnings","sum_Bets"))
  
  Agg_min_Prod <- dataset %>% group_by(UserID)%>% 
    summarise_at(c("Stakes", "Winnings","Bets"),min, na.rm = TRUE)%>% 
    rename_at(vars(c("Stakes", "Winnings","Bets")), ~ c("min_Stakes", "min_Winnings","min_Bets"))
  
  Agg_max_Prod <- dataset %>% group_by(UserID)%>% 
    summarise_at(c("Stakes", "Winnings","Bets"),max, na.rm = TRUE)%>% 
    rename_at(vars(c("Stakes", "Winnings","Bets")), ~ c("max_Stakes", "max_Winnings","max_Bets"))
  
  Agg_mean_Prod <- dataset %>% group_by(UserID)%>% 
    summarise_at(c("Stakes", "Winnings","Bets"),mean, na.rm = TRUE)%>% 
    rename_at(vars(c("Stakes", "Winnings","Bets")), ~ c("mean_Stakes", "mean_Winnings","mean_Bets"))
  
  Agg_count_Prod <- dataset %>% group_by(UserID)%>% 
    summarise(count = n())%>% 
    rename_at(vars("count"), ~"Freq")
  
  Agg_Date_max_Prod <- dataset %>% group_by(UserID)%>% 
    summarise_at("Date",max, na.rm = TRUE)%>% 
    rename_at(vars("Date"), ~"Last_date_played")
  
  Agg_Date_min_Prod <- dataset %>% group_by(UserID)%>% 
    summarise_at(("Date"),min, na.rm = TRUE)%>% 
    rename_at(vars("Date"), ~"First_date_played")
  
  UserDailyAgg <- left_join(Agg_Sum_Prod,Agg_min_Prod, by='UserID') %>% 
    left_join(.,Agg_max_Prod, by='UserID')%>% 
    left_join(.,Agg_mean_Prod, by='UserID')%>% 
    left_join(.,Agg_count_Prod, by='UserID')%>%
    left_join(.,Agg_Date_max_Prod, by='UserID')%>%
    left_join(.,Agg_Date_min_Prod, by='UserID')
  
  UserDailyAgg$Active_Days <- UserDailyAgg$Last_date_played - UserDailyAgg$First_date_played
  
  UserDailyAgg$Total_Diff <- UserDailyAgg$sum_Winnings - UserDailyAgg$sum_Stakes
  
  names(UserDailyAgg) = paste0(prefix,"_", names(UserDailyAgg))
  
  return(UserDailyAgg)
}

# Apply the aggregation function for total and each product
UserDailyAggTotal <- agg_func(UserDaily)

agg_list <- list()
for (i in c(1,2,4,5,6,7,8)){
  agg_list[[i]] <- prod_agg_func(prod_list[[i]],paste0('Prod',i))
  names(agg_list[[i]])[1] <- 'UserID'
}

# Merge each product's list into a dataframe
UserDailyAggProd = agg_list[[1]]
for (i in c(2,4,5,6,7,8)){
UserDailyAggProd <- left_join(UserDailyAggProd,agg_list[[i]], by='UserID') 
}

# Merge Total with products dataframes
UserDailyFinal <- left_join(UserDailyAggTotal,UserDailyAggProd, by='UserID')

# create basetable 
Basetable <- left_join(Demographics,UserDailyFinal,by='UserID')%>%
  left_join(.,PokerFinal, by='UserID')


# have a copy of the basetable 
segmentation <- Basetable[,c(1,28,25,13)] %>% 
  rename("Recency"="Active_Days","Frequency"="count","Monetary"="sum_Stakes")
 
segmentation[,2] <- lapply(segmentation[,2],function(x) as.numeric(gsub(".*?([0-9]+).*", "\\1", x))) 
summary(segmentation)

# Segmentation 
segmentation$R_score <- 0
segmentation$R_score[segmentation$Recency >= 211.00] <- 1
segmentation$R_score[segmentation$Recency >= 115.00 & segmentation$Recency <211.00] <- 2
segmentation$R_score[segmentation$Recency >= 21.00 & segmentation$Recency <115.00] <- 3
segmentation$R_score[segmentation$Recency < 21.00] <- 4
segmentation$F_score<- 0
segmentation$F_score[segmentation$Frequency >=52] <- 4
segmentation$F_score[segmentation$Frequency <52 & segmentation$Frequency >= 39.7] <- 3
segmentation$F_score[segmentation$Frequency <39.7 & segmentation$Frequency >= 8.0] <- 2
segmentation$F_score[segmentation$Frequency <8.0] <- 1
segmentation$M_score <- 0
segmentation$M_score[segmentation$Monetary >= 1127196.0] <- 4
segmentation$M_score[segmentation$Monetary < 1127196.0 & segmentation$Monetary >= 2773.2] <- 3
segmentation$M_score[segmentation$Monetary >= 2773.2 & segmentation$Monetary < 77.3] <- 2
segmentation$M_score[segmentation$Monetary <77.3] <- 1

segmentation <- segmentation %>% mutate(RFM_score = 100 *R_score +10 * F_score + M_score)

# Labeling 
summary(segmentation$RFM_score)

segmentation$Segment <- "0"
segmentation$Segment[which(segmentation$RFM_score <= 430.0 & segmentation$RFM_score >= 343.0)] <-"Loyalists"
segmentation$Segment[which(segmentation$RFM_score <343.0 & segmentation$RFM_score >= 270.5)] <-"Potential Loyalists"
segmentation$Segment[which(segmentation$RFM_score <270.5 & segmentation$RFM_score >= 143.0)] <-"Promising"
segmentation$Segment[which(segmentation$RFM_score <143.0)] <-"Need Attention"

table(segmentation$Segment)
ggplot(segmentation) + geom_bar(aes(x = Segment, fill = Segment))+theme(axis.text.x=element_text(angle=90,hjust=1)) +labs(title = "Barplot for Segments of customers")


#Appending RFM segment to the basetable
Basetable$rfm_segment <- segmentation$Segment

# Kmeans clustering for Products
basetable_k_means <- Basetable
basetable_k_means$Prod3_Freq <- basetable_k_means$Prod3_BuyFreq + basetable_k_means$Prod3_SellFreq
basetable_k_means$Prod3_sum_Stakes <- basetable_k_means$Prod3_BuyTotalAmount
basetable_k_means$Prod3_Active_Days <- as.numeric(make_date(2006, 1, 1) - as.Date(basetable_k_means$Prod3_FirstBuyDate))

li_prod <- c('Prod1_', 'Prod2_', 'Prod3_', 'Prod4_', 'Prod5_','Prod6_', 'Prod7_', 'Prod8_')
li_col <- c('Active_Days', 'Freq', 'sum_Stakes')
summary <- list()
for(name in li_prod){
  col <- vector()
  for(name_2 in  li_col){
    col <- append(col, paste(name,name_2, sep=""))
  }
  data <- basetable_k_means[col]
  for(c in col){
    data <- data %>% mutate(!!c:= as.numeric(!!as.name(c)))
  }
  col <- append(col, 'UserID')
  data[is.na(data)] <- 0
  data <- scale(data)
  kms <- kmeans(data, centers = 4, nstart = 50)
  col_name <- paste(name, 'cluster', sep="")
  basetable_k_means <- basetable_k_means%>%mutate(!!col_name := kms$cluster)
  s<-basetable_k_means%>%group_by(!!as.name(col_name))%>%summarise(bets =  mean(!!as.name(col[3])), days =  mean(!!as.name(col[1])),fre = mean(!!as.name(col[2])),count = n())%>%
    arrange(desc(bets) , desc(fre), desc(days))
  summary <- append(summary, list(s))
}
print(summary)

for(s in 1:length(summary)){
  str1 <- paste('Prod', s, sep="")
  str <- colnames(summary[[s]][1])
  t <- summary[[s]][str]
  t_1 <- dplyr::pull(t, !!as.name(str))
  t_values <- list(paste(str1,'_high_value', sep=""), paste(str1,'_medium_value', sep=""), paste(str1,'_low_value', sep=""), paste(str1,'_never_played', sep=""))
  basetable_k_means <- basetable_k_means %>% mutate(!!str:=factor(!!as.name(str), 
                                                      levels=t_1, 
                                                      labels=t_values))
}

#Assigning Clusters based on Products
Basetable$Prod1_cluster <- basetable_k_means$Prod1_cluster
Basetable$Prod2_cluster <- basetable_k_means$Prod2_cluster
Basetable$Prod3_cluster <- basetable_k_means$Prod3_cluster
Basetable$Prod4_cluster <- basetable_k_means$Prod4_cluster
Basetable$Prod5_cluster <- basetable_k_means$Prod5_cluster
Basetable$Prod6_cluster <- basetable_k_means$Prod6_cluster
Basetable$Prod7_cluster <- basetable_k_means$Prod7_cluster
Basetable$Prod8_cluster <- basetable_k_means$Prod8_cluster

################# Calculate the count per product #################
Prod1_Freq <- na.omit(Basetable$Prod1_Freq)
Prod1_total_count <- sum(Prod1_Freq)

Prod2_Freq <- na.omit(Basetable$Prod2_Freq)
Prod2_total_count <- sum(Prod2_Freq)

Prod3_Freq <- na.omit(basetable_k_means$Prod3_Freq)
Prod3_total_count <- sum(Prod3_Freq)

Prod4_Freq <- na.omit(Basetable$Prod4_Freq)
Prod4_total_count <- sum(Prod4_Freq)

Prod5_Freq <- na.omit(Basetable$Prod5_Freq)
Prod5_total_count <- sum(Prod5_Freq)

Prod6_Freq <- na.omit(Basetable$Prod6_Freq)
Prod6_total_count <- sum(Prod6_Freq)

Prod7_Freq <- na.omit(Basetable$Prod7_Freq)
Prod7_total_count <- sum(Prod7_Freq)

Prod8_Freq <- na.omit(Basetable$Prod8_Freq)
Prod8_total_count <- sum(Prod8_Freq)

product_count <- c(Prod1_total_count, Prod2_total_count, Prod3_total_count, Prod4_total_count, Prod5_total_count, Prod6_total_count, Prod7_total_count, Prod8_total_count)
product <- c("Sports book fixed-odd", "Sports book live-action", "Poker BossMedia", "Casino BossMedia", "Supertoto", "Games VS", "Games bwin", "Casino Chartwell")

product_df <- data.frame(Product = product, Number_of_Visits = product_count)
product_df <- product_df[order(product_df$Number_of_Visits, decreasing = TRUE),]
product_df
# Save tables as csv format 
write.csv(UserDailyFinal,".\\Data\\UserDailyFinal.csv", row.names = FALSE)
write.csv(PokerFinal,".\\Data\\PokerFinal.csv", row.names = FALSE)
write.csv(Demographics,".\\Data\\DemographicsFinal.csv", row.names = FALSE)
write.csv(Basetable,".\\Data\\Basetable.csv", row.names = FALSE)
write.csv(segmentation,".\\Data\\Segments.csv", row.names = FALSE)
write.csv(product_df,".\\Data\\product_count.csv", row.names = FALSE)
