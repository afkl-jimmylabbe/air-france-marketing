##############################################################################
#                                                                            #
#            Title:  Search Engine Marketing with Air France Dataset         #
#                                                                            #
#           Author:  Jose Palacios, Nicole Lindsay, Kapil Seth,              #
#                    Dachao Sun, Jeanne Leong                                #
#           Course:  Data Science: R                                         #
#       Instructor:  Professor Thomas Kurnicki                                   #
#                                                                            # 
#       Num. Lines:  375                                                     #
#      Num. Commts:   87                                                     #
#     Commts Ratio:   23.2%                                                  #
#                                                                            #
#             Date:  December 17, 2020                                       #
#     Read-Me Note:  This is a collection of originally independent scripts  #
#                    written by multiple contributors and all members in the #
#                    team, and it may contain some running errors due to any #
#                    non-unform variable namings that had not been changed   #
#                    completely upon the completion of the project. The      #
#                    sections of script are organized by the questions which #
#                    guide the analyses at the best as it can be collected.  #
#                    Importing/cleaning data is included at the beginning.   #
#                                                                            #
#            Copyright (C) 2020  Hult International Business School          #
#                                                                            #
##############################################################################


# esential libraries used in all analyses that follow
library(readxl)
library(ggplot2)
library(plotly)
library(tibble)
library(fastDummies)
library(corrplot)
library(formattable)
library(dplyr)

# user-defined helper functions
pause_enter <- function() {
   cat("Press [ENTER] to proceed.")
   invisible(scan("stdin", character(), nlines = 1, quiet = TRUE))
}

my_normal <- function(x) {
   my_min     <- min(x, na.rm=TRUE)
   my_max     <- max(x, na.rm=TRUE)
   normalized <- (x - my_min)/(my_max - my_min)
   return(normalized)
}


##############################################################################
#********************* Importing and Cleaning Data **************************#  
##############################################################################
file_name  <- "./airfrance_data.xlsx"
sheet_name <- "DoubleClick"
AF_data    <- as.data.frame(read_excel(file_name, sheet=sheet_name))
cat(paste("Dataset has dimensions", nrow(AF_data),
          "by", ncol(AF_data), "columns"), "\n")

# re-name all variables by simplification
names(AF_data)[names(AF_data) == "Publisher ID"]             <- "publisher_id"
names(AF_data)[names(AF_data) == "Publisher Name"]           <- "publisher_name"
names(AF_data)[names(AF_data) == "Keyword ID"]               <- "keyword_id"
names(AF_data)[names(AF_data) == "Keyword"]                  <- "keyword"
names(AF_data)[names(AF_data) == "Match Type"]               <- "match_type"
names(AF_data)[names(AF_data) == "Campaign"]                 <- "campaign"
names(AF_data)[names(AF_data) == "Keyword Group"]            <- "keyword_group"
names(AF_data)[names(AF_data) == "Category"]                 <- "category"
names(AF_data)[names(AF_data) == "Bid Strategy"]             <- "bid_strategy"
names(AF_data)[names(AF_data) == "Keyword Type"]             <- "keyword_type"
names(AF_data)[names(AF_data) == "Status"]                   <- "status"
names(AF_data)[names(AF_data) == "Search Engine Bid"]        <- "search_engine_bid"
names(AF_data)[names(AF_data) == "Clicks"]                   <- "clicks"
names(AF_data)[names(AF_data) == "Click Charges"]            <- "click_charges"
names(AF_data)[names(AF_data) == "Avg. Cost per Click"]      <- "avg_cpc"
names(AF_data)[names(AF_data) == "Impressions"]              <- "impressions"
names(AF_data)[names(AF_data) == "Engine Click Thru %"]      <- "ctr"
names(AF_data)[names(AF_data) == "Avg. Pos."]                <- "avg_pos"
names(AF_data)[names(AF_data) == "Trans. Conv. %"]           <- "tcr"
names(AF_data)[names(AF_data) == "Amount"]                   <- "amount"
names(AF_data)[names(AF_data) == "Total Cost"]               <- "total_cost"
names(AF_data)[names(AF_data) == "Total Volume of Bookings"] <- "total_bookings"

AF_data["" == AF_data] <- "unknown"  # impute with "unknown"

# re-code new variable for Return On Advertising (ROA)
AF_data <- within(AF_data, roa <- amount / total_cost)

cat(paste("After re-coding ROA, there are", ncol(AF_data), "columns."), "\n")
pause_enter()

summary(AF_data[, "ctr"])
summary(AF_data[, "tcr"])



##############################################################################
#************************ Descriptive Statistics ****************************#  
##############################################################################
# glimpse(AF_data) # look into row data
# summary(AF_data) # summary stats for all variable
# pause_enter()



##############################################################################
#****************** Q1: (General) Approach to Improve ROA *******************#  
##############################################################################

## CPC Analysis (by Jose Palacios)
#

# create a spearate dataframe with 2 columns
Publishers       <- AF_data[, "publisher_name"]
CPC              <- AF_data[, "avg_cpc"]
airfrance_df     <- as.data.frame( cbind(CPC, Publishers) )

# create an 8-column dataframe
reg_df           <- dummy_cols(airfrance_df, select_columns="Publishers")
reg_df           <- subset(reg_df, select=-c(Publishers))
colnames(reg_df) <- c("CPC", "Google_Global", "Google_US", "MSN_Global",
                      "MSN_US", "Overture_Global", "Overture_US", "Yahoo_US")
reg_df[, "CPC"]  <- as.numeric(reg_df[, "CPC"])
reg_df$CPC       <- my_normal(reg_df$CPC)  # normalize CPC variable

# sample and train model
training_i       <- sample(1:nrow(reg_df), nrow(reg_df)*0.8)
testing_i        <- c(1:nrow(reg_df))[-training_i]
df_training      <- reg_df[training_i, ]
df_testing       <- reg_df[testing_i, ]
CPC_model        <- lm(data=df_training, formula=CPC ~ Google_Global + Google_US +
                       MSN_Global + MSN_US + Overture_Global + Overture_US)
#summary(CPC_model)


## Prob of Booking or not Analysis (by Jose Palacios)
#
AF_1 <- subset(AF_data, select=-c(1:11))
str(AF_1)

# converting every variable into normal
AF_1$search_engine_bid_norm <- my_normal(x=AF_1$search_engine_bid)
AF_1$clicks_norm            <- my_normal(x=AF_1$clicks)
AF_1$click_charges_norm     <- my_normal(x=AF_1$click_charges)
AF_1$avg_cost_p_click_norm  <- my_normal(x=AF_1$avg_cpc)
AF_1$impressions_norm       <- my_normal(x=AF_1$impressions)
AF_1$engine_click_thru_norm <- my_normal(x=AF_1$ctr)
AF_1$avg_pos_norm           <- my_normal(x=AF_1$avg_pos)
AF_1$trans_cov_norm         <- my_normal(x=AF_1$tcr)
#AF_1$trans_cost_trans_norm  <- my_normal(x=AF_1$tcr)
AF_1$amount_norm            <- my_normal(x=AF_1$amount)
AF_1$total_cost_norm        <- my_normal(x=AF_1$total_cost)
AF_1$total_bookings_norm    <- my_normal(x=AF_1$total_bookings)
AF_1$roa_norm               <- my_normal(x=AF_1$roa)

AF_1 <- subset(AF_1,select = -c(1:13))
AF_1$total_bookings_norm_b<-ifelse(AF_1$total_bookings_norm > 0, 1, 0)
str(AF_1)

# sampling and training/testing data
training_i  <- sample(1:nrow(AF_1), nrow(AF_1)*0.8)
testing_i   <- c(1:nrow(AF_1))[-training_i]

df_training <- AF_1[training_i,]
df_testing  <- AF_1[testing_i,]

# build models
AF_model    <- lm(data = df_training, formula = total_bookings_norm~search_engine_bid_norm+
                  clicks_norm+click_charges_norm+avg_cost_p_click_norm+impressions_norm+
                  engine_click_thru_norm+avg_pos_norm+trans_cov_norm+amount_norm+total_cost_norm,
                  family = "binomial")
AF_model2   <- lm(data = df_training, formula = total_bookings_norm~search_engine_bid_norm+
                  clicks_norm+click_charges_norm+impressions_norm+
                  engine_click_thru_norm+avg_pos_norm+trans_cov_norm+amount_norm+total_cost_norm,
                  family = "binomial")
AF_model3   <- lm(data = df_training, formula = total_bookings_norm~search_engine_bid_norm+
                  clicks_norm+click_charges_norm+impressions_norm+avg_pos_norm+trans_cov_norm+
                  amount_norm+total_cost_norm, family = "binomial")
AF_model4   <- lm(data = df_training, formula = total_bookings_norm~search_engine_bid_norm+
                  clicks_norm+click_charges_norm+impressions_norm+trans_cov_norm+
                  amount_norm+total_cost_norm, family = "binomial")
AF_model5   <- glm(data = df_training, formula = total_bookings_norm~search_engine_bid_norm+
                   clicks_norm+click_charges_norm+impressions_norm+trans_cov_norm+
                   amount_norm+total_cost_norm, family = "binomial")
#summary(AF_model)
#summary(AF_model2)
#summary(AF_model3)
#summary(AF_model4)
#summary(AF_model5)
pause_enter()



##############################################################################
#**************** Q2: Broad vs. Focused Keyword match_types *****************#  
##############################################################################

# distinct match type names
mtchtypes <- unique(AF_data[, "match_type"])
mtchtypes <- mtchtypes[mtchtypes != "N/A"]

# compute total & average ROA's
roa_mtchtypes_total <- c(1:length(mtchtypes)) * 0
roa_mtchtypes_avg   <- c(1:length(mtchtypes)) * 0

for (i in 1:length(mtchtypes)) {
   roa_mtchtypes_total[i] <- sum(AF_data[AF_data[,"match_type"]==mtchtypes[i], "roa"])
   roa_mtchtypes_avg[i]   <- mean(AF_data[AF_data[,"match_type"]==mtchtypes[i], "roa"])
}

# bar plots
barplot(roa_mtchtypes_total,
        names.arg=,
        main="Total ROA upon each match_type",
        xlab="match_type", ylab="Total ROA",
        border="black", col="gray")

#**************** Word Cloud of Keywords (Keyword Groups)  ******************#  
library(wordcloud)
kwords      <- unique(AF_data[, "keyword_group"]) # distinct keywords
kwords      <- kwords[!kwords %in% "Unassigned"]  # exclude "Unassigned"
kwords_freq <- numeric(length(kwords))            # "frequency" list
kwords_roa  <- numeric(length(kwords))            # total ROA for each word
for (i in 1:length(kwords)) {
   kwords_freq[i] <- sum(1 * (kwords[i] == AF_data[, "keyword_group"]))
   kwords_roa[i]  <- sum(AF_data[which(kwords[i] == AF_data[, "keyword_group"]), "roa"])
}

# generate the word cloud
wordcloud(kwords, kwords_freq, max.words=65, color=c("#2577aa", "blue", "#555555","green"))
pause_enter()



##############################################################################
#************** Q3: SE Publishers--Uniform or Tailored Strategy *************#  
##############################################################################

campaigns_distinct <- unique(AF_data[ , "campaign"]) # distinct campaign names
campaigns_roa_total <- numeric(length(campaigns_distinct))  # initialize
campaigns_roa_avg  <- numeric(length(campaigns_distinct))
for (i in 1:length(campaigns_distinct)) {
  campaigns_roa_total[i] <- sum(AF_data[which(campaigns_distinct[i] == AF_data[, "campaign"]), "roa"])
  campaigns_roa_avg[i]  <- mean(AF_data[which(campaigns_distinct[i] == AF_data[, "campaign"]), "roa"])
}
campaigns_transconv_total <- numeric(length(campaigns_distinct))
campaigns_transconv_avg  <- numeric(length(campaigns_distinct))
for (i in 1:length(campaigns_distinct)) {
  campaigns_transconv_total[i] <- sum(AF_data[which(campaigns_distinct[i] == AF_data[, "campaign"]), "tcr"])
  campaigns_transconv_avg[i]  <- mean(AF_data[which(campaigns_distinct[i] == AF_data[, "campaign"]), "tcr"])
}

campaign_name<-as.factor(AF_data$campaign)
campaigns_roa_total<-round(campaigns_roa_total, digits = 2)
campaigns_roa_avg<-round(campaigns_roa_avg, digits = 2)
campaigns_transconv_total<-round(campaigns_transconv_total, digits = 2)
campaigns_transconv_avg<-round(campaigns_transconv_avg, digits = 2)
levels<-c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", 
          "O", "P", "Q", "R", "S", "T", "U", "V", "W","X")
legend = c("A: Western Europe Destinations", "B: Geo Targeted DC", 
           "C: Air France Brand & French Destinations", "D: Air France Global Campaign", 
           "E: Unassigned", "F: Geo Targeted San Francisco", "G: Air France Branded", 
           "H: Geo Targeted New York", "I: Geo Targeted Miami", "J: Geo Targeted Detroit", 
           "K: Geo Targeted Boston", "L: Geo Targeted Houston", "M: Paris & France Terms", 
           "N: Google_Yearlong 2006", "O: Geo Targeted Philadelphia", "P: Geo Targeted Chicago", 
           "Q: Geo Targeted Seattle", "R: Geo Targeted Los Angeles", "S: French Destinations", 
           "T: Geo Targeted Atlanta", "U: General Terms", "V: Business Class", 
           "W: Outside Western Europe", "X: Geo Targeted Cincinnati")

campaigns_df <- as.data.frame(cbind(campaigns_distinct, campaigns_roa_avg, 
                              campaigns_transconv_total,campaigns_transconv_avg))

# campaign bar charts
b_camp<-barplot(height=campaigns_roa_avg, names=campaigns_distinct)

print(head(campaigns_roa_avg))
avg_roa_cp <- ggplot(campaigns_df, aes(x=campaigns_distinct, y=campaigns_roa_avg), fill=legend)+
  geom_bar(stat="identity", position=position_dodge(), colour="seashell")
avg_roa_cp + guides (fill = guide_legend(ncol = 1)) + xlab("Campaign") + ggtitle("Average ROA per Campaign")

avg_roa_cp <-ggplot(campaigns_df, aes(x = reorder(campaigns_distinct, campaigns_roa_avg), y = campaigns_roa_avg)) +
  geom_col(fill = "steelblue") +
  labs(x = "Campaign", y = "ROA (Average)", title = "ROA by Campaign", caption = "Air France") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##############################################################################
#**************** Q4: Improving Campaigns (for Publishers) ******************#  
#******** Q5: Campaign (Keywords Added or Dropped, Tatics Adjusted?) ********#
#**************** Q7: Future (SEM) Campaigns Suggestions ********************#
##############################################################################

aggregate(round(AF_data$roa, digits = 2), by=list(campaign=AF_data$campaign), FUN=sum)

campaigns_distinct  <- unique(AF_data[ , "campaign"]) # distinct campaign names

campaigns_roa_total <- numeric(length(campaigns_distinct))   # initialize
campaigns_roa_avg   <- numeric(length(campaigns_distinct))
for (i in 1:length(campaigns_distinct)) {
   campaigns_roa_total[i] <- round( sum(AF_data[which(campaigns_distinct[i] == AF_data[, "campaign"]), "roa"]), digits=2)
   campaigns_roa_avg[i]   <- round(mean(AF_data[which(campaigns_distinct[i] == AF_data[, "campaign"]), "roa"]), digits=2)
}

campaigns_transconv_total <- numeric(length(campaigns_distinct))
campaigns_transconv_avg   <- numeric(length(campaigns_distinct))
for (i in 1:length(campaigns_distinct)) {
   campaigns_transconv_total[i] <- round( sum(AF_data[which(campaigns_distinct[i] == AF_data[, "campaign"]), "tcr"]), digits=2)
   campaigns_transconv_avg[i]   <- round(mean(AF_data[which(campaigns_distinct[i] == AF_data[, "campaign"]), "tcr"]), digits=2)
}
pause_enter()



##############################################################################
#********************* Q6: KPI's (with Campaign Changes) ********************#  
##############################################################################

## Making Correlation Matrix to understand the correlation between the variables 
correlations<- cor(AF_data[, 12:22])
corrplot(correlations, method = "color")

# EXPLORATORY DATA ANALYYSIS
#
# Defining own function theme
theme_air_france <- function(){
  theme_minimal() +
    theme(
      text = element_text(color = "steelblue"),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(color = "#FC4E07"),
      plot.background = element_rect(fill = "white"),
      plot.margin = unit(c(5, 10, 5, 10), units = "mm")
    )
}

#Bar plot of Search Engine Bid Vs. Publisher name
ggplot(AF_data, aes(x = publisher_name, y = search_engine_bid, fill = publisher_name)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90))

#Bar plot of Average cost per click Vs. Publisher name. 
ggplot(AF_data, aes(x = publisher_name, y = avg_cpc, fill = publisher_name)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90))

#Bar plot of Impressions Vs. Publisher name.
ggplot(AF_data, aes(x = publisher_name, y = impressions, fill = publisher_name)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90))

#Bar plot of Total_Volume_of_Bookings Vs. Publisher name.
ggplot(AF_data, aes(x = publisher_name, y = total_bookings, fill = publisher_name)) + geom_col() + labs(x = "Publisher Name", y = "Total Volume of Bookings", title = "Total volume of bookings by publisher", caption = "Data source: Air France Internet Marketing") + theme_air_france() + theme(axis.text.x = element_text(angle = 90))

#Bar plot of Amount Vs. Publisher name. 
ggplot(AF_data, aes(x = publisher_name, y = amount, fill = publisher_name)) + geom_col() + labs(x = "Publisher Name", y = "Revenue", title = "Revenue generated from each publisher", caption = "Data source: Air France Internet Marketing") + theme_air_france() + theme(axis.text.x = element_text(angle = 90))

#Bar plot of Total_Cost Vs. Publisher name. 
ggplot(AF_data, aes(x = publisher_name, y = total_cost, fill = publisher_name)) + geom_col() + labs(x = "Publisher Name", y = "Total Cost", title = "Total Cost by publisher", caption = "Data source: Air France Internet Marketing") + theme_air_france() + theme(axis.text.x = element_text(angle = 90))

# Creating a new variable Average_CTR(Click Through Rate) for the analysis
Average_CTR <- AF_data %>%
  group_by(publisher_name) %>%
  summarise(mean_CTR = mean(clicks/impressions))
#Average_CTR
ggplot(Average_CTR, aes(x = publisher_name, y = mean_CTR, fill = publisher_name)) + geom_col() + labs(x = "Publisher Name", y = "Average Click Through Rate", title = "Average Click Through Rate per Publisher", caption = "Data source: Air France Internet Marketing") + theme_air_france() + theme(axis.text.x = element_text(angle = 90))

# Creating a new variable Average_TCP(Transaction Conversion Percentage) for the analysis 
Average_TCP <- AF_data %>%
  group_by(publisher_name) %>%
  summarise(mean_Trans_conv = mean(tcr))
#Average_TCP
ggplot(Average_TCP, aes(x = publisher_name, y = mean_Trans_conv, fill = publisher_name)) + geom_col() + labs(x = "Publisher Name", y = "Average Transaction conversion %", title = "Average Transaction conversion % for each publisher", caption = "Data source: Air France Internet Marketing") + theme_air_france() + theme(axis.text.x = element_text(angle = 90))

# Creating a new variable Average_Vol_of_Bookings for the analysis 
Average_Vol_of_Bookings <- AF_data %>%
  group_by(publisher_name) %>%
  summarise(mean_Volume_of_Bookings = mean(total_bookings))
#Average_Vol_of_Bookings
ggplot(Average_Vol_of_Bookings, aes(x = publisher_name, y = mean_Volume_of_Bookings, fill = publisher_name)) + geom_col() + theme(axis.text.x = element_text(angle = 90))

# Creating a new variable Average_Cost_per_Click for the analysis  
Cost_per_Click <- AF_data %>%
  group_by(publisher_name) %>%
  summarise(mean_CPC = mean(avg_cpc))
#Cost_per_Click
ggplot(Cost_per_Click, aes(x = publisher_name, y = mean_CPC, fill = publisher_name)) + geom_col() + theme(axis.text.x = element_text(angle = 90))
