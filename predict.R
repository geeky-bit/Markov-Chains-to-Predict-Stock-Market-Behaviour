library(ggplot2)
library(TTR)
library(dplyr)
library(reshape2)
library(gridExtra)
library(quantmod)
library(dplyr)
library(infotheo)
library(caret)


#DJ data frame
dj <- read.csv("../input/Dow_Jones_Industrial_Average.csv")
dj <- dj[1:1000,]
DJ<- data.frame(dj)

DJ$Close.Date <- as.Date(DJ$Date, format="%Y %m %d")
DJ$DJ.Open <- DJ$Open
DJ$DJ.High <- DJ$High
DJ$DJ.Low <- DJ$Low
DJ$DJ.Close <- DJ$Close
DJ$DJ.Volume <- DJ$Volume
DJ$DJ.Adjusted <- DJ$Adj.Close
DJ$Close.Date <- DJ$Date

# extract the date row name into a date column
DJ$Close.Date <- row.names(DJ)

#Look at the head
head(DJ)
##         Date     Open     High      Low    Close    Volume Adj.Close
## 1 2016-07-01 17924.24 18002.38 17916.91 17949.37  82160000  17949.37
## 2 2016-06-30 17712.76 17930.61 17711.80 17929.99 133030000  17929.99
## 3 2016-06-29 17456.02 17704.51 17456.02 17694.68 106380000  17694.68
## 4 2016-06-28 17190.51 17409.72 17190.51 17409.72 112190000  17409.72
## 5 2016-06-27 17355.21 17355.21 17063.08 17140.24 138740000  17140.24
## 6 2016-06-24 17946.63 17946.63 17356.34 17400.75 239000000  17400.75

# take random sets of sequential rows 
new_set <- c()
for (row_set in seq(100)) {
     row_quant <- sample(10:30, 1)
     print(row_quant)
     row_start <- sample(1:(nrow(DJ) - row_quant), 1)
     market_subset <- DJ[row_start:(row_start + row_quant),]
     market_subset <- dplyr::mutate(market_subset, 
                                    Close_Date = max(market_subset$Close.Date),
                                    Close_Gap=(DJ.Close - lag(DJ.Close))/lag(DJ.Close) ,
                                    High_Gap=(DJ.High - lag(DJ.High))/lag(DJ.High) ,
                                    Low_Gap=(DJ.Low - lag(DJ.Low))/lag(DJ.Low),
                                    Volume_Gap=(DJ.Volume - lag(DJ.Volume))/lag(DJ.Volume),
                                    Daily_Change=(DJ.Close - DJ.Open)/DJ.Open,
                                    Outcome_Next_Day_Direction= (lead(DJ.Volume)-DJ.Volume)) %>%
          dplyr::select(-DJ.Open, -DJ.High, -DJ.Low, -DJ.Close, -DJ.Volume, -DJ.Adjusted, -Close.Date) %>%
          na.omit
        
     market_subset$Sequence_ID <- seq.int(nrow(market_subset))
     new_set <- rbind(new_set, market_subset)
}

dim(new_set)
## [1] 1976   15

# Create sequences here. This simplifies the data by binning values in only three groups.

# Close_Gap
range(new_set$Close_Gap)
data_dicretized <- discretize(new_set$Close_Gap, disc="equalfreq", nbins=3)
new_set$Close_Gap <- data_dicretized$X
new_set$Close_Gap_LMH <- ifelse(new_set$Close_Gap == 1, 'L', 
                                ifelse(new_set$Close_Gap ==2, 'M','H'))


# Volume_Gap
range(new_set$Volume_Gap)
data_dicretized <- discretize(new_set$Volume_Gap, disc="equalfreq", nbins=3)
new_set$Volume_Gap <- data_dicretized$X
new_set$Volume_Gap_LMH <- ifelse(new_set$Volume_Gap == 1, 'L', 
                                 ifelse(new_set$Volume_Gap ==2, 'M','H'))

# Daily_Change
range(new_set$Daily_Change)
data_dicretized <- discretize(new_set$Daily_Change, disc="equalfreq", nbins=3)
new_set$Daily_Change <- data_dicretized$X
new_set$Daily_Change_LMH <- ifelse(new_set$Daily_Change == 1, 'L', 
                                   ifelse(new_set$Daily_Change ==2, 'M','H'))

# new set
new_set <- new_set[,c("Sequence_ID", "Close_Date", "Close_Gap_LMH", "Volume_Gap_LMH", "Daily_Change_LMH", "Outcome_Next_Day_Direction")]

new_set$Event_Pattern <- paste0(new_set$Close_Gap_LMH,      
                                new_set$Volume_Gap_LMH, 
                                new_set$Daily_Change_LMH) 


# reduce set 
compressed_set <- dplyr::group_by(new_set, Sequence_ID, Close_Date) %>%
     dplyr::summarize(Event_Pattern = paste(Event_Pattern, collapse = ",")) %>%
     data.frame
compressed_set <- merge(x=compressed_set,y=select(new_set, Sequence_ID, Outcome_Next_Day_Direction) %>%
                             dplyr::group_by(Sequence_ID) %>% 
                             dplyr::slice(n()) %>%
                             dplyr::distinct(Sequence_ID), by='Sequence_ID')


compressed_set <- merge(x=compressed_set,y=new_set[,c(1,6)], by="Sequence_ID")

#Validation time 

library(dplyr)
compressed_set_validation <- filter(compressed_set, Close_Date >= 50)
dim(compressed_set_validation)
compressed_set <- filter(compressed_set, Close_Date < 50)
dim(compressed_set)

compressed_set <- select(compressed_set, -Close_Date)
compressed_set_validation <- select(compressed_set_validation, -Close_Date)


# only keep big moves
summary(compressed_set$Outcome_Next_Day_Direction)
compressed_set <- compressed_set[abs(compressed_set$Outcome_Next_Day_Direction) > 5260500,]
compressed_set$Outcome_Next_Day_Direction <- ifelse(compressed_set$Outcome_Next_Day_Direction > 0, 1, 0)
summary(compressed_set$Outcome_Next_Day_Direction)
dim(compressed_set)
compressed_set_validation$Outcome_Next_Day_Direction <- ifelse(compressed_set_validation$Outcome_Next_Day_Direction > 0, 1, 0)

# create two data sets - won/not won
compressed_set_pos <- dplyr::filter(compressed_set, Outcome_Next_Day_Direction==1) %>% dplyr::select(-Outcome_Next_Day_Direction)
dim(compressed_set_pos)
compressed_set_neg <- dplyr::filter(compressed_set, Outcome_Next_Day_Direction==0) %>% dplyr::select(-Outcome_Next_Day_Direction)
dim(compressed_set_neg)

# build the markov transition grid
build_transition_grid <- function(compressed_grid, unique_patterns) {
     grids <- c()
     for (from_event in unique_patterns) {
          print(from_event)
          
          # how many times 
          for (to_event in unique_patterns) {
               pattern <- paste0(from_event, ',', to_event)
               IDs_matches <- compressed_grid[grep(pattern, compressed_grid$Event_Pattern),]
               if (nrow(IDs_matches) > 0) {
                    Event_Pattern <- paste0(IDs_matches$Event_Pattern, collapse = ',', sep='~~')
                    found <- gregexpr(pattern = pattern, text = Event_Pattern)[[1]]
                    grid <- c(pattern,  length(found))
               } else {
                    grid <- c(pattern,  0)
               }
               grids <- rbind(grids, grid)
          }
     }
     
     # create to/from grid
     grid_Df <- data.frame(pairs=grids[,1], counts=grids[,2])
     grid_Df$x <- sapply(strsplit(as.character(grid_Df$pairs), ","), `[`, 1)
     grid_Df$y <- sapply(strsplit(as.character(grid_Df$pairs), ","), `[`, 2)
     head(grids)
     
     all_events_count <- length(unique_patterns)
     transition_matrix = t(matrix(as.numeric(as.character(grid_Df$counts)), ncol=all_events_count, nrow=all_events_count))
     transition_dataframe <- data.frame(transition_matrix)
     names(transition_dataframe) <- unique_patterns
     row.names(transition_dataframe) <- unique_patterns
     head(transition_dataframe)
     
     # replace all NaN with zeros
     transition_dataframe[is.na(transition_dataframe)] = 0
     # transition_dataframe <- opp_matrix
     transition_dataframe <- transition_dataframe/rowSums(transition_dataframe) 
     return (transition_dataframe)
}
unique_patterns <- unique(strsplit(x = paste0(compressed_set$Event_Pattern, collapse = ','), split = ',')[[1]])

grid_pos <- build_transition_grid(compressed_set_pos, unique_patterns)
grid_neg <- build_transition_grid(compressed_set_neg, unique_patterns)

# predict on out of sample data
actual = c()
predicted = c()
for (event_id in seq(nrow(compressed_set_validation))) {
     patterns <- strsplit(x = paste0(compressed_set_validation$Event_Pattern[event_id], collapse = ','), split = ',')[[1]]
     pos <- c()
     neg <- c()
     log_odds <- c()
     for (id in seq(length(patterns)-1)) {
          
          # logOdds = log(tp(i,j) / tn(i,j)
          log_value <- log(grid_pos[patterns[id],patterns[id+1]] / grid_neg[patterns[id],patterns[id+1]])
          if (is.na(log_value) || (length(log_value)==0) || (is.nan(log(grid_pos[patterns[id],patterns[id+1]] / grid_neg[patterns[id],patterns[id+1]]))==TRUE)) {
               log_value <- 0.0
          } else if (log_value == -Inf) {
               log_value <- log(0.00001 / grid_neg[patterns[id],patterns[id+1]])
          } else if (log_value == Inf) {
               log_value <- log(grid_pos[patterns[id],patterns[id+1]] / 0.00001)
               
          }
          log_odds <- c(log_odds, log_value)
          
          pos <- c(pos, grid_pos[patterns[id],patterns[id+1]])
          neg <- c(neg, grid_neg[patterns[id],patterns[id+1]])
     }
     print(paste('outcome:', compressed_set_validation$Outcome_Next_Day_Direction[event_id]))
     print(sum(pos)/sum(neg))
     print(sum(log_odds))
     
     actual <- c(actual, compressed_set_validation$Outcome_Next_Day_Direction[event_id])
     predicted <- c(predicted, sum(log_odds))
     
}

 
result <- confusionMatrix(ifelse(predicted>0,1,0), actual)

#Printing the results 
result 
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction     0     1
##          0 43477 41492
##          1     0     0
##                                          
##                Accuracy : 0.5117         
##                  95% CI : (0.5083, 0.515)
##     No Information Rate : 0.5117         
##     P-Value [Acc > NIR] : 0.5014         
##                                          
##                   Kappa : 0              
##  Mcnemar's Test P-Value : <2e-16         
##                                          
##             Sensitivity : 1.0000         
##             Specificity : 0.0000         
##          Pos Pred Value : 0.5117         
##          Neg Pred Value :    NaN         
##              Prevalence : 0.5117         
##          Detection Rate : 0.5117         
##    Detection Prevalence : 1.0000         
##       Balanced Accuracy : 0.5000         
##                                          
##        'Positive' Class : 0              

#### ENJOY !!