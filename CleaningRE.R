library(dplyr)
library(tidyverse)
library(zoo)
library(fredr)
library(forcats)
library(glmnet)
library(pROC)
library(ggplot2)
library(Matrix)
library(tidymodels)
library(randomForest)
library(rpart)
library(logistf)
library(ranger)

#Read in Real Estate Sales 
re <- read.csv("Real_Estate_Sales_2001-2023_GL_20251112.csv")

# Convert currency columns to numeric
re$Assessed.Value <- as.numeric(gsub("[$,]", "", re$Assessed.Value))
re$Sale.Amount    <- as.numeric(gsub("[$,]", "", re$Sale.Amount))

# Remove rows where Sale or Assessed Value == 0
re <- re %>% 
  filter(re$Assessed.Value != 0,
         re$Sale.Amount    != 0)

# Adjust assessment value to market value (CT assessment = 70% of market)
re$Market.Value <- re$Assessed.Value / 0.7

# Market Value Ratio to Sale amount
re$mv.ratio <- re$Sale.Amount / re$Market.Value

# Count above 1 to confirm that we have a viable number of above mv.ratio values to work with
sum(re$mv.ratio > 1)

#API key from Fred to call to
fredr_set_key("d18e4d190e7033ec5f7f3ae55e0f0f7c")
#function to grab data from Fred and add it to data set.
pull_and_join_quarterly <- function(series_ids) {
  
  df_final <- NULL
  
  for (id in series_ids) {
    
    # Pull monthly data between 2002 and 2023
    df_m <- fredr(
      series_id = id,
      observation_start = as.Date("2002-01-01"),
      observation_end   = as.Date("2023-01-01")
    )
    
    # NASDAQ has missing values for Januar 2002, so filling the data backwards to adjust
    df_m_filled <- df_m %>%
      arrange(date) %>%
      mutate(value = na.locf(value, fromLast = TRUE, na.rm = FALSE))
    
    # Converting monthly to Quarterly
    df_q <- df_m_filled %>%
      mutate(date_q = as.yearqtr(date)) %>%
      group_by(date_q) %>%
      summarise(!!id := last(value))
    #to allow the dataset loop to begin
    if (is.null(df_final)) {
      df_final <- df_q
    } else {
      df_final <- full_join(df_final, df_q, by = "date_q")
    }
  }
  
  return(df_final)
}

#ariables to pull
variables <- c( "UNRATE", "MVMTD027MNFRBDAL","REITTMA","GDPC1","NASDAQCOM", "NASDAQNQEM","MEDCPIM158SFRBCLE","PCE","GDP"
)

#calling function
df <- pull_and_join_quarterly(variables)

#convert to dates
re$Date.Recorded <- as.Date(re$Date.Recorded, format = "%m/%d/%Y")
#quarter for merge with new data
re$date_q <- as.yearqtr(re$Date.Recorded)
#date of quarter end
re$date_q_end <- as.Date(re$date_q, frac = 1)

#merge
dre <- full_join(re, df, by = "date_q")

#Replacing Assessor Remarks with boolean
dre$Assessor.Remarks <- ifelse(
  trimws(dre$Assessor.Remarks) == "" | is.na(dre$Assessor.Remarks), NA,
  dre$Assessor.Remarks
)
dre$Remarks <- ifelse(is.na(dre$Assessor.Remarks), 0, 1)
#removing assessor remarks, as we do not care about the specific remarks
dre <- dre %>%
  dplyr::select(-c(
    Assessor.Remarks
  ))

#Replacing null values in residential type with UNK(unknown)
dre$Residential.Type <- ifelse(
  trimws(dre$Residential.Type) == "" |
    is.na(dre$Residential.Type),
  NA,
  dre$Residential.Type
)
dre$Residential.Type <- ifelse(is.na(dre$Residential.Type), "UNK", dre$Residential.Type)


#Due to us having complete data only between Q1 2002, and Q4 2022, we need to make sure all data newer is deleted
dre <- dre[ as.numeric(substr(dre$date_q, 1, 4)) < 2023 , ]
#This converted rows outside our desired range to NA, so we deleted them
colSums(is.na(dre))
dre <- dre %>% drop_na()

#Change some columns to factors
dre <- dre %>%
  mutate(
    Property.Type    = as.factor(Property.Type),
    Residential.Type = as.factor(Residential.Type),
    Town             = as.factor(Town)
  )
#allows us to interpret factors compared to Unknown types
dre$Residential.Type <- relevel(dre$Residential.Type, ref = "UNK")


#Reduce Town to the top 10 towns + "Other" because it's also taking up too much RAM
dre$Town <- forcats::fct_lump(dre$Town, n = 10)

# Create prediction variable above_mv
dre <- dre %>%
  mutate(above_mv = ifelse(mv.ratio > 1, 1, 0))
dre$above_mv <- as.factor(dre$above_mv)
summary(dre$Remarks)

#Removes unneccessary columns
dre <- dre %>%
  dplyr::select(-c(
    Address,
    Non.Use.Code,
    OPM.remarks,
    Location,
    Serial.Number,
    Date.Recorded,
    mv.ratio,
    date_q,
    Sale.Amount,
    Sales.Ratio,
    Assessed.Value
  ))

#check amount of factors for each column for later
sapply(dre, function(x) if (is.factor(x)) nlevels(x) else NA)

#computer refuses to run, so I randomly selected 10% of the rows.
drefull <- dre #storing full dataset incase it will be necessary later
dim(drefull)
set.seed(123)
summary(dre$above_mv)
neg <- 367932/1043350 #negative rows/total
pos <- 675418/1043350 #positive rows/total
totalrows <- 100000
nneg <- round(totalrows *neg)
npos <- round(totalrows*pos)
dre <- dre %>%
  group_by(above_mv) %>%
  sample_n(if_else(first(above_mv) == 0, nneg,npos)) %>%
  ungroup()
dim(dre)


# Train-test split (70/30)
set.seed(123)
sample_size <- floor(0.70 * nrow(dre))
train_indices <- sample(seq_len(nrow(dre)), size = sample_size)

train_data <- dre[train_indices, ]
test_data  <- dre[-train_indices, ]
summary(train_data)
dim(train_data)