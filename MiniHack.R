training <- read.csv("C:/Users/Mayur/Desktop/train_63qYitG.csv", header=TRUE, na.strings=c("","NA"))
testing <- read.csv("C:/Users/Mayur/Desktop/test_XaoFywY.csv", header=TRUE, na.strings=c("","NA"))

str(training)

set.seed(100)

# Actual train set size 131662 obs of 14 variables
train_size <- 131662

# Combine both datasets

testing$Surge_Pricing_Type <- NA

training_df <- rbind(training, testing)


proper_feature_names <- function(input_table){
  
  #--------------------------------------------
  # This function normalizes the column names.
  # INPUT -- Table with messed up column names.
  # OUTPUT -- Table with proper column names.
  #--------------------------------------------
  
  colnames(input_table) <- tolower(colnames(input_table))
  
  colnames(input_table) <- gsub('([[:punct:]])|\\s+','_',colnames(input_table))
  
  while (any(grepl("__",colnames(input_table),fixed = TRUE)) == TRUE){
    colnames(input_table) <- gsub("__","_",colnames(input_table),fixed = TRUE) 
  }
  
  colnames(input_table) <- gsub("\\*$", "",colnames(input_table))
  
  return(input_table)
}

training <- proper_feature_names(training_df)

table(is.na(training_df$Trip_Distance))
table(is.na(training_df$Type_of_Cab)) # Replace Na values with most used car type
table(is.na(training_df$Customer_Since_Months)) # Replace NA values with mean of months
table(is.na(training_df$Life_Style_Index))     # Replace NA values with mean of index
table(is.na(training_df$Confidence_Life_Style_Index)) #Replace NA values with most repeated index
table(is.na(training_df$Destination_Type))
table(is.na(training_df$Customer_Rating))
table(is.na(training_df$Cancellation_Last_1Month))
table(is.na(training_df$Var1))       # Replace NA values with mean 
table(is.na(training_df$Var2))
table(is.na(training_df$Var3))
table(is.na(training_df$Gender))
table(is.na(training_df$Surge_Pricing_Type))

# Missing Value Treatment

tail(names(sort(table(training_df$Type_of_Cab))), 1)

training_df$Type_of_Cab <- ifelse(is.na(training_df$Type_of_Cab) == TRUE, 'B', training_df$Type_of_Cab)

summary(training_df$Customer_Since_Months)

training_df$Customer_Since_Months[is.na(training_df$Customer_Since_Months)] <- 6

summary(training_df$Life_Style_Index)

training_df$Life_Style_Index[is.na(training_df$Life_Style_Index)] <- 2.802

summary(training_df$Confidence_Life_Style_Index)

training_df$Confidence_Life_Style_Index[is.na(training_df$Confidence_Life_Style_Index)] <- 'B'

summary(training_df$Var1)

training_df$Var1[is.na(training_df$Var1)] <- 64.2

table(is.na(training_df))
table(is.na(testing$Surge_Pricing_Type))

# Create variable types in numeric and factors respectively

summary(training_df)

training_df$Trip_Distance <- as.numeric(training_df$Trip_Distance)
training_df$Type_of_Cab <- as.factor(training_df$Type_of_Cab)
training_df$Customer_Since_Months <- as.numeric(training_df$Customer_Since_Months)
training_df$Life_Style_Index <- as.numeric(training_df$Life_Style_Index)
training_df$Confidence_Life_Style_Index <- as.factor(training_df$Confidence_Life_Style_Index)
training_df$Destination_Type <- as.factor(training_df$Destination_Type)
training_df$Customer_Rating <- as.numeric(training_df$Customer_Rating)
training_df$Cancellation_Last_1Month <- as.numeric(training_df$Cancellation_Last_1Month)
training_df$Var1 <- as.numeric(training_df$Var1)
training_df$Var2 <- as.numeric(training_df$Var2)
training_df$Var3 <- as.numeric(training_df$Var3)
training_df$Gender <- as.factor(training_df$Gender)
training_df$Surge_Pricing_Type <- as.factor(training_df$Surge_Pricing_Type)

# Split data into train ana test

set.seed(123)

test <- training_df[is.na(training_df$Surge_Pricing_Type),]

train <- training_df[1:train_size,]

test$Surge_Pricing_Type<-NULL

# Fit Classifier

library(xgboost)
library(randomForest)

formula <- Surge_Pricing_Type ~ Trip_Distance + Type_of_Cab + Customer_Since_Months + Life_Style_Index + Confidence_Life_Style_Index + Destination_Type + Customer_Rating + Cancellation_Last_1Month + Var3 + Var2 + Var1 + Gender

myfit <- randomForest(formula, train, importance=TRUE, ntree = 50)

# Process test dataset


pred <- predict(myfit, test)

test$Surge_Pricing_Type <- pred

s<-data.frame(Trip_ID = test$Trip_ID, Surge_Pricing_Type = test$Surge_Pricing_Type)

write.csv(s,file="Sample_Submission.csv",row.names=FALSE)
