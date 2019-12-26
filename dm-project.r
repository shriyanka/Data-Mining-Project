install.packages("timetk")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("broom")
install.packages("caret")
library(timetk)
library(caret)
#library(dplyr)
library(ggplot2)
library(broom)
library(rpart)
theme_set(theme_minimal())

# all the ggplot has been commented as they take so much time for plotting
data = read.csv("C:\\Users\\kajal\\Kajal\\Course Work Sem 1\\Data Mining\\Project\\dataset\\sales_train_agg.csv")

# finding noisy data in the datset
findInvalidData <- function(data) {
  invalidDate = data[is.null(data$date),]
  invalidDateBlockNum = data[is.null(data$date_block_num) | data$date_block_num < 0,]
  invalidShopId = data[is.null(data$shop_id) | data$shop_id < 0 | data$shop_id > 59,]
  invalidCategoryId = data[is.null(data$category_id) | data$category_id < 0 | data$category_id > 83,]
  invalidItemId = data[is.null(data$item_id) | data$item_id < 0 | data$item_id > 22169,]
  invalidItemPrice = data[data$item_price < 0,]
  invalidItemCount = data[data$item_cnt_day < 0,]
  
  invalidData = data.frame()
  if (!is.null(invalidDate))
    if (is.null(invalidData))
      invalidData <- invalidDate
  else
    invalidData <- rbind(invalidData, invalidDate)
  
  if (nrow(invalidDateBlockNum) != 0)
    if (is.null(invalidData))
      invalidData <- invalidDateBlockNum
  else
    invalidData <- rbind(invalidData, invalidDateBlockNum)
  
  if (!is.null(invalidShopId))
    if (is.null(invalidData))
      invalidData <- invalidShopId
  else
    invalidData <- rbind(invalidData, invalidShopId)
  
  if (!is.null(invalidCategoryId))
    if (is.null(invalidData))
      invalidData <- invalidCategoryId
  else
    invalidData <- rbind(invalidData, invalidCategoryId)
  
  if (!is.null(invalidItemId))
    if (is.null(invalidData))
      invalidData <- invalidItemId
  else
    invalidData <- rbind(invalidData, invalidItemId)
  
  if (!is.null(invalidItemPrice))
    if (is.null(invalidData))
      invalidData <- invalidItemPrice
  else
    invalidData <- rbind(invalidItemPrice)
  
  if (!is.null(invalidItemCount))
    if (is.null(invalidData))
      invalidData <- invalidItemCount
  else
    invalidData <- rbind(invalidData, invalidItemCount)
  
  return (invalidData)
}

outlierDetection <- function(dt, var, string) {
  m1 <- mean(var, na.rm = T)
  outlier <- boxplot.stats(var)$out
  outlierMonths <- ifelse(dt$`dataForShop$revenue` %in% outlier, dt$`dataForShop$date_block_num`, NA)
  outlierMonths <- subset(outlierMonths, !is.na(outlierMonths))
  mo <- mean(outlier)
  cat(string, "\n")
  
  cat("Number of outliers identified:", length(outlier), "\n")
  cat("Outlier months:", outlierMonths, "\n")
  cat("Mean of the outliers:", round(mo, 2), "\n")
  cat("Percentage of outliers identified:", 100*length(outlier) / nrow(dt), "\n\n")
}

outlierDetectionForShops <- function(){
  for (i in 0:59){
    shop_id = i
    dataForShop = data[data$shop_id==shop_id, ]
    dataAggregated = aggregate(dataForShop$revenue ~ dataForShop$date_block_num, dataForShop,  FUN = sum)
    outlierDetection(dataAggregated, dataAggregated$`dataForShop$revenue`, cat("Outlier detection for shop ", shop_id))
  }
}

performCrossValidation <- function(trainSubset, testSubset)
{
  train.control <- trainControl(method = "cv", number = 10)
  meanErrorLM = array()
  meanErrorRT = array()
  
  for (i in 0:9)
  {
    rtCV <- train(item_cnt_day ~., data = trainSubset, method = "rpart", trControl = train.control)
    fitCV <- train(item_cnt_day ~., data = trainSubset, method = "lm", trControl = train.control)
    
    rt = predict(rtCV, newdata = testSubset)
    linear = predict(fitCV, newdata = testSubset)
    rmseRt = sqrt(mean((rt - testSubset$item_cnt_day)^2))
    rmseLinear = sqrt(mean((linear - testSubset$item_cnt_day)^2))
    
    meanErrorRT[i] = rmseRT
    meanErrorLM[i] = rmseLinear
  }
  
  # meanErrorRT
  # [1] 2.379837 2.386819 2.434136 2.404185 2.402139 2.441632 2.386631 2.385635 2.424901 2.433267
  # meanErrorLM
  # [1] 2.455840 2.468854 2.458750 2.454349 2.458482 2.457275 2.456542 2.471016 2.465548 2.459961
  
  avgLM = sum(meanErrorLM)/10
  avgRT = sum(meanErrorRT)/10
  var = sum((meanErrorLM - meanErrorRT)^2)/10
  t = (avgRT - avgLM)/var
  cat("t value after calculation is: ",t, "\n")
}

# 1. Data Preparation, Exploratory Analysis, Cleaning Data:

# getting all the noisy data from the dataset
invalidData = findInvalidData(data)

# plotting the missing data of item_cnt_day
histItemCount <- hist(invalidData$item_cnt_day)

# plotting the missing data month wise
missingItemsByMonth <- aggregate(invalidData$item_cnt_day ~ invalidData$date_block_num, invalidData,  FUN = function(x){NROW(x)})
barplot(missingItemsByMonth$`invalidData$item_cnt_day`, names.arg = missingItemsByMonth$`invalidData$date_block_num`, xlab="Month",ylab="Missing Item Sold Count",col="red", main="Missing item Sold count by Month")

# plotting the missing data shop wise
missingItemsByShop <- aggregate(invalidData$item_cnt_day ~ invalidData$shop_id, invalidData,  FUN = function(x){NROW(x)})
barplot(missingItemsByShop$`invalidData$item_cnt_day`, names.arg = missingItemsByShop$`invalidData$shop_id`, xlab="Shop Id",ylab="Missing Item Sold Count",col="red", main="Missing item Sold count by Shop")

# plotting the missing data category wise
missingItemsByCategory <- aggregate(invalidData$item_cnt_day ~ invalidData$category_id, invalidData,  FUN = function(x){NROW(x)})
barplot(missingItemsByCategory$`invalidData$item_cnt_day`, names.arg = missingItemsByCategory$`invalidData$category_id`, xlab="Category Id",ylab="Missing Item Sold Count",col="red", main="Missing item Sold count by Category")

# plotting the missing data item id wise
missingItemsByItem <- aggregate(invalidData$item_cnt_day ~ invalidData$item_id, invalidData,  FUN = function(x){NROW(x)})
barplot(missingItemsByItem$`invalidData$item_cnt_day`, names.arg = missingItemsByItem$`invalidData$item_id`, xlab="Item Id",ylab="Missing Item Sold Count",col="red", main="Missing item Sold count by Items")

# removing the invalid data from the original data
invalidDataRows = type.convert(row.names(invalidData))
data = data[-invalidDataRows,]

# analysing the impact of removing the data from the dataset
monthNoMissingData = aggregate(data$item_cnt_day ~ data$date_block_num, data, FUN = function(x){NROW(x)})
monthNoMissingData = data.frame(monthNoMissingData, missingItemsByMonth$`invalidData$item_cnt_day`)
ratio = (monthNoMissingData$missingItemsByMonth..invalidData.item_cnt_day./monthNoMissingData$data.item_cnt_day)*100
monthNoMissingData = data.frame(monthNoMissingData, ratio)

# analysing the items counts in dataset
itemsShop <- aggregate(data$item_cnt_day ~ data$shop_id, invalidData,  FUN = function(x){NROW(x)})
barplot(itemsShop$`data$item_cnt_day`, names.arg = itemsShop$`data$shop_id`, xlab="Shop Id",ylab="Items Sold Count",col="red", main="Items Sold count by Shop")

# 2. Outlier Detection
revenue = data$item_price*data$item_cnt_day
data = data.frame(data, revenue)
dataAggregated = aggregate(data$revenue ~ data$date_block_num, data,  FUN = sum)
summary(dataAggregated$`data$revenue`)
outlierDetection(dataAggregated, dataAggregated$`data$revenue`, "Outlier detection in all dataset for all months")

# detecting months of sale which is considered as an outlier for all the shops
# Analysing it first for shop_id 56 # for project presentation purpose
dataForShop = data[data$shop_id==56, ]
dataAggregated = aggregate(dataForShop$revenue ~ dataForShop$date_block_num, dataForShop,  FUN = sum)
cat("Month wise sale for shop id 56")
View(dataAggregated)
summary(dataAggregated)
cat("Outliers for shop id 56")
outlier <- boxplot.stats(dataAggregated$`dataForShop$revenue`)$out
outlier
outlierMonths <- ifelse(dataAggregated$`dataForShop$revenue` %in% outlier, dataAggregated$`dataForShop$date_block_num`, NA)
outlierMonths <- subset(outlierMonths, !is.na(outlierMonths))
outlierMonths

# for all shop
outlierDetectionForShops()

# 3. Feature Selection and Engineering
cat("Doing Feature Selection and Engineering. \n")
# since all items where uniquely mapped to category id we can simply
# ignore these from our data
featuredData = data
featuredData = within(featuredData, rm(category_id))

# dropping date_block_num as we already have time information
featuredData = within(featuredData, rm(date_block_num))

# dropping revenue as either price or revenue should be used for training
featuredData = within(featuredData, rm(revenue))

# converting the date in desired format for futher analysis of time series data
dates = featuredData$date
dates = as.character(featuredData$date)
dates = as.Date(as.character(featuredData$date), "%d.%m.%Y")
featuredData = data.frame(featuredData, dates)
featuredData = within(featuredData, rm(date))

# lets plot to see how time series data looks with item_cnt_day
dev.off()
ggplot(data = featuredData, aes(x = dates, y = item_cnt_day))+
  geom_point(color = "#00AFBB", alpha = 0.5)

# plotting except october 2015
ss = featuredData[featuredData$dates < as.Date("2015-10-1"),]
ggplot(data = ss, aes(x = dates, y = item_cnt_day))+
  geom_point(color = "#00AFBB", alpha = 0.5)
ggplot(data = ss, aes(x = dates, y = revenue))+
  geom_point(color = "#00AFBB", alpha = 0.5)

# plotting for october 2015
ss = featuredData[featuredData$dates >= as.Date("2015-10-1"),]
ggplot(data = ss, aes(x = dates, y = item_cnt_day))+
  geom_point(color = "#00AFBB", alpha = 0.5)
ggplot(data = ss, aes(x = dates, y = revenue))+
  geom_point(color = "#00AFBB", alpha = 0.5)

# we add additional information known as augmenting the data such as quater, month, week etc
x <- tk_augment_timeseries_signature(featuredData)
featuredData = data.frame(featuredData, x$half)
featuredData = data.frame(featuredData, x$quarter)
featuredData = data.frame(featuredData, x$month)
featuredData = data.frame(featuredData, x$half)

# ignoring first half year of 2013 as it shows no major trend
featuredData = featuredData[featuredData$dates > as.Date("2013-5-31"),]

# creating test and train data from above selected features
trainSubset = featuredData[featuredData$dates < as.Date("2015-10-1"),]
testSubset = featuredData[featuredData$dates >= as.Date("2015-10-1"),]
shop_id = testSubset$shop_id # taking it out as we need to predict for
item_id = testSubset$item_id # each shop and item id
trainSubset = within(trainSubset, rm(shop_id))
trainSubset = within(trainSubset, rm(item_id))
testSubset = within(testSubset, rm(shop_id))
testSubset = within(testSubset, rm(item_id))

# normalizing prices since it ranges varies
trainSubset$item_price = scale(trainSubset$item_price)
testSubset$item_price = scale(testSubset$item_price)

# 4. Modelling
# training first model: regression trees
cat("Training the models now. \n")
rt <- rpart(formula = item_cnt_day ~ .,data    = trainSubset, method  = "anova")
p1 = predict(rt, newdata = testSubset)

# training second model: linear regression
fit <- lm(item_cnt_day ~ ., data = trainSubset)
p2 = predict(fit, newdata = testSubset)

#5. Validation
# visualize both predictions
x = data.frame(shop_id)
x = data.frame(x, item_id)
x = data.frame(x, testSubset$item_cnt_day)
x = data.frame(x, p1)
x = data.frame(x, p2)

# viewing the predictions for each shop id and item id for both models
View(x)
m1 = mean((p1 - testSubset$item_cnt_day)^2)
m2 = mean((p2 - testSubset$item_cnt_day)^2)
sq1 = sqrt(m1)
sq2 = sqrt(m2)

cat("MSE first model: ", m1, "\n")
cat("MSE second model: ", m2, "\n")

cat("RMSE first model: ", sq1, "\n")
cat("RMSE second model: ", sq2, "\n")

# visualizing data for linear regression model
trainSubsetViz = trainSubset
trainSubsetViz$pred = predict(fit)
ggplot(trainSubsetViz, aes(x = dates, y = pred))+geom_point(color="red")
ggplot(trainSubsetViz, aes(x = dates, y = item_cnt_day))+geom_point(color="blue")+geom_point(aes(y = predicted), shape = 1, color="red", alpha=0.5)

# 6. calculating confidence values
# performing 10 fold cross validation for both models to get 10 error rates
cat("Computing t-statistics for model comparison. \nThis code piece takes more than 3-4 hrs for computation as to get 10 error rates values we are doing kfold 10 times")
performCrossValidation(trainSubset, testSubset)


