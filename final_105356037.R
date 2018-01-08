library('scales')
library('dplyr') 
library('randomForest') 
library('methods')
library('argparser')
library('caret')
library('Hmisc')
library('e1071')

model_all <- function(){
  set.seed(1234)
  model_all <- train(Occupancy~.,method="rf",data=train)
  model_all
  plot(model_all)
  # model_all$finalModel
  # varImp(model_all)
  plot(varImp(model_all,scale=TRUE))
  
  print(sum(test$Occupancy==predict(model_all,test))/dim(test)[1]*100)
  set.seed(1234)
  print(confusionMatrix(test$Occupancy,predict(model_all,test)))
  
  print(sum(test$Occupancy==null_list)/dim(test)[1]*100)
  print(sum(test$Occupancy==one_list)/dim(test)[1]*100)
  # set.seed(1234)
  # confusionMatrix(test$Occupancy,null_list)
}

model_without_light <- function(){
  #without Light
  set.seed(1234)
  model_no_light <- train(Occupancy~.-Light,method="rf",data=train)
  model_no_light
  plot(modelL_no_light)
  #model_no_light$finalModel
  #varImp(model_no_light)
  plot(varImp(model_no_light,scale=TRUE))
  
  sum(test$Occupancy==predict(model_no_light,test))/dim(test)[1]*100
  set.seed(1234)
  confusionMatrix(test$Occupancy,predict(model_no_light,test))
}

model_without_CO2 <- function(){
  #without CO2
  set.seed(1234)
  model_no_CO2 <- train(Occupancy~.-CO2,method="rf",data=train)
  model_no_CO2
  plot(modelL_no_CO2)
  #model_no_CO2$finalModel
  #varImp(model_no_CO2)
  plot(varImp(model_no_CO2,scale=TRUE))
  
  sum(test$Occupancy==predict(model_no_CO2,test))/dim(test)[1]*100
  set.seed(1234)
  confusionMatrix(test$Occupancy,predict(model_no_CO2,test))
}

p <- arg_parser("")
p <- add_argument(p, "-nco2", help="model without CO2", flag = TRUE)
p <- add_argument(p, "-nlight", help="modle without Light", flag = TRUE)
#p <- add_argument(p, "-out", help="output file name", default = NULL)
argv <- parse_args(p)
nco2 <- argv$nco2
nlight <- argv$nlight
#out_f <- argv$out

#if (argv$fold == 'FALSE' || argv$out == 'FALSE'){
#  print(p)
#  stop("Please supplied fold and output file.", call.=FALSE)
#}

print('=> Data Loading ...')
train <- read.csv('./occupancy_data/train.csv', stringsAsFactors = F)
test1  <- read.csv('./occupancy_data/test1.csv', stringsAsFactors = F)
test2  <- read.csv('./occupancy_data/test2.csv', stringsAsFactors = F)

full <- bind_rows(train, test1, test2)

print('=> Data Preprocessing ...')
full$Weekdays <- sapply(full$Date, function(x) if(weekdays(as.Date(strsplit(x, " ")[[1]][1])) == '週六' || weekdays(as.Date(strsplit(x, " ")[[1]][1])) == '週日') 'weekend' else 'weekday')
full$Time <- sapply(full$Date, function(x) if(strsplit(x, " ")[[1]][2] < '12:00' && strsplit(x, " ")[[1]][2] > '00:00') 'morning' 
                    else if(strsplit(x, " ")[[1]][2] > '18:00') 'evening' else 'afternoon')

print(head(full))
train <- full[1:8144, 2:9]
test <- full[8145:20560, 2:9]

null_list <- c()
one_list <- c()
for(i in c(1:12416)){
  null_list <- c(null_list, 0)
  one_list <- c(one_list, 1)
}

train$Occupancy <- as.factor(train$Occupancy)
test$Occupancy  <- as.factor(test$Occupancy)

print(prop.table(table(train$Occupancy)))
summary(train)

correlation_result<-rcorr(as.matrix(train[2:6]))
# correlation_result
# correlation_result$P

#library('corrplot')
#corrplot(correlation_result$r,type="upper", order="hclust", tl.col="black", tl.srt=45)

print('=> Model Constructing ...')

if(nlight){
  model <- model_without_CO2()
}else if(nco2){
  model <- model_without_CO2()
}else{
  model <- model_all()
}