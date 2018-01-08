library('scales')
library('dplyr') 
library('randomForest') 
library('methods')
library('argparser')
library('caret')
library('Hmisc')
library('e1071')

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

#print(prop.table(table(train$Occupancy)))
summary(train)

correlation_result<-rcorr(as.matrix(train[2:6]))
# correlation_result
# correlation_result$P

#library('corrplot')
#corrplot(correlation_result$r,type="upper", order="hclust", tl.col="black", tl.srt=45)

print('=> Model Constructing ...')

set.seed(1234)
model_all <- train(Occupancy~.,method="rf",data=train)
#plot(model_all)
# model_all$finalModel
# varImp(model_all)
#plot(varImp(model_all,scale=TRUE))

sum(test$Occupancy==predict(model_all,test))/dim(test)[1]*100
set.seed(1234)
confusionMatrix(test$Occupancy,predict(model_all,test))

sum(test$Occupancy==null_list)/dim(test)[1]*100
sum(test$Occupancy==one_list)/dim(test)[1]*100