library('shiny')
library('scales')
library('dplyr') 
library('randomForest') 
library('methods')
library('argparser')
library('caret')
library('Hmisc')
library('e1071')
library('corrplot')

# Define server logic ----
function(input, output) {
  train <- read.csv('./occupancy_data/train.csv', stringsAsFactors = F)
  test1  <- read.csv('./occupancy_data/test1.csv', stringsAsFactors = F)
  test2  <- read.csv('./occupancy_data/test2.csv', stringsAsFactors = F)
  
  full <- bind_rows(train, test1, test2)
  
  print('=> Data Preprocessing ...')
  full$Weekdays <- sapply(full$Date, function(x) if(weekdays(as.Date(strsplit(x, " ")[[1]][1])) == '週六' || weekdays(as.Date(strsplit(x, " ")[[1]][1])) == '週日') 'weekend' else 'weekday')
  full$Time <- sapply(full$Date, function(x) if(strsplit(x, " ")[[1]][2] < '12:00' && strsplit(x, " ")[[1]][2] > '00:00') 'morning' 
                      else if(strsplit(x, " ")[[1]][2] > '18:00') 'evening' else 'afternoon')
  
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
  
  correlation_result<-rcorr(as.matrix(train[2:6]))
  
  print('=> Model Constructing ...')
  
  set.seed(1234)
  model_all <- train(Occupancy~.,method="rf",data=train)
  # model_all
  # model_all$finalModel
  # varImp(model_all)
  
  rf_acc <- sum(test$Occupancy==predict(model_all,test))/dim(test)[1]*100
  set.seed(1234)

  zero_acc <- sum(test$Occupancy==null_list)/dim(test)[1]*100
  one_acc <- sum(test$Occupancy==one_list)/dim(test)[1]*100
  
  m <- c("Random Forest", "null model-0", "null model-1")
  acc <- c(rf_acc, zero_acc, one_acc)
  
  acc_table <- data.frame(Model=m, 
                          Accuracy=acc, 
                          stringsAsFactors = F)
  
  print('Done')
  
  output$train <- renderTable({
    train
  },
  bordered = TRUE,
  width = '6cm',
  align = 'c',
  rownames = TRUE
  )
  
  output$test <- renderTable({
    test
  },
  bordered = TRUE,
  width = '6cm',
  align = 'c',
  rownames = TRUE
  )
  
  output$summary <- renderPrint({
    summary(train)
  })
  
  output$corr <- renderPlot({
    corrplot(correlation_result$r,type="upper", order="hclust", tl.col="black", tl.srt=45)
  })
  
  output$var_imp <- renderPlot({
    plot(varImp(model_all,scale=TRUE))
  })
  
  output$acc <- renderTable({
    acc_table
  },
  bordered = TRUE,
  width = '10cm',
  align = 'c',
  rownames = TRUE
  )
  
  output$conf_matrix <- renderPrint({
    confusionMatrix(test$Occupancy,predict(model_all,test))
  })
}