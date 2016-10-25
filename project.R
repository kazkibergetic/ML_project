prediction <- function()
{
        training <-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", na.strings=c("NA","#DIV/0!", ""))
        test <-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", na.strings=c("NA","#DIV/0!", ""))
        
        training<-training[,colSums(is.na(training)) == 0]
        test <-test[,colSums(is.na(test)) == 0]
        
        training   <-training[,-c(1:7)]
        test <-test[,-c(1:7)]
        
        
        head(training)
        head(test)
        
        subsamples <- createDataPartition(y=training$classe, p=0.75, list=FALSE)
        subTraining <- training[subsamples, ] 
        subTesting <- training[-subsamples, ]
        
        print("Decision Tree Model")
        model1 <- rpart(classe ~ ., data=subTraining, method="class")
        
        prediction1 <- predict(model1, subTesting, type = "class")
        table(prediction1, subTesting$classe)
        print(confusionMatrix(prediction1, subTesting$classe))
        
        print("Random Forest Model")
        model2 <- randomForest(classe ~. , data=subTraining, method="class")
        prediction2 <- predict(model2, subTesting, type = "class")
        table(prediction2, subTesting$classe)
        print(confusionMatrix(prediction2, subTesting$classe))
}