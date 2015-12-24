#############################################
####### 15.071x - The Analytics Edge. #######  
#############################################

#Develop an analytics model that will help buyers and sellers predict the sales success 
#of a set of eBay listings for Apple iPads from spring 2015.

#AUC achieved : 0.86230 (Top 10%)


#Set working directory and load train and test datasets

        Sys.setlocale("LC_ALL", "C")
        setwd("Your working directory")
        train <- read.csv("ebayiPadTrain.csv",stringsAsFactors = FALSE)
        test <- read.csv("ebayiPadTest.csv",stringsAsFactors = FALSE)

#Checking for missing values (NAs)
        
        library("Amelia")
        missmap(train)
        missmap(test)

#Join both training set and test set in order to simplify data transformation
        
        test$sold <- rep(NA,nrow(test))
        Total <- rbind(train,test)

#Features:
        
        Total$startprice <- Total$startprice
        Total$condition <- factor(Total$condition)
        Total$storage <- factor(Total$storage,ordered = F,levels = c("16","32","64","128","Unknown"))
        Total$productline <- factor(Total$productline)
        Total$biddable <- factor(Total$biddable)
        Total$cellular <- factor(Total$cellular)
        Total$carrier <- factor(Total$carrier)
        Total$sold <- factor(Total$sold)
        Total$color <- factor(Total$color)
        levels(Total$sold) <- c(0,1)
        
#Simple feature extraction:
        
        Total$DescriptionTrue <- ifelse(grepl(" ",Total$description) == TRUE,1,0) #Create new variable: 1 if there is a description,0 if not.
        Total$DescriptionTrue <- factor(Total$DescriptionTrue)
        
#Remove useless labels in Productline:
                
        nums <- which(Total$productline == "iPad 5") #Rename iPad5 as unknown
        for (i in nums) {
                Total$productline[i] <- c("Unknown")
        }
        nums <- which(Total$productline == "iPad mini Retina")#Rename iPad mini Retina as unknown
        for (i in nums) {
                Total$productline[i] <- c("Unknown")
        }

        Total$productline <- factor(Total$productline)

#Bag of words:

        library(tm)
        library(SnowballC)
        CorpusDescription <- Corpus(VectorSource(Total$description))
        CorpusDescription <- tm_map(CorpusDescription, content_transformer(tolower), lazy=TRUE)
        CorpusDescription <- tm_map(CorpusDescription, PlainTextDocument, lazy=TRUE)
        CorpusDescription <- tm_map(CorpusDescription, removePunctuation, lazy=TRUE)
        CorpusDescription <- tm_map(CorpusDescription, removeWords, stopwords("english"), lazy=TRUE)
        CorpusDescription <- tm_map(CorpusDescription, stemDocument, lazy=TRUE)
        dtm <- DocumentTermMatrix(CorpusDescription)
        sparse <- removeSparseTerms(dtm, 0.98)
        
        DescriptionWords <- as.data.frame(as.matrix(sparse))
        DescriptionWords$condition <- NULL #Repeated
        colnames(DescriptionWords) <- make.names(colnames(DescriptionWords))
        Total <- cbind(Total,DescriptionWords)

#Final traininig set/ test set
        
        Train <- head(Total,nrow(train))
        Test <- tail(Total,nrow(test))
        Test$sold <- rep(1,nrow(test))
        Test$sold <- factor(Test$sold)
        levels(Test$sold) <- c(0,1)
        
        Test$description <- NULL #Remove non-significant variables
        Test$cellular <- NULL
        Test$carrier <- NULL
        Test$color <- NULL
        Test$UniqueID <- NULL
        
        Train$description <- NULL
        Train$cellular <- NULL
        Train$carrier <- NULL
        Train$color <- NULL
        Train$UniqueID <- NULL

#A simple logistic regression model for binary classification.To evaluate performance AIC was used.

        model1 <- glm(sold ~ .-back -box -case -condit -cosmet -crack -devic -doe -functional -good -great -item -light 
                      -like -minor -new -pleas -scratch -screen -the -this -use -used -veri -wear -work
                      +biddable:startprice +productline:startprice +DescriptionTrue:startprice + productline:biddable,data = Train,family ="binomial")

        summary(model1) #Check

#AUC
        library(ROCR)
        predictions <- predict(model1,type = "response")
        ROCRpred <- prediction(predictions, Train$sold)
        as.numeric(performance(ROCRpred, "auc")@y.values)

#Submission

        predictions1 <- predict(model1,Test,type ="response")
        MySubmission <- data.frame(UniqueID = test$UniqueID, Probability1 = predictions1)
        write.csv(MySubmission, "FinalSubmission.csv", row.names=FALSE)
