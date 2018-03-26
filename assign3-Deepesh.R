# Name - Deepesh Kumar Singh 
# CSULB ID - 016149963

# Exercise 1 partition
partition <- function(data_frame,alpha) {
	s_size <- round(alpha * nrow(data_frame))
	train_ind <- sample(seq_len(nrow(data_frame)), size = s_size)
	train <- data_frame[train_ind, ]
	test <- data_frame[-train_ind, ]
	result <- list(t1=train,t2=test)
	return(result)
}

# Exercise 2 
#Exercise 3 best_svm below is the observation 
#1st iteration degree 3 , cost 1000 , accuracy 100
#2nd iteration degree 3 , cost 1000 , accuracy 99.71014
#3rd iteration degree 2 , cost 10000 , accuracy 100
#4th iteration degree 3 , cost 1000 , accuracy 100
#5th iteration degree 3 , cost 1000 , accuracy 100
#6th iteration degree 2 , cost 1000 , accuracy 100
#7th iteration degree 3 , cost 1000 , accuracy 99.42029
#8th iteration degree 3 , cost 1000 , accuracy 99.13043
#9th iteration degree 3 , cost 1000 , accuracy 100
#10th iteration degree 2, cost 10000 , accuracy 99.71014

best_svm <- function(data_frame,alpha,d,c) {
	result <- partition(data_frame,alpha)
	df_train <- result[[1]]
	df_test <- result[[2]]
	max <- 0
	for(i in 1:length(d))
	{
		for(j in 1:length(c))
		{
			data_model <- svm(Class~., data = df_train, kernel="polynomial", degree = d[i],type = "C-classification",cost=c[j])
			second <- nrow(subset(df_test,predict(data_model,df_test)==df_test$Class))
			if(second > max)
			{
				max <- second
				deg <- d[i]
				cost <- c[j]
			}
		}
	}
	acc = (max/nrow(df_test))*100
	result2 <- list(degree=deg,cost=cost,accuracy=acc)
	print(result2)
	return(result2)
}

#Exercise 4. 
# 2 is the least degree for which 100 percent accuracy is observed.
exercise4 <- function(data_frame,d,c){
	max <- 0
	deg <- -1
	for(i in 1:length(d))
	{
		data_model <- svm(Class~., data = data_frame, kernel="polynomial", degree = d[i],type = "C-classification",cost=c)	
		second <- nrow(subset(data_frame,predict(data_model,data_frame)==data_frame$Class))
		accuracy = second/nrow(data_frame)
		if(accuracy==1)
		{
			deg <- d[i]
			break
		}
	}
	print(deg)
}

#Exercise 5
#Exercise 6. Max Accuracy = 93.21, For Degree = 1 and cost = 10
best.svm.cross <- function(data_frame,d,c,n) {
	max <- 0
	max_list <- c()
	best_model <- 0
	for(i in 1:length(d))
	{
		for(j in 1:length(c))
		{
			data_model <- svm(Class~., data = data_frame, kernel="polynomial", degree = d[i],type = "C-classification",cost=c[j],cross=n)
			accuracy <- data_model$tot.accuracy
			if(accuracy > max)
			{
				best_model <- data_model
				max <- accuracy
				deg <- d[i]
				cost <- c[j]
			}
		}
	}
	result2 <- list(l1=deg,l2=cost,l3=accuracy)
	print(result2)
	return(result2)
}

#Exercise 7. Return lower bound and uppper bound
#Exercise 8. "Error Lower bound" = 1.289398 , "Error Upper bound" = 3.581662, "Accuracy Lower bound" = 96.41834 , "Accuracy Upper bound" = 98.7106
bootstrap <- function(dataset,model,p,n)
{
	result_list <- c()
	accuracy_list <- c()
	for(i in 1:n)
	{
		sampledata <- dataset[sample(nrow(dataset),replace=TRUE),]
		confusion_matrix <- table(predict(model,subset(sampledata,select=-Class)),sampledata$Class)
		error <- (confusion_matrix[1,2]+confusion_matrix[2,1])/length(dataset$Class)*100
		accuracy <- (confusion_matrix[1,1]+confusion_matrix[2,2])/length(dataset$Class)*100
		result_list <- c(result_list,error)
		accuracy_list <- c(accuracy_list,accuracy)
	}
	result_list <- sort(result_list)
	accuracy_list <- sort(accuracy_list)
	n <- (100-p)/2
	error_lower <- result_list[n]                             # 5th location in list
	error_upper <- result_list[100-n]						 # 95th location in list
	accuracy_lower <- accuracy_list[n]
	accuracy_upper <- accuracy_list[100-n]
	error_result <- list(error_lower_bound=error_lower,error_uppper_bound=error_upper)
	accuracy_result <- list(accuracy_lower_bound=accuracy_lower,accuracy_uppper_bound=accuracy_upper)
	print(error_result)
	print(accuracy_result)
	return(error_result)
}

#Main Function from which all other functions are getting called 
main_learner <- function()
{
	library(e1071)
	data_frame <- read.csv(file="car.data",header=TRUE)
	names(data_frame) <- c("buying","maint","doors","persons","lug_boot","safety","Class")
	alpha <- 0.8
	sim_list <- partition(data_frame,alpha)                     #Exercise 1 . This will partition the data frame.
	b <- c(1, 2, 3, 4)
	cost <- c(0.1, 1.0, 10.0, 100.0, 1000.0, 10000.0)
	alpha = 0.8
	for(i in 1:10)
	{
			best_svm(data_frame,alpha,b,cost)                   # Exercise 3 best_svm called 10 different times . Shown the observation above in function .
	}
	exercise4(data_frame,b,100000)                              # Exercise 4. 2 is the least degree for which 100 percent accuracy is seen.
	data_frame2 <- read.csv(file="breast-cancer-wisconsin.data",header=TRUE)
	names(data_frame2) <- c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","Class")
	cross_model_list <- best.svm.cross(data_frame2,b,cost,10)   #Exercise 6. Degree 1 , Cost = 10 , Accuracy = 92.12 . 
	cross_data_model <- svm(Class~., data = data_frame2, kernel="polynomial", degree = cross_model_list[[1]],type = "C-classification",cost=cross_model_list[[2]],cross=10)
	bootstrap(data_frame2,cross_data_model,90,100)              #Exercise 8. "Lower bound" = 1.432665 , "Upper bound" = 3.438395
}