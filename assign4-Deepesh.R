# Name - Deepesh Kumar Singh 
# CSULB ID - 016149963

#Exercise 1 : i) 5-fold cross validation, and ii) training accuracy from training over the entire data set
best.svm.cross <- function(data_frame,d,c,n) {
	max <- 0
	max_list <- c()
	best_model <- 0
	accuracy <- c()
	k <- 1
	accuracy1 <- c()
	training_acc <- c()
	error <- c()
	degree <- c()
	cost <- c()
	correct <- c()
	i=1
	j=1
	m=1
	best_model <- 0
	for(i in 1:length(d))
	{
		for(j in 1:length(c))
		{
			data_model <- svm(Class~., data = data_frame, kernel="polynomial", degree = d[i],type = "C-classification",cost=c[j],cross=n)
			degree[k]=d[i]
			cost[k]=c[j]
			accuracy1[k] <- data_model$tot.accuracy
			training_acc[k] <- ((nrow(subset(data_frame, data_frame$Class == fitted(data_model))))*100)/nrow(data_frame)
			if(accuracy1[k] > max)
			{
				max <- accuracy1[k]
				d1 <- d[i]
				c1 <- c[j]
				best_model <- data_model
			}
			k <- k+1
		}
	}
	pred = predict(best_model,data_frame)
	v = c()
	v = c(v,pred)
	vec_diff = c()
	avg_sum = 0
	for(f in 1:nrow(data_frame)){
		vec_diff = c(vec_diff,(data_frame$Class[f]-v[f]))
		avg_sum =  avg_sum + (data_frame$Class[f]-v[f])
	}
	avg_distance <- avg_sum/nrow(data_frame)
#	print(data_model)
#	print(summary(data_model))
	final.df=data.frame(cost,degree,accuracy1,training_acc) #create a dataframe
	colnames(final.df)=c("COST","DEGREE","ACCURACY","TRAINING ACCURACY") #Give the column names
	print(final.df) # print dataframe
	highest_accuracy_degree <- d1 
	highest_cost <- c1
	print(highest_accuracy_degree)
	print(highest_cost)
	print(avg_distance)
	hist(vec_diff,right=FALSE)
}

# FUNCTION USED in Exercise 2  . Used to provide the best svm model to be used in below function 
best.svm.cross2 <- function(data_frame,d,c,n) {
	max <- 0
	max_list <- c()
	best_model <- 0
	accuracy <- c()
	k <- 1
	accuracy1 <- c()
	error <- c()
	degree <- c()
	cost <- c()
	correct <- c()
	i=1
	j=1
	m=1
	
	for(i in 1:length(d))
	{
		for(j in 1:length(c))
		{
			data_model <- svm(Class~., data = data_frame, kernel="polynomial", degree = d[i],type = "C-classification",cost=c[j],cross=n)
			accuracy1 <- data_model$tot.accuracy
			if(accuracy1 > max)
			{
				max <- accuracy1
				d1 <- d[i]
				c1 <- c[j]
			}
		}
	}
	cat("Best DEGREE")
	print(d1)
	cat("BEST COST")
	print(c1)
	data_model_best <- svm(Class~., data = data_frame, kernel="polynomial", degree = d1,type = "C-classification",cost=c1,cross=n)
	return(data_model_best)
}

#Exercise 2 and 3
binary_search <- function(data_frame,b,cost)
{
	cat("9 or 10")
	data_frame2_9or10 <- data_frame[data_frame$Class==9|data_frame$Class==10,]
	print(nrow(data_frame2_9or10))
	data_model_9or10 <- best.svm.cross2(data_frame2_9or10,b,cost,5)
	accuracy1 <- data_model_9or10$tot.accuracy
	print(accuracy1)
	training_acc <- ((nrow(subset(data_frame2_9or10, data_frame2_9or10$Class == fitted(data_model_9or10))))*100)/nrow(data_frame2_9or10)
	print(training_acc)
	
	cat("##############################################################################################")
	cat("7 or 8 or 9")
	data_frame2_789 <- data_frame[data_frame$Class==7|data_frame$Class==8|data_frame$Class==9,]
	print(nrow(data_frame2_789))
	data_model_789 <- best.svm.cross2(data_frame2_789,b,cost,5)
	accuracy1 <- data_model_789$tot.accuracy
	print(accuracy1)
	training_acc <- ((nrow(subset(data_frame2_789, data_frame2_789$Class == fitted(data_model_789))))*100)/nrow(data_frame2_789)
	print(training_acc)

	cat("##############################################################################################")
	cat("5 or 6 or 7")
	
	data_frame2_567 <- data_frame[data_frame$Class==5|data_frame$Class==6|data_frame$Class==6,]
	print(nrow(data_frame2_567))
	data_model_567 <- best.svm.cross2(data_frame2_567,b,cost,5)
	accuracy1 <- data_model_567$tot.accuracy
	print(accuracy1)
	training_acc <- ((nrow(subset(data_frame2_567, data_frame2_567$Class == fitted(data_model_567))))*100)/nrow(data_frame2_567)
	print(training_acc)	
	
	cat("##############################################################################################")
	cat("8 or 9")
	
	data_frame2_89 <- data_frame[data_frame$Class==8|data_frame$Class==9,]
	print(nrow(data_frame2_89))
	data_model_89 <- best.svm.cross2(data_frame2_89,b,cost,5)
	accuracy1 <- data_model_89$tot.accuracy
	print(accuracy1)
	training_acc <- ((nrow(subset(data_frame2_89, data_frame2_89$Class == fitted(data_model_89))))*100)/nrow(data_frame2_89)
	print(training_acc)
	
	cat("##############################################################################################")
	cat("6 or 7")
	
	data_frame2_6or7 <- data_frame[data_frame$Class==6|data_frame$Class==7,]
	print(nrow(data_frame2_6or7))
	data_model_6or7 <- best.svm.cross2(data_frame2_6or7,b,cost,5)
	accuracy1 <- data_model_6or7$tot.accuracy
	print(accuracy1)
	training_acc <- ((nrow(subset(data_frame2_6or7, data_frame2_6or7$Class == fitted(data_model_6or7))))*100)/nrow(data_frame2_6or7)
	print(training_acc)
	
	cat("##############################################################################################")
	cat("10 or 11 or 12")
	
	data_frame2_10or11or12 <- data_frame[data_frame$Class==10|data_frame$Class==11|data_frame$Class==12,]
	print(nrow(data_frame2_10or11or12))
	data_model_10or11or12 <- best.svm.cross2(data_frame2_10or11or12,b,cost,5)
	accuracy1 <- data_model_10or11or12$tot.accuracy
	print(accuracy1)
	training_acc <- ((nrow(subset(data_frame2_10or11or12, data_frame2_10or11or12$Class == fitted(data_model_10or11or12))))*100)/nrow(data_frame2_10or11or12)
	print(training_acc)
	
	cat("##############################################################################################")
	cat("12 or 13 or 14")
	
	data_frame2_12or13or14 <- data_frame[data_frame$Class==12|data_frame$Class==13|data_frame$Class==14,]
	print(nrow(data_frame2_12or13or14))
	data_model_12or13or14 <- best.svm.cross2(data_frame2_12or13or14,b,cost,5)
	accuracy1 <- data_model_12or13or14$tot.accuracy
	print(accuracy1)
	training_acc <- ((nrow(subset(data_frame2_12or13or14, data_frame2_12or13or14$Class == fitted(data_model_12or13or14))))*100)/nrow(data_frame2_12or13or14)
	print(training_acc)
	
	cat("##############################################################################################")
	cat("10 or 11")
	
	data_frame2_10or11 <- data_frame[data_frame$Class==10|data_frame$Class==11,]
	print(nrow(data_frame2_10or11))
	data_model_10or11 <- best.svm.cross2(data_frame2_10or11,b,cost,5)
	accuracy1 <- data_model_10or11$tot.accuracy
	print(accuracy1)
	training_acc <- ((nrow(subset(data_frame2_10or11, data_frame2_10or11$Class == fitted(data_model_10or11))))*100)/nrow(data_frame2_10or11)
	print(training_acc)
	
	cat("##############################################################################################")
	cat("12 or 13")
	
	data_frame2_12or13 <- data_frame[data_frame$Class==12|data_frame$Class==13,]
	print(nrow(data_frame2_12or13))
	data_model_12or13 <- best.svm.cross2(data_frame2_12or13,b,cost,5)
	accuracy1 <- data_model_12or13$tot.accuracy
	print(accuracy1)
	training_acc <- ((nrow(subset(data_frame2_12or13, data_frame2_12or13$Class == fitted(data_model_12or13))))*100)/nrow(data_frame2_12or13)
	print(training_acc)
	
	cat("##############################################################################################")
	
	# Exercise 3 - Decision tree
	df2 <- subset(data_frame,select=-Class)
	predict_first <- c()
	final_list <- c()
	for(i in 1:nrow(data_frame)){
		pred <- predict(data_model_9or10,df2[i,])
		predict_first <- as.numeric(as.character(pred))
		final_value <- 0
		if(predict_first>10|predict_first==10)
		{
			if(as.numeric(as.character(predict(data_model_10or11or12,df2[i,])))>11)
			{
				final_value <- as.numeric(as.character(predict(data_model_12or13or14,df2[i,])))
				if(final_value < 14)
				{
					final_value <- as.numeric(as.character(predict(data_model_12or13,df2[i,])))
					if(final_value < 13)
					{
						final_list <- c(final_list,final_value)
					}
					else
					{
						final_list <- c(final_list,final_value)
					}
				}	
				else
				{
					final_list <- c(final_list,14)
				}
			}
			else
			{
				final_value <- as.numeric(as.character(predict(data_model_10or11,df2[i,])))
				if(final_value < 11)
				{
					final_list <- c(final_list,final_value)
				}
				else
				{
					final_list <- c(final_list,final_value)
				}
			}
		}
		else
		{
			final_value <- as.numeric(as.character(predict(data_model_789,df2[i,])))
			if(final_value > 7)
			{
				final_value <- as.numeric(as.character(predict(data_model_89,df2[i,])))
				if(final_value < 9)
				{
					final_list <- c(final_list,final_value)
				}
				else
				{
					final_list <- c(final_list,final_value)
				}
			}
			else
			{
				final_value <- as.numeric(as.character(predict(data_model_567,df2[i,])))
				if(final_value > 5)
				{
					final_value <- as.numeric(as.character(predict(data_model_6or7,df2[i,])))
					if(final_value < 7)
					{
						final_list <- c(final_list,final_value)
					}
					else
					{
						final_list <- c(final_list,final_value)
					}
				}
				else
				{
					final_list <- c(final_list,5)
				}
			}
		}
	}
	count <- 0
	new_list <- c()
	actual_class <- as.numeric(as.character(data_frame$Class))
	distance <- c()
	sum <- 0
	for(i in 1:nrow(data_frame))
	{
		diff <- actual_class[i]-final_list[i]
		new_list <- c(new_list,diff)
		sum <- sum + diff
		if(diff==0)
		{
			count <- count+1
		}
	}
	cat("Average Distance")
	print((sum)/nrow(data_frame))
	cat("TRAINING ACCURACY")
	print((count)*100/nrow(data_frame))
	hist(new_list,right=FALSE)
# training accuracy and the average distance of the predicted class from the true class
}

# Exercise 4 i) 5-fold cross validation of mean squared error (mse), and ii) mse over the entire data set.
ex4 <- function(data_frame,e,c)
{
	plot(data_frame$X,data_frame$Y)

	k <- 1
	i=1
	j=1
	ep <- c()
	cost2 <- c()
	sum_val = c()
	mean_sum = 0
	minimum_model <- 0
	minimum <- 1000000000000000000
	for(i in 1:length(e))
	{
		mean_sum = 0
		for(j in 1:length(c))
		{
			mean_sum = 0
			data_model <- svm(Y~., data = data_frame, kernel="polynomial", degree = 2,type = "eps-regression",cost=c[j],epsilon=e[i],cross=5)
			pred = predict(data_model,data_frame)
			v = c()
			v = c(v,pred)
			mean_sum = 0
			for(f in 1:nrow(data_frame)){
				mean_sum =  mean_sum + (data_frame$Y[f]-v[f])^2
			}
			temp_sum = mean_sum/nrow(data_frame)
			if(temp_sum < minimum)
			{
				minimum <- temp_sum
				minimum_model <- data_model
			}
			ep[k]=e[i]
			cost2[k]=c[j]
			sum_val[k] = mean_sum/nrow(data_frame)
			k <- k+1
		}
	}
#	print(data_model)
	final.df=data.frame(cost2,ep,sum_val) #create a dataframe
	colnames(final.df)=c("COST","EPSILON","MSE") 
	print(final.df) # print dataframe
	return(minimum_model)
}

#Exercise 5 part 1  Plotted data points against the curve provided by the best svm -
ex5 <- function(data_frame22,min_model)
{
	pl <- predict(min_model,data_frame22)
	points(data_frame22$X,pl,col="Red")
	points(data_frame22$X,pl,col="Red")
	points(data_frame22$X,pl,col="Blue")
}

#Exercise 5 part 2   Plotted the svm model using 1,000 data points equally spaced between 0 to 10.
ex5_2 <- function(data_frame22,min_model)
{
	vec_test <- seq(from=0.01,to=10,by=0.01)
	myData.frame <- as.data.frame(vec_test)
	colnames(myData.frame) <- c("X")
	predY <- predict(min_model,myData.frame)
	points(vec_test,predY,col="Yellow")
}

#Exercise 6   Finding a good support-vector regression machine for the abalone data set
ex6 <- function(data_frame,d,e,c)
{
	k <- 1
	i=1
	j=1
	mean_sum = 0
	minimum <- 1000000000000000000
	minimum_model <- 0
	for(i in 1:length(e))
	{
		for(j in 1:length(c))
		{
			for(k in 1:length(d))
			{
				mean_sum = 0
				data_model <- svm(Class~., data = data_frame, kernel="polynomial", degree = d[k],type = "eps-regression",cost=c[j],epsilon=e[i],cross=5)
				pred = predict(data_model,data_frame)
				v = c()
				v = c(v,pred)
				mean_sum = 0
				for(f in 1:nrow(data_frame)){
					mean_sum =  mean_sum + (data_frame$Class[f]-v[f])^2
				}
				temp_sum = mean_sum/nrow(data_frame)
				if(temp_sum < minimum)
				{
					minimum <- temp_sum
					minimum_model <- data_model
				}
			}
		}
	}
	pred = predict(minimum_model,data_frame)
	v1 = c()
	v1 = c(v1,pred)
	final_diff <- c()
	for(h in 1:nrow(data_frame)){
		mean_sum =  mean_sum + (data_frame$Class[h]-v1[h])
		final_diff <- c(final_diff,(data_frame$Class[h]-v1[h]))
	}
	hist(final_diff,right=FALSE)
	print(mean_sum/nrow(data_frame))
}

#Main Function from which all other functions are getting called 
main_learner <- function()
{
	library(e1071)
	b <- c(3, 2, 1)
	cost <- c(100.0,10.0,1.0,0.1)
	data_frame2 <- read.csv(file="abalone.data")
	names(data_frame2) <- c("X1","X2","X3","X4","X5","X6","X7","X8","Class")  # Class here is rings coloumn
	cross_model_list <- best.svm.cross(data_frame2,b,cost,5)   # Exercise 1

	cross_model_list <- binary_search(data_frame2,b,cost)      # Exercise 2 and 3
	data_frame22 <- read.csv(file="Exercise-4.data")
	names(data_frame22) <- c("X","Y")
	cost2 <- c(100,10.0,1.0,0.1)
	e <- c(1.75,1.5,1.0,0.1)

	min_model <- ex4(data_frame22,e,cost2) # Exercise 4
	ex5(data_frame22,min_model)     	   # Exercise 5 Part 1
	ex5_2(data_frame22,min_model)		   # Exercise 5 Part 2
	ex6(data_frame2,b,e,cost2)			   # Exercise 6
}