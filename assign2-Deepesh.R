# Name - Deepesh Kumar Singh 
# CSULB ID - 016149963

#EXERCISE 1 Passing data frame to simple_learner function
simple_learner <- function(data_frame) {
	label <- data_frame[,ncol(data_frame)]
	vec_pos <- (as.numeric(colMeans(subset(data_frame,label==1))))
	vec_neg <- (as.numeric(colMeans(subset(data_frame,label==-1))))
	c_final <- (vec_pos+vec_neg)/2
	c_final <- c_final[1:length(c_final)-1]
	w_final <- (vec_pos - vec_neg)
	w_final <- w_final[1:length(w_final)-1]
	b_final <- sum(w_final*c_final)
	result <- list(w=w_final,b=b_final)
	print(result)
	return(result)
}

#EXERCISE 2 Passing data frame to perceptron_learner function
perceptron_learner <- function(data_frame) {
	label <- data_frame[,ncol(data_frame)]
	w_final <- 0
	b_final <- 0
	e <- 0
	repeat{
		e <- 0
		for(i in 1:nrow(data_frame))
		{
			X <- as.numeric(data_frame[i,1:length(data_frame)-1])
			z <- sign((sum(w_final*X)-b_final))
			z = ifelse(z>0,1,-1)
			if(z!=label[i]){
				w_final <- w_final + (label[i]*X)
				b_final <- b_final - label[i]
			}
			else
			{
				e <- e + 1
			}
		}
		if(e == nrow(data_frame))
		{
			break
		}
	}
	result <- list(w=w_final,b=b_final)
	print(result)
	return(result)
}

#EXERCISE 3 Passing data_frame , w and b to classify function
classify <- function(data_frame, w, b) {
	vec <- c()
	for(i in 1:nrow(data_frame))
	{
		X <- as.numeric(data_frame[i,1:length(data_frame)-1])
		z <- sign(sum(w*X)-b)
		if(z>=0)
		{
			vec <- c(vec,1)
		}
		else
		{
			vec <- c(vec,z)
		}
	}
	return(vec)
}

# Printing percentage of rows correctly classified by algorithm
# 97.4 For Simple Learning
# 100  For Perceptron Learning
classfied_perc <- function(vec_class,data_frame){
		count <- 0
		for(i in 1:nrow(data_frame))
		{
			if(data_frame[i,length(data_frame)] == vec_class[i])
			{
				count <- count+1
			}
		}
		print((count*100)/nrow(data_frame))
}

# Please change the name of csv file according to your usage below . I have taken Exercise-4.csv
main_learner <- function()
{
	data_frame <- read.csv(file="Exercise-4.csv",header=TRUE)
	sim_list <- simple_learner(data_frame) #EXERCISE 4 #w = [11.87250  20.36818 -16.60066  27.87608 -22.29279] # b = -70.39144
	sim_class_vec <- classify(data_frame, sim_list[[1]],sim_list[[2]]) # Passing w and b returned from simple_learner
	classfied_perc(sim_class_vec,data_frame) # Output = 97.4 
	perc_list <- perceptron_learner(data_frame) #Exercise 5 #w = [409.2977  757.0583 -722.5130  908.0482 -929.8089] #b = -278
	perc_class_vec <- classify(data_frame,perc_list[[1]],perc_list[[2]]) # Passing w and b returned from perceptron_learner
	classfied_perc(perc_class_vec,data_frame) # Output = 100
}