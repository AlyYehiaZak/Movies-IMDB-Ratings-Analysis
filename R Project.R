library(stringr)
setwd("C:/Users/Aly/OneDrive/Desktop/cv/Statistical analysis R project/data")
tbl_movies<-read.csv("imdb_top_1000_data.csv")  

attributes(tbl_movies)


#remove min from runtime column and make numeric
movie_runtime <- tbl_movies$Runtime
remove_min <- str_remove_all(string = movie_runtime, pattern = "min")
numeric_remove_min <- as.numeric(remove_min)

#make released year numeric
released_year_not <- tbl_movies$Released_Year
released_year_numeric <- as.numeric(released_year_not)

#make IMDB vector
IMDB_vec <- tbl_movies$IMDB_Rating


# Step 2: 

summary(tbl_movies)

IQR(tbl_movies$No_of_Votes)  
IQR(released_year_numeric)
IQR(tbl_movies$IMDB_Rating)



range(tbl_movies$IMDB_Rating)
range(released_year_numeric)    
range(tbl_movies$No_of_Votes)

mean(tbl_movies$IMDB_Rating)
mean(released_year_numeric)  
mean(tbl_movies$No_of_Votes)
mean(numeric_remove_min)


sd(tbl_movies$IMDB_Rating)
sd(released_year_numeric)
sd(tbl_movies$No_of_Votes)
sd(numeric_remove_min)

var(tbl_movies$IMDB_Rating)

plot(tbl_movies$IMDB_Rating)
plot(released_year_numeric)   
plot(tbl_movies$No_of_Votes)

plot(density(tbl_movies$IMDB_Rating))
plot(density(released_year_numeric))  
plot(density(tbl_movies$No_of_Votes))  

boxplot(tbl_movies$IMDB_Rating)
boxplot(released_year_numeric)
boxplot(numeric_remove_min)


lines(plot(density(tbl_movies$IMDB_Rating), lty=2, lwd=2))

hist(tbl_movies$IMDB_Rating)
hist(released_year_numeric)
hist(tbl_movies$No_of_Votes)


median(tbl_movies$IMDB_Rating)
median(released_year_numeric)   




#calculate z-score for IMDB with outlier
IMDB_Z <- ((IMDB_vec-mean(IMDB_vec))/sd(IMDB_vec))
plot(IMDB_Z)
plot(density(IMDB_Z))


# step3: correlation between fields/data


cor(tbl_movies$IMDB_Rating,numeric_remove_min) 
cor(numeric_remove_min,released_year_numeric) 
cor(tbl_movies$IMDB_Rating,released_year_numeric) 



# Step 4: Removing Outliers

#remove released year outlier
new_released_y<-subset(tbl_movies,released_year_numeric> 1928)
boxplot(new_released_y$Released_Year)

#remove IMDB rating outlier
new_released_IMDB<-subset(tbl_movies,tbl_movies$IMDB_Rating<8.7)
boxplot(new_released_IMDB$IMDB_Rating)

#calculate z-score IMDB no outlier
IMDB_Z_no_outlier<-((new_released_IMDB$IMDB_Rating-mean(new_released_IMDB$IMDB_Rating))/sd(new_released_IMDB$IMDB_Rating))
plot(IMDB_Z_no_outlier)
plot(density(IMDB_Z_no_outlier))

#remove Runtime Outlier
new_released_runtime<-subset(tbl_movies,numeric_remove_min<183&numeric_remove_min>68)
   #make runtime outliers numeric
movie_runtime_outlier <- new_released_runtime$Runtime
remove_min_outlier <- str_remove_all(string = movie_runtime_outlier, pattern = "min")
numeric_remove_min_outlier <- as.numeric(remove_min_outlier)
   #end of runtime numeric
boxplot(numeric_remove_min_outlier)


plot(density(log10(tbl_movies$IMDB_Rating)))
plot(density(log10(released_year_numeric)))



# regression--> linear relation

results <- lm(IMDB_Rating ~ numeric_remove_min, tbl_movies) 
summary(results)
plot(results)

results3 <- lm(IMDB_Rating ~ released_year_numeric, tbl_movies) 
summary(results3)
plot(results3)



