#Homework 1: Anil Brahmajosyula
#Analyzing patterns in a data set that contains all the movies within the Marvel Cinematic Universe

marvel_movies <- read_csv("marvel_box_office.csv")
head(marvel_movies)

#Plot 1: Shows the increase in Revenue for Marvel Movies over the years
plot(marvel_data$`Release Year`, marvel_data$`Worldwide Box Office`, main = "Relationship between Year Created and Amount Grossed Worldwide",
      xlab = "Release Year", ylab = "Amount Grossed (Millions)",)

#Plot 2: Anthony and Joe Russo is the most successful MCU director in terms of Box Office
director_gross <- tapply(marvel_data$`Worldwide Box Office`, marvel_data$Director, sum)
barplot(director_gross, main = "Total Grossing Amount by Director", xlab = "Director", ylab = "Total Amount Grossed Worldwide (Millions)",cex.names = 0.7) 

#Plot 3: Shows the average IMDB score by the Phases of marvel
avg_imdb_by_Phase <- tapply(marvel_movies$`IMDb Score`, marvel_movies$Phase, mean)
barplot(avg_imdb_by_Phase, main = "Average IMDb Score by Phase",xlab = "Phase", ylab = "Average IMDb Score",col="green")

#Plot 4: Shows the relationship between Box Office and Budget 
plot(marvel_movies$Budget, marvel_movies$`Worldwide Box Office`, main="Relationship between Budget and Box Office", xlab = "Budget", ylab="Worldwide Box Office",)

#Plot 5: Shows the relationship between the success of 20th Century Fox MCU movies vs. other Production Companies
boxplot(subset_Ownership$`Worldwide Box Office`, subset_Ownership2$`Worldwide Box Office`, names = c("20th Century Fox", "Other Productions"), main="WorldWide Box Office (20th Century Fox vs. Others)", xlab = "20th Century Fox vs. Other Production Companies", ylab="World Wide Box Office", col="red")

#Plot 6: Shows which Production company amassed the most amount of money
total <- tapply(marvel_movies$`Worldwide Box Office`, marvel_movies$Ownership, sum)
barplot(total, main="Total Amount of Money Amassed by each Production Company",xlab="Production Companies", ylab="Worldwide Box Office Totals", cex.names=0.7, col="red")
#After categorizing the budget in Low and High, compare it to the worldwide box office to get an easier look
mean_budget <- mean(marvel_movies$Budget)
marvel_movies$Budget_Level <- ifelse(marvel_movies$Budget > mean_budget, "High Budget", "Low Budget")
table(marvel_movies$Budget_Level, marvel_movies$`Successful`)
#Shows that usually movies with higher budgets, tend to perform better in the worldwide box office

#Query 1: Compare the Phase of the Movie to the performance of the movie(Worldwide or National)
table(marvel_movies$Phase, marvel_movies$Successful)
#Compare the opening weekend to the worldwide box office

#Query 2: Box Office numbers of 20th century fox movies
subset_Ownership <- subset(marvel_movies, marvel_movies$Ownership=="20th Century Fox")
subset_Ownership2 <- subset(marvel_movies, marvel_movies$Ownership!="20th Century Fox")

#Query 3: Compare runtime to worldwide box office to see if audience engagement is sustained for longer movies (Movies appear to do better when they have the longer run time)
plot(marvel_movies$`Run Time In Minutes`, marvel_movies$`Worldwide Box Office`, main="Run Time vs Success", xlab="Run Time", ylab="Worldwide Box Office",col="blue") 

#Query 4: Compare the mean of the worldwide box office movies and derive a new row that has successful or not
mean_BoxOffice <- mean(marvel_movies$`Worldwide Box Office`)
marvel_movies$Successful <- ifelse(marvel_movies$`Worldwide Box Office`>mean_BoxOffice, "Successful", "Unsuccessful")
table(marvel_movies$Budget_Level, marvel_movies$Successful)

#Query 5: Comparing IMDB and Rotten Tomatoes score to Budget of Film
mean_IMDB <- mean(marvel_movies$`IMDb Score`)
mean_Tomato <- mean(marvel_movies$`Rotten Tomato Audience Score`)
marvel_movies$Critics <- ifelse(marvel_movies$`IMDb Score`>mean_IMDB & marvel_movies$`Rotten Tomato Audience Score`>mean_Tomato, "Critics & Audience Liked", "Critics & Audience Disliked")
table(marvel_movies$Budget_Level, marvel_movies$Critics)

#Query 6: Does the month of the release of the film have any relation to its success? (Marvel prefers releasing movies in May)
table(marvel_movies$`Release Month`, marvel_movies$Successful)

#Query 7: Which year of the MCU was the most 'successful (Not enough differences to say for sure)
table(marvel_movies$`Release Year`, marvel_movies$Successful)

#Query 8: Based on the last query, if we were to make another column that has pre-2010 and post-2010 we can see a more concise relationship
#Shows that before 2010, MCU movies were relatively unsuccessful
marvel_movies$Decade <- ifelse(marvel_movies$`Release Year`<2010, "1998-2009", "2010-Present")
table(marvel_movies$Decade, marvel_movies$Successful)

#Query 9: Maybe if we were to account for inflation in the determination of a successful film, the 1998-2009 time period could have conceived more successful movies than percieved
marvel_movies$Successful_Inflation <- ifelse(marvel_movies$`Inflation Adjusted Worldwide` > mean(marvel_movies$`Inflation Adjusted Worldwide`), "Successful", "Unsuccessful")
table(marvel_movies$Decade, marvel_movies$Successful_Inflation)
#Even with inflation factored in, the pre-2010s time period was an unsuccessful point in the MCU

#Query 10: Does a good opening weekend convey that the film was successful overall?
marvel_movies$Opening <- ifelse(marvel_movies$`Opening Weekend`>mean(marvel_movies$`Opening Weekend`), "Good Opening Weekend", "Bad Opening Weekend")
table(marvel_movies$`Opening`, marvel_movies$Successful)

#Query 11:
#Compare the owner of the movie to the performance of the movie
#20th Century Fox does not appear to make good marvel movies
table(marvel_movies$Ownership, marvel_movies$Successful)

#Prediction Vector for marvel movies' success
decision <- rep('P', nrow(marvel_movies))
decision[marvel_movies$Budget< mean(marvel_movies$Budget)] <- 'Unsuccessful'
decision[marvel_movies$Budget_Level == "Low Budget" & (marvel_movies$Successful == "Unsuccessful" | 
marvel_movies$Critics == "Critics & Audience Disliked" | marvel_movies$Phase != "Three") &
marvel_movies$Ownership=="20th Century Fox"] <- 'Unsuccessful'
mean(decision == marvel_movies$Successful)
