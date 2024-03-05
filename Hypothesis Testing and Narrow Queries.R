source("hypothesis_test.R")
data <- read.csv("marvel_box_office.csv")

#Hypothesis 1 (Strong Alternative Hypothesis)
#Significance Level: 0.05
  #Null Hypothesis: MCU movies owned by 20th Century Fox have the relatively the same amount of success regardless run time.
  #Alternative Hypothesis: MCU movies owned by 20th Century Fox and a run time above 115 minutes tends to perform better than those released on other days of the month
fox <- data[data$Ownership == "20th Century Fox",]
mean_Over <- mean(fox[fox$Run.Time.In.Minutes >= 115,]$Worldwide.Box.Office)
cat("The average worldwide box office for movies released by 20th Century Fox over 115 minutes", mean_Over)
mean_Under <- mean(fox[fox$Run.Time.In.Minutes <= 115,]$Worldwide.Box.Office)
cat("The average worldwide box office for movies released by 20th Century Fox, under 115 minutes", mean_Under)
fox$Long <- ifelse(fox$Run.Time.In.Minutes >= 115, "Yes", "No")
p <-  permutation_test(fox, "Long", "Worldwide.Box.Office", 1000, "No", "Yes")
cat("The p-value of the hypothesis is equal to ", p)
#Hypothesis 2 (Close Call)
#Significance Level: 0.05
  #Null Hypothesis: MCU movies released in May do not differ from movies released throughout the rest of the months in terms of success
  #Alternative Hypothesis: MCU movies released in May differs from movies released throughout the rest of the months in terms of success
data$May_Other <- ifelse(data$Release.Month == "May", "May", "Other")
mean_May <- mean(data[data$May_Other == "May", ]$"Worldwide.Box.Office")
cat("The average worldwide box office for movies released in May is", mean_May)
mean_Other <- mean(data[data$May_Other == "Other",]$"Worldwide.Box.Office")
cat("The average worldwide box office for movies released in months other than May is", mean_Other)
p <- z_test_from_data(data, "May_Other", "Worldwide.Box.Office", "Other", "May")
cat("The p-value of the hypothesis is equal to ", p)

#Hypothesis 3 (Failed To Reject Null Hypothesis)
#Significance Level: 0.05
  #Null Hypothesis: MCU movies in Phase Three and Phase Four did not differ in opening weekend Performance
  #Alternative Hypothesis: MCU movies in Phase Four performed better than Phase 3 in opening weekend performance.
mean_PhaseThree <- mean(data[data$Phase == "Three",]$Opening.Weekend)
cat("The average is ", mean_PhaseThree)
mean_PhaseFour <- mean(data[data$Phase == "Four",]$Opening.Weekend)
cat("The average is ", mean_PhaseFour)
permutation_test(data, "Phase", "Opening.Weekend", 1000, "Four", "Three")

#Narrow Query
  #A MCU movie that is owned by Marvel Studios, is in Phase Three, and is at least 120 minutes long
M <- mean(data[data$Ownership == "Marvel Studios" & data$Phase == "Three" & data$Run.Time.In.Minutes >= 120,]$'Worldwide.Box.Office')
M0 <- mean(data$Worldwide.Box.Office)
cat("Value of M:", M)
cat("Value of M0:", M0)
cat("M / M0:", M/M0)
cat("Satisfies Equation 1 where M > 2 x M0")

