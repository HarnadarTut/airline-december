library('ggplot2')


# read in the data set #
data<- read.csv('December.csv')
df<- data.frame(data)

#clean out NA values from arrival delay column (this means a flight did not even take off) #
new_df<- df[!is.na(data$ARR_DELAY),]

# head, structure and summary of data #
head(new_df)
str(new_df)
summary(new_df)

# TAKING A LOOK AT THE DATA #

# taking a look at the number of delays #
new_df$DELAY_STATUS <- ifelse(new_df$ARR_DELAY_NEW > 0, 0, 1)
ggplot(new_df, aes(x = DELAY_STATUS)) + 
  geom_bar(fill= 'red') + 
  labs(title = "Number of Delayed and Not Delayed Flights", x = "Delay Status", y = "Count")

# flight count for each day of the week #
ggplot(data, aes(x = DAY_OF_WEEK)) +
  geom_bar(fill= 'lightgreen') +
  labs(x = "Day of the Week", y = "Distance (miles)") +
  ggtitle("Number of Flights by Day of the Week")


# plotting the average arrival delay for each airline in December 2022 #
delay_summary <- aggregate(new_df$ARR_DELAY_NEW, by=list(AIRLINE=new_df$MKT_UNIQUE_CARRIER), FUN=mean)

ggplot(delay_summary, aes(x=AIRLINE, y=x)) +
  geom_bar(stat="identity", fill="blue") +
  ggtitle("Average Arrival Delay by Airline") + ylab("Average Arrival Delay (minutes)")

# decision tree model that predicts arrival delay based on predictor variables #
library(rpart)
library(rpart.plot)
tree_model <- rpart(ARR_DELAY ~ DEP_DELAY + DISTANCE + MKT_UNIQUE_CARRIER + DAY_OF_WEEK, data = new_df)
rpart.plot(tree_model, extra = 1, under = TRUE)

# example that implements the model above to provide a prediction #
new_data <- data.frame(DEP_DELAY = 20, DISTANCE = 300, MKT_UNIQUE_CARRIER = "WN", DAY_OF_WEEK = 7)
predicted_arr_delay <- predict(tree_model, new_data)
predicted_arr_delay

new_data1 <- data.frame(DEP_DELAY = 100, DISTANCE = 300, MKT_UNIQUE_CARRIER = "WN", DAY_OF_WEEK = 7)
predicted_arr_delay1 <- predict(tree_model, new_data1)
predicted_arr_delay1

# Explanation as to why Decision Tree Regression is not too effective #

# correlation between departure delay and arrival delay #
ggplot(data = new_df, aes(x = DEP_DELAY_NEW, y = ARR_DELAY_NEW, color= MKT_UNIQUE_CARRIER)) +
       geom_point(alpha = 0.5) +
       labs(x = "Departure Delay (minutes)", y = "Arrival Delay (minutes)",
             title = "Departure Delay vs. Arrival Delay")

# correlation between distance and arrival delay #
ggplot(data = new_df, aes(x = DISTANCE, y = ARR_DELAY_NEW, color= MKT_UNIQUE_CARRIER)) +
  geom_point(alpha = 0.5) +
  labs(x = "Distance", y = "Arrival Delay (minutes)",
       title = "Distance vs. Arrival Delay")

# constructing a linear model to see if airline has an effect on the arrival delay #
model <- lm(ARR_DELAY_NEW ~ DAY_OF_WEEK + MKT_UNIQUE_CARRIER, data = new_df)
summary(model)