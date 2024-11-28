library(readr)
library(ggplot2)
user_data <- OA_11_6_yelp_academic_dataset_user_json # Saves dataframe as user_data variable
business_data <- OA_11_7_yelp_academic_dataset_business_json # Saves Dataframe as business_data variable

head(business_data,n=5) #prints out the first five rows of the business data frame

ggplot(business_data)+geom_bar(aes(x=state)) # Presents the number of businesses in each state in the Dataframe

contingency_table<-table(business_data$stars) # Creates a contingency table to be used in the pie graph.
pie(contingency_table,main="Business Stars")  # Creates a pie graph to see the spread of business stars.

# Creates a Box plot to compare the number of Stars vs. the number of reviews.
# ChatGPT was used in helping determining how to eliminate outliers.
stars <- factor(business_data$stars)
ggplot(business_data, aes(x = factor(stars), y = review_count, fill = factor(stars))) +
  geom_boxplot(show.legend = FALSE,outlier.shape = NA)+scale_y_continuous(limits = c(0,100))+
  labs(title = "Review Count vs. Stars", x="Stars",y="Number of Reviews")


# Performing a Chi-squared test to compare stars=1 and stars=5
one_star_data <- business_data[business_data$stars==1,] # creates a sub-data frame for 1 star reviews
five_star_data<- business_data[business_data$stars==5,] # creates a sub-data frame for 5 star reviews
business_data_cont_table = table(one_star_data$categories,five_star_data$categories) # Creates a contingency table of the to sub-data frames
print(business_data_cont_table)
chi2_test<-chisq.test(business_data_cont_table) # performs the chi-squared test
print(chi2_test)
       
# Printing column names of the user data frame
colnames(user_data)

library(corrplot)
# Use Pearson r correlation to compare cool_votes,funny_votes,and useful_votes
vote_data<-user_data[,c("cool_votes","funny_votes","useful_votes")] # makes a sub-data frame of the columns that are being compared.
correlation_matrix<-cor(vote_data) # Creates a correlation matrix corresponding to the sub-data frame.
print(correlation_matrix)


# Creating a linear regression between cool_votes and useful_votes
linear_model <- lm(user_data$cool_votes~user_data$useful_votes)
coefs <- coef(linear_model)
print(coefs)
lm_slope <- coefs[2]
lm_intercept <- coefs[1]
cat("Slope:", lm_slope, "Intercept:", lm_intercept)
ggplot(user_data) + geom_point(aes(cool_votes,useful_votes)) + 
  geom_smooth(aes(cool_votes,useful_votes), method="lm", se=F) + 
  labs(x="Cool Votes", y="Useful Votes")

# Determining if writing more reviews results in more fans or not
linear_model <- lm(user_data$fans~user_data$review_count)
coefs <- coef(linear_model)
print(coefs)
lm_slope <- coefs[2]
lm_intercept <- coefs[1]
cat("Slope:", lm_slope, "Intercept:", lm_intercept)
ggplot(user_data) + geom_point(aes(fans,review_count)) + 
  geom_smooth(aes(fans,review_count), method="lm", se=F) + 
  labs(x="Fans", y="Review Count")
# From the linear regression there is a slight indication that an increase in reviews also results in more fans.







