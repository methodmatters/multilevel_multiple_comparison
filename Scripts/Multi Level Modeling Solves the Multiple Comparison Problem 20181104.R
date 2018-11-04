
# load the packages we'll need
library(plyr); library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(reshape2)

# identify the directory where the data file is stored
in_dir <- 'C:\\Folder\\'
# load the data 
aggregate_day_hour <- read_csv(paste0(in_dir,'aggregate_day_hour.csv'))



# Step 1: Calculate the Model
# Step 1: Calculate the Model
# Step 1: Calculate the Model

# load the lme4 package
library(lme4)
# load the lmerTest package
# (provides p-values for multi-level models
# run with lme4)
library(lmerTest)
# load the arm package
# to extract standard errors for coefficients
# and estimates of the standard deviation
# of our predictions (sigma hat)
library(arm)

# center the hour variable for 6 pm
aggregate_day_hour$hour_centered <- aggregate_day_hour$hour -18 

# compute the model 
lme_model <- lmer(steps ~ 1 + hour_centered + week_weekend  + (1 | oneday),
                  data=aggregate_day_hour)

# view the model results
summary(lme_model)


# Step 2: Extract Model Output 
# Step 2: Extract Model Output 
# Step 2: Extract Model Output 

# examine the coefficients for each day
coef(lme_model)
round(summary(lme_model)[10]$coefficients,2)

# extract coefficients from the lme_model
coefficients_lme <- as.data.frame(coef(lme_model)[1])
names(coefficients_lme) <- c('intercept', "hour_centered", "week_weekendWeekend")
coefficients_lme$date <- row.names(coefficients_lme)
# make the date column a date object in R
coefficients_lme$date <- ymd(coefficients_lme$date)


# create a day-level dataset with the indicator
# of day (week vs. weekend)
week_weekend_df <- unique( aggregate_day_hour[ , c('oneday', 'week_weekend') ] )
# join the week/weekend dataframe to the coefficients dataframe
coefficients_lme <- left_join(coefficients_lme, 
                                  week_weekend_df[,c('oneday', 'week_weekend')], 
                                by = c("date" = 'oneday'))
# make a dummy variable for weekend which
# takes on the value of 0 for weekday
# and 1 for the weekend
coefficients_lme$weekend <- ifelse(coefficients_lme$week_weekend=='Weekend',1,0)

# extract the standard deviation of the predicted step counts
# store in a variable called "sigma_y_hat"
sigma_y_hat <- sigma.hat(lme_model)$sigma$data
# 2913.093

sigma_y_hat


# Step 3: Sample 50 days and Plot
# Step 3: Sample 50 days and Plot
# Step 3: Sample 50 days and Plot

# sample 50 random days
set.seed(1)
sampled_dates <- coefficients_lme[sample(nrow(coefficients_lme), 50), ]

# visualize estimated step count
# for each date
ggplot(sampled_dates, aes(x = as.factor(date), y = intercept + (week_weekendWeekend*weekend), 
                          color = week_weekend)) +
  # point plot
  geom_point() + 
  # set the title
  labs(title = "Model Estimated Step Count at 6 PM") +
  # set the y axis title
  ylab('Estimated Number of Steps at 6 PM') +
  # turn off x axis title, make dates on x axis
  # horizontal
  theme(axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = .5)) +
  # set the legend title
  scale_colour_discrete(name  ="Week/Weekend") 



# Step 4: Simulate Posterior Distributions of Step Counts Per Day
# Step 4: Simulate Posterior Distributions of Step Counts Per Day
# Step 4: Simulate Posterior Distributions of Step Counts Per Day

# this function calculates the estimated step count
# and then draws 1000 samples from the model-implied
# posterior distribution
lme_create_samples <- function(intercept, w_wk_coef, w_wk_dum){
  intercept_f <- intercept + (w_wk_coef * w_wk_dum)
  y_tilde <- rnorm(1000, intercept_f, sigma_y_hat)
}

# here we apply the function to our sampled dates
# and store the results in a matrix called
# "posterior_samples"
posterior_samples <- mapply(lme_create_samples, 
                            sampled_dates$intercept, 
                            sampled_dates$week_weekendWeekend, 
                            sampled_dates$weekend)
dim(posterior_samples)
# [1] 1000   50

# what does it look like?
round(posterior_samples[1:10,1:5],2)


# Step 5: Compare the Simulations For Each Day 
# Step 5: Compare the Simulations For Each Day 
# Step 5: Compare the Simulations For Each Day 

# do the pairwise comparisons
# first construct an empty matrix
# to contain results of comparisons
comparison_matrix<-matrix(nrow=ncol(posterior_samples),ncol=ncol(posterior_samples))
# give column and row names for the matrix
# (these are our observed dates)
colnames(comparison_matrix) <- as.factor(sampled_dates$date)
rownames(comparison_matrix) <- as.factor(sampled_dates$date)
# loop over the columns of the matrix
# and count the number of times the values
# in each column are greater than the values
# in the other columns
for(col in 1:ncol(posterior_samples)){
  comparisons<-posterior_samples[,col]>posterior_samples
  comparisons_counts<-colSums(comparisons)
  comparisons_counts[col]<- 0 # Set the same column comparison to zero.
  comparison_matrix[,col]<-comparisons_counts
}

# shape of output comparison matrix
dim(comparison_matrix)
# [1] 50 50

# what does it look like?
head(comparison_matrix[1:10,1:5],10)


# Step 6: Make the Pairwise Comparison Plot
# Step 6: Make the Pairwise Comparison Plot
# Step 6: Make the Pairwise Comparison Plot

# https://stackoverflow.com/questions/12081843/r-matrix-plot-with-colour-threshold-and-grid
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

# load the reshape2 package
# for melting our data
# melt the data
melted_cormat <- melt(comparison_matrix)
# rename the variables
names(melted_cormat)=c("x","y","count")
# identify which comparisons are "significant"
melted_cormat$meaningful_diff = factor(melted_cormat$count>950)
# and set the levels
levels(melted_cormat$meaningful_diff) = c('No Difference', 
                                          'Row > Column')
# sort the matrix by the dates
# first turn the date columns
# into date types in R using
# the lubridate package
melted_cormat$x <- ymd(melted_cormat$x)
melted_cormat$y <- ymd(melted_cormat$y)
# then arrange the dataset by x and y dates
melted_cormat %>% arrange(x,y) -> melted_cormat 

# what does it look like?
head(melted_cormat,10)


# make the heatmap
ggplot(melted_cormat, aes(as.factor(x), as.factor(y), 
                          fill = meaningful_diff)) + 
  # tile plot
  geom_tile() +
  # remove x and y axis titles
  # rotate x axis dates 90 degrees
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.x = element_text(angle = 90, 
                                   vjust = .5)) + 
  # choose the colors for the plot 
  # and specify the legend title
  scale_fill_manual(name = "Comparison", 
                    values=c("#999999", "#800000")) + 
  # add a title to the plot
  labs(title = "Pairwise Comparisons of Model-Estimated Step Counts at 6 PM")




