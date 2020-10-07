####### Solution for section 3 ###########

# Scoring scheme function
sim = function(x){
  points = 0
  for (i in x){
    points = ifelse(i == 1, points*2, points + 1)
  }
  return(points)
}

# Q1. Let's start with a fair coin. What is our expected score after 5 flips?
set.seed(100)
avg_fair_5 = mean(replicate(50000,sim(rbinom(5,1,0.5))))

# Q2. Suppose we played the game many times, with five flips each. 
#     What is the expected standard deviation of the scores?
set.seed(100)
avg_fair_15 = mean(replicate(50000,sim(rbinom(15,1,0.5))))

# Q3. What is our expected score after 15 flips?
set.seed(100)
sd_fair_5 = sd(replicate(50000,sim(rbinom(5,1,0.5))))

# Q4. What is the standard deviation?
set.seed(100)
sd_fair_15 = sd(replicate(50000,sim(rbinom(15,1,0.5))))

# Q5. After 10 flips, what is the probability our score is a power of two? 
#     Express this probability as a number between 0 and 1

#~~~# Heuristic approach #~~~#

# Simulate large number of possible scenarios using binomial distribution.
# Take the log2 of the scores. If the floor and the ceiling of the log2 values are the same,
# indicates a score that is power of 2. Extract the probability as a ratio. 
set.seed(100)
scores = replicate(500000,sim(rbinom(10,1,0.5)))
plot(density(scores))
# replace 0 with a value that is not a power of 2
possible_scores = ifelse(scores==0,3, scores)
pow_of_2 = (ifelse(ceiling(log2(scores))==floor(log2(scores)),T,F))
prob_pow2_10 = sum(pow_of_2 == T)/length(pow_of_2)
prob_pow2_10 

# Q5. We will start a new game, now with an unfair coin. 
#     It has a 2/3 probability of heads, and a 1/3 probability of tails. 
#     What is our expected score after 10 flips?
set.seed(100)
avg_unfair_10 = mean(replicate(50000,sim(rbinom(5,1,0.33333))))
avg_unfair_10

# Q6. What is the standard deviation?
set.seed(100)
sd_unfair_10 = sd(replicate(50000,sim(rbinom(5,1,0.33333))))
sd_unfair_10

# Q7. If we can choose how unfair our coin is, what probability of heads gives 
#     us the highest expected score for 10 flips? 
#     Express this probability as a number between 0 and 1

#~~~# Heuristic approach #~~~#

set.seed(100)
p_val = seq(0,1,0.01)
df = tibble(p_val) %>%
  mutate(exp_score = unlist(lapply(p_val, function(x) 
    mean(replicate(10000,sim(rbinom(10,1,x)))))))
df[which.max(df$exp_score),]

#################### solution for Section 2 ###################
library(tidyverse)
library(readr)
library(rstatix)

df = read_delim("~/Documents/Personal_files/Data_incubator/Historical_DOB_Permit_Issuance.csv",
                delim = ",", col_names = T)
names(df)
samp = df %>% slice(c(1:100))
#Q1. What fraction of all construction permits in 
#     this data set correspond to renewed permits?

df %>% dplyr::count(`Filing Status`) %>% 
  mutate(prop = prop.table(n))

# Q2. If you consider only renewed permits, what is the ratio of the number of permits 
#     issued to corporations to the number of permits issued to individuals?

df %>% 
  filter(`Filing Status` == "RENEWAL") %>% 
  dplyr::count(`Owner's Business Type`) %>%
  filter(`Owner's Business Type` %in% c("CORPORATION","INDIVIDUAL")) %>%
  mutate(ratio = n / lead(n))

# Q3. What fraction of these at least year-long permits were issued in the borough with 
#     the highest number of such permits?

# First identify burough with highest number of such year long permits
class(df$`Issuance Date`)   #checking datatype 
class(df$`Expiration Date`) #checking datatype 

df %>% 
  mutate(permit_length = difftime(`Expiration Date`,`Issuance Date`,units = 'days')) %>% 
  filter(permit_length > 365) %>%
  dplyr::count(`BOROUGH`) %>% arrange(-n)
# As observed, it is MANHATTAN 
# Now estimate the fraction  
df %>% filter(`BOROUGH`=="MANHATTAN") %>%
  mutate(permit_length = difftime(`Expiration Date`,`Issuance Date`,units = 'days')) %>%
  mutate(permit_duration = ifelse(permit_length > 365,"Long","Short")) %>%
  count(permit_duration) %>% na.omit(.) %>% mutate(prop = prop.table(n))

# Q4. Limit your analysis to permits that were filed in 2010. 
# The column recording the date of permit filing is called 'Filing Date'. 
# For each ZIP code compute the ratio between the number of permits issued in 2010 
# and number of residents in that ZIP code. Make sure to discard any ZIP codes with 
# population lower than 1000 people. Note: you will need to use both the DOB permits 
# dataset and the US Census dataset. In the DOB permits dataset, the ZIP code is 
# recorded under column 'Postcode'. How many ZIP codes are outliers in terms of 
# number of construction permits per number of people - more specificaly, for how 
# many ZIP codes does this computed ratio exceed the mean by more than twice the 
# standard deviation?

# Load US census dataset

census.data = read_delim("~/Documents/Personal_files/Data_incubator/2010+Census+Population+By+Zipcode+(ZCTA).csv",
                         delim = ",", col_names = T)
df_2010 = df %>% 
  filter(format(.$`Filing Date`,"%Y") == "2010") %>%
  filter(Postcode %in% census.data$`Zip Code ZCTA`) %>%
  count(Postcode) %>% mutate(Postcode = as.character(Postcode))

# Assuming one tailed
census.data.df = census.data %>% 
  filter(`Zip Code ZCTA` %in% df_2010$Postcode) %>%
  rename(Postcode = `Zip Code ZCTA`) %>%
  left_join(df_2010, by = "Postcode") %>%
  filter(`2010 Census Population` >= 1000) %>%
  mutate(ratio = n/`2010 Census Population`) %>%
  mutate(ratio_outlier = ifelse(ratio > mean(ratio) + (2*sd(ratio)),T,F)) %>%
  count(ratio_outlier)
census.data.df

# Q5. Observe how the number of issued permits changes accross the years. 
#     Limit your analysis to years 1990-2012 (both inclusive). 
#     What is the coefficient of determination (R squared) between the year a permit was
#     issued and the number of issued permits that year? 
#     The column recording the date when a permit was issued is called 'Issuance Date'.

permits_per_year = df %>% 
  mutate(Year = as.integer(format(.$`Filing Date`,"%Y"))) %>%
  filter(Year > 1989) %>%
  filter(Year < 2013) %>%
  count(Year)

ggplot(permits_per_year, aes(x = Year, y = n))+ 
  geom_point() + geom_smooth(method = 'lm') 
# The relationship between year and number of permits appears roughly linear
mod1 = lm(Year~n, data = permits_per_year) # modeling relationship
mod1 %>% summary(.) # estimating R-squared (adjusted for # of explanatory variables)
par(mfrow=c(2,2))
plot(mod1) # model diagnostic plots indicate most model assumptions are approximately true
par(mfrow=c(1,1))

# Q6. Let's investigate how the number of construction jobs that start in the summer 
#     vs the winter changes across the years. The date of construction job start is 
#     recorded in column 'Job Start Date'. For every year, compute the ratio between 
#     the number of construction jobs that start in the peak of summer (in July and August) 
#     and the number of jobs that start in the peak of winter (in January and February). 
#     Again limit your analysis to years 1990-2012 (both inclusive). Find the year when this 
#     ratio was maximal - what was the value of this ratio for that year?

seasons_info = df %>% 
  mutate(Year = as.integer(format(.$`Job Start Date`,"%Y"))) %>%
  #mutate(Year = as.integer(format(.$`Issuance Date`,"%Y"))) %>%
  filter(Year > 1989) %>%
  filter(Year < 2013) %>%
  mutate(season = ifelse(format(.$`Job Start Date`,"%B") %in% c("July","August"),"Summer",
                         ifelse(format(.$`Job Start Date`,"%B") %in% c("January","February"),
                                "Winter",NA))) %>% 
  drop_na(season) %>%
  count(Year,season) %>% 
  arrange(season) %>%
  dplyr::group_by(Year) %>% 
  mutate(ratio = n/lead(n)) 
seasons_info[order(-seasons_info$ratio),]

# Q7. If we look at how permit duration varies with the construction start date, 
#     it appears like jobs that start in November tend to have shorter permit duration. 
#     The date of construction job start is recorded in column 'Job Start Date'. 
#     The date on which a permit was issued is recorded in column 'Issuance Date', 
#     the date the permit expires is in column 'Expiration Date'. 
#     Calculate the chi-square test statistic for testing whether a permit is more 
#     likely to be issued for less than 60 days when construction job start date is November. 
#     Again limit your analysis to data from years 1990-2012 (both inclusive).


Nov_constr_table = df %>%
  mutate(Nov_Construction = ifelse(format(.$`Job Start Date`,"%B") %in% c("November"),"Nov","Others")) %>%
  # taking job start year as study focus year filter
  mutate(Year = as.integer(format(.$`Job Start Date`,"%Y"))) %>% 
  filter(Year > 1989) %>%
  filter(Year < 2013) %>%
  mutate(permit_length = 
           ifelse(difftime(`Expiration Date`,`Issuance Date`,units = 'days') < 60, "Less","More")) %>%
  group_by(Nov_Construction, permit_length)%>%
  drop_na(permit_length) %>%
  summarise(n=n())%>%
  spread(permit_length, n)
Nov_constr_table %>% 
  column_to_rownames("Nov_Construction") %>% 
  chisq_test()
#  chisq_descriptives()

# Therefore, we reject the null hypothesis that there is no relationship between 
# the month of November and length of permit

########## Section 1 ################
library(readr)
library(tidyverse)
lnkdn = read_delim("~/Documents/Personal_files/Data_incubator/Data_collected/temp_datalab_records_linkedin_company.csv",
                delim = ",")
head(lnkdn)
lnkdn %>% select(as_of_date) %>% mutate(Year = format(.$as_of_date, "%Y")) %>%
  count(Year)
lnkdn = lnkdn %>% select(-c("description","website","entity_id","cusip","isin"))

#plot for goldman sachs
lnkdn %>% filter(company_name == "goldmansachs") %>%
  mutate(Year = format(.$as_of_date, "%Y")) %>% 
  ggplot(aes(x = log(followers_count), y = employees_on_platform)) +
  geom_point() + geom_smooth(method = "lm") + theme_bw()+
  labs(title = "GoldmanSachs")
lnkdn %>% filter(company_name == "novonordisk") %>%
  mutate(Year = format(.$as_of_date, "%Y")) %>% 
  ggplot(aes(x = log(followers_count), y = employees_on_platform)) +
  geom_point() + geom_smooth(method = "lm") + theme_bw() +
  labs(title = "NovoNordisk")

fb = read_delim("~/Documents/Personal_files/Data_incubator/Data_collected/temp_datalab_records_social_facebook.csv",
                  delim = ",")
head(fb)
fb %>% select(time) %>% mutate(Year = format(.$time, "%Y")) %>%
  count(Year)

lnkdn$company_name = sub(" ","",tolower(lnkdn$company_name))
fb$username = sub(" ","",tolower(fb$username))

lnkdn.part = lnkdn %>% filter(company_name %in% fb$username)
fb.part = fb %>% filter(username %in% lnkdn.part$company_name)

fb.part  = fb.part %>% select(-c(has_added_app, date_added,entity_id, cusip, isin))
fb.part %>% #filter(username == "novonordisk") %>%
  ggplot(aes(x = checkins, y = log(likes))) + geom_point()


# novonordisk
NN.l = lnkdn %>% filter(company_name == "novonordisk")
NN.f = fb %>% filter(username == "novonordisk")
