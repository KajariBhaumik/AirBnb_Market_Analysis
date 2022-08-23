##################
#### LISTINGS ####
##################

library(dplyr)
setwd("/Users/my_MAC/Graduate Studies/MS/Business Analytics/MBAD_6211/Data_AirBnb/Data")

lst_original=read.csv('listings_initial_subset.csv',header = TRUE, na.strings = c('NA','?',''))

lst_subset=read.csv('listing_full.csv',header = TRUE, na.strings = c('NA','?',''))

lst_subset$price=as.numeric(gsub('[$]', '',lst_subset$price))

summary(lst_subset)

# Missing values

missing.values <- lst_subset %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing)) 

missing.values %>%
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity') +
  labs(x='variable', y="number of missing values", title='Number of missing values') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


missing.values <- lst_subset %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

levels <-
  (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x =
         'Variable', y = "% of missing values")

percentage.plot

#EDA

room_type <-  lst_subset %>%
  count(room_type, sort = TRUE) %>%
  mutate(room_type = reorder(room_type, n)) %>%
  ggplot(aes(room_type, n)) +
  geom_col() +
  xlab(NULL) +
  ylab("count of Room Type") +
  coord_flip()

property_type <-  lst_subset %>%
  count(property_type, sort = TRUE) %>%
  mutate(property_type = reorder(property_type, n)) %>%
  ggplot(aes(property_type, n)) +
  geom_col() +
  xlab(NULL) +
  ylab("count of Property Type") +
  coord_flip()
library(gridExtra)
grid.arrange(room_type, property_type, nrow = 1)

################################################


host_loc = as.data.frame(table(lst_subset$host_location))
colnames(host_loc) = c("location", "host_counts")

host_loc = host_loc[order(host_loc$host_counts, decreasing = T),]

host_loc$state = sub("^([^,]+),\\s*([^,]+),.*", "\\2", host_loc$location)
tail(host_loc)
host_loc_state = host_loc %>% group_by(state) %>%
  summarise(total_host = sum(host_counts))

host_loc_state = as.data.frame(host_loc_state)

host_loc_state$NC = ifelse(host_loc_state$state %in% c("North Carolina",
                                                       "North Carolina, United States"), 
                           "North Carolina", "Other")
host_loc_state_NC = host_loc_state %>% group_by(NC) %>%
  summarise(total_host = sum(total_host))

ggplot(host_loc_state_NC, aes(x='', y=total_host, fill=NC)) +     
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(y = total_host,
                label = round((total_host/sum(host_loc_state_NC$total_host ))*100,2)))

host_loc_state_noNC = host_loc_state[!host_loc_state$state %in% c("North Carolina",
                                                                  "North Carolina, United States",
                                                                  "US", "United States",
                                                                  "Uttar Pradesh"),]


ggplot(host_loc_state_noNC, aes(factor(state), total_host, fill=state)) +     
  geom_col() + 
  theme_classic() + 
  xlab("Hosts from different States") +
  ylab("Number of Hosts") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##################################################


ggplot(lst_original, aes(x=review_scores_rating))+
  geom_histogram(fill='orange')+
  theme_minimal()+
  ggtitle("Historgram of Review Scores")

ggplot(lst_subset, aes(x=price))+
  geom_histogram(fill='brown')+
  theme_minimal()+
  ggtitle("Historgram of Price")

boxplot(price~bedrooms,data=lst_subset,col='gold', main="Listing Data", 
        xlab="Number of Bedrooms", ylab="Price")


boxplot(price~room_type,data=lst_subset, main="Listing Data", 
        xlab="Type of Room", ylab="Price", col='orange')


################################################

################################################

##################
#### CALENDAR ####
##################

calendar=read.csv('calendar_full.csv',header = TRUE, na.strings = c('NA','?','','.'))
head(calendar)

# how many have different price and adjusted_price (19,253 records)
dim(subset(calendar, calendar$price != calendar$adjusted_price))

str(calendar)

# converting data types
library(lubridate)
calendar$date = ymd(calendar$date)
# f = 1; t = 2 >> in factoring
calendar$available = as.factor(calendar$available)
calendar$price = as.numeric(gsub('[$]', '',calendar$price))
calendar$adjusted_price = as.numeric(gsub('[$]', '',calendar$adjusted_price))

str(calendar)
head(calendar)

summary(calendar)
library(Hmisc)
calendar$price = with(calendar, impute(price, mean))
calendar$adjusted_price = with(calendar, impute(adjusted_price, mean))

library(dplyr)
calendar$occupied = ifelse(calendar$available == 'f', 1,0)
calendar$booking = ifelse(calendar$available == 'f', "Not Available", "Available")
# to reduce noise in data - looking at monthly aggregates
calendar$month = format(calendar$date,'%m')
calendar$year = format(calendar$date,'%Y')
time_series_agg = calendar %>% group_by(month)  %>%
  summarise(total_price = sum(price),
            avg_price = mean(price))
#occupancy rate for individual listing
occupancy_rate_listing = calendar %>% group_by(month,listing_id) %>%
  summarise(occ_rate = sum(occupied)/ n())

library(ggplot2)

occupancy_rate_listing = as.data.frame(occupancy_rate_listing)
ggplot(occupancy_rate_listing, aes(x=occ_rate))+
  geom_histogram(fill= 'blue', bins = 20) +
  theme_bw() +
  xlab('Histogram of Occupancy rate')
#occupancy rate in asheville

occupancy_rate_asheville = calendar %>% group_by(month) %>%
  summarise(occ_rate = sum(occupied)/ n())

library(ggplot2)

ggplot(data=occupancy_rate_asheville, aes(x=month, y=occ_rate, group = 1)) +
  geom_line()

###
occupancy_rate_asheville = calendar %>% group_by(year,month) %>%
  summarise(occ_rate = sum(occupied)/ n())

library(ggplot2)
library(ts)

x <- ts(occupancy_rate_asheville[,3],start=c(2022,3),frequency=12)
plot(x, xaxt = "n")
tsp = attributes(x)$tsp
dates = seq(as.Date("2022-03-01"), by = "month", along = x)
axis(1, at = seq(tsp[1], tsp[2], along = x), labels = format(dates, "%Y-%m"))


#Market supply for asheville
calendar$non_occupied = ifelse(calendar$available == 't', 1, 0)

#number of Active Listing
total_active_listing=calendar %>% group_by(month) %>%
  summarise(total_active_listing=n_distinct(listing_id))

ggplot(data=total_active_listing, aes(x=month, y=total_active_listing, group = 1)) +
  geom_line()


# monthly total_price times series
ggplot(data=time_series_agg, aes(x=month, y=total_price, group = 1)) +
  geom_line()


################################################

################################################

##################
#### REVIEWS ####
##################

reviews=read.csv('review_full.csv',header = TRUE, na.strings = c('NA','?','','.'))

# only keeping 4 columns
df_review= subset(reviews,select = c(listing_id,id,date,reviewer_id))  


# checking the data types
str(df_review)

# converting date to Date
df_review$date=as.Date(df_review$date)
str(df_review)

# extracting year and month
df_review$year=format(df_review$date,'%Y')
df_review$month=format(df_review$date,'%m')

#calculating Average_monthly_review_per_listing
review_by_month = df_review %>% group_by(month) %>%
  summarise(total_review_by_month = n())

mean(review_by_month$total_review_by_month)
#20,373 review on average per month

review_by_year_month = df_review %>% group_by(year, month) %>%
  summarise(total_review_by_month = n())
ggplot(review_by_year_month, aes(x=total_review_by_month))+
  geom_histogram(fill= 'blue') +
  theme_bw() +
  xlab('Total reviews by month - since 2011')

recent_reviews=subset(df_review, year %in% c(2019,2020,2021) )
review_by_year_month = recent_reviews %>% group_by(year, month) %>%
  summarise(total_review_by_month = n())
ggplot(review_by_year_month, aes(x=total_review_by_month))+
  geom_histogram(fill= 'red') +
  theme_bw() +
  xlab('Total reviews by month - since 2019')

recent_reviews=subset(df_review, year %in% c(2019,2020,2021) )
review_by_year_month = as.data.frame(recent_reviews %>% group_by(listing_id, year, month) %>%
                                       summarise(total_review_by_month = n()))

review_avg = review_by_year_month %>% group_by(listing_id) %>%
  summarise(avg_review_by_listing = mean(total_review_by_month))
ggplot(review_avg, aes(x=avg_review_by_listing))+
  geom_histogram(fill= 'orange') +
  theme_bw() +
  xlab('Avg reviews by month per listing - since 2019')





ggplot(data=review_by_month, aes(x=month, y=total_review_by_month, group = 1)) +
  geom_line()

test=subset(df_review, !year %in% c(2022) )
listing_per_year = test %>% group_by(year) %>%
  summarise(count_distinct = n_distinct(listing_id))


ggplot(listing_per_year, aes(factor(year), count_distinct, fill = year)) +     
  geom_col(position = 'dodge') + 
  theme_minimal() + 
  xlab("Year") +
  ylab("Number of Distinct Listings with Reviews")



## Daily
customer_influx_daily = test %>% group_by(date) %>%
  summarise(number_of_reviews = n_distinct(reviewer_id))

ggplot(data=customer_influx_daily, aes(x=date, y=number_of_reviews, group = 1)) +
  geom_line() + 
  xlab("Year-Month") +
  ylab("Number of Reviews")




## Monthly
test$year_month = as.numeric(test$year)*100 + as.numeric(test$month)
customer_influx_monthly = test %>% group_by(year_month) %>%
  summarise(number_of_reviews = n_distinct(reviewer_id))

ggplot(data=customer_influx_monthly, aes(x=year_month, y=number_of_reviews, group = 1)) +
  geom_line() + 
  xlab("Year-Month") +
  ylab("Number of Reviews")



plot(customer_influx_monthly$number_of_reviews, type = 'l')

#install.packages('ts')
#install.packages('forecast')


library(forecast)
library(ts)

x <- ts(customer_influx_monthly[,2],start=c(2011,7),frequency=12)

plot(x, main="Number of Unique Reviews over the years")

x_d = decompose(x)
plot(x_d)


z<- log10(x)
plot(z)

pacf(z,main="ACF")


y <- diff(z,1)
plot(y)


PP.test(y)

acf(y,main="ACF")

ARIMAfit <- auto.arima(z, approximation=FALSE,trace=TRUE)

summary(ARIMAfit)

pred <- predict(ARIMAfit,n.ahead=12)
# pred
# # Plot the data
# # Remember initial log-transformation?
# par(mfrow = c(1,1))
# plot(x,type='l',xlim=c(2022,2023),ylim=c(1,1600),xlab = 'Year',ylab = 
#        'Tractor Sales')
# lines(10^(pred$pred),col='blue') 
# lines(10^(pred$pred+2*pred$se),col='orange')
# lines(10^(pred$pred-2*pred$se),col='orange')
# 

new_model = auto.arima(x)
summary(new_model)

forecast(new_model, h = 12)
plot(forecast(new_model, h = 12))


## Impact of COVID - yearly data
customer_influx_yearly = test %>% group_by(year) %>%
  summarise(number_of_reviews = n_distinct(reviewer_id))

ggplot(data=customer_influx_yearly, aes(x=year, y=number_of_reviews, group = 1)) +
  geom_line() + 
  xlab("Year") +
  ylab("Number of Reviews")
# Keeping only 2017,18,19,20,21 data
df_review2=subset(df_review, year %in% c(2017,2018,2019,2020,2021) )
print(dim(df_review2))

df_review2$pandemic = ifelse(df_review2$year <= 2019, 'Before', 'After')

library(ggplot2)
tbl <- with(df_review2, table(year, pandemic))
ggplot(as.data.frame(tbl), aes(factor(year), Freq, fill = pandemic)) +     
  geom_col(position = 'dodge') + 
  xlab("Year") +
  ylab('Number of Reviews')


tbl <- with(df_review2, table(month, year, pandemic))
ggplot(as.data.frame(tbl), aes(factor(month), Freq, fill = year)) +     
  geom_col(position = 'dodge') 









