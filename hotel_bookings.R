library(tidyverse)
library(skimr)
library(janitor)

bookings_df <- read.csv("hotel_bookings.csv")

skim_without_charts(bookings_df)
head(bookings_df)
str(bookings_df)
colnames(bookings_df)

### Cleaning Data#######

trimmed_df <- bookings_df %>%
  select(hotel, is_canceled, lead_time) %>%
  rename(hotel_type = hotel)

example_df <- bookings_df %>%
  summarize(number_cancelled = sum(is_canceled), average_lead_time = mean(lead_time))

head(example_df)

sort_by_lead_time <- arrange(bookings_df, desc(lead_time))

#### Creating a bar chart to visualize the data ############

ggplot(data = bookings_df) + geom_bar(mapping=aes
                                      (x = distribution_channel))

### Using the 'fill' attribute to visualize according to the deposit type

ggplot(data = bookings_df) + geom_bar(mapping=aes
                                      (x = distribution_channel
                                        ,fill=deposit_type))

### Using the 'fill' attribute to visualize according to the market segment 

ggplot(data = bookings_df) + geom_bar(mapping=aes
                                      (x = distribution_channel
                                        ,fill=market_segment))

########################################
### Using Facets in my visualizations###
########################################

# 1. Using deposit type

ggplot(data = bookings_df) + geom_bar(mapping=aes
                                      (x = distribution_channel)) +
                                     facet_wrap(~deposit_type) +
                                  theme(axis.text.x = element_text(angle = 45))


# 2. Using market segment


ggplot(data = bookings_df) + geom_bar(mapping=aes
                                      (x = distribution_channel)) +
                                        facet_wrap(~market_segment) +
                                  theme(axis.text.x = element_text(angle = 45))


########################################
### Filtering Data #####################
########################################

ggplot(data = bookings_df) + geom_bar(mapping=aes
                                      (x = hotel, fill=market_segment)) +
                                  theme(axis.text.x = element_text(angle = 45))


ggplot(data = bookings_df) + geom_bar(mapping=aes
                                    (x =hotel)) + facet_wrap(~market_segment) +
                                  theme(axis.text.x = element_text(angle = 45))

#1. using a filter
onlineta_city_hotels <- filter(bookings_df, 
                               (hotel=="City Hotel" & 
                                bookings_df$market_segment=="Online TA"))

#1.1 Using pipes
onlineta_city_hotels_V2 <- bookings_df %>%
  filter(hotel=="City Hotel") %>%
  filter(market_segment=="Online TA")


#Visualizing the filter data

ggplot(data = onlineta_city_hotels) + geom_point(mapping = 
                                            aes(x= lead_time, y = children))



########################################
### Saving my visualization ############
########################################

mindate <- min(bookings_df$arrival_date_year)
maxdate <- max(bookings_df$arrival_date_year)

ggplot(data = bookings_df) +
  geom_bar(mapping = aes(x = market_segment)) +
  facet_wrap(~hotel) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title="Comparison of market segments by hotel type for hotel bookings",
       caption=paste0("Data from: ", mindate, " to ", maxdate),
       x="Market Segment",
       y="Number of Bookings")























