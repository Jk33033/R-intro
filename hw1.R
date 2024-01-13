library(ggplot2)
library(data.table)
hotels <- readRDS(url('http://bit.ly/CEU-R-hotels-2018-merged'))

#How many hotels are from Austria?
nrow(hotels[country == "Austria"])

#What is the rating of the most expensive hotel (based on the price per night)?
hotels[order(avg_price_per_night), .(rating)][1]

#How many bookings are in 4-star hotels?
nrow(hotels[stars == 4])

#Which country has the highest number of 5-star hotels?
hotels[stars == 5, .N, by = .(country)][order(-N)][1, country]

#Plot the number of bookings per country!
hotels$booking <- as.numeric(hotels$booking)
bookings_per_country <- hotels[, .(total_bookings = sum(booking)), by = .(country)]
ggplot(bookings_per_country, aes(x = country, y = total_bookings)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  ylab("Number of Bookings") +
  xlab("Country")

#Flip the coordinates and use the "classic dark-on-light theme"!
ggplot(bookings_per_country, aes(x = country, y = total_bookings)) +
  geom_bar(stat = "identity") +
  coord_flip() +  
  theme_classic() +  
  ylab("Number of Bookings") +
  xlab("Country")

#Drop the Y axis title, and rename the X axis to "Number of bookings"!
ggplot(bookings_per_country, aes(x = total_bookings, y = country)) +
  geom_bar(stat = "identity") +
  theme_classic() +  
  xlab("Number of bookings") + 
  ylab("")

#Count the number of hotels per country!
ggplot(hotels, aes(x = country)) +
  geom_bar() +
  coord_flip() +
  theme_classic() +
  ylab("Number of hotels") +
  xlab("")

#Order by alphabet!
ggplot(hotels, aes(x = factor(country, levels = sort(unique(country))))) +
  geom_bar() +
  coord_flip() +
  theme_classic() +
  ylab("Number of hotels") +
  xlab("")

#Count the number of bookings per country, order by the number of bookings!
bookings_by_country <- hotels[, .(total_bookings = sum(booking, na.rm = TRUE)), by = .(country)]
setorder(bookings_by_country, -total_bookings)
bookings_by_country
  
#Compute the average rating per number of stars! Use the weighted.mean function to account for the number of ratings of the hotels, and experiment with the na.rm argument. Eliminate NAs. Order by stars.
avg_rating_by_stars <- hotels[!is.na(rating) & !is.na(rating_count), 
                              .(avg_rating = weighted.mean(rating, rating_count, na.rm = TRUE)), 
                              by = .(stars)]
setorder(avg_rating_by_stars, stars)
avg_rating_by_stars

#Plot this computed average rating per stars!
#Make sure that each star category is printed on the X axis!
avg_rating_by_stars <- avg_rating_by_stars[!is.na(stars) & !is.na(avg_rating)]
ggplot(avg_rating_by_stars, aes(x = stars, y = avg_rating)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  xlab("Number of Stars") +
  ylab("Average Rating")

#Create a boxplot on ratings per stars!
hotels <- hotels[!is.na(stars) & !is.na(rating)]
ggplot(hotels, aes(x = factor(stars), y = rating, group = stars)) +
  geom_boxplot() +
  theme_minimal() +
  xlab("Number of Stars") +
  ylab("Rating")

#Create histograms on the nightly prices for each star category! Check out the arguments and disable forcing the same Y axis range for the subplots.
hotels <- hotels[!is.na(stars) & !is.na(sd_price_per_night)]
ggplot(hotels, aes(x = sd_price_per_night)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ stars, scales = "free") +
  xlab("Nightly Price") +
  ylab("Frequency")
