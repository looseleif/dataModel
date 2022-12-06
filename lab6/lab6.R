price_prediction_error <- function(price, bedrooms, bathroom, sqft_living, sqft_lot, grade, 
                                   yr_built) {
  # Create a new data frame for the variables to be used in the price prediction
  house_info.dat <- data.frame(price, bedrooms, bathroom, sqft_living, sqft_lot, grade, yr_built)
  
  rows <- nrow(house_info.dat) 
  f <- 0.6
  upper_bound <- floor(f * rows)
  permuted_house.dat <- house_info.dat[sample(rows) , ]
  train.dat <- permuted_house.dat[1:upper_bound, ]
  test.dat <- permuted_house.dat[(upper_bound+1): rows, ]
  house_new.lm <- lm(price ~ bedrooms+bathroom+sqft_living+sqft_lot+grade+yr_built, data = train.dat)
  predicted.dat <- predict(house_new.lm, newdata=test.dat)
  #(test.dat$pricess)
  delta <- predicted.dat - test.dat$price
  rmse <- sqrt(sum(delta^2)/rows)
  return(rmse)
}


# START LAB 6

house_data <- read.csv("house_data.csv")

# --- PART 1 ---


for(z in 1:5){

rows <- nrow(house_data) 
f <- 0.6
upper_bound <- floor(f * rows)
permuted_house.dat <- house_data[sample(rows) , ]
train.dat <- permuted_house.dat[1:upper_bound, ]
test.dat <- permuted_house.dat[(upper_bound+1): rows, ]
house_new.lm <- lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot+grade+yr_built, data = train.dat)
predicted.dat <- predict(house_new.lm, newdata=test.dat)
delta <- predicted.dat - test.dat$price
rmse <- sqrt(sum(delta^2)/rows)
print(rmse)

}

# --- PART 2 ---

data_by_zipcode <- house_data %>% 
                   group_by(zipcode) %>% 
                   summarize(
                    count = n(),
                    med_price = median(price),
                    med_yr_built = median(yr_built),
                    error = price_prediction_error(price, bedrooms, bathrooms, sqft_living, sqft_lot, grade, yr_built)
                   )

head(data_by_zipcode)

print(mean(data_by_zipcode$error))
print(median(data_by_zipcode$error))
print(sd(data_by_zipcode$error))
print(min(data_by_zipcode$error))
print(max(data_by_zipcode$error))