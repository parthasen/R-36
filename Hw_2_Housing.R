setwd("/Users/SUNG/Desktop/STA141/HW2")
load("Data/housing.rda")

library(RColorBrewer)
library(maps)
library(lattice)
library(outliers)
library(gplots)
library(ggplot2)

##################################################################

names(housing)
dim(housing)
housing[1:10,]

# 1. How many houses are in the data set?
dim(housing)[1]

# 2. What period of time does the data cover?
range(unique(housing["date"], na.rm = TRUE)[,1])

# 3. How many houses were sold in each county?
table(housing["county"])

# 4. How many houses were sold in each county in each year?
table(housing$county, housing$year)

# 5. What is the average price of a house in the Bay Area?
mean(housing$price, rm.na = TRUE)

# 6. Which county has the largest average price per square foot?
which.max(tapply(housing$price/housing$bsqft, housing$county, mean, na.rm = TRUE))

# 7. Which county has the largest variation in price per bedroom?
which.max(tapply(housing$price/housing$br, housing$county, var, na.rm = TRUE))

# 8. What is the average price per square foot of houses with between 2 and 4 
# bedrooms (inclusive) in Berkeley that have between 1000 and 3000 square feet 
# or which have a lot size that is greater than 90% of all houses in the Bay Area?
price_square = housing[c("price","bsqft")][housing$city == "Berkeley" & housing$bsqft >= 1000 & housing$bsqft <= 3000 & 2 <= housing$br & housing$br <= 4, ]
mean(price_square[,1]/price_square[,2], na.rm=TRUE)
price_square_2 = housing[c("price","bsqft")][housing$city == "Berkeley" & housing$lsqft > quantile(housing$lsqft, 0.9, na.rm=TRUE) & 2 <= housing$br & housing$br <= 4, ]
mean(price_square_2[,1]/price_square_2[,2], na.rm=TRUE)

# 9. Find 5 houses which have very unusual characteristics and may be erroneous? 
# Explain how you identified these.
housing[which(housing$year < 100), ][1:5,]


# 10. Find all houses that have a price per square foot greater than $3000 and 
# order the corresponding data frame by price from highest to lowest. (Make 
# certain to keep the variables for each observations together/aligned.)
p_s = housing$price/housing$bsqft
p_s[is.na(p_s)] = 0
housing[p_s > 3000, ][order(housing[p_s > 3000, ][["price"]], decreasing = TRUE),]


# 2.1
avgByCounty = tapply(housing$price, housing$county, mean, na.rm = TRUE)
avgByCounty
county_name = substr(names(avgByCounty), 1, nchar(names(avgByCounty)) - 7)
weekEachCounty = table(housing$wk, housing$county)
n = dim(weekEachCounty)[2]
cols = palette(rev(rich.colors(9)))
matplot(weekEachCounty[, 1:n], type = "l", xaxt = "n", 
        main = "Number of houses sold by week in each county", 
        ylab = "Number of houses sold", xlab = "Week", lty = 1, col = cols)
xticks = seq(1, dim(weekEachCounty)[1], 1)
xlabels = rownames(weekEachCounty)
axis(1, at = xticks, las = 2, cex.axis = 0.3, labels = xlabels)
legend("topleft", county_name, lty=1, cex=0.35, col = cols)

# na.omit()
# avgByCounty[as.character(housing$county)]
# tmp = housing$price - avgByCounty[as.character(housing$county)]
# table(is.na(tmp))
# tapply(tmp, housing$county, mean, na.rm = TRUE)
# all.equal(tapply(tmp, housing$county, mean, na.rm = TRUE), 0)


# 2.2 For each county, draw a scatter plot of price versus building square foot,
# and color each point according to the number of bedrooms in the house.
# reference to http://eeyore.ucdavis.edu/stat141/QuakeLocationsByMagDepth.html
xrange = range(housing$bsqft, na.rm = TRUE)
yrange = range(housing$price, na.rm = TRUE)
p.byCounty = split(housing[c("bsqft", "price", "br")], housing$county)
cols_2 = palette(rev(rich.colors(8)))
par(mfrow = c(3, 3))
sapply(1:9, function(x) 
  plot(p.byCounty[[x]][,1], p.byCounty[[x]][,2], xlim = c(0, 5000), 
       ylim = c(0,3000000), col = cols_2[p.byCounty[[x]][,3]], 
       xlab = "Building Square feet", ylab = "Price", main = paste(county_name[x])
       )
  )
par(xpd=NA)
#sapply(p.byCounty, function(x) 
#  plot(price ~ bsqft, x, xlim = c(0, 5000), 
#       ylim = c(0,3000000), 
#       xlab = "Building Square feet", ylab = "Price"
#  )
#)

# 2.3 Draw a map of the counties of the Bay Area and color the counties by average price.
names(avgByCounty)
sort(names(avgByCounty))
avgByCounty_2 = avgByCounty
Bay_long= range(housing$long,na.rm=TRUE)
Bay_lat= range(housing$lat,na.rm=TRUE)
map("county", namesonly = TRUE, plot = FALSE, xlim = Bay_long, ylim = Bay_lat)
Bay_names = sort(c("california,solano","california,contra costa","california,sonoma",
                    "california,alameda","california,napa","california,santa clara",
                      "california,marin","california,san mateo","california,san francisco"))

names(avgByCounty_2) = Bay_names
color_sample = as.data.frame(brewer.pal(9,"OrRd"))
my_color = as.character(color_sample[rank(avgByCounty_2),])

map("county", region= names(avgByCounty_2), col = my_color, fill = TRUE, )
legend("bottomleft", title = "County", legend = county_name,
       fill = my_color, cex = 0.5)

#map("county", xlim = c(-123.6,-121.5), ylim = c(36.98,38.85),
#    col = brewer.pal(9,"Purples"), fill = TRUE)


# 2.4 Suggest ways we could effectively display and compare the distributions of 
# house prices by county and create that plot.
densityplot( ~ price, housing, groups = county, plot.points = FALSE, col = cols, lex = 3,
            xlim = c(0, 2.5e+06), auto.key = list(title = "County", corner = c(0.95, 1), cex=1.0)
             )
       

# 3.1
#plot.new()
#par(mfrow = c(1, 1))
new_year = as.numeric(substr(housing$date, 1, nchar(housing$date) - 6))
barplot(table(housing$county, new_year), main = "Number of houses sold in each county & year",
        xlab = "Year", ylab = "Frequency", col = brewer.pal(9,"Purples") , beside=TRUE,
      )
legend ("topright",legend = substr(names(avgByCounty), 1, nchar(names(avgByCounty)) - 7), cex = 0.7, fill =  brewer.pal(9,"Purples"))

#my_color=brewer.pal(9,"Purples")
#sapply(1:9, function(x) 
#  lines(table(housing$county, new_year)[x,], col = my_color[x], lwd = 3))
#hyear = cut(housing$date, "year")
#each_year = split(housing, hyear)  



# 3.2
new_year = as.numeric(substr(housing$date, 1, nchar(housing$date) - 6))
my_year = new_year[ 2003 <= new_year  & new_year <=2006]
densityplot( ~ price, housing, groups = my_year, plot.points = FALSE, lex = 3,
             xlim = c(0, 2.0e+06), auto.key = list(title = "Year", corner = c(0.95, 1), cex=1.0)
             )

# test smoothScatter

par(mfrow = c(2, 2))
smoothScatter(housing2003$long, housing2003$lat, xlab = "Longtitude", ylab = "Latitude", main = "2003")
map("county", region= names(avgByCounty_2), col = cols, fill = FALSE, names = TRUE , add = TRUE)
smoothScatter(housing2004$long, housing2004$lat, xlab = "Longtitude", ylab = "Latitude", main = "2004")
map("county", region= names(avgByCounty_2), col = cols, fill = FALSE, names = TRUE , add = TRUE)
smoothScatter(housing2005$long, housing2005$lat, xlab = "Longtitude", ylab = "Latitude", main = "2005")
map("county", region= names(avgByCounty_2), col = cols, fill = FALSE, names = TRUE , add = TRUE)
smoothScatter(housing2006$long, housing2006$lat, xlab = "Longtitude", ylab = "Latitude", main = "2006")
map("county", region= names(avgByCounty_2), col = cols, fill = FALSE, names = TRUE , add = TRUE)

# sapply(c(2002,2003), function(x, y = housing[c("price","long","lat")][new_year == x, ])
#   smoothScatter(y$long, y$lat, xlab = "Longtitude", ylab = "Latitude", main = "123")
#        #map("county", region= names(avgByCounty_2), col = cols, fill = FALSE, names = TRUE , add = TRUE)
# )


#solution
#8
price_per_br <- with(housing, price / br)
price_per_bsqft <- with(housing, price / bsqft)
housing <- data.frame(housing, price_per_bsqft)


# Get 2-4 bedroom houses in Berkeley.
berkeley <- subset(housing, (2 <= br) & (br <= 4) & (city == 'Berkeley'))

# Get 90th quantile of lot size.
lsqft90 <- quantile(housing$lsqft, .9, na.rm = TRUE)

# From the Berkeley houses, get 1000-3000 square foot homes OR homes with lot
# size greater than 90% of all houses.
berkeley <- subset(berkeley, 
                   ((1000 <= bsqft) & (bsqft <= 3000)) | (lsqft > lsqft90))

mean(berkeley$price_per_bsqft)
