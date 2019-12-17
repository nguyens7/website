---
title: Presidential Chicken
author: Sean Nguyen
date: '2018-10-05'
slug: presidential-chicken
categories:
  - R
  - Shiny
tags:
  - Shiny
  - Scraping
  - MSU
  - Food
subtitle: ''
summary: ''
authors: []
lastmod: '2019-12-13T00:21:54-05:00'
featured: no

header:
  image: "headers/eatatstate_wordcloud.png"
  caption: "Wordcloud of top menu items at MSU dining halls from 10/5/18 - 10/15/18"
  focal_point: ''
  preview_only: yes
projects: []
---


I remember back in undergrad when I visited my friends at Michigan State and they took me to the residential dining hall since they had a meal plan. The cafeteria had typical dorm food like salad bar, burgers, hot dogs, fries etc.  Fortunately, the  dining halls have improved drastically since then and now the undergraduate students have it great with so many options available to them.  

# Eat at State 

A common theme in the lab is to ask one another what do you have for lunch today?  Sometimes we don't bring anything and our options are to go to the international center food court, the restaurants on Grand River or the residential dining halls.  MSU employees have a great option to buy admission into the dining hall at a discounted rate so I always tag along with them so they can swipe me in.  We typically preview the menus at [eatatstate.msu.edu](https://eatatstate.msu.edu/) and will choose to go to the dining hall one with the best menu.    One of my favorite dishes from the cafeteria is Presidential Chicken which is a chicken dish with an alfredo like sauce with vegetables and is wrapped up in puff pastry envelope.  It tastes great and is exponentially better than the food I've had at weddings.  We haven't seen the dish on the menu in well over a year so I decided to see if I could scrape the menus and make a quick shiny dashboard to figure out the next time they would serve it. 


```r
library(rvest)
library(knitr)
library(kableExtra)
library(tidyverse)
library(lubridate)
```

# Generate urls
The key to generating a list of URLs for scraping is to identify the patterns of interest and stick them together using the `paste()` function.  Here I used the main eatatstate domain and simply created an object for each dining hall.  Then I can use the paste function to link them together so then I have URLs for each dining hall.  Then I generated a sequence of dates and appended it to the dining hall URLs.

```r
domain <- "https://eatatstate.msu.edu/menu/"
shaw <- "The%20Vista%20at%20Shaw/all/"
sny_phy <- "The%20Gallery%20at%20Snyder%20Phillips/all/"
landon <- "Heritage%20Commons%20at%20Landon/all/"

dining_halls <- c(shaw, sny_phy, landon)

# List of dates
future_date <- ymd(today()) + days(10)
dates <- seq(ymd(today()), future_date, by = 'day')

# Number of dining halls
dining_hall_no <- length(dining_halls) %>%
  unlist()

# Create lunch urls
day_url <- dining_halls %>% 
  map(~paste(domain, .x, dates, sep = "")) %>%
  unlist()

dining_halls
```

```
## [1] "The%20Vista%20at%20Shaw/all/"               
## [2] "The%20Gallery%20at%20Snyder%20Phillips/all/"
## [3] "Heritage%20Commons%20at%20Landon/all/"
```

```r
dates
```

```
##  [1] "2019-12-13" "2019-12-14" "2019-12-15" "2019-12-16" "2019-12-17"
##  [6] "2019-12-18" "2019-12-19" "2019-12-20" "2019-12-21" "2019-12-22"
## [11] "2019-12-23"
```

```r
day_url %>% 
  head()
```

```
## [1] "https://eatatstate.msu.edu/menu/The%20Vista%20at%20Shaw/all/2019-12-13"
## [2] "https://eatatstate.msu.edu/menu/The%20Vista%20at%20Shaw/all/2019-12-14"
## [3] "https://eatatstate.msu.edu/menu/The%20Vista%20at%20Shaw/all/2019-12-15"
## [4] "https://eatatstate.msu.edu/menu/The%20Vista%20at%20Shaw/all/2019-12-16"
## [5] "https://eatatstate.msu.edu/menu/The%20Vista%20at%20Shaw/all/2019-12-17"
## [6] "https://eatatstate.msu.edu/menu/The%20Vista%20at%20Shaw/all/2019-12-18"
```

Next we will generate the URLs for lunch and dinner and combine them all.

```r
# Create dinner urls
dinner <- "?field_mealtime_target_id=191"

night_url <- dining_halls %>% 
  map(~paste(domain, .x, dates, dinner, sep = "")) %>%
  unlist()

# Combine url list
all_url <- c(day_url, night_url)

all_url %>% 
  head()
```

```
## [1] "https://eatatstate.msu.edu/menu/The%20Vista%20at%20Shaw/all/2019-12-13"
## [2] "https://eatatstate.msu.edu/menu/The%20Vista%20at%20Shaw/all/2019-12-14"
## [3] "https://eatatstate.msu.edu/menu/The%20Vista%20at%20Shaw/all/2019-12-15"
## [4] "https://eatatstate.msu.edu/menu/The%20Vista%20at%20Shaw/all/2019-12-16"
## [5] "https://eatatstate.msu.edu/menu/The%20Vista%20at%20Shaw/all/2019-12-17"
## [6] "https://eatatstate.msu.edu/menu/The%20Vista%20at%20Shaw/all/2019-12-18"
```

### Dates and Time
We'll want to clean up the data with regular expressions and repeat "Lunch"  and "Dinner" for the dates and different dining halls.

```r
# Date
date <- rep(dates, times = 2 * dining_hall_no)

# Cafe

no_dates <- length(dates) 
names <- str_replace_all(dining_halls, "%20|\\/all\\/"," ")
cafe <- rep(names, each = no_dates, times = 2)

# Time
time <- rep(c("Lunch", "Dinner"), each = no_dates * dining_hall_no)
time
```

```
##  [1] "Lunch"  "Lunch"  "Lunch"  "Lunch"  "Lunch"  "Lunch"  "Lunch"  "Lunch" 
##  [9] "Lunch"  "Lunch"  "Lunch"  "Lunch"  "Lunch"  "Lunch"  "Lunch"  "Lunch" 
## [17] "Lunch"  "Lunch"  "Lunch"  "Lunch"  "Lunch"  "Lunch"  "Lunch"  "Lunch" 
## [25] "Lunch"  "Lunch"  "Lunch"  "Lunch"  "Lunch"  "Lunch"  "Lunch"  "Lunch" 
## [33] "Lunch"  "Dinner" "Dinner" "Dinner" "Dinner" "Dinner" "Dinner" "Dinner"
## [41] "Dinner" "Dinner" "Dinner" "Dinner" "Dinner" "Dinner" "Dinner" "Dinner"
## [49] "Dinner" "Dinner" "Dinner" "Dinner" "Dinner" "Dinner" "Dinner" "Dinner"
## [57] "Dinner" "Dinner" "Dinner" "Dinner" "Dinner" "Dinner" "Dinner" "Dinner"
## [65] "Dinner" "Dinner"
```

### Scrape
Here I used the rvest package and combined it with purrr so I read multiple URLs and return the the text so we can put it into a data frame.

```r
# Menu 
menu <- all_url %>%   
 map(~read_html(.x) %>% 
      html_nodes(".meal-title") %>% 
      html_text()) 
```


### Create a data frame
We can bind the data into a data frame so we can easily filter and find menus that we like.

```r
food <- tibble(date, cafe, time, menu) %>% 
  unnest(menu) 

dim(food)
food
```





```
## [1] 2349    4
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:600px; "><table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> date </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> cafe </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> time </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> menu </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Chicken Pasta Primavera </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Corn Muffin </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Cuban Black Bean and Pork Chili </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Chocolate Chip Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Double Chocolate Chip Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> M&amp;M Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Snickerdoodle Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Eggplant Lentil Chili </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Fruit Bowl </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Smokey BBQ Tofu Sandwich </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Steamed Corn </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Sweet Potato Fries </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Wild and Jasmine Rice Blend </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Beef Hot Dog </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Breaded Chicken Patty </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Burger Bar </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> French Fries </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Grilled Cheese </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Baked Fresh Fish </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Breadsmith's Bakery Peasant Roll </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Honey Butter </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Orange Mustard Glaze </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Roasted Red Potatoes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Sauteed Capri Vegetables </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Brown Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Guinness Lamb Stew </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Steamed Baby Carrots with Parsley </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Basmati Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Crab Rangoons </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Lo Mein </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Peking Pork </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Singapore Broth </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Tofu </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Cheese Pizza </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Chicken Enchilada Soup </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Pepperoni Pizza </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Chocolate Chip Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Double Chocolate Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Snickerdoodle Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Fire Roasted Tomato Salsa </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Roasted Sweet Corn Soup </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Tortilla Chips </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Vegan Nacho Casserole </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Wild and Jasmine Rice Blend </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Basil Roasted Chicken </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Breadsmith's Bakery Peasant Roll </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Confetti Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Honey Butter </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Roasted Lemon Thyme Carrots </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Pork Pozole </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Quinoa </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Sweet and Spicy Brussels Sprouts </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Beef Shiitake Broth </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Beef Sirloin Roast </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Fried Pork Wontons </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Jasmine Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Lo Mein </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Sweet Ginger Sesame Sauce </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-06 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Tofu </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Beef, Bean and Bacon Soup </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Cheese Pizza </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Pepperoni Pizza </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Chocolate Chip Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Double Chocolate Chip Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> M&amp;M Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Steamed Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Sweet Corn Macaroni and Cheese </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Sweet Potato Fries </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Vegan Chicken Tenders </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Vegetable Minestrone Soup </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Wild Ruby Rice Blend </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Breadsmith's Bakery Peasant Roll </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Honey Butter </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Jade Rice Blend </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Pork Loin with Mushroom Sauce </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Vegetable of the Day </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Bourbon Chicken </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Steamed Broccoli Florets </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Vegetable Fried Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Basmati Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Ginger and Garlic Broth </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Lo Mein </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Pork Roast </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Saigon Sizzle Sauce </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Tofu </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-07 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Vegetable Egg Rolls </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Black Bean Chili with Sausage </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Cheese Stuffed Breadsticks </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Shrimp and Crab Penne Alfredo </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Chocolate Chip Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Double Chocolate Chip Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Oatmeal Raisin Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Basmati Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Garlic Naan Bread </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Lentil and Garbanzo Curry </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Steamed Snap Peas and Carrots </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Vegan White Bean Chick'n Chili </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Wild Ruby Rice Blend </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Beef Hot Dog </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Breaded Chicken Patty </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Burger Bar </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> French Fries </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Grilled Cheese </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Breadsmith's Bakery Peasant Roll </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Honey Butter </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Jerk Pork Loin </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Roasted Sweet Potatoes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Steamed Cauliflower and Carrots </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Caviar Medley Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Chili Lime Glaze </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Roasted Zucchini and Summer Squash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Spicy Soy Marinated Chicken Thighs </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Beef Sirloin Roast </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Japanese Beef Curry Broth </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Jasmine Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Lo Mein </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Pork Dumplings </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Spicy Szechuan Sauce </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-08 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Tofu </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Cheese Stuffed Breadsticks </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Cheese Tortellini w/ Red Pepper Marinara </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Chicken Cordon Bleu Soup </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Chocolate Chip Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Double Chocolate Chip Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Oatmeal Raisin Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Blackened Tofu </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Cauliflower Salad </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Fettuccine Pasta </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Garlic and Herb Spaghetti Sauce </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Mushroom Barley Soup </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Roasted Asparagus </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Sundried Tomato Alfredo Sauce </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Wild Ruby Rice Blend </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Beef Hot Dog </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Breaded Chicken Patty </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Burger Bar </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> French Fries </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Grilled Cheese </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Breadsmith's Bakery Peasant Roll </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Buttered Peas and Carrots </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Cherry Chicken </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Cherry Pecan Wild Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Honey Butter </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Lime Cilantro Yogurt Sauce </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Steak Fajita Salad </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Basmati Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Boneless Skinless Chicken Thighs </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Lo Mein </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Mushroom Broth </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Thai Peanut Sauce </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Thai Vegetable Potstickers </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-09 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Tofu </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Breadsticks </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Buffalo Chicken Pasta </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Lentil and Chorizo Soup </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Chocolate Chip Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Double Chocolate Chip Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Sugar Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> French Fries </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Roasted Red Pepper Aioli </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Steamed Mixed Vegetables </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Tomato Basil Soup </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Vegan Chick'n Sandwich </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Wild Ruby Rice Blend </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Beef Hot Dog </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Breaded Chicken Patty </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Burger Bar </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> French Fries </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Grilled Cheese </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Breadsmith's Bakery Peasant Roll </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Chicken Pasta Primavera </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Honey Butter </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Roasted Zucchini and Summer Squash </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Cilantro Lime Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Coleslaw </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Roasted Fajita Vegetables </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Tortilla Encrusted Fresh Fish </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Crab Rangoons </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Jasmine Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Lo Mein </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Miso Broth </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Pork Roast </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Sweet Onion Teriyaki Sauce </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-10 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Tofu </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Brick Oven Chicken Pot Pie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Spicy Steak Chili </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Chocolate Chip Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Double Chocolate Chip Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Sugar Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Apple Snicker Bar Salad </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Breadsticks </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Cheese Tortellini with Alfredo </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Italian Vegetable Saute </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Onion Soup </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Roasted Red Pepper Marinara </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Spaghetti Noodles </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Wild Ruby Rice Blend </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Beef Hot Dog </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Breaded Chicken Patty </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Burger Bar </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> French Fries </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Grilled Cheese </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Beef and Noodles </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Breadsmith's Bakery Peasant Roll </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Honey Butter </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Steamed Broccoli and Cauliflower </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Garlic Buttered Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Roasted Ginger Curry Carrots </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Tandoori Chicken </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Asian Pineapple Sauce </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Basmati Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Crab Meat </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Lo Mein </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Pork Wonton </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Seafood and Bok Choy Broth </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Shrimp </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-11 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Tofu </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Garlic Breadsticks </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Macaroni and Cheese </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Pepper Jack Crab Soup </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Chocolate Chip Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Double Chocolate Chip Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> M&amp;M Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Anaheim Chili Soup </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Apple Baked Beans </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Broccoli Pasta Salad </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Corn Fritters </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Vegan BBQ Beef Tips </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Wild Ruby Rice Blend </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Beef Hot Dog </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Breaded Chicken Patty </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Burger Bar </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> French Fries </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Grilled Cheese </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Breadsmith's Bakery Peasant Roll </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Fresh Fish with Garlic Herb Butter </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Honey Butter </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Roasted Root Vegetables </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Steamed Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Sauteed Bok Choy </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Sesame Brown Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Sesame Chili Crusted Chicken </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Stir Fry Sauce </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Bangkok Pork Broth </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Duck Potstickers </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Jasmine Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Lo Mein </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Orange Citrus Sauce </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Pork Roast </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-12 </td>
   <td style="text-align:left;"> The Vista at Shaw </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Tofu </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Chocolate Chip Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Chocolate Peanut Butter Marble Cake </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Iced Brownie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Snickerdoodle Cookie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Vegan Chocolate Cake </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Battered Haddock </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Beyond Meat Vegan Burger </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Breaded Chicken Patty </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Burger </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Cheeseburger </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Chef's Choice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Chipotle Mayonnaise </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Dill Pickle Chips </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> French Fries </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Grilled Cheese </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Grilled Chicken Breast </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Iceberg Lettuce </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Lemons </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Mayonnaise </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Mustard </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Red Onions </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Sweet Baby Ray's BBQ Sauce </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Tartar Sauce </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Tomatoes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Bacon </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Battered Haddock </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Beanless Chili </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Cheddar Cheese </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Cheese Sauce </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> French Fries </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Mushrooms </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Pepper Jack Cheese </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Philly Beef Sandwich </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Provolone Cheese </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Sauteed Bell Peppers and Onions </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Swiss Cheese </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Caramel Apple Pizza </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Cheese Pizza </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Pepperoni Pizza </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Farro </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Fattoush Salad </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Jasmine Rice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Steamed Peas </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Teriyaki Tofu and Vegetable Yakitori </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Vegan French Onion Soup </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Woody's Garlic Scallion Hummus </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Woody's Original Hummus </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Woody's Tabbouleh Salad </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Woody's Thin Pita Bread </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-10-05 </td>
   <td style="text-align:left;"> The Gallery at Snyder Phillips </td>
   <td style="text-align:left;"> Lunch </td>
   <td style="text-align:left;"> Corn O'Brien </td>
  </tr>
</tbody>
</table></div>

I made it on Monday this week and to my surprise I saw that they would be serving it today for dinner in Landon! 

```r
food %>% 
  filter(str_detect(menu, "Presidential"))
```

```
## # A tibble: 1 x 4
##   date       cafe                       time   menu                
##   <date>     <chr>                      <chr>  <chr>               
## 1 2018-10-05 Heritage Commons at Landon Dinner Presidential Chicken
```


We can also filter to look at the menu for today only.

```r
food %>% 
  filter(date == today())
```

```
## # A tibble: 0 x 4
## # … with 4 variables: date <date>, cafe <chr>, time <chr>, menu <chr>
```

# Wordcloud

Since we have a tidy data frame of all the dining halls we can use the tidytext package from Julia Silge and David Robinson to tokenize the menu and make a word cloud.  First I used the palette from MSU's website and set it as an object `pal`.  Next we will unnest the tokens from the `food` data frame and count up the most frequent words.  We can remove filler words with an `anti_join()` command and then tell R to create a wordcloud!

```r
library(tidytext)
library(wordcloud)
library(RColorBrewer)

# MSU palette colors  
pal <- c("#97A2A2","#F08521","#6E005F","#F08521","#97A2A2","#0DB14B")

set.seed(1234)

food %>% 
  unnest_tokens(menu, menu) %>% 
  count(menu, sort = TRUE) %>% 
  rename(word = menu) %>% 
  anti_join(stop_words, by = "word") %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 40, color = pal,fixed.asp = TRUE))
```

<img src="/post/2019-12-13-presidential-chicken_files/figure-html/unnamed-chunk-11-1.png" width="672" />


# Shiny Dashboard
Now that we learned how to make a wordcloud, scrape the menu into a tidy data frame we can work on making a interact with the data by making a shiny dashboard.


```r
library(shiny)
library(shinydashboard)

ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Eat at State"),
  dashboardSidebar( selectInput(inputId = 'dininghall',
                  label = 'Choose Dining Hall:',
                  choices = c("All","The Vista at Shaw ",
                              "The Gallery at Snyder Phillips ",
                              "Heritage Commons at Landon "))
  ),
  
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(dataTableOutput("menu"),title = "MSU Dining Menu",
          width = 12, status = "primary")
      )
    )
  )

server <- function(input, output) {
  
   menu <- reactive({
    if(input$dininghall == "All")
        return(food)
    else
      food %>% 
       filter(cafe %in% input$dininghall)
  })

  output$menu <- renderDataTable(menu())
}

shinyApp(ui, server)
```

![Eat at State Shiny Dashboard](https://media.giphy.com/media/1evFtMR37i8VXzMwtJ/giphy.gif)


# Presidential Chicken!

![Landon](https://i.imgur.com/14UQkFE.jpg)
I had the idea to scrape for presidential chicken on Monday this week and I was thrilled that Landon had it for dinner on Friday.  Now that I was able to go I decided to create this blog post to share with everyone!
