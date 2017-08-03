# Fun data: open data that is fun to analyse



Jeremy Singer-Vine sends out a [newsletter](https://tinyletter.com/data-is-plural) every week where he highlights a number of interesting open datasets (you can explore all the datasets [here](https://docs.google.com/spreadsheets/d/1wZhPLMCHKJvwOkP4juclhjFgqIY8fQFMemwKL2c64vk/edit#gid=0)). At Mango we are all for open data so we thought we would also share some of the open datasets we think are fun to explore.  

### Open Food Facts
[Open Food Facts](https://world.openfoodfacts.org/) is a collaborative, free and open database of food products. It is a prime example of how effective crowdsourcing your data is. People from around the world have collected details about more than 300.000 food products and uploaded the information through mobile apps. The data is available as a MongoDB dump, CSV export and an experimental API. We have downloaded the CSV export and wil try to visualise the ingredients across all products.


```r
# http://world.openfoodfacts.org/data/en.openfoodfacts.org.products.csv
foodFacts <- read_tsv("data/en.openfoodfacts.org.products.csv")
dim(foodFacts)
## [1] 295958    162
```

```r
library(stringr)
ingredients <- foodFacts %>%
  # ideally, the unnest_tokens function is what we want but it was too slow
  #tidytext::unnest_tokens(ingredient, ingredients_text,
  #                        token = stringr::str_split, pattern=",|\\(|\\)|\\[|\\]") %>%
  # so instead we chose a different approach involving transmute and unlist
  # transmute will give us a list-column
  transmute(ingredients = str_split(ingredients_text, pattern=",|\\(|\\)|\\[|\\]")) %>%
  # unlist will convert the list-column into a character vector
  unlist() %>%
  # enframe will convert the vector into a data frame which is easier to groupby
  enframe() %>%
  # now we clean up some of the text
  mutate(value = str_replace(value, "org|organic", ""),
         value = str_replace(value, "-> en:\\w+", ""),
         value = str_replace_all(value, "'", ""),
         value = str_trim(value)) %>%
  # and finally filter some of the weirder entries
  filter(value!="", value!=".",
         !str_detect(value, "completed|\\d")) %>%
  # to then group by and count the ingredients
  count(value) %>%
  arrange(desc(n))
head(ingredients, 10)
```

```
## # A tibble: 10 x 2
##          value      n
##          <chr>  <int>
##  1        salt 122183
##  2       sugar  88463
##  3       water  80037
##  4         sel  38852
##  5       sucre  29971
##  6         eau  28502
##  7 citric acid  28475
##  8  riboflavin  21527
##  9        milk  21265
## 10      niacin  21201
```

There are no surprises at the top but further down there are a few ingredients that are odd. Let's create a word cloud to show the relative frequencies.


```r
library(wordcloud)
top100 <- head(ingredients, 100)
wordcloud::wordcloud(top100$value, top100$n)
```

![](fun_data_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Ingredients are only one aspect of this very interesting dataset. We could go on and look at the co-occurence of certain ingredients (using network analysis) and then continue analysing their quantities. We could also include the data on nutritional value and calculate correlations. The data could also use some more cleaning considering there some ingredients in different languages (e.g. water and eau).

### North Korea Missile Attacks
The next dataset is from the James Martin Center for Nonproliferation Studies (CNS) North Korea Missile Test Database. We agree it's not a fun topic but we also wanted to show the breadth of open data that is out there. You can get the data as an [Excel file](https://www.nti.org/documents/2137/north_korea_missile_test_database.xlsx) but it is also hosted on [data.world](https://data.world/ian/the-cns-north-korea-missile-test-database). And fortunately for the R community data.world have a R package to access their API.   



```r
library(data.world)

path <- "ian/the-cns-north-korea-missile-test-database"
missiles <- query(qry_sql("SELECT f1, facility_latitude, facility_longitude, 
                          distance_travelled_km, facility_name 
                          FROM missile_tests 
                          WHERE success='Success'"), 
                  path)
# additional filtering outside of query
missiles <- missiles %>% 
  filter(distance_travelled_km!="Unknown", distance_travelled_km!="N/A") %>% 
  drop_na() %>% 
  mutate(distance_travelled_km = as.integer(distance_travelled_km),
         facility_name = substr(facility_name, 0, 20))
head(missiles)
```

```
## # A tibble: 6 x 5
##      f1 facility_latitude facility_longitude distance_travelled_km
##   <int>             <dbl>              <dbl>                 <int>
## 1   101          39.65960           124.7057                  1000
## 2   102          39.65960           124.7057                  1000
## 3    12          40.85000           129.6667                   500
## 4    40          38.99083           127.6236                   200
## 5    41          38.99083           127.6236                   200
## 6    42          38.99083           127.6236                   200
## # ... with 1 more variables: facility_name <chr>
```
The data contains information on the launch location but not on the precise location of where the missile landed. However we can use the distance travelled to approximate this by calculating a radius.


```r
# slightly adapted from https://stackoverflow.com/questions/34183049/plot-circle-with-a-certain-radius-around-point-on-a-map-in-ggplot2#34187454

# drop duplicate locations and distances
dups <- duplicated(missiles %>% select(facility_name, distance_travelled_km))
missiles <- missiles %>% filter(!dups)

# define the circle we want for each missile
circles <- data_frame(f1 = rep(missiles$f1, each = 100),
                      angle = rep(seq(0, 2*pi, length.out = 100), nrow(missiles)))

meanLatitude <- mean(missiles$facility_latitude)

missile_circles <- missiles %>% 
  # length per longitude changes with latitude, so need correction
  mutate(radiusLon = distance_travelled_km/111/cos(meanLatitude/57.3),
         radiusLat = distance_travelled_km/111) %>% 
  left_join(circles) %>% 
  mutate(longitude = facility_longitude + radiusLon * cos(angle),
         latitude = facility_latitude + radiusLat * sin(angle))
```

So now we have our circles we can plot them on a map.


```r
library(ggmap)
nk = get_map(location = c(lon = mean(missile_circles$facility_longitude), 
                          lat = mean(missile_circles$facility_latitude)), 
             zoom = 5, maptype = "terrain")
ggmap(nk, extent = "panel") + 
  geom_point(aes(x = facility_longitude, y = facility_latitude, colour=facility_name), 
             data = missiles) +
  ########### add circles
  geom_polygon(data = missile_circles, aes(longitude, latitude, group = f1, 
                                           colour=facility_name), alpha = 0)
```

![](fun_data_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

These are obviously not missile ranges and none of the missiles will have gone over China. The CNS have also created [visualisations](http://www.nti.org/analysis/articles/cns-north-korea-missile-test-database/) with this data and from that we can see that the time dimension is important. That is something we could add to our visualisation or we could perform more analyses on the success/fail dimension.   

