Lab 05 - Data Wrangling
================

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/JSC370-2024/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages),
    `mgcv`, `ggplot2`, `leaflet`, `kableExtra`.

``` r
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.9-0. For overview type 'help("mgcv-package")'.

``` r
library(ggplot2)
library(leaflet)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
fn <- "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz"
if (!file.exists("met_all_2023.gz"))
  download.file(fn, destfile = "met_all_2023.gz")
met <- data.table::fread("met_all_2023.gz")
```

2.  Load the met data from
    <https://github.com/JSC370/JSC370-2024/main/data/met_all_2023.gz> or
    (Use
    <https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE, LAT, LON)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]

# Read in the met data and fix lat, lon, temp
met$lat <- met$lat/1000
met$lon <- met$lon/1000
met$wind.sp <- met$wind.sp/10
met$temp <- met$temp/10
met$dew.point <- met$dew.point/10
met$atm.press <- met$atm.press/10
```

3.  Merge the data as we did during the lecture. Use the `merge()` code
    and you can also try the tidy way with `left_join()`

``` r
mer <- merge(met, stations, 
             by.x = "USAFID", by.y="USAF",
             all.x = TRUE, all.y = FALSE)
```

## Question 1: Identifying Representative Stations

Across all weather stations, which stations have the median values of
temperature, wind speed, and atmospheric pressure? Using the
`quantile()` function, identify these three stations. Do they coincide?

``` r
# Calculate the median values for temperature, wind speed, and atmospheric pressure
national_median_temp <- quantile(mer$temp, probs = 0.5, na.rm = TRUE)
national_median_wind_speed <- quantile(mer$wind.sp, probs = 0.5, na.rm = TRUE)
national_median_atm_pressure <- quantile(mer$atm.press, probs = 0.5, na.rm = TRUE)
```

Next identify the stations have these median values.

``` r
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mer_by_stations <- mer %>% 
  # filter(!is.na(temp) & !is.na(wind.sp) & !is.na(atm.press)) %>%
  group_by(USAFID) %>% 
  summarise(
    med_temp = median(temp, na.rm = TRUE),
    med_wind_speed = median(wind.sp,na.rm = TRUE),
    med_atm_press = median(atm.press,na.rm = TRUE),
    state = get_mode(STATE),
    lat = median(lat),
    lon = median(lon),
    elev = median(elev)
            )
nrow(mer_by_stations)
```

    ## [1] 1852

``` r
# Find stations where these median values occur
stations_median_temp <- unique(mer_by_stations$USAFID[which(mer_by_stations$med_temp == national_median_temp)])
stations_median_wind_speed <- unique(mer_by_stations$USAFID[which(mer_by_stations$med_wind_speed == national_median_wind_speed)])
stations_median_atm_pressure <- unique(mer_by_stations$USAFID[which(mer_by_stations$med_atm_press == national_median_atm_pressure)])
# Output the stations with median values
cat("Stations with median temperature:", stations_median_temp, "\n")
```

    ## Stations with median temperature: 720263 720312 720327 722076 722180 722196 722197 723075 723086 723110 723119 723190 723194 723200 723658 723895 724010 724345 724356 724365 724373 724380 724397 724454 724517 724585 724815 724838 725116 725317 725326 725340 725450 725472 725473 725480 725499 725513 725720 726515 726525 726546 726556 726560 727570 727845 727900 745046 746410 747808

``` r
cat("Stations with median wind speed:", stations_median_wind_speed, "\n")
```

    ## Stations with median wind speed: 720110 720113 720258 720261 720266 720267 720268 720272 720283 720284 720293 720296 720303 720304 720306 720307 720309 720314 720323 720330 720333 720344 720351 720358 720367 720371 720373 720374 720377 720379 720384 720395 720405 720406 720412 720414 720415 720426 720447 720481 720528 720531 720542 720543 720544 720575 720578 720581 720586 720589 720596 720601 720602 720607 720611 720612 720613 720627 720633 720638 720639 720643 720647 720651 720652 720655 720713 720734 720771 720839 720903 720904 720927 720928 720929 720932 720942 720944 720961 721031 721042 721044 721048 722003 722006 722011 722020 722026 722029 722032 722033 722038 722039 722041 722055 722059 722067 722081 722082 722085 722089 722090 722093 722094 722097 722098 722099 722103 722108 722114 722120 722123 722124 722125 722129 722130 722138 722140 722149 722151 722156 722157 722160 722164 722165 722168 722170 722172 722180 722182 722185 722198 722202 722213 722221 722230 722235 722239 722244 722248 722249 722252 722256 722261 722268 722269 722270 722279 722280 722291 722310 722314 722316 722320 722323 722329 722330 722331 722343 722346 722350 722354 722362 722390 722404 722405 722410 722427 722429 722430 722444 722447 722470 722480 722485 722489 722542 722552 722587 722588 722600 722728 722740 722749 722780 722784 722787 722823 722897 722899 722903 722928 722934 722956 722972 723030 723034 723035 723060 723066 723068 723069 723074 723087 723095 723100 723106 723108 723109 723115 723119 723120 723122 723140 723146 723170 723190 723194 723230 723240 723260 723270 723273 723280 723290 723300 723306 723340 723346 723403 723406 723407 723416 723434 723441 723444 723449 723495 723536 723537 723560 723626 723629 723710 723762 723840 723895 723896 723910 723940 723990 724007 724030 724036 724043 724055 724056 724058 724060 724066 724075 724095 724100 724107 724110 724116 724175 724177 724230 724233 724235 724237 724238 724240 724284 724288 724297 724320 724330 724336 724338 724345 724347 724365 724373 724375 724384 724395 724400 724420 724430 724450 724454 724455 724460 724462 724463 724468 724475 724490 724502 724504 724507 724519 724520 724550 724555 724556 724560 724565 724625 724627 724666 724673 724674 724675 724676 724677 724699 724700 724768 724769 724770 724800 724810 724815 724837 724880 724885 724915 724973 724975 725025 725027 725029 725037 725045 725046 725059 725064 725065 725069 725080 725085 725087 725088 725103 725116 725117 725118 725124 725125 725126 725127 725140 725145 725157 725170 725172 725175 725180 725190 725196 725207 725229 725235 725247 725256 725266 725267 725292 725314 725326 725345 725354 725370 725373 725374 725375 725383 725384 725387 725394 725395 725405 725406 725408 725409 725414 725416 725417 725418 725420 725430 725440 725453 725454 725456 725462 725463 725464 725465 725466 725467 725469 725479 725480 725487 725488 725490 725493 725494 725497 725498 725513 725514 725515 725533 725540 725541 725556 725564 725565 725566 725570 725700 725705 725717 725724 725750 725760 725776 725825 725867 725920 725976 726055 726060 726064 726077 726079 726170 726190 726223 726225 726355 726357 726358 726360 726364 726380 726384 726385 726387 726391 726395 726396 726404 726405 726409 726410 726413 726415 726416 726419 726426 726430 726435 726444 726452 726455 726457 726463 726466 726467 726480 726487 726502 726503 726509 726514 726530 726550 726555 726561 726562 726563 726567 726568 726569 726574 726577 726584 726589 726596 726626 726660 726665 726667 726700 726720 726764 726770 726776 726810 726830 726836 726875 726880 726883 726886 726904 726940 726980 726986 727033 727120 727347 727437 727444 727453 727455 727456 727469 727470 727476 727505 727507 727508 727515 727517 727550 727566 727684 727686 727700 727720 727755 727790 727810 727834 727845 727850 727855 727857 727920 727924 727930 740035 742060 742071 742513 743700 744104 744662 744666 744672 744865 744989 744994 745048 745057 745430 746710 746716 746925 746930 746940 747040 747043 747540 747570 747680 747760 747809 747900 747918

``` r
cat("Stations with median atmospheric pressure:", stations_median_atm_pressure, "\n")
```

    ## Stations with median atmospheric pressure: 720394 722085 722348 723119 723124 723270 723658 724010 724035 724100 724235 724280 724336 724926 725126 725266 725510 725570 725620 725845 726690 726810

``` r
common_stations <- Reduce(intersect, list(stations_median_temp, stations_median_wind_speed, stations_median_atm_pressure))
cat("Stations with national median temperature, wind speed, and atmospheric pressure:", common_stations)
```

    ## Stations with national median temperature, wind speed, and atmospheric pressure: 723119

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Identifying Representative Stations per State

Now let’s find the weather stations by state with closest temperature
and wind speed based on the euclidean distance from these medians.

``` r
temp_windsp_dist <- function(station_temp, state_temp, station_wsp, state_wsp) {
  sqrt((state_temp - station_temp)^2+(state_wsp - station_wsp)^2)
}

mer_by_states <- mer %>% 
  group_by(STATE) %>% 
  summarise(
    state_med_temp = median(temp, na.rm = TRUE),
    state_med_wind_speed = median(wind.sp,na.rm = TRUE),
    state_med_atm_press = median(atm.press,na.rm = TRUE),
    state_med_lat = median(lat),
    state_med_lon = median(lon)
  )
mer_by_states
```

    ## # A tibble: 48 × 6
    ##    STATE state_med_temp state_med_wind_speed state_med_atm_press state_med_lat
    ##    <chr>          <dbl>                <dbl>               <dbl>         <dbl>
    ##  1 AL              23.3                  2.6               1011           32.9
    ##  2 AR              24                    2.6               1011.          35.2
    ##  3 AZ              25.6                  3.6               1009.          33.6
    ##  4 CA              17                    3.6               1013           36.8
    ##  5 CO              15                    3.6               1012.          39.1
    ##  6 CT              19.4                  3.1               1010.          41.4
    ##  7 DE              21.3                  3.6               1010.          39.1
    ##  8 FL              26.7                  3.6               1013.          28.5
    ##  9 GA              23                    2.6               1011.          32.6
    ## 10 IA              22                    3.1               1012.          41.7
    ## # ℹ 38 more rows
    ## # ℹ 1 more variable: state_med_lon <dbl>

``` r
mere_by_stations <- mer_by_stations %>%
  merge(mer_by_states) %>% 
  mutate(dist = temp_windsp_dist(state_temp = state_med_temp,
                                 station_temp = med_temp,
                                 state_wsp = state_med_wind_speed,
                                 station_wsp = med_wind_speed))
  


# Group by state and find closest station
closest_stations <- mere_by_stations %>%
  group_by(state) %>%
  top_n(-1, dist) %>%
  ungroup()

# Select only necessary columns
closest_stations <- closest_stations %>%
  select(USAFID, med_temp, med_wind_speed, state, lat, lon, elev, dist, ) %>% 
  unique()

closest_stations
```

    ## # A tibble: 388 × 8
    ##    USAFID med_temp med_wind_speed state   lat   lon  elev  dist
    ##     <int>    <dbl>          <dbl> <chr> <dbl> <dbl> <dbl> <dbl>
    ##  1 720708     23.3            2.6 MS     32.8 -88.8   165     0
    ##  2 722054     23.3            2.6 AR     35.1 -90.2    65     0
    ##  3 722073     23.3            2.6 NC     35.0 -78.4    45     0
    ##  4 723307     23.3            2.6 MS     33.4 -88.6    80     0
    ##  5 723347     23.3            2.6 TN     36   -89.4   103     0
    ##  6 724350     23.3            2.6 KY     37.1 -88.8   126     0
    ##  7 720348     24              2.6 GA     33.2 -83.2   117     0
    ##  8 720632     24              2.6 SC     33.1 -80.3    17     0
    ##  9 720636     24              2.6 MO     38.4 -93.7   251     0
    ## 10 720738     24              2.6 GA     30.9 -83.9    80     0
    ## # ℹ 378 more rows

Knit the doc and save it on GitHub.

## Question 3: In the Geographic Center?

For each state, identify which station is closest to the geographic
mid-point (median) of the state. Combining these with the stations you
identified in the previous question, use `leaflet()` to visualize all
~100 points in the same figure, applying different colors for the
geographic median and the temperature and wind speed median.

``` r
stations_median_temp_df <- data.frame(USAFID = stations_median_temp, Median_Temperature = national_median_temp)
```

    ## Warning in data.frame(USAFID = stations_median_temp, Median_Temperature =
    ## national_median_temp): row names were found from a short variable and have been
    ## discarded

``` r
stations_median_wind_speed_df <- data.frame(USAFID = stations_median_wind_speed, Median_Wind_Speed = national_median_wind_speed)
```

    ## Warning in data.frame(USAFID = stations_median_wind_speed, Median_Wind_Speed =
    ## national_median_wind_speed): row names were found from a short variable and
    ## have been discarded

``` r
stations_median_atm_pressure_df <- data.frame(USAFID = stations_median_atm_pressure, Median_Atmospheric_Pressure = national_median_atm_pressure)
```

    ## Warning in data.frame(USAFID = stations_median_atm_pressure,
    ## Median_Atmospheric_Pressure = national_median_atm_pressure): row names were
    ## found from a short variable and have been discarded

``` r
# TODO: fix question 3
calculate_distance <- function(lat1, lon1, lat2, lon2) {
  distance <- sqrt((lat2 - lat1)^2 + (lon2 - lon1)^2)
  return(distance)
}

mere_by_stations <- mere_by_stations %>%
  mutate(geo_dist = calculate_distance(lat, lon, state_med_lat, state_med_lon))

# Find the closest station to the geographic midpoint of each state
geo_closest_stations_by_state <- mere_by_stations %>%
  group_by(state) %>%
  arrange(geo_dist) %>%
  slice(1)

# Combine the stations identified in the previous question with the closest stations by state
all_stations <- bind_rows(stations_median_temp_df, stations_median_wind_speed_df, geo_closest_stations_by_state)

# Convert latitude and longitude to numeric
all_stations$lat <- as.numeric(all_stations$lat)
all_stations$lon <- as.numeric(all_stations$lon)

# Define colors for different types of stations
colors <- c("Median Temperature" = "red", "Median Wind Speed" = "blue", "Geographic Median of State" = "purple")

# Create the leaflet map
leaflet(all_stations) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon,
    lat = ~lat,
    color = ~ifelse(USAFID %in% unique(geo_closest_stations_by_state$USAFID), 
                    colors["Geographic Median of State"], 
                    ifelse(med_temp == national_median_temp, 
                           colors["Median Temperature"], 
                           colors["Median Wind Speed"])),
    radius = 5,
    popup = ~paste("USAFID: ", USAFID, "<br>",
                   "Median Temperature: ", med_temp, "<br>",
                   "Median Wind Speed: ", med_wind_speed, "<br>",
                   "State: ", state, "<br>",
                   "Latitude: ", lat, "<br>",
                   "Longitude: ", lon, "<br>",
                   "Elevation: ", elev, "<br>",
                   "Distance to Median: ", dist, "<br>",
                   "Geo distance: ", geo_dist, "<br>")
  )
```

    ## Warning in validateCoords(lng, lat, funcName): Data contains 627 rows with
    ## either missing or invalid lat/lon values and will be ignored

<div class="leaflet html-widget html-fill-item" id="htmlwidget-f0b343b364a4821be78c" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-f0b343b364a4821be78c">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"https://openstreetmap.org/copyright/\">OpenStreetMap<\/a>,  <a href=\"https://opendatacommons.org/licenses/odbl/\">ODbL<\/a>"}]},{"method":"addCircleMarkers","args":[[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,33.178,35.257,33.466,36.985,39.05,41.384,39.133,28.821,32.633,41.691,43.567,40.483,41.066,38.068,37.578,30.558,42.212,39.173,44.533,43.322,45.544,38.947,32.32,47.517,35.582,48.39,40.893,43.205,40.624,33.45,39.601,40.851,40.28,35.357,44.5,40.218,41.597,33.967,43.767,35.38,31.133,40.219,37.4,44.533,47.445,44.783,39,43.062],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,-86.782,-93.095,-111.721,-120.11,-105.516,-72.506,-75.467,-81.81,-83.59999999999999,-93.566,-116.24,-88.95,-86.182,-97.861,-84.77,-92.099,-71.114,-76.684,-69.667,-84.688,-94.05200000000001,-92.68300000000001,-90.078,-111.183,-79.101,-100.024,-97.997,-71.503,-74.669,-105.516,-116.005,-72.619,-83.11499999999999,-96.943,-123.283,-76.855,-71.41200000000001,-80.467,-99.318,-86.246,-97.717,-111.723,-77.517,-72.61499999999999,-122.314,-89.667,-80.274,-108.447],5,null,null,{"interactive":true,"className":"","stroke":true,"color":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple","purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,"purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple"],"weight":5,"opacity":0.5,"fill":true,"fillColor":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple","purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,"purple",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,"purple",null,"purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple","purple"],"fillOpacity":0.2},null,null,["USAFID:  720263 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720312 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720327 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722076 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722180 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722196 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722197 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723075 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723086 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723110 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723119 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723190 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723194 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723200 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723658 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723895 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724010 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724345 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724356 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724365 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724373 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724380 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724397 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724454 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724517 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724585 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724815 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724838 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725116 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725317 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725326 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725340 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725450 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725472 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725473 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725480 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725499 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725513 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725720 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726515 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726525 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726546 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726556 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726560 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727570 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727845 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727900 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  745046 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  746410 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  747808 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720110 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720113 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720258 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720261 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720266 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720267 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720268 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720272 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720283 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720284 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720293 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720296 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720303 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720304 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720306 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720307 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720309 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720314 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720323 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720330 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720333 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720344 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720351 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720358 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720367 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720371 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720373 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720374 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720377 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720379 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720384 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720395 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720405 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720406 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720412 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720414 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720415 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720426 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720447 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720481 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720528 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720531 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720542 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720543 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720544 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720575 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720578 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720581 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720586 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720589 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720596 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720601 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720602 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720607 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720611 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720612 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720613 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720627 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720633 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720638 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720639 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720643 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720647 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720651 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720652 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720655 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720713 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720734 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720771 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720839 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720903 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720904 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720927 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720928 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720929 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720932 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720942 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720944 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  720961 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  721031 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  721042 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  721044 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  721048 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722003 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722006 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722011 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722020 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722026 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722029 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722032 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722033 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722038 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722039 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722041 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722055 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722059 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722067 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722081 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722082 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722085 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722089 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722090 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722093 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722094 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722097 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722098 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722099 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722103 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722108 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722114 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722120 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722123 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722124 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722125 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722129 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722130 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722138 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722140 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722149 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722151 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722156 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722157 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722160 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722164 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722165 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722168 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722170 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722172 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722180 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722182 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722185 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722198 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722202 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722213 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722221 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722230 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722235 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722239 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722244 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722248 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722249 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722252 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722256 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722261 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722268 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722269 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722270 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722279 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722280 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722291 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722310 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722314 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722316 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722320 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722323 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722329 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722330 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722331 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722343 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722346 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722350 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722354 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722362 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722390 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722404 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722405 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722410 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722427 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722429 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722430 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722444 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722447 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722470 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722480 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722485 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722489 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722542 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722552 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722587 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722588 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722600 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722728 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722740 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722749 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722780 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722784 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722787 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722823 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722897 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722899 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722903 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722928 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722934 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722956 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722972 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723030 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723034 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723035 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723060 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723066 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723068 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723069 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723074 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723087 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723095 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723100 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723106 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723108 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723109 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723115 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723119 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723120 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723122 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723140 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723146 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723170 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723190 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723194 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723230 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723240 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723260 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723270 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723273 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723280 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723290 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723300 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723306 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723340 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723346 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723403 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723406 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723407 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723416 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723434 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723441 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723444 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723449 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723495 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723536 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723537 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723560 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723626 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723629 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723710 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723762 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723840 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723895 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723896 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723910 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723940 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  723990 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724007 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724030 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724036 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724043 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724055 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724056 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724058 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724060 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724066 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724075 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724095 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724100 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724107 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724110 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724116 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724175 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724177 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724230 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724233 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724235 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724237 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724238 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724240 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724284 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724288 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724297 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724320 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724330 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724336 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724338 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724345 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724347 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724365 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724373 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724375 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724384 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724395 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724400 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724420 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724430 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724450 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724454 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724455 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724460 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724462 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724463 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724468 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724475 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724490 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724502 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724504 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724507 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724519 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724520 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724550 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724555 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724556 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724560 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724565 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724625 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724627 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724666 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724673 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724674 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724675 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724676 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724677 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724699 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724700 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724768 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724769 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724770 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724800 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724810 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724815 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724837 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724880 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724885 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724915 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724973 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  724975 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725025 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725027 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725029 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725037 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725045 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725046 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725059 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725064 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725065 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725069 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725080 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725085 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725087 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725088 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725103 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725116 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725117 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725118 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725124 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725125 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725126 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725127 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725140 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725145 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725157 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725170 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725172 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725175 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725180 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725190 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725196 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725207 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725229 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725235 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725247 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725256 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725266 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725267 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725292 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725314 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725326 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725345 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725354 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725370 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725373 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725374 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725375 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725383 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725384 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725387 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725394 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725395 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725405 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725406 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725408 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725409 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725414 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725416 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725417 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725418 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725420 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725430 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725440 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725453 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725454 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725456 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725462 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725463 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725464 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725465 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725466 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725467 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725469 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725479 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725480 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725487 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725488 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725490 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725493 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725494 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725497 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725498 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725513 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725514 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725515 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725533 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725540 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725541 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725556 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725564 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725565 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725566 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725570 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725700 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725705 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725717 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725724 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725750 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725760 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725776 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725825 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725867 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725920 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  725976 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726055 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726060 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726064 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726077 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726079 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726170 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726190 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726223 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726225 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726355 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726357 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726358 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726360 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726364 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726380 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726384 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726385 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726387 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726391 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726395 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726396 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726404 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726405 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726409 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726410 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726413 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726415 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726416 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726419 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726426 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726430 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726435 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726444 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726452 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726455 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726457 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726463 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726466 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726467 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726480 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726487 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726502 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726503 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726509 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726514 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726530 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726550 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726555 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726561 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726562 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726563 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726567 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726568 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726569 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726574 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726577 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726584 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726589 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726596 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726626 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726660 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726665 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726667 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726700 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726720 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726764 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726770 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726776 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726810 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726830 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726836 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726875 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726880 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726883 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726886 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726904 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726940 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726980 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  726986 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727033 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727120 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727347 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727437 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727444 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727453 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727455 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727456 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727469 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727470 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727476 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727505 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727507 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727508 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727515 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727517 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727550 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727566 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727684 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727686 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727700 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727720 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727755 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727790 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727810 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727834 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727845 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727850 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727855 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727857 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727920 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727924 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  727930 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  740035 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  742060 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  742071 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  742513 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  743700 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  744104 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  744662 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  744666 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  744672 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  744865 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  744989 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  744994 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  745048 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  745057 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  745430 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  746710 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  746716 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  746925 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  746930 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  746940 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  747040 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  747043 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  747540 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  747570 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  747680 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  747760 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  747809 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  747900 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  747918 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  NA <br> Latitude:  NA <br> Longitude:  NA <br> Elevation:  NA <br> Distance to Median:  NA <br> Geo distance:  NA <br>","USAFID:  722300 <br> Median Temperature:  22.8 <br> Median Wind Speed:  2.1 <br> State:  AL <br> Latitude:  33.178 <br> Longitude:  -86.782 <br> Elevation:  178 <br> Distance to Median:  0.707106781186548 <br> Geo distance:  0.346112698409054 <br>","USAFID:  723429 <br> Median Temperature:  23.3 <br> Median Wind Speed:  2.1 <br> State:  AR <br> Latitude:  35.257 <br> Longitude:  -93.095 <br> Elevation:  123 <br> Distance to Median:  0.860232526704262 <br> Geo distance:  0.331072499613002 <br>","USAFID:  722783 <br> Median Temperature:  30.6 <br> Median Wind Speed:  2.6 <br> State:  AZ <br> Latitude:  33.466 <br> Longitude:  -111.721 <br> Elevation:  425 <br> Distance to Median:  5.09901951359278 <br> Geo distance:  0.156016024817969 <br>","USAFID:  745046 <br> Median Temperature:  21.7 <br> Median Wind Speed:  3.6 <br> State:  CA <br> Latitude:  36.985 <br> Longitude:  -120.11 <br> Elevation:  77 <br> Distance to Median:  4.7 <br> Geo distance:  0.369153084776493 <br>","USAFID:  726396 <br> Median Temperature:  8 <br> Median Wind Speed:  3.1 <br> State:  CO <br> Latitude:  39.05 <br> Longitude:  -105.516 <br> Elevation:  3438 <br> Distance to Median:  7.0178344238091 <br> Geo distance:  0.310575272679595 <br>","USAFID:  720545 <br> Median Temperature:  19 <br> Median Wind Speed:  2.6 <br> State:  CT <br> Latitude:  41.384 <br> Longitude:  -72.506 <br> Elevation:  127 <br> Distance to Median:  0.640312423743284 <br> Geo distance:  0.176000000000002 <br>","USAFID:  724088 <br> Median Temperature:  21 <br> Median Wind Speed:  4.1 <br> State:  DE <br> Latitude:  39.133 <br> Longitude:  -75.467 <br> Elevation:  9 <br> Distance to Median:  0.58309518948453 <br> Geo distance:  0 <br>","USAFID:  722213 <br> Median Temperature:  25 <br> Median Wind Speed:  3.1 <br> State:  FL <br> Latitude:  28.821 <br> Longitude:  -81.81 <br> Elevation:  23 <br> Distance to Median:  1.77200451466693 <br> Geo distance:  0.353220894059229 <br>","USAFID:  722175 <br> Median Temperature:  22.8 <br> Median Wind Speed:  2.85 <br> State:  GA <br> Latitude:  32.633 <br> Longitude:  -83.6 <br> Elevation:  90 <br> Distance to Median:  0.320156211871642 <br> Geo distance:  0.11099999999999 <br>","USAFID:  725466 <br> Median Temperature:  22 <br> Median Wind Speed:  3.1 <br> State:  IA <br> Latitude:  41.691 <br> Longitude:  -93.566 <br> Elevation:  277 <br> Distance to Median:  0 <br> Geo distance:  0.00900000000000034 <br>","USAFID:  726810 <br> Median Temperature:  20 <br> Median Wind Speed:  3.1 <br> State:  ID <br> Latitude:  43.567 <br> Longitude:  -116.24 <br> Elevation:  874 <br> Distance to Median:  4 <br> Geo distance:  0.620239469882397 <br>","USAFID:  724397 <br> Median Temperature:  21.7 <br> Median Wind Speed:  4.1 <br> State:  IL <br> Latitude:  40.483 <br> Longitude:  -88.95 <br> Elevation:  265 <br> Distance to Median:  1.00498756211209 <br> Geo distance:  0.345962425705446 <br>","USAFID:  720736 <br> Median Temperature:  20 <br> Median Wind Speed:  4.1 <br> State:  IN <br> Latitude:  41.066 <br> Longitude:  -86.182 <br> Elevation:  241 <br> Distance to Median:  0.5 <br> Geo distance:  0.15467385040788 <br>","USAFID:  724506 <br> Median Temperature:  23.3 <br> Median Wind Speed:  3.6 <br> State:  KS <br> Latitude:  38.068 <br> Longitude:  -97.861 <br> Elevation:  470 <br> Distance to Median:  1.1 <br> Geo distance:  0.210000000000008 <br>","USAFID:  720448 <br> Median Temperature:  20 <br> Median Wind Speed:  3.6 <br> State:  KY <br> Latitude:  37.578 <br> Longitude:  -84.77 <br> Elevation:  312 <br> Distance to Median:  1.11803398874989 <br> Geo distance:  0.0129999999999981 <br>","USAFID:  720468 <br> Median Temperature:  27.2 <br> Median Wind Speed:  2.6 <br> State:  LA <br> Latitude:  30.558 <br> Longitude:  -92.099 <br> Elevation:  23 <br> Distance to Median:  0.5 <br> Geo distance:  0.0150000000000006 <br>","USAFID:  744907 <br> Median Temperature:  NA <br> Median Wind Speed:  NA <br> State:  MA <br> Latitude:  42.212 <br> Longitude:  -71.114 <br> Elevation:  194 <br> Distance to Median:  NA <br> Geo distance:  0.116275534829991 <br>","USAFID:  724060 <br> Median Temperature:  22.2 <br> Median Wind Speed:  3.1 <br> State:  MD <br> Latitude:  39.173 <br> Longitude:  -76.684 <br> Elevation:  47 <br> Distance to Median:  1.2 <br> Geo distance:  0.192 <br>","USAFID:  726073 <br> Median Temperature:  15.6 <br> Median Wind Speed:  2.6 <br> State:  ME <br> Latitude:  44.533 <br> Longitude:  -69.667 <br> Elevation:  101 <br> Distance to Median:  0.781024967590665 <br> Geo distance:  0.134 <br>","USAFID:  725405 <br> Median Temperature:  19.5 <br> Median Wind Speed:  3.1 <br> State:  MI <br> Latitude:  43.322 <br> Longitude:  -84.688 <br> Elevation:  230 <br> Distance to Median:  1.2 <br> Geo distance:  0.111004504413106 <br>","USAFID:  726550 <br> Median Temperature:  21.1 <br> Median Wind Speed:  3.1 <br> State:  MN <br> Latitude:  45.544 <br> Longitude:  -94.052 <br> Elevation:  312 <br> Distance to Median:  0.100000000000001 <br> Geo distance:  0.152738338343705 <br>","USAFID:  720869 <br> Median Temperature:  23.9 <br> Median Wind Speed:  2.6 <br> State:  MO <br> Latitude:  38.947 <br> Longitude:  -92.683 <br> Elevation:  218 <br> Distance to Median:  0.509901951359278 <br> Geo distance:  0.289110705439976 <br>","USAFID:  722350 <br> Median Temperature:  26.1 <br> Median Wind Speed:  3.1 <br> State:  MS <br> Latitude:  32.32 <br> Longitude:  -90.078 <br> Elevation:  101 <br> Distance to Median:  0.5 <br> Geo distance:  0.575920133351843 <br>","USAFID:  727755 <br> Median Temperature:  16 <br> Median Wind Speed:  3.1 <br> State:  MT <br> Latitude:  47.517 <br> Longitude:  -111.183 <br> Elevation:  1058 <br> Distance to Median:  0.100000000000001 <br> Geo distance:  0.599754116284336 <br>","USAFID:  722201 <br> Median Temperature:  21.4 <br> Median Wind Speed:  2.6 <br> State:  NC <br> Latitude:  35.582 <br> Longitude:  -79.101 <br> Elevation:  75 <br> Distance to Median:  0.400000000000002 <br> Geo distance:  0.11761377470348 <br>","USAFID:  720867 <br> Median Temperature:  20 <br> Median Wind Speed:  3.6 <br> State:  ND <br> Latitude:  48.39 <br> Longitude:  -100.024 <br> Elevation:  472 <br> Distance to Median:  0 <br> Geo distance:  0.594000000000001 <br>","USAFID:  725513 <br> Median Temperature:  21.7 <br> Median Wind Speed:  3.1 <br> State:  NE <br> Latitude:  40.893 <br> Longitude:  -97.997 <br> Elevation:  550 <br> Distance to Median:  0.53851648071345 <br> Geo distance:  0.301438219209177 <br>","USAFID:  726050 <br> Median Temperature:  17.2 <br> Median Wind Speed:  2.6 <br> State:  NH <br> Latitude:  43.205 <br> Longitude:  -71.503 <br> Elevation:  105 <br> Distance to Median:  1.1 <br> Geo distance:  0.0753126815350552 <br>","USAFID:  722247 <br> Median Temperature:  19.4 <br> Median Wind Speed:  2.6 <br> State:  NJ <br> Latitude:  40.624 <br> Longitude:  -74.669 <br> Elevation:  30 <br> Distance to Median:  0.781024967590667 <br> Geo distance:  0.10223991392798 <br>","USAFID:  722683 <br> Median Temperature:  22 <br> Median Wind Speed:  4.6 <br> State:  NM <br> Latitude:  33.45 <br> Longitude:  -105.516 <br> Elevation:  2077 <br> Distance to Median:  0.58309518948453 <br> Geo distance:  0.778052054813807 <br>","USAFID:  724770 <br> Median Temperature:  14.4 <br> Median Wind Speed:  3.1 <br> State:  NV <br> Latitude:  39.601 <br> Longitude:  -116.005 <br> Elevation:  1809 <br> Distance to Median:  6.61891229734916 <br> Geo distance:  0.979652999791261 <br>","USAFID:  744865 <br> Median Temperature:  18.9 <br> Median Wind Speed:  3.1 <br> State:  NY <br> Latitude:  40.851 <br> Longitude:  -72.619 <br> Elevation:  20 <br> Distance to Median:  0.5 <br> Geo distance:  0.536710350189002 <br>","USAFID:  720928 <br> Median Temperature:  18 <br> Median Wind Speed:  3.1 <br> State:  OH <br> Latitude:  40.28 <br> Longitude:  -83.115 <br> Elevation:  288 <br> Distance to Median:  1.1 <br> Geo distance:  0.0369999999999919 <br>","USAFID:  722187 <br> Median Temperature:  26.3 <br> Median Wind Speed:  4.1 <br> State:  OK <br> Latitude:  35.357 <br> Longitude:  -96.943 <br> Elevation:  327 <br> Distance to Median:  2.0591260281974 <br> Geo distance:  0.178443268295563 <br>","USAFID:  726945 <br> Median Temperature:  15.6 <br> Median Wind Speed:  4.1 <br> State:  OR <br> Latitude:  44.5 <br> Longitude:  -123.283 <br> Elevation:  75 <br> Distance to Median:  1.16619037896906 <br> Geo distance:  0.420000000000002 <br>","USAFID:  725118 <br> Median Temperature:  21.1 <br> Median Wind Speed:  3.1 <br> State:  PA <br> Latitude:  40.218 <br> Longitude:  -76.855 <br> Elevation:  106 <br> Distance to Median:  2.2 <br> Geo distance:  0.227107903869501 <br>","USAFID:  725074 <br> Median Temperature:  22 <br> Median Wind Speed:  6.2 <br> State:  RI <br> Latitude:  41.597 <br> Longitude:  -71.412 <br> Elevation:  5 <br> Distance to Median:  4.82700735445887 <br> Geo distance:  0.019999999999996 <br>","USAFID:  747900 <br> Median Temperature:  22.8 <br> Median Wind Speed:  3.1 <br> State:  SC <br> Latitude:  33.967 <br> Longitude:  -80.467 <br> Elevation:  74 <br> Distance to Median:  0.199999999999999 <br> Geo distance:  0.0999999999999943 <br>","USAFID:  726530 <br> Median Temperature:  21.6 <br> Median Wind Speed:  3.1 <br> State:  SD <br> Latitude:  43.767 <br> Longitude:  -99.318 <br> Elevation:  517 <br> Distance to Median:  1.11803398874989 <br> Geo distance:  0.284015844628428 <br>","USAFID:  721031 <br> Median Temperature:  21 <br> Median Wind Speed:  3.1 <br> State:  TN <br> Latitude:  35.38 <br> Longitude:  -86.246 <br> Elevation:  330 <br> Distance to Median:  1.11803398874989 <br> Geo distance:  0.213000000000001 <br>","USAFID:  722570 <br> Median Temperature:  28.65 <br> Median Wind Speed:  4.1 <br> State:  TX <br> Latitude:  31.133 <br> Longitude:  -97.717 <br> Elevation:  267 <br> Distance to Median:  0.986154146165799 <br> Geo distance:  0.109658560997302 <br>","USAFID:  725724 <br> Median Temperature:  19.4 <br> Median Wind Speed:  3.1 <br> State:  UT <br> Latitude:  40.219 <br> Longitude:  -111.723 <br> Elevation:  1371 <br> Distance to Median:  0.781024967590667 <br> Geo distance:  0.293000000000006 <br>","USAFID:  720498 <br> Median Temperature:  20.6 <br> Median Wind Speed:  2.6 <br> State:  VA <br> Latitude:  37.4 <br> Longitude:  -77.517 <br> Elevation:  72 <br> Distance to Median:  0.58309518948453 <br> Geo distance:  0.0829999999999984 <br>","USAFID:  726114 <br> Median Temperature:  16.1 <br> Median Wind Speed:  2.1 <br> State:  VT <br> Latitude:  44.533 <br> Longitude:  -72.615 <br> Elevation:  223 <br> Distance to Median:  0.707106781186548 <br> Geo distance:  0.0820060973342801 <br>","USAFID:  727930 <br> Median Temperature:  14.4 <br> Median Wind Speed:  3.1 <br> State:  WA <br> Latitude:  47.445 <br> Longitude:  -122.314 <br> Elevation:  132 <br> Distance to Median:  0.6 <br> Geo distance:  0.167999999999999 <br>","USAFID:  726465 <br> Median Temperature:  17 <br> Median Wind Speed:  2.6 <br> State:  WI <br> Latitude:  44.783 <br> Longitude:  -89.667 <br> Elevation:  389 <br> Distance to Median:  2.06155281280883 <br> Geo distance:  0.200024998437698 <br>","USAFID:  720328 <br> Median Temperature:  19 <br> Median Wind Speed:  2.6 <br> State:  WV <br> Latitude:  39 <br> Longitude:  -80.274 <br> Elevation:  498 <br> Distance to Median:  0.699999999999999 <br> Geo distance:  0.167260276216444 <br>","USAFID:  726720 <br> Median Temperature:  13.9 <br> Median Wind Speed:  3.1 <br> State:  WY <br> Latitude:  43.062 <br> Longitude:  -108.447 <br> Elevation:  1684 <br> Distance to Median:  1.0295630140987 <br> Geo distance:  0.27224988521577 <br>"],null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[28.821,48.39],"lng":[-123.283,-69.667]}},"evals":[],"jsHooks":[]}</script>

Knit the doc and save it on GitHub.

## Question 4: Summary Table with `kableExtra`

Generate a summary table using `kable` where the rows are each state and
the columns represent average temperature broken down by low, median,
and high elevation stations.

Use the following breakdown for elevation:

- Low: elev \< 93
- Mid: elev \>= 93 and elev \< 401
- High: elev \>= 401

``` r
# Load the required packages
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# Categorize stations based on elevation
closest_stations_by_state <- closest_stations %>%
  mutate(elev_category = case_when(
    elev < 93 ~ "Low",
    elev >= 93 & elev < 401 ~ "Mid",
    elev >= 401 ~ "High"
  ))

# Calculate average temperature for each elevation category within each state
summary_table <- closest_stations_by_state %>%
  group_by(state, elev_category) %>%
  summarise(avg_temp = mean(med_temp, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'state'. You can override using the
    ## `.groups` argument.

``` r
# Reshape the data to have elevation categories as columns
summary_table <- pivot_wider(summary_table, names_from = elev_category, values_from = avg_temp)

# Add row names for better readability
rownames(summary_table) <- summary_table$state
```

    ## Warning: Setting row names on a tibble is deprecated.

``` r
# Print the summary table using kable
kable(summary_table, format = "html") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
state
</th>
<th style="text-align:right;">
Low
</th>
<th style="text-align:right;">
Mid
</th>
<th style="text-align:right;">
High
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
AL
</td>
<td style="text-align:right;">
23.33333
</td>
<td style="text-align:right;">
22.37500
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
AR
</td>
<td style="text-align:right;">
23.30000
</td>
<td style="text-align:right;">
23.00000
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
AZ
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
24.30000
</td>
</tr>
<tr>
<td style="text-align:left;">
CA
</td>
<td style="text-align:right;">
17.82308
</td>
<td style="text-align:right;">
15.91429
</td>
<td style="text-align:right;">
22.80000
</td>
</tr>
<tr>
<td style="text-align:left;">
CO
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
14.77500
</td>
</tr>
<tr>
<td style="text-align:left;">
CT
</td>
<td style="text-align:right;">
20.00000
</td>
<td style="text-align:right;">
18.30000
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
DE
</td>
<td style="text-align:right;">
21.10000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
FL
</td>
<td style="text-align:right;">
25.72727
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
GA
</td>
<td style="text-align:right;">
23.50000
</td>
<td style="text-align:right;">
22.75556
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
IA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
21.80800
</td>
<td style="text-align:right;">
21.60000
</td>
</tr>
<tr>
<td style="text-align:left;">
ID
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
18.35000
</td>
</tr>
<tr>
<td style="text-align:left;">
IL
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
21.22500
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
IN
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
19.45714
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
KS
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
23.00000
</td>
<td style="text-align:right;">
22.20000
</td>
</tr>
<tr>
<td style="text-align:left;">
KY
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
21.05000
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
LA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
23.00000
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
MA
</td>
<td style="text-align:right;">
17.90000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
MD
</td>
<td style="text-align:right;">
20.91667
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
ME
</td>
<td style="text-align:right;">
15.50000
</td>
<td style="text-align:right;">
15.55000
</td>
<td style="text-align:right;">
15.00000
</td>
</tr>
<tr>
<td style="text-align:left;">
MI
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
18.42000
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
MN
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
21.25676
</td>
<td style="text-align:right;">
20.13750
</td>
</tr>
<tr>
<td style="text-align:left;">
MO
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
23.00000
</td>
<td style="text-align:right;">
23.00000
</td>
</tr>
<tr>
<td style="text-align:left;">
MS
</td>
<td style="text-align:right;">
23.43333
</td>
<td style="text-align:right;">
23.65000
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
MT
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
17.20000
</td>
</tr>
<tr>
<td style="text-align:left;">
NC
</td>
<td style="text-align:right;">
22.68182
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
ND
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
21.00000
</td>
<td style="text-align:right;">
20.27273
</td>
</tr>
<tr>
<td style="text-align:left;">
NE
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
20.50000
</td>
</tr>
<tr>
<td style="text-align:left;">
NH
</td>
<td style="text-align:right;">
18.30000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
NJ
</td>
<td style="text-align:right;">
19.88000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
NM
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
25.70000
</td>
</tr>
<tr>
<td style="text-align:left;">
NV
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
17.57500
</td>
</tr>
<tr>
<td style="text-align:left;">
NY
</td>
<td style="text-align:right;">
19.43333
</td>
<td style="text-align:right;">
18.85000
</td>
<td style="text-align:right;">
16.10000
</td>
</tr>
<tr>
<td style="text-align:left;">
OH
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
19.60000
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
OK
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
23.65000
</td>
<td style="text-align:right;">
20.60000
</td>
</tr>
<tr>
<td style="text-align:left;">
OR
</td>
<td style="text-align:right;">
16.10000
</td>
<td style="text-align:right;">
17.80000
</td>
<td style="text-align:right;">
18.05000
</td>
</tr>
<tr>
<td style="text-align:left;">
PA
</td>
<td style="text-align:right;">
19.40000
</td>
<td style="text-align:right;">
19.10000
</td>
<td style="text-align:right;">
17.20000
</td>
</tr>
<tr>
<td style="text-align:left;">
RI
</td>
<td style="text-align:right;">
18.90000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
SC
</td>
<td style="text-align:right;">
22.75000
</td>
<td style="text-align:right;">
22.25000
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
SD
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
22.20000
</td>
<td style="text-align:right;">
20.00000
</td>
</tr>
<tr>
<td style="text-align:left;">
TN
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
22.81667
</td>
<td style="text-align:right;">
18.30000
</td>
</tr>
<tr>
<td style="text-align:left;">
TX
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
27.36000
</td>
<td style="text-align:right;">
22.20000
</td>
</tr>
<tr>
<td style="text-align:left;">
UT
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
20.27500
</td>
</tr>
<tr>
<td style="text-align:left;">
VA
</td>
<td style="text-align:right;">
22.13333
</td>
<td style="text-align:right;">
20.20000
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
VT
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
17.20000
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
WA
</td>
<td style="text-align:right;">
15.00000
</td>
<td style="text-align:right;">
20.30000
</td>
<td style="text-align:right;">
18.60000
</td>
</tr>
<tr>
<td style="text-align:left;">
WI
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
19.51176
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
WV
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
18.60000
</td>
<td style="text-align:right;">
16.10000
</td>
</tr>
<tr>
<td style="text-align:left;">
WY
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
14.50000
</td>
</tr>
</tbody>
</table>

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, first create a
  lazy table. Filter out values of atmospheric pressure outside of the
  range 1000 to 1020. Examine the association between temperature (y)
  and atmospheric pressure (x). Create a scatterplot of the two
  variables using ggplot2. Add both a linear regression line and a
  smooth line.

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.

``` r
# Load required packages
library(dplyr)
library(ggplot2)
library(mgcv)

# Assume your data frame is called "median_values_per_station"
# Create a lazy table and filter out atmospheric pressure values outside the range 1000 to 1020
lazy_table <- mere_by_stations %>%
  filter(med_atm_press >= 1000 & med_atm_press <= 1020)

# Examine the association between temperature (y) and atmospheric pressure (x)
# Create a scatterplot with linear regression line and smooth line
scatterplot <- ggplot(lazy_table, aes(x = med_atm_press, y = med_temp)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Linear regression line
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, color = "red") +  # Smooth line with cubic regression spline
  labs(x = "Atmospheric Pressure", y = "Temperature") +  # Labels
  theme_minimal()  # Theme

# Print scatterplot
print(scatterplot)
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# Fit linear model
linear_model <- lm(med_temp ~ med_atm_press, data = lazy_table)

# Fit spline model with cubic regression spline on wind speed
spline_model <- gam(med_temp ~ s(med_wind_speed, bs = "cs"), data = lazy_table)

# Summarize linear model
summary(linear_model)
```

    ## 
    ## Call:
    ## lm(formula = med_temp ~ med_atm_press, data = lazy_table)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.3696  -2.6173   0.1844   2.3012  11.6394 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    1.176e+03  8.441e+00   139.3   <2e-16 ***
    ## med_atm_press -1.142e+00  8.343e-03  -136.8   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.759 on 52078 degrees of freedom
    ## Multiple R-squared:  0.2645, Adjusted R-squared:  0.2644 
    ## F-statistic: 1.872e+04 on 1 and 52078 DF,  p-value: < 2.2e-16

``` r
# Summarize spline model
summary(spline_model)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## med_temp ~ s(med_wind_speed, bs = "cs")
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 20.93910    0.01864    1123   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                     edf Ref.df     F p-value    
    ## s(med_wind_speed) 8.975      9 371.8  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.0604   Deviance explained = 6.05%
    ## GCV = 18.071  Scale est. = 18.068    n = 51984

``` r
# Plot results from both models
plot(linear_model)
```

![](README_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-11-5.png)<!-- -->

``` r
plot(spline_model)
```

![](README_files/figure-gfm/unnamed-chunk-11-6.png)<!-- -->

``` r
# Interpretation: Compare the R-squared values and AIC/BIC values of both models to determine the best fit.
```

## Deliverables

- .Rmd file (this file)

- link to the .md file (with all outputs) in your GitHub repository
