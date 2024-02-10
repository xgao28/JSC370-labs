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
library(tidyr)
library(knitr)
library(styler)
library(webshot)
```

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
geo_distance <- function(lat1, lon1, lat2, lon2) {
  distance <- sqrt((lat2 - lat1)^2 + (lon2 - lon1)^2)
  return(distance)
}

mere_by_stations <- mere_by_stations %>%
  mutate(geo_dist = geo_distance(lat, lon, state_med_lat, state_med_lon))

# Find the closest station to the geographic midpoint of each state
geo_closest_stations_by_state <- mere_by_stations %>%
  group_by(state) %>%
  arrange(geo_dist) %>%
  slice(1)

# Create the leaflet map
map <- leaflet(geo_closest_stations_by_state) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon,
    lat = ~lat,
    color = ~"purple",
    radius = 5,
    #popup = ~paste("USAFID: ", USAFID, "<br>",
    #               "Median Temperature: ", med_temp, "<br>",
    #               "Median Wind Speed: ", med_wind_speed, "<br>",
    #               "State: ", state, "<br>",
    #               "Latitude: ", lat, "<br>",
    #               "Longitude: ", lon, "<br>",
    #               "Elevation: ", elev, "<br>",
    #               "Geo distance: ", geo_dist, "<br>") 
  ) %>% 
  addCircleMarkers(
    data = closest_stations,
    lng = ~lon,
    lat = ~lat,
    color = ~"orange",
    radius = 2
  ) %>% 
  addLegend(colors = c("purple","orange"), labels = c("Geographic closest", "Temperature and wind speed closest"))
map
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

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
mer <- mer %>%
  mutate(elev_category = case_when(
    elev < 93 ~ "Low",
    elev >= 93 & elev < 401 ~ "Mid",
    elev >= 401 ~ "High"
  ))

# Calculate average temperature for each elevation category within each state
summary_table <- mer %>%
  group_by(STATE, elev_category) %>%
  summarise(avg_temp = mean(temp, na.rm = TRUE),.groups = "drop")

# Reshape the data to have elevation categories as columns
summary_table <- spread(summary_table, key = elev_category, value = avg_temp)

# Add row names for better readability
rownames(summary_table) <- summary_table$STATE
```

    ## Warning: Setting row names on a tibble is deprecated.

``` r
# Print the summary table using kable
kable(summary_table, caption = "average temperature by low, median, and high elevation stations", format = "pipe") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
average temperature by low, median, and high elevation stations
</caption>
<thead>
<tr>
<th style="text-align:left;">
STATE
</th>
<th style="text-align:right;">
High
</th>
<th style="text-align:right;">
Low
</th>
<th style="text-align:right;">
Mid
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
AL
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
25.07106
</td>
<td style="text-align:right;">
23.79775
</td>
</tr>
<tr>
<td style="text-align:left;">
AR
</td>
<td style="text-align:right;">
23.723926
</td>
<td style="text-align:right;">
25.58698
</td>
<td style="text-align:right;">
24.40578
</td>
</tr>
<tr>
<td style="text-align:left;">
AZ
</td>
<td style="text-align:right;">
23.892609
</td>
<td style="text-align:right;">
29.28585
</td>
<td style="text-align:right;">
30.38057
</td>
</tr>
<tr>
<td style="text-align:left;">
CA
</td>
<td style="text-align:right;">
18.148808
</td>
<td style="text-align:right;">
18.25508
</td>
<td style="text-align:right;">
18.77071
</td>
</tr>
<tr>
<td style="text-align:left;">
CO
</td>
<td style="text-align:right;">
15.184075
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
CT
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
19.37249
</td>
<td style="text-align:right;">
18.78433
</td>
</tr>
<tr>
<td style="text-align:left;">
DE
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
21.40611
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
NA
</td>
<td style="text-align:right;">
26.61484
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
NA
</td>
<td style="text-align:right;">
24.80529
</td>
<td style="text-align:right;">
23.23841
</td>
</tr>
<tr>
<td style="text-align:left;">
IA
</td>
<td style="text-align:right;">
21.992787
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
22.26228
</td>
</tr>
<tr>
<td style="text-align:left;">
ID
</td>
<td style="text-align:right;">
16.415667
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
IL
</td>
<td style="text-align:right;">
20.843173
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
22.11707
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
NA
</td>
<td style="text-align:right;">
20.12731
</td>
</tr>
<tr>
<td style="text-align:left;">
KS
</td>
<td style="text-align:right;">
22.098776
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
24.16196
</td>
</tr>
<tr>
<td style="text-align:left;">
KY
</td>
<td style="text-align:right;">
20.178196
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
21.36103
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
27.61819
</td>
<td style="text-align:right;">
26.09414
</td>
</tr>
<tr>
<td style="text-align:left;">
MA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
17.44477
</td>
<td style="text-align:right;">
17.59058
</td>
</tr>
<tr>
<td style="text-align:left;">
MD
</td>
<td style="text-align:right;">
20.648332
</td>
<td style="text-align:right;">
21.25462
</td>
<td style="text-align:right;">
20.62255
</td>
</tr>
<tr>
<td style="text-align:left;">
ME
</td>
<td style="text-align:right;">
15.329681
</td>
<td style="text-align:right;">
15.23159
</td>
<td style="text-align:right;">
15.43930
</td>
</tr>
<tr>
<td style="text-align:left;">
MI
</td>
<td style="text-align:right;">
17.977982
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
18.54432
</td>
</tr>
<tr>
<td style="text-align:left;">
MN
</td>
<td style="text-align:right;">
19.931963
</td>
<td style="text-align:right;">
22.66275
</td>
<td style="text-align:right;">
21.15523
</td>
</tr>
<tr>
<td style="text-align:left;">
MO
</td>
<td style="text-align:right;">
23.300286
</td>
<td style="text-align:right;">
25.79654
</td>
<td style="text-align:right;">
23.77652
</td>
</tr>
<tr>
<td style="text-align:left;">
MS
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
26.34285
</td>
<td style="text-align:right;">
24.66682
</td>
</tr>
<tr>
<td style="text-align:left;">
MT
</td>
<td style="text-align:right;">
16.293015
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
NC
</td>
<td style="text-align:right;">
18.046833
</td>
<td style="text-align:right;">
22.82945
</td>
<td style="text-align:right;">
21.21073
</td>
</tr>
<tr>
<td style="text-align:left;">
ND
</td>
<td style="text-align:right;">
20.415848
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
21.79236
</td>
</tr>
<tr>
<td style="text-align:left;">
NE
</td>
<td style="text-align:right;">
21.048920
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
23.48598
</td>
</tr>
<tr>
<td style="text-align:left;">
NH
</td>
<td style="text-align:right;">
7.243417
</td>
<td style="text-align:right;">
17.78844
</td>
<td style="text-align:right;">
16.77731
</td>
</tr>
<tr>
<td style="text-align:left;">
NJ
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
19.96563
</td>
<td style="text-align:right;">
19.31963
</td>
</tr>
<tr>
<td style="text-align:left;">
NM
</td>
<td style="text-align:right;">
22.448418
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
NV
</td>
<td style="text-align:right;">
20.849170
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
NY
</td>
<td style="text-align:right;">
15.887585
</td>
<td style="text-align:right;">
18.75621
</td>
<td style="text-align:right;">
18.31489
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
NA
</td>
<td style="text-align:right;">
19.43774
</td>
</tr>
<tr>
<td style="text-align:left;">
OK
</td>
<td style="text-align:right;">
24.000040
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
25.07676
</td>
</tr>
<tr>
<td style="text-align:left;">
OR
</td>
<td style="text-align:right;">
16.711553
</td>
<td style="text-align:right;">
15.20318
</td>
<td style="text-align:right;">
16.39100
</td>
</tr>
<tr>
<td style="text-align:left;">
PA
</td>
<td style="text-align:right;">
17.286934
</td>
<td style="text-align:right;">
20.34185
</td>
<td style="text-align:right;">
19.40527
</td>
</tr>
<tr>
<td style="text-align:left;">
RI
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
17.88116
</td>
<td style="text-align:right;">
17.46589
</td>
</tr>
<tr>
<td style="text-align:left;">
SC
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
23.68407
</td>
<td style="text-align:right;">
22.38995
</td>
</tr>
<tr>
<td style="text-align:left;">
SD
</td>
<td style="text-align:right;">
20.639922
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
22.79495
</td>
</tr>
<tr>
<td style="text-align:left;">
TN
</td>
<td style="text-align:right;">
19.457179
</td>
<td style="text-align:right;">
25.81362
</td>
<td style="text-align:right;">
22.89642
</td>
</tr>
<tr>
<td style="text-align:left;">
TX
</td>
<td style="text-align:right;">
26.500393
</td>
<td style="text-align:right;">
28.74462
</td>
<td style="text-align:right;">
28.08021
</td>
</tr>
<tr>
<td style="text-align:left;">
UT
</td>
<td style="text-align:right;">
19.754720
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
VA
</td>
<td style="text-align:right;">
17.954522
</td>
<td style="text-align:right;">
21.34826
</td>
<td style="text-align:right;">
20.49998
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
NaN
</td>
<td style="text-align:right;">
16.89971
</td>
</tr>
<tr>
<td style="text-align:left;">
WA
</td>
<td style="text-align:right;">
16.810354
</td>
<td style="text-align:right;">
15.25193
</td>
<td style="text-align:right;">
17.80542
</td>
</tr>
<tr>
<td style="text-align:left;">
WI
</td>
<td style="text-align:right;">
17.994615
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
19.56563
</td>
</tr>
<tr>
<td style="text-align:left;">
WV
</td>
<td style="text-align:right;">
17.492150
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
19.31079
</td>
</tr>
<tr>
<td style="text-align:left;">
WY
</td>
<td style="text-align:right;">
13.748173
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
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
  regression spline on atmospheric pressure). Summarize and plot the
  results from the models and interpret which model is the best fit and
  why.

``` r
lazy_table <- mere_by_stations %>%
  filter(med_atm_press >= 1000 & med_atm_press <= 1020)

scatterplot <- ggplot(lazy_table, aes(x = med_atm_press, y = med_temp)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Linear regression line
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr"), se = FALSE, color = "red") +  # Smooth line with cubic regression spline
  labs(x = "Atmospheric Pressure", y = "Temperature") +  # Labels
  theme_minimal()

print(scatterplot)
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
linear_model <- lm(med_temp ~ med_atm_press, data = lazy_table)

spline_model <- gam(med_temp ~ s(med_atm_press, bs = "cr"), data = lazy_table)

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
summary(spline_model)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## med_temp ~ s(med_atm_press, bs = "cr")
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 20.93783    0.01604    1305   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                    edf Ref.df    F p-value    
    ## s(med_atm_press) 8.993      9 2508  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.302   Deviance explained = 30.2%
    ## GCV = 13.406  Scale est. = 13.404    n = 52080

``` r
plot(linear_model)
```

![](README_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-10-5.png)<!-- -->

``` r
plot(spline_model)
```

![](README_files/figure-gfm/unnamed-chunk-10-6.png)<!-- -->

- the GAM (gam) model appears to be a better fit for the data compared
  to the linear model (lm) since The GAM has a higher adjusted R-squared
  value (0.302) compared to the adjusted R-squared value of the linear
  model (0.2644), indicating that the GAM explains more variance in the
  response variable.
- The GAM also has a lower residual standard error, suggesting better
  fit to the data.
- This might because the smooth term in the GAM captures the non-linear
  relationship between med_temp and med_atm_press.

## Deliverables

- .Rmd file (this file)

- link to the .md file (with all outputs) in your GitHub repository
