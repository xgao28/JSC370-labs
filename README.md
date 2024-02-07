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
colnames(mer)
```

    ##  [1] "USAFID"            "WBAN"              "year"             
    ##  [4] "month"             "day"               "hour"             
    ##  [7] "min"               "lat"               "lon"              
    ## [10] "elev"              "wind.dir"          "wind.dir.qc"      
    ## [13] "wind.type.code"    "wind.sp"           "wind.sp.qc"       
    ## [16] "ceiling.ht"        "ceiling.ht.qc"     "ceiling.ht.method"
    ## [19] "sky.cond"          "vis.dist"          "vis.dist.qc"      
    ## [22] "vis.var"           "vis.var.qc"        "temp"             
    ## [25] "temp.qc"           "dew.point"         "dew.point.qc"     
    ## [28] "atm.press"         "atm.press.qc"      "CTRY"             
    ## [31] "STATE"             "LAT"               "LON"

``` r
# Calculate the median values for temperature, wind speed, and atmospheric pressure
national_median_temp <- quantile(mer$temp, probs = 0.5, na.rm = TRUE)
national_median_wind_speed <- quantile(mer$wind.sp, probs = 0.5, na.rm = TRUE)
national_median_atm_pressure <- quantile(mer$atm.press, probs = 0.5, na.rm = TRUE)
```

Next identify the stations have these median values.

``` r
mer_by_stations <- mer %>% 
  filter(!is.na(temp) & !is.na(wind.sp) & !is.na(atm.press)) %>%
  group_by(USAFID) %>% 
  summarise(
    med_temp = median(temp),
    med_wind_speed = median(wind.sp),
    med_atm_press = median(atm.press),
            )
head(mer_by_stations)
```

    ## # A tibble: 6 × 4
    ##   USAFID med_temp med_wind_speed med_atm_press
    ##    <int>    <dbl>          <dbl>         <dbl>
    ## 1 690150     26.1            4.1         1009.
    ## 2 720175     28.3            2.6         1011 
    ## 3 720198     12.8            2.6         1014.
    ## 4 720269     29.8            4.6         1006.
    ## 5 720306     24.4            3.1         1012.
    ## 6 720333     18.3            3.1         1013.

``` r
# Find stations where these median values occur
stations_median_temp <- unique(mer_by_stations$USAFID[which(mer_by_stations$med_temp == national_median_temp)])
stations_median_wind_speed <- unique(mer_by_stations$USAFID[which(mer_by_stations$med_wind_speed == national_median_wind_speed)])
stations_median_atm_pressure <- unique(mer_by_stations$USAFID[which(mer_by_stations$med_atm_press == national_median_atm_pressure)])
# Output the stations with median values
cat("Stations with median temperature:", stations_median_temp, "\n")
```

    ## Stations with median temperature: 722247 722689 723010 723150 723170 723627 723630 723676 723783 723990 724020 724057 724074 724085 724110 724140 724250 724273 724287 724288 724297 724380 724397 724510 724516 724760 724815 724838 725103 725113 725118 725340 725360 725461 725465 725470 725473 725485 725555 725620 725720 725810 725970 726410 726499 726518 726555 726557 726587 726685 726883 727535 727676 727830 727900 744665 745046 745946

``` r
cat("Stations with median wind speed:", stations_median_wind_speed, "\n")
```

    ## Stations with median wind speed: 720306 720333 720377 720379 720652 722011 722020 722026 722029 722034 722037 722038 722039 722055 722085 722090 722093 722103 722106 722108 722140 722151 722160 722170 722175 722180 722185 722213 722255 722268 722269 722270 722280 722310 722316 722320 722329 722390 722400 722405 722410 722427 722429 722430 722444 722447 722470 722480 722485 722489 722542 722587 722728 722740 722748 722780 722784 722823 722869 722885 722897 722900 722903 722904 722920 722928 722956 722970 722975 723030 723034 723035 723060 723066 723068 723069 723087 723095 723100 723106 723108 723109 723115 723116 723119 723120 723170 723190 723194 723240 723260 723270 723273 723280 723290 723300 723306 723340 723406 723407 723416 723449 723495 723537 723560 723710 723840 723895 723896 723925 723990 724010 724030 724035 724036 724057 724060 724066 724075 724093 724095 724110 724175 724177 724230 724233 724235 724237 724238 724240 724275 724276 724288 724303 724320 724336 724338 724345 724347 724365 724373 724375 724384 724400 724420 724430 724450 724454 724455 724460 724463 724468 724490 724502 724504 724506 724507 724509 724520 724550 724560 724565 724580 724625 724640 724666 724673 724676 724677 724700 724754 724768 724770 724800 724815 724837 724880 724885 724915 724957 725016 725025 725027 725029 725037 725045 725059 725064 725065 725067 725069 725079 725080 725085 725087 725088 725100 725103 725114 725116 725117 725118 725124 725125 725126 725127 725140 725145 725150 725155 725157 725170 725172 725180 725190 725196 725229 725235 725256 725266 725267 725290 725314 725326 725327 725342 725347 725370 725374 725375 725387 725394 725395 725396 725420 725430 725440 725461 725462 725465 725472 725480 725490 725514 725524 725525 725527 725533 725540 725565 725570 725700 725710 725717 725724 725750 725776 725785 725805 725825 725867 725895 725920 725945 725955 725976 726055 726060 726077 726079 726170 726190 726223 726225 726357 726380 726385 726387 726410 726416 726419 726435 726452 726455 726456 726458 726463 726480 726487 726506 726525 726550 726555 726574 726575 726579 726584 726667 726676 726700 726710 726720 726770 726810 726816 726830 726836 726875 726880 726881 726883 726886 726904 726940 726980 726986 727033 727120 727347 727437 727453 727455 727458 727469 727470 727476 727684 727686 727687 727700 727720 727755 727790 727810 727827 727834 727845 727846 727850 727855 727857 727920 727924 727930 727937 727970 727985 740035 742060 742071 742300 743700 744550 744989 745048 745056 745058 746930 747040 747570 747760 747830 747900 747931

``` r
cat("Stations with median atmospheric pressure:", stations_median_atm_pressure, "\n")
```

    ## Stations with median atmospheric pressure: 722140 722215 722246 722400 723060 723086 723096 723150 723194 723406 724035 724237 724420 724926 725217 725499 726667 727535

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Identifying Representative Stations per State

Now let’s find the weather stations by state with closest temperature
and wind speed based on the euclidean distance from these medians.

Knit the doc and save it on GitHub.

## Question 3: In the Geographic Center?

For each state, identify which station is closest to the geographic
mid-point (median) of the state. Combining these with the stations you
identified in the previous question, use `leaflet()` to visualize all
~100 points in the same figure, applying different colors for the
geographic median and the temperature and wind speed median.

Knit the doc and save it on GitHub.

## Question 4: Summary Table with `kableExtra`

Generate a summary table using `kable` where the rows are each state and
the columns represent average temperature broken down by low, median,
and high elevation stations.

Use the following breakdown for elevation:

- Low: elev \< 93
- Mid: elev \>= 93 and elev \< 401
- High: elev \>= 401

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

## Deliverables

- .Rmd file (this file)

- link to the .md file (with all outputs) in your GitHub repository
