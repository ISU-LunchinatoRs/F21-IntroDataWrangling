# LunchinatoRS: Data wrangling Part 3 ----------

# Subsetting & summarizing 
# 1) Subset data (filter)
# 2) Select columns (select)
# 2) Summarize data (summarize)
# 3) Group data (group_by)

# Load libraries ----------
library("tidyverse") #loads dplyr, stringr, ggplot2, tidyr, forcats

# Load data, explore ----------
# Use tidy, wrangled data from Data Wrangling Part 2
transplant <- read_csv("data/tidy/transplant_tidy_clean.csv")

# Explore data
str(transplant)
summary(transplant)

# 1: Subset data - pick observations by their values (filter) ----------
#use filter to extract rows based on values of columns. 

# Create a table from transplant data with only data from the anao site.  
(transplant_anao <- transplant %>%
  filter(site == "anao")) #note the use of  == not =

# Note - adding the parentheses around the whole thing makes it produce a variable and print the head of the tibble in the console. 

# Can use any standard operation: >, >=, <, <=, != (not equal), == (equal)

# Create a table with web area greater than 0.2
(transplant_largeweb <- transplant %>%
  filter(webarea > 0.2)) 

# Can combine multiple variables using Boolean operators 
# & means “and”
# | means “or”
# ! means “not”

(transplant_anaobig <- transplant %>%
  filter(site == "anao" & webarea > 0.2))

# If you want to filter on multiple values of the same variable, need to enter each as a separate command
(transplant_sites <- transplant %>%
  filter(site == "anao" | site == "ladt")) 

# Another way to do this is to use the %>% function, which can be used to identify if an element belongs to a vector or dataframe. For example, this will select every row where the site is anao, ladt, or forb
(transplant_sites2 <- transplant %>%
  filter(site %in% c("anao", "ladt", "forb"))) 

# NA's: filter() only includes rows where the condition is TRUE; it excludes both FALSE and NA values. If you want to preserve missing values, ask for them explicitly
(transplantbig_na <- transplant %>%
  filter(is.na(webarea) | webarea > 0.2)) #note, there aren't any webarea values that are "NA", but this is the code to use if there were and you wanted to keep them as well as the web areas greater than 0.2


#Your turn: Use filter to keep only rows with "native" webs where the web is absent, and the duration is less than or equal to 4. How many rows and variables does this produce?

#Your turn 2: keep rows where startdate is before 2013-07-30 and end date is before 2013-08-02 and webpres is yes. 


# 2: Choose columns by their names (select) ----------
# use select to choose columns 
(transplant_select <- transplant %>%
  select(island, site, websize, duration))

# can also use it to omit certain columns
(transplant_select2 <- transplant %>%
  select(-island, -site, -websize, -duration))

# can select all columns between two columns
(transplant_select3 <- transplant %>%
  select(web:webpres))

# useful helper functions for select (from the tidyverse)
# starts_with("abc"): matches names that begin with “abc”.
# ends_with("xyz"): matches names that end with “xyz”.
# contains("ijk"): matches names that contain “ijk”.
# num_range("x", 1:3): matches a numerical range like x1, x2 and x3.

(transplant_select4 <- transplant %>%
  select(contains("eb"))) #7 columns contain eb in their column name

# Can use select() in conjunction with the everything() helper. This is useful if you have a handful of variables you’d like to move to the start of the data frame.

(transplant_select5 <- transplant %>%
  select(native, netting, duration, everything()))


# 3: Summarize data (summarize, count) ----------
# use summarize to compute a table using whatever summary function you want 
# handy functions: 
  # finding the center or total: mean, median, sum
  # finding the spread or range: max, min, sd, var 
  # identifying by position: first(), last(), nth()
  # counting: n, n_distinct
  # logical: any(), all()
# Without using group_by (below), summarise collapses a dataframe to a single row

(transplant_summ <- transplant %>%
  summarize(avgweb = mean(websize))) # name the new column "avgweb"

# You can do multiple summary calculations at a time
(transplant_summ2 <- transplant %>%
  summarise(avgwebsize = mean(websize), 
            medwebsize = median(websize), 
            sdwebsize = sd(websize), 
            maxarea = max(webarea), 
            minarea = min(webarea, na.rm = TRUE)))

# note, if you have na's in a column, you'll get NA as the result if you try to calculate mean, median, sd, max, min, var, sum without including "na.rm = T" as an argument

# You can also do summary calculations on a subset of the data. 
# This calculates the average websize of all webs larger than 50 cm. 
(transplant_summ3 <- transplant %>%
    filter(websize > 50) %>%
    summarise(avglarge = mean(websize))) ## look at this more 

# There are a bunch of ways to get the # of rows or # of levels of a variable
(transplant_summ4 <- transplant %>%
  summarise(numtransprows = n(), 
            nisl = n_distinct(island))) 

# n() takes no arguments, and returns the size of the group (number total rows)
# n_distinct(variablename) gives the number of unique levels
# To count the number of non-missing values, use sum(!is.na(x)) 

# Count the number of rows in each group (count) - this is a function on it's own, not a function within summarize

transplant %>%
  count(site) 

transplant %>%
  count(site, island)  # creates a table of counts by site/island combos. Can add continuous values, but not very useful unless the same values are repeated frequently. 

## Base R functions for summarizing ---------
# apply, tapply, lapply, sapply are the base R versions of summarize

# tapply() computes a measure (mean, median, min, max, etc..) or a function for each factor variable in a vector.
tapply(transplant$websize, transplant$site, median)

# table is a handy base R function that counts number of rows based on 1 or more variables
with(transplant, table(site)) # OR
table(transplant$site)
with(transplant, table(native, netting))
with(transplant, table(island, native, netting))
with(transplant, ftable(island, native, netting)) #ftable works better for 3 variables (ftable = "flat contingency table")


# 4: Group data (group_by) ----------
#use group_by to split a dataframe into different groups, then do something to each group

transplant %>%
  group_by(island) %>%
  summarize (avg = mean(websize))

(transplant_summ <- transplant %>%
  group_by(island, site, netting) %>%
  summarize (avgweb = mean(websize),
             avgduration = mean(duration), 
             numobs = n()))

# Can use group_by with other functions too. 
# Here, we use filter to pull out the sites that have more than 2 rows for a combination of island/site/native (i.e. a site that has 3 or more spiders that were already in place (native) or moved (not native)), and then summarize the mean websize within each of those groups (e.g. 'native' spiders at anao)

transplant_summ2 <- transplant %>%
  group_by(island, site, native) %>%
  filter(n() > 2) %>%
  summarize (avgweb = mean(websize), .groups = "drop")  #.groups ungroups the output, which can be handy for future calculations using this tibble. 

# Your turn: Calculate the mean, median, min, and max duration for webs with and without netting on Guam and Saipan, and calculate the number of webs in each group

# Your turn, advanced version: 1. Calculate mean duration of webs with and without netting at each site on each island. 2. Add these average duration values to the tidy transplant dataset (the one we loaded at the start of this script). 3. Select just the island, site, netting, mean_dur, web, websize, duration columns. 4. Add a column that calculates the difference between duration of each web and the mean_duration for all webs at that island/site/netting combo. 












#**********************
# Answers for the "My Turns"
  
(transplant_myturn <- transplant %>%
    filter(native == "yes" & webpres == "no" & duration <= 4))

(transplant_dates <- transplant %>%
    filter(startdate < "2013-07-30" & enddate < "2013-08-02" & webpres =="yes"))
  
  
  transplant %>%
  group_by(island, netting) %>%
  summarize(mean_dur = mean(duration), 
            med_dur = median(duration), 
            min_dur = min(duration), 
            max_dur = max(duration), 
            nwebs = n())

#better approach
transplant2a <- transplant %>%
  group_by(island, site, netting) %>%
  mutate(avg_dur = mean(duration),
         diff_avg_duration = duration - avg_dur) %>%
  select(island, site, netting, web, websize, avg_dur, diff_avg_duration)

#also works
transplant2b <- transplant %>%
    group_by(island, site, netting) %>%
    summarize(mean_dur = mean(duration), .groups = "drop") %>%
    right_join(transplant, by = c("island", "site", "netting")) %>%
    select(island, site, netting, mean_dur, web, websize, duration) %>%
    mutate(diff_meandur = duration - mean_dur)
