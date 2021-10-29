# LunchinatoRs: Data wrangling Part 2 ----------

# Part 2: Finish getting data frame tidied and wrangled 
  # 1) Create new columns or edit observations within column (mutate)
  # 2) Change class of columns
  # 3) Fix cells within columns (change levels tolower/toupper, rename factor levels (forcats package), reorder factor levels, ghost factor levels, trimws, scale/center continuous variable, work with dates)
  # 4) Arrange data by the levels of a particular column (arrange)
  # 5) Print tidied, wrangled database

# Load Libraries ----------
library("tidyverse") #loads dplyr, stringr, ggplot2, tidyr, forcats
library("lubridate")

# Load tidy data, explore ----------
transplant <- read_csv("data/tidy/transplant_tidy.csv")
str(transplant)
summary(transplant)

# 1: Create new columns or edit observations within existing columns (mutate) ----------

(transplant <- transplant %>%
  mutate(webarea = pi * ((websize/2)/100)^2))

#assume circle, divide in half to get radius, divide by 100 to get from cm to m, calculate area (pi * radius squared)

# 2: Change class of columns ----------
# note - the as.character and as.numeric lines are in here for teaching purposes since those columns are already in the right class. 

transplant <- transplant %>%
  mutate(websize = as.numeric(websize)) %>% # to change class in one column
  mutate(across(c(island, site, web, native, netting, spidpres, webpres), 
            as.factor)) %>% # to change multiple columns at a time
  mutate(across(c(web_a, web_b), as.character)) 

# 3: Fix cells within columns (e.g. naming, capitalization) ----------

summary(transplant) #need to fix capitalization, spelling, whitespace (maybe)

## 2.1: Change levels of variable to lower case (mutate, tolower) ----------
# Note that tolower creates a character vector, so need to change to factor.

transplant <- transplant %>%
  mutate(across(c(island, site), tolower)) %>%
  mutate(across(c(island, site), as.factor)) %>%
  print() #shows first few lines of dataframe 

# across() allows you to apply the same function to multiple columns 

#base R code: transplant$island <- tolower(transplant$island)

## 2.2: Rename levels of a variable (forcats) ----------
# There are a lot of ways to do this. Here is the forcats approach

transplant <- transplant %>%
  mutate(island = fct_recode(island, 
                             "saipan" = "siapan", 
                             "guam" = "gaum"))
  
levels(transplant$island)

#can also combine groups using fct_collapse. For each new variable, you  provide a vector of old levels. Note - this is just to demonstrate, because the new variable is not useful, so I am saving to transplant2. 

transplant2 <- transplant %>%
  mutate(siteisl = fct_collapse(site,
                               "saipan_site" = c("forbi", "ladt", "mtr"), 
                               "guam_site" = c("anao", "nblas")))
str(transplant2)


## 2.3: Re-order levels within a variable (forcats) ----------
# default order is alphabetical. When you graph or run analyses, you may want a different order. 

levels(transplant$site)

# tidyverse approach 

# Manually re-order
transplant <- transplant %>%
  mutate(island = fct_relevel(island, c("saipan", "guam")))

# Can move one level to the first position without stating all of the other levels
transplant <- transplant %>%
  mutate(island = fct_relevel(island, "guam"))

levels(transplant$island)

#base R approach for fct_relevel
levels(transplant$site) #in alphabetical order
transplant$site <- factor(transplant$site, levels = c("nblas", "anao", "ladt", "forbi", "mtr"))
levels(transplant$site)

# Reorder by the levels of another variable (reorder sites by mean websize, in ascending order)
transplant <- transplant %>%
  mutate(site = fct_reorder(site, websize, mean))

#view shows that the rows are still in the same order, but the levels have been reordered
levels(transplant$site)

# Reorder by the frequency of the variable, largest first
transplant <- transplant %>%
  mutate(site = fct_infreq(site))

levels(transplant$site) 

# Reverse order of factor levels
transplant <- transplant %>%
  mutate(site = fct_rev(site))

levels(transplant$site) #now shows order from fewer to more obs

## 2.4: Get rid of ghost levels ----------
# sometimes you get rid of a level and R still thinks it is there (i.e. there are 0 rows, but it still shows up when you use levels() on your variable)

#first create ghost level by adding level to factor
transplant <- transplant %>%
  mutate(island = fct_expand(island, "hawaii"))
levels(transplant$island)

#tidyverse approach
transplant <- transplant %>%
  mutate(island = fct_drop(island)) 
levels(transplant$island)

#base R approach
transplant2$island <- droplevels(transplant$island) # or
transplant2$island <- factor(transplant$island)
levels(transplant2$island)

## create anonymous/arbitrary identifier for a factor
transplant3 <- transplant %>%
  mutate(anon_web = fct_anon(web, prefix = "test"))

## 2.5: Deal with character data or complex strings (stringr) -----
# string functions work with regular expressions, patterns of text
# https://stringr.tidyverse.org/ shows the full extent of the package

# str_length() tells you the # of characters in a string
  str_length(transplant$site)
  
# str_c() allows you to combine the characters from two variables 
  transplant2 <- transplant %>%
    mutate(islandsite = str_c(island, site, sep = ", "))
  
# str_sub() allows you to extract parts of a string, say only the first 3 characters 

  transplant2 <- transplant %>%
    mutate(site = str_sub(site, 1,3)) #keep characters in the 1st to 3rd position
  
  transplant2 <- transplant %>%
    mutate(endsite = str_sub(site, -3, -1)) #to extract the last 3 characters

# str_replace() allows you to replace parts of a string. str_replace just replaces the first instance, str_replace_all replaces all instances. 
  transplant2 <- transplant %>%
    mutate(web = str_replace(web, "'", "something")) %>% 
    mutate(island = str_replace_all(island, "[aeiou]", "-"))
  
  levels(transplant2$island)
  
## 2.6: Remove trailing whitespace ----------
# sometimes excel will leave spaces in a factor, which makes "guam" and "guam ", for example, into different factor levels. trimws() gets rid of this trailing white space
transplant$site <- as.factor(trimws(transplant$site))

## 2.7: Center continuous predictors ----------
# Centering continuous predictors with large values is a useful practice for analysis - may help with convergence
transplant <- transplant %>%
    mutate(websize_c = as.numeric(scale(websize)))

## 2.8: Deal with dates (lubridate) ----------
#Change date format to standard yyyymmdd format
#helpful site: https://www.r-bloggers.com/date-formats-in-r/
class(transplant$startdate)

# use lubridate to tell R the format of the date
transplant <- transplant %>%
  mutate(startdate = dmy(startdate))

transplant <- transplant %>%
  mutate(enddate = dmy(enddate))

#now, can do math on your dates!
transplant <- transplant %>%
  mutate(duration = enddate - startdate) %>%
  mutate(duration = as.numeric(duration))

summary(transplant$duration)
  
# 4: Re-arrange order of rows of data (arrange) ----------
# can re-arrange the data frame by the levels of a particular column. This can be helpful for visualization. Note that this does not change the order of levels. 

transplant <- transplant %>%
  arrange(websize) 

#use desc inside the arrange fx to go from high to low
transplant <- transplant %>%
  arrange(desc(websize))

# 5: Print tidy, wrangled database ----------

write.csv(transplant, "data/tidy/transplant_tidy_clean.csv", row.names = F)
