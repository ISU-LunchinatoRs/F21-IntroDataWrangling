# LunchinatoRs Data Wrangling part 1 ------

# Use this script to:
  # 1) Load data into R
  # 2) Explore data
  ## Side Note - learn how to pipe
  # 3) Fix column names (rename)
  # 4) add/remove/split/combine columns (unite,separate)
  # 5) reshape (spread, gather)
  # 6) change class of columns
  # 7) fill in missing values
  # 8) join tables
  # 9) print tidy database

# load libraries --------
library(tidyverse) #loads ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats
library(readxl) #hadley wickham's package for reading in excel files. not in Tidyverse.


# Step 1 - Load data --------
# There are multiple ways to do this, which impacts your imported dataframe. The key things that vary are how R interprets your blank values & NA's, and how R assigns a class to each column. For this exercise, we will use read.csv (option 1)

## option 1: if a csv or table, use read.csv or read.table. -------
transplant2 <- read.csv("data/raw/transplant_raw.csv", na.strings = c("", "NA", "na", " "))

## option 2: if excel, use readxl(). ---------
# note- load this version too to use to see differences in how read.csv and readxl work, but after we do that, the rest of code is set up to work with the read.csv version

transplant_excel <- read_excel("data/raw/transplant_raw.xlsx", sheet = 1, col_names = T, col_types = NULL) #col_types = null is default, and readxl guesses what each column is. by default, readxl converts blank cells to missing data. use na="yourcode" if you have a code you use for missing values.



# Step 2: Explore data ------------

# compare dataframes from above to see what happens using read.csv and readxl
str(transplant) # str shows the structure of your dataset
str(transplant_excel) # most variables are character class. dates are read in intelligently, though, although it added the current year because year was missing.

summary(transplant) #gives summary information, which can help you identify if any columns are in the wrong class or if there are data entry errors or a lot of NA's etc.
names(transplant) #show column names

# **Side note: Introduction to piping** -------------
# Each function returns an object, so calls can be chained together in a single statement, without needing variables to store the intermediate results.
# it takes the output of one statement and makes it the input of the next statement. When reading the code, you can think of the piping symbol as saying "AND THEN".

# %>%  is piping function

transplant %>%
  filter(Island == "Guam") %>%
  summarize(meanwebsize = mean(WebSize.cm.))

#non-piping approach, using square brackets to subset
meanguam <- summarize(transplant[transplant$Island == "Guam",], meanwebsize = mean(WebSize.cm.))

# the code chunk above will translate to something like "you take the transplant data, then you subset the guam data and then you calculate the meanwebsize".

# Here are four reasons why you should be using pipes in R:

# You'll structure the sequence of your data operations from left to right, as apposed to from inside and out;
# You'll avoid nested function calls;
# You'll minimize the need for local variables and function definitions; And
# You'll make it easy to add steps anywhere in the sequence of operations.



# Step 3: Standardize/clean inconsistencies in column names ----------

# 3.1: Rename column headings
# Use the rename function in the dplyr package
# new name is on left, old name is on right.
transplant <- transplant %>% 
  rename(island = Island, 
         site = Site, 
         web = Web.., 
         native = Native, 
         netting = Netting, 
         startdate = Start.Date, 
         enddate = End.Date, 
         totaldays = Total.Days, 
         spidpres = SpidPres, 
         webpres = WebPres, 
         WebSize = WebSize.cm.)

colnames(transplant) #note that I left one with uppercase letters; I will return to that later

# 3.2: Fix upper case/lower case issues in column names
#change column names from upper to lowercase

transplant <- transplant %>%
  rename_with(tolower) #rename_with() renames columns using a function like "tolower"

# To see column names, use colnames or names
colnames(transplant)
names(transplant)

#check out function clean_names in janitor package. This cleans up your column names for you! 
library(janitor)
transplant_excel2 <- clean_names(transplant_excel) 

# Step 4: Add, remove, split, combine columns ----------

## 4.1: Add a column ------

### 4.1.1: Add column directly ------

transplant <- transplant %>%
  mutate(year = 2013)
#this experiment happened in 2013. Added this in a new column, so now have 12 columns/variables. All rows will have the same value for this column. 

# transplant$year <- 2013 #base R approach



## 4.2: Remove a column --------
# use "Select" - to select columns from a dataframe to include or drop (use the - to indicate drop)
transplant <- transplant %>%
  select(-totaldays) #check to make sure you have 1 less column than before (back to 11 variables)

colnames(transplant)

## 4.3: Separate one column into two columns ------
transplant <- transplant %>%
  separate(col = web, into = c("web_a", "web_b"), sep = "'", remove = FALSE) 
# remove=F tells R to leave the original column. So now we have 13 columns/variables

colnames(transplant)

## 4.4: Combine two columns into one ---------
# currently start date has a day and a month, but no year. 
# we can combine two columns using the function unite(). Note that the columns must be in the character class to start with. 
class(transplant$startdate) #already a character

transplant <- transplant %>%
  unite(startdate, c(startdate, year), sep="-", remove = FALSE)
#this overwrites the startdate column with the new value, but does not remove the year column (still 13 columns)

transplant <- transplant %>%
  unite(enddate, c(enddate, year), sep="-")
#this overwrites the enddate column, but also removes the Year column, so now back to 12 columns. 




# Step 5: Reshape database ----------
# i.e. if wide, change to long; if long, change to wide 

## 5.1: Change data from long to wide format using tidyr::pivot_wider() ------
# Pivot_wider only requires two arguments ("names_from" and "values_from") that are the names of the column whose values will turn into column names and the column whose values will populate those new columns. 

transplantwide <- transplant %>%
  pivot_wider(names_from = webpres, values_from = websize) #the info in webpres will be the new column names, and the columns will be populated by the values in websize

## 5.2: Change data from wide to long using tidyr::pivot_longer() -------
# Pivot_longer moves column names into a "key" column, gathering the column values into a single value column.
# The arguments to pivot_longer(cols, names_to, values_to):
# cols: list the names of all columns that will be reshaped, using c()
# names_to: Create a name of new column with the variable names that were the column headings in the wide format
# values_to: Name of new value column (make a new name for this column that will contain the values currently populating the rows in the wide format)

transplant_long <- transplantwide %>%
  pivot_longer(cols = c(no, yes), names_to = "webpres", values_to = "websize") # this makes a new column called "webpres" populated by "no" or "yes", and then another new column called "websize" populated by the values within the former "no" and "yes" columns. 
# note that we have now added a row with "NA" for every observation - this is undesirable for analysis, but serves to demonstrate wrangling a database from wide to long

# Step 6: Change class of columns -----------

## 6.1: Change class of a single column ---------
transplant$island <- as.factor(transplant$island)

## 6.2: Change class of multiple columns at a time --------
transplant <- transplant %>%
  mutate(across(c(island, site, web, native, netting, spidpres), as.factor)) #use across to apply a function across multiple columns

str(transplant)

# Step 7: Fill in missing values ------------
# complete function gives you all combinations of the columns you choose. E.g. if you want to make sure you sampled every island on each of every year, and add rows with NA's for data for any island:year combos you missed, use this function. 

transplant_comp <- transplant %>%
  complete(island, site)  #note - this is meaningless data-wise, just to show how it works

# The Fill function fills in information from the most recent non-missing value. This is useful when people enter data and fail to fill in repetitive data (e.g. the first 30 rows are from Site A, but they only write Site once on row 1 and assume it follows for the next 29 rows). 

transplant_comp <- transplant_comp %>%
  fill(web) 

# Step 8: Combine datasets (Join) -------------
# you have two tables, table x (considered "left" table, here 'transplant') and table y (considered "right" table, here 'preycap')

str(transplant)
# transplant has 91 obs of 12 variables. 
# need to make island & site lowercase to join with preycap
transplant <- transplant %>%
  mutate(island = str_to_lower(as.character(island)),   site = str_to_lower(as.character(site)))

preycap <- read.csv("data/tidy/preycap_tidy.csv")
# preycap has 361 obs of 6 variables

# Left Join: join matching values from y to x. Return all values of x, and all columns from x and y, but only those from y that match. If multiple matches between x and y, then all combinations are returned.
leftjoin_transprey <- transplant %>%
  left_join(preycap, by = c("island", "site"))

# Use by = c("col1", "col2", ...) to specify one or more common columns to match on. 
# returns 4133 obs of 16 variables

# Right Join: join matching values from x to y. Return all rows of y, all columns from x and y, but only those from x that match. As above, if multiple matches, all combinations are returned.
rightjoin_transprey <- transplant %>%
  right_join(preycap, by = c("island" = "island", "site" = "site"))

# Use a named vector, by = c("col1" = "col2"), to match on columns that have different names in each table. 
# Returns 4238 obs of 16 variables

# Inner Join: Join data. Retain only rows from x and y that match, and all columns from both. If multiple matches between x and y, then all combination of matches are returned.
innerjoin_transprey <- transplant %>%
  inner_join(preycap, by = c("island", "site"))
# Returns 4086 observations of 16 variables

# Full Join: Join data. retain all values, all rows from both x and y
fulljoin_transprey <- transplant %>%
  full_join(preycap, (by = c("island", "site")))
# Returns 4285 obs of 16 variables

# let's figure out why we have the # of rows & columns for each of these join types
levels(as.factor(preycap$island))
levels(as.factor(transplant$island))
levels(as.factor(preycap$site))
levels(as.factor(transplant$site))

fulljoin_transprey %>%
  group_by(island, site) %>%
  summarize(numrows = n())

preycap %>%
  group_by(island, site) %>%
  summarize(numrows = n())

transplant %>%
  group_by(island, site) %>%
  summarize(numrows = n())

# Since there are lots of matches for island and site including some misspellings, you get a row for every combination. i.e. for anao, get 12 rows in transplant each duplicated for 112 rows from preycap, for a total of 1344 rows in the fulljoin_transprey tibble. 


# Step 9: Write csv file with tidy dataset -------

# create tidy database for analysis. Add to tidy folder
write.csv(transplant, "data/tidy/transplant_tidy.csv", row.names=FALSE)
