### Assignment 2 
### Martin Gubler

# Load the biofam data set that comes with the TraMineR library.
library(TraMineR)
data(biofam)

# Print the variable names.
names(biofam)

# Create an age variable by subtracting the birth year 
# from the year of the survey and add it to the biofam data frame.
biofam$age <- 2002-biofam$birthyr
# Check whether variable has been added
names(biofam)

# What is the minimum, maximum, median and mean age in the sample?
min(biofam$age) # 45
max(biofam$age) # 93
mean(biofam$age) # 59.5
summary(biofam$age) # provides all three values with one command

# What is the minimum, maximum, median and mean age of the women?
if(biofam$sex="woman") min(biofam$age)

### QUESTION ###
### How to solve number 4 without using an if-clause???
### I saw and copy-pasted the code below from Scott Gordon's results. 
### It works, but it this the best/most direct way to do it?
summary(biofam[biofam$sex=="woman",]$age)

#### GR: Yes, in R it is much more efficient to work with conditional indexing rather than using loops and ifs.

# Add a cohort factor to the biofam data frame grouping the birth years 
# into the following categories: 1900-1929, 1930-1939, 1940-1949, 1950-1959.
cohort <- cut(biofam$birthyr, c(1900, 1930, 1940, 1950, 1960), labels = c("1900-1929", "1930-1939", "1940-1949", "1950-1959"), right = FALSE)

### QUESTION ###
### How to add that factor to the dataframe?

#### GR: Just assign it to  biofam$cohort as you did for the variable age.
biofam$cohort <- cohort


# Generate an histogram of the distribution of birthyear 
# using the above birth year classes.
hist(biofam$birthyr, breaks = "cohort")

## "cohort" is a string! You should give: breaks=c(1900, 1930, 1940, 1950, 1960)

### QUESTION ###
### Produces the following error message:
### 'x' must be numeric --> Why is that not a numeric vector (years...)?

# Produce a frequency table of the cohort factor.
table(cohort)

# Cross tabulate the cohort with the state at 25 years old.
crosstab <- table (cohort, biofam$a25)

### QUESTION ###
### Produces the following error message:
### "all arguments must have the same length"
### I would assume this is because "cohort" is not part of the dataset right now?

#### No, this would not change the length of cohort. It works for me when I run your code

# Fit a logistic regression for the probability to be married with a child 
# and having left home at 25 years old in terms of the language of the 
# questionnaire and the sex.
biofam.lg.gr <- glm (a25==6 ~ plingu02 + sex, family = binomial, data = biofam)
summary(biofam.lg.gr)

# Fit the same logistic regression, but for the youngest cohort only.
biofam.lg.gr <- glm (a25==6 ~ plingu02 + sex, family = binomial, data = biofam[cohort=="1950-1959"])

### QUESTION ###
### Produces the following error message:
### "undefined columns selected"
### I would assume this is because "cohort" is not part of the dataset right now?

#### gr: No! biofam is a data frame: it has two dimensions, you have to specify which columns you want.
#### since you want all columns you should give data = biofam[cohort=="1950-1959",]