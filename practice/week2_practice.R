#dplyr review for class


#step 1: libraries ####

library(tidyverse)
library(tidycensus)

#step 2: data ####

pa_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "PA",
  year = 2023,
  survey = "acs5"
)


dim(pa_income) #dimensions

glimpse(pa_income) #kind of like a preview

head(pa_income) #first few rows

#There are 67 rows and 5 columns. The row count matches the number of PA counties.


#step 3: GEOID ####

pa_income$GEOID

#every number starts with 42 to show that they all belong to PA

as.numeric("01001") #running this command eliminates the first zero; this could be bad if a GEOID started with zero


#step 4: filter() ####

#I predict there will not be a lot of counties where the estimate is larger than 60,000 (i.e. fewer than 67 rows)

filter(pa_income, estimate > 60000)


# Counties where the margin of error is bigger than 3000
filter(pa_income, estimate > 3000)

# Counties where the estimate is under 50,000
filter(pa_income, estimate < 50000)


#Step 5: select()####

#I predict the number of columns will change

select(pa_income, NAME, estimate, moe)


# Show only GEOID and estimate

select(pa_income, GEOID, estimate)


#step 6: mutate()####

#I predict an additional column named moe_pct

mutate(pa_income, moe_pct = moe / estimate * 100)

pa_income$moe_pct

pa_income <- mutate(pa_income, moe_pct = moe / estimate * 100)

pa_income

#Moe_pct is the margin of error as a percentage rather than a number


#Step 7: arrange() #####

#I predict neither of these actions will change the number of rows

arrange(pa_income, moe_pct)

arrange(pa_income, desc(moe_pct))

#the county at the top of the desc() version: Cameron County


#Step 8: %>% ####

step1 <- filter(pa_income, moe_pct > 5)
step2 <- arrange(step1, desc(moe_pct))
step3 <- select(step2, NAME, estimate, moe, moe_pct)
step3

pa_income %>%
  filter(moe_pct > 5) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, estimate, moe, moe_pct)

#"take pa_income, and then keep the unreliable ones, and then sort worst first, and then show me these four columns"

# Keep counties with moe_pct over 8, sort by estimate, show NAME and moe_pct:

worst <- pa_income %>%
  filter(moe_pct > 8) %>%
  arrange(desc(estimate)) %>%
  select(NAME, moe_pct)

worst


#Step 9: group_by and summarise ####

pa_income <- mutate(pa_income, reliable = moe_pct < 5)

#I predict there will be less than 67 rows

pa_income %>%
  group_by(reliable) %>%
  summarize(n = n(),
            avg_income = mean(estimate))

#group_by essentially changes the unit of analysis; the "reliable" column sorted every row into > and < 5 moe_%

#Step 10: case_when() ####

#case_when() allows you sort data into multiple categories

pa_income <- pa_income %>%
  mutate(reliability = case_when(
    moe_pct < 3 ~ "High confidence",   #The ~ separates the condition from the label
    moe_pct < 6 ~ "Moderate",
    TRUE        ~ "Low confidence"     #TRUE at the end means “everything that didn’t match the above criteria”
  ))

count(pa_income, reliability)

#High confidence    26
#Low confidence      7
#Moderate           34


#Step 11: Is the margin of error bigger in small counties?

pa_two <- get_acs(
  geography = "county",
  variables = c("B19013_001", "B01003_001"),
  state = "PA", year = 2023, survey = "acs5"
)

pa_two

#there are 134 rows instead of 67 because there are 2 different variables per county (thus double the rows)

#can't compare the moe because there are 2 observations per county - need to reorganize the table ("change the shape")

pa_wide <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001",
                pop    = "B01003_001"),
  state = "PA", year = 2023, survey = "acs5",
  output = "wide"
)

pa_wide

#E is for estimate, M is for moe

pa_wide %>%
  mutate(moe_pct = incomeM / incomeE * 100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, incomeE, moe_pct) %>%
  head(10)

#Smaller counties do not necessarily have larger margins of error
