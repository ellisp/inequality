library(dplyr)


# only needed for exploration
library(ggplot2)
library(scales)
library(ineq)
library(IC2)

nz <- read.csv("raw_data/nzis11-cart-surf.csv", check.names = FALSE, stringsAsFactors = FALSE)



#------------labels for sex------------
sex_labs <- data.frame(value = 1:2, sex_meaning = c("male", "female"))

nz <- merge(nz, sex_labs, by.x = "sex", by.y = "value", all.x = TRUE)


#-------------labels for ethnicity----------------
ethnic_labs <- data.frame(value = c(1,2,3,4,5,6,9),
                          ethnicity_meaning = c(
                             "European",
                             "Maori",
                             "Pacific Peoples",
                             "Asian",
                             "Middle Eastern/Latin American/African",
                             "Other Ethnicity",
                             "Residual Categories"))

# note - they can give two ethnicities, and they've just pasted them together.  Resolve later
table(nz$ethnicity)
nz <- merge(nz, ethnic_labs, by.x = "ethnicity", by.y = "value", all.x = TRUE) %>%
   mutate(ethnicity_meaning = ifelse(is.na(ethnicity_meaning), "Multiple", as.character(ethnicity_meaning)))


#-------------labels for age------------------
nz$age_meaning <- ifelse(nz$age == 65, "65+", paste0(nz$age, "-", nz$age + 4))


#------------labels for occupation---------------
occ_labs <- data.frame(value = 1:10,
                       occupation_meaning = c(
                          "Managers",
                          "Professionals",
                          "Technicians and Trades Workers",
                          "Community and Personal Service Workers",
                          "Clerical and Adminsitrative Workers",
                          "Sales Workers",
                          "Machinery Operators and Drivers",
                          "Labourers",
                          "Residual Categories",
                          "No occupation"                          
                          ))

nz <- merge(nz, occ_labs, by.x = "occupation", by.y = "value", all.x = TRUE) 

#---------------qualification------------
qual_labs <- data.frame(value = 1:5,
                       qual_meaning = c(
                          "None",
                          "School",
                          "Vocational/Trade",
                          "Bachelor or Higher",
                          "Other"
                       ))

nz <- merge(nz, qual_labs, by.x = "qualification", by.y = "value", all.x = TRUE) 


#------------make income positive----------
nz <-  nz %>%
   mutate(income_positive = ifelse(income > 1, income, 1))


#------------save----------
save(nz, file = "data/nz.rda")

#================exploring=================
nz %>%
   ggplot(aes(y = income, x = ethnicity_meaning)) +
   geom_boxplot() +
   scale_y_log10() +
   coord_flip()


nz %>%
   mutate(income = as.numeric(income)) %>%
   group_by(ethnicity_meaning) %>%
   summarise(mean_income = round(mean(income), 1), 
             median_income = median(income),
             ratio = round(mean_income / median_income, 2),
             sd_income = sd(income),
             sample = length(income),
             Gini = round(ineq(income), 3))  %>%
   arrange(Gini)


model <- lm(income ~ sex_meaning + ethnicity_meaning + ethnicity_meaning + age_meaning, data = nz)
summary(model) # dodgy because non-normal distribution

ineq(nz$income)

# don't know why this doesn't work
decompAtkinson(nz$income_positive, z = nz$age, epsilon = 3)
