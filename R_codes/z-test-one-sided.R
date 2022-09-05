###########################################################
##############  ONE SAMPLE Z TEST   #######################
###########################################################

library(palmerpenguins) #data 
library(ggplot2) #plotting
library(dplyr) # operators
library(patchwork) #combine plots

#testing if the props (species and sex) among penguins 
# are the equal to the ones that we postulate theoretically

dat_Biscoe <- penguins %>%
  select(species, island, body_mass_g) %>%
  filter((species %in% c('Adelie','Gentoo')) & (island == 'Biscoe') )

summary(dat_Biscoe)


test <- prop.test(
  x = 124, # number of successes (Gentoo)
  n = 168, # total number of trials (total num penguins)
  p = 0.5, # we test for prob = 0.5
  conf.level = 0.95 # confidence level 
)

# we do not reject the null hypothesis 
print(test)

#filter penguins using the %in% operator
dat_sex<- penguins %>%
  select(species, sex) %>%
  filter((sex %in% c('female','male'))  )

test_sex <- prop.test(
  x = 165, # number of successes (female)
  n = 333, # total number of trials (total num penguins)
  p = 0.5, # we test for prob = 0.5
  conf.level = 0.95 # confidence level 
)

# we do not reject the null hypothesis 
print(test_sex)


#Plot for sex proportion
sex_prop <- ggplot(dat_sex) +
  aes(x = sex) +
  geom_bar(fill = "#8B0000") +
  theme_minimal() +
  labs(title = "Biscoe island")
