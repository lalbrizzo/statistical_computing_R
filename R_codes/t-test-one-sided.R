library(palmerpenguins) #data 
library(ggplot2) #plotting 
library(BSDA) #statistical tests 
library(dplyr) # operators

#selecting one specie to have more homogeneous variable
dat <- penguins %>%
  select(species, body_mass_g,flipper_length_mm) %>%
  filter(species == 'Adelie')

summary(dat)

boxplot(dat$flipper_length_mm)

ggplot(dat) +
  aes(x = species, y = body_mass_g) +
  geom_boxplot(fill="#0c4c8a") +
  theme(legend.position = "none")

#One Sample t-test with almost correct mean, unknown variance
test_right <- t.test(dat$body_mass_g,
               mu = 3600,
               alternative = 'greater')

#we cannot reject the null hypothesis here (mu = mu_0)
test_right

#One Sample t-test with lower mean, unknown variance
test_false <- t.test(dat$body_mass_g,
               mu = 3000,
               alternative = 'greater')

#we reject the null hypothesis here (mu = mu_0)
test_false