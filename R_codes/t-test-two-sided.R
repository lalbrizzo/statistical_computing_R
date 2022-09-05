###########################################################
##############  TWO SAMPLES T TEST   ######################
###########################################################

library(palmerpenguins) #data 
library(ggplot2) #plotting 
library(BSDA) #statistical tests 
library(dplyr) # operators
library(patchwork) #combine plots

#uploading penguins datasets for 2 particular species
dat_adelie_chinstrap <- penguins %>%
  select(species,body_mass_g) %>%
  filter(species %in% c("Adelie", "Chinstrap"))

#uploading penguins datasets for 2 particular species
dat_adelie_gentoo <- penguins %>%
  select(species,body_mass_g) %>%
  filter(species %in% c("Adelie", "Gentoo"))


par(mfrow = c(1, 2)) # combine plots

#boxplots: used to have an intuition of the possible results
# and check the equality of variances hypothesis 
# the latter can be done also using Levene test formally
ad_chin <- ggplot(dat_adelie_chinstrap) +
  aes(x = species, y = body_mass_g, color = species) +
  geom_boxplot() +
  theme(legend.position = "none")


ad_gentoo <- ggplot(dat_adelie_gentoo) +
  aes(x = species, y = body_mass_g, color = species) +
  geom_boxplot() +
  theme(legend.position = "none")



print(dat_adelie_chinstrap)
#Welch Two Sample t-test 2 unequal and unknown variances
test_ad_chin <- t.test(body_mass_g ~ species,
               data = dat_adelie_chinstrap,
               var.equal = FALSE,
               alternative = "less"
)

cat('p value adelie chinstrap = ', test_ad_chin$p.value)

#Welch Two Sample t-test 2 unequal and unknown variances
test_ad_gentoo <- t.test(body_mass_g ~ species,
               data = dat_adelie_gentoo,
               var.equal = FALSE,
               alternative = "less"
)
#print on screen p values for the two test 
cat('p value adelie chinstrap = ', test_ad_chin$p.value)
cat('p value adelie gentoo = ', test_ad_gentoo$p.value)

#boxplots to compare species on different islands 
ad_chin + ad_gentoo
