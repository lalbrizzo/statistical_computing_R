###########################################################
##############  TWO SAMPLES Z TEST   ######################
###########################################################


library(palmerpenguins) #data 
library(ggplot2) #plotting 
library(dplyr) # operators
library(patchwork) #combine plots

#define dataframe on Biscoe and Dream island
dat_Biscoe <- penguins %>%
  select(island, sex, species) %>%
  filter(island == 'Biscoe')

dat_Dream <- penguins %>%
  select(island, sex, species) %>%
  filter(island == 'Dream')


#z-test for equality of sex on Biscoe vs Dream
female <- c(80, 61)
total_penguins <- c(168,124)

#p value >> 0.88 we cannot reject H0
#(sex proportion is similar on two islands)
print(prop.test(female, total_penguins))

#testing for equality of species on Biscoe vs Dream
adelie <- c(44, 56)
total_penguins <- c(168,124)

#p value << 0.05  we can reject H0 with high confidence 
#(specie proportion is not similar on two islands)
print(prop.test(adelie, total_penguins))

#plotting for Biscoe
levels(dat_Biscoe$species) <- c(levels(dat_Biscoe$species), "Other specie") 
dat_Biscoe$species[dat_Biscoe$species != 'Adelie'] <- "Other specie"


species_Biscoe <- ggplot(dat_Biscoe) +
  aes(x = species ) +
  geom_bar(fill = "#0c4c8a") +
  theme_minimal() +
  labs(title = "Biscoe")

#plotting for Dream
levels(dat_Dream$species) <- c(levels(dat_Dream$species), "Other specie") 
dat_Dream$species[dat_Dream$species != 'Adelie'] <- "Other specie"


species_Dream <-  ggplot(dat_Dream) +
  aes(x = species) +
  geom_bar(fill = "#8B0000") +
  theme_minimal() +
  labs(title = "Dream")  + 
  scale_fill_brewer(palette = "Set1", labels = c("a", "b")) +
  scale_fill_discrete(labels=c("a", "b"))

#combine plots and setting limits on axes
species_Biscoe + species_Dream & scale_y_continuous(limits = c(0, 125))

