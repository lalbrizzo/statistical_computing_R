###########################################################
##########   ANOVA & POST-HOC TEST   ######################
###########################################################

library(palmerpenguins)
library(tidyverse)
library(ggplot2)
library(dplyr)

#select subset of penguis dataframe for more clarity
dat <- penguins %>%
  select(species, flipper_length_mm)

summary(dat)


#visualizing the flipper length per specie
ggplot(dat) +
  aes(x = species, y = flipper_length_mm, color = species) +
  geom_jitter() +
  theme(legend.position = "none")


#testing normality (even if samples are big enough)
#computing ANOVA to check if residuals are normally distributed
res_aov <- aov(flipper_length_mm ~ species,
               data = dat
)

# histogram
hist(res_aov$residuals)

#boxplot allow to see that averages among groups are different
#and visually check that there are no outliers

boxplot(flipper_length_mm ~ species,
        data = dat
)

###########################################################
##################   ANOVA   ##############################
###########################################################

#compute some descriptive statistics (mean and std) for the groups

aggregate(flipper_length_mm ~ species,
          data = dat,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)

#same but using groupby function

group_by(dat, species) %>%
  summarise(
    mean = mean(flipper_length_mm, na.rm = TRUE),
    sd = sd(flipper_length_mm, na.rm = TRUE)
  )


# 1st method for ANOVA
oneway.test(flipper_length_mm ~ species,
            data = dat,
            var.equal = TRUE # assuming equal variances
)


# 2nd method for ANOVA (more info)
res_aov <- aov(flipper_length_mm ~ species,
               data = dat
)

#ANOVA summary for this method. In this particular case
# groups are sign. different since p is very small
summary(res_aov)

###########################################################
##################   POST-HOC TESTS #######################
###########################################################


# We just proved that the 3 groups are different, we still do not know
# which one is different. We could perform several t-tests
# but the probability of observing one significant result
# due to chance gets quickly high even with just 3 groups.

#Some other analysis must be performed, sometimes called post-hoc 
#analysis 


#performing Turkey test to correct p-values 
Turkey_test <- TukeyHSD(res_aov)
#showing results, in particular p adj
Turkey_test

plot(Turkey_test)
