library(MASS) 
install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

data_1 <- read.csv(file.choose())
data_1
print(str(data_1))
summary(data_1)

# Performing one-way anova on Stereotype Percentage and Gender
one.way <- aov(Stereotype.Percentage ~ Gender, data = data_1)
summary(one.way)

# Performing one-way anova on Spending and Gender
one.way <- aov(Spending ~ Gender, data = data_1)
summary(one.way)



# Performing two-way anova
two.way <- aov(Empowerment.Percentage ~ Gender+Age , data = data_1)
summary(two.way)
two.way<- aov(Empowerment.Percentage ~ Gender * Age, data = data_1)
two.way <- aov(Empowerment.Percentage ~ Gender + Age + Gender:Age, data = data_1)
summary(two.way)

# Performing one-way anova Empowerment Percentage and Gender
one.way <- aov(Empowerment.Percentage ~ Gender, data = data_1)
summary(one.way)

"
# Performing one-way anova on age and Stereotype Percentage
one.way <- aov(Stereotype.Percentage ~ Age, data = data_1)  
summary(one.way)
# Performing one-way anova on age and Empowerment Percentage
one.way <- aov( Empowerment.Percentage~ Age, data = data_1)
summary(one.way)
one.way <- aov(Stereotype ~ Education, data = data_1)
summary(one.way)"



one.way <- aov( Stereotype.Percentage ~ Education, data = data_1)
summary(one.way)











#char <- as.character(sample(c(19, 11, 46, 21), 10, replace = TRUE))
#char
#char <- data_1$Stereotype.Percentage
#typeof(data_1$Stereotype.Percentage)
#num <- as.numeric(char)
#num
#is.numeric(num)

one.way <- aov( Stereotype.Percentage ~ Education, data = data_1)
summary(one.way)






















# Create a data frame from the main data set.
adv_data = data.frame(data_1$Education,data_1$Empowerment.Percentage)
adv_data

# Create a contingency table with the needed variables.           
adv_data = table(data_1$Education,data_1$Empowerment.Percentage) 

print(adv_data)
# applying chisq.test() function
chisq.test(adv_data, simulate.p.value = TRUE)
levels(adv_data)

adve_data = data.frame(data_1$Gender,data_1$Reinforcing)
adve_data = table(data_1$Gender,data_1$Reinforcing) 
print(adve_data)
# applying chisq.test() function for Gender and Reinforcing
chisq.test(adve_data, simulate.p.value = TRUE)


test <- fisher.test(adv_data)
test
test$expected

