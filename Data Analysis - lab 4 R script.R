setwd("C:/Users/limre/Desktop/QMSS/Semester 1/5015 Data Analysis for Soc Sci/Assignments/Lab 4/Data")
data= read.csv("trends-gss.csv")
# removing rows with missing cells for real income, years of education, race
data=data[!is.na(data$realinc), ]
data=data[!is.na(data$educ), ]
data=data[!is.na(data$race), ]

# 1. Run a simple regression, with at least two Xs in it, and interpret your results. (Did the results fit your expectations? Why? Why not?)
Y=log(data$realinc)
X1=data$educ
X2=""
X2=ifelse(data$race==1,"White",X2)
X2=ifelse(data$race==2,"Black",X2)
X2=ifelse(data$race==3,"Other",X2)

reg1=lm(Y~X1+X2, data=data)
summary(reg1)

##### I ran a regression of log real income on years of education and race. I expected education and log real income to be positively associated. For race, I expected individuals with "White" as their race to earn more on average compared to individuals with "Black" as their race. The results from the regression fully fit with my expectations. An additional year of education is associated with an 11.60 percentage point increase in real income on average, and is statistically significant at the 1% level. For race, with "Black" as the omitted reference category, people who are "White" earn 46.59 percentage points higher income on average, and this association is statistically significant at the 1% level. Individuals in the "Other" race group also earn 26.89 percentage points more real income on average than individuals in the "Black" race group, and this relationship is also statistically significant at the 1% level. 

# 2. Add an interaction term to that model that you think might moderate the original relationship between X1 and X2. Explain why you think an interaction might be present and in what direction it would work. Explain your results. Did it work out? Yes? No?

reg2=lm(Y~X1+X2+X1:X2, data=data)
summary(reg2)

##### I think that an interaction might be present between years of education and race as I felt that the income returns to educational attainment might be mediated by an individual's race. Specifically, I thought that individuals in the "White" race group might possibly benefit more from an additional year of education attainment compared oto individuals in the other race groups if they also had more social capital and networks to capitalize on their educational attainment and secure better paying jobs.

##### Based on the results, the direction of the interaction goes in the opposite direction from what I had expected. "Black" race individuals are the ones that the most from an additional year of education.
##### With "Black" as the omitted category in the interaction term between race and years of education, "White" individuals get 1.17 percentage points less than "Black" individuals for each year of additional education attained on average, and this is significant at the 5% level.
##### Individuals in the "Other" race group get 1.50 percentage points less than "Black" individuals for an additional year of education on average, but this association is only statistically significant at the 10% level.

##### Hence, the results did not support my initial expectations and it is actually "Black" individuals who have greater income returns from additional years of education than "White" individuals.

# 3. Extra Credit: Plot the relationship found in the interaction.
library(ggplot2)

Interactions_graph=ggplot(data, aes(x = X1, y = Y, color = X2))+geom_smooth(method = "lm", se = FALSE)+labs(x = "Education", y = "Log Real Income")+  scale_color_discrete(name = "Race")

Interactions_graph


Interactions_graph=ggplot(data, aes(x = X1, y = Y, color = X2)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Education", y = "Log Real Income") +
  scale_color_discrete(name = "Race")

Interactions_graph
