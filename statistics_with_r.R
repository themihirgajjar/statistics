#Statistics Finals
#The file ‘bacteria’ shows data on the presence of the bacteria H. influenzae in children as a function of treatment. 
# Read description of file and variables in the MASS package before answering the questions!

# a. The variable ap shows whether children were receiving a drug (a, active) or only a placebo (p, placebo). 
# Is there a difference in the proportion of children infected with bacteria between groups a and p?
  
bacteria
?bacteria
table(bacteria$ap, bacteria$y)
prop.test(c(93,84), c(124,96))

#Conclusion
#P-value = 0.03177
#There is a difference between the proportion of children infected with bacteria between groups ‘a’ and ‘p’. 
#With 95% confidence, it can be said that there is a difference of -0.23516294 to -0.01483706 between the proportions. 
#Since 0 Difference is not included within the Confidence Interval, therefore, there is a difference.

#b. Now look at the variable trt (treatment). 
#Is there a difference in presence of bacteria across the three different treatments?

table(bacteria$trt, bacteria$y)
chisq.test(matrix(c(84, 44, 49, 12, 18, 13), nrow=3)) 

#Ans: With p-value of 0.03582, there is a significant difference between the tested groups.

#----------

#The file birthwt (package MASS) has data on risk factors associated with low birth weight. 
#Read file description before answering!
#Question: is maternal weight before pregnancy (variable lwt) a good predictor of birth weight (bwt)?
#a.	Run a linear regression of birth weight (the outcome y) on maternal weight (the predictor x). 
#What is the result? Is the regression signficant? Justify your answer.

birthwt
?birthwt
range(birthwt$lwt, birthwt$bwt, na.rm = T)
hist(as.numeric(birthwt$lwt, birthwt$bwt))

weightreg <- lm(birthwt$bwt ~ birthwt$lwt)
weightreg
summary(weightreg)

#Conclusion: With significant p-value of the intercept and the slope, 
#we reject the null hypothesis that a = 0 and b = 0. 

#b.	What is the effect size of maternal weight on birth weight? What does the slope say?
#Ans: The regression is significant. An extra unit of weight of the mother, increases the likelihood of the birth weight of the baby by 4.429 units.

##c.	How much variance in birth weight is explained by maternal weight? Provide the answer in percentage (%).
#Ans: Multiple R-squared:  0.0345
#It is a low value. Around 3.45% variance in birth weight is explained by maternal weight.

#d. Plot birth weight against maternal weight. Add the regression line. Then attach the plot below.

#visualising
plot(birthwt$bwt ~ birthwt$lwt, xlab = "Maternal Weight", ylab = "Birth Weight", pch = 4, breaks = seq(0, 300, 50))
table(birthwt$bwt ~ birthwt$lwt, na.rm = T)
#adding regression line
abline(weightreg, col = "red")

#----------

#The data frame anorexia (package MASS) has 72 rows and 3 columns. it shows data on weight change in young female anorexia patients as a function of treatment. 
#Read file description before answering!
#The question is: what was the best treatment for anorexia based on that sample? Control, CBT or FT?

#a.	To answer that question, which tests do you need to use? Justify the answer.

anorexia
table(anorexia$Treat)
control <- subset(anorexia, Treat == "Cont")
cbt <- subset(anorexia, Treat == "CBT")
ft <- subset(anorexia, Treat == "FT")

#Ans: Small sample size. Must use a non-parametric test.

#b. Run the required tests. Based on the results, which treatment would you recommend? 
#Show outputs to justify your answer.

#Kruskal-Wallis test
summary(anorexia$Treat) #Small Sample Size
tapply(control$Prewt, control$Postwt, mean, na.rm=T)

kruskal.test(anorexia$Postwt, anorexia$Treat)

pairwise.wilcox.test(anorexia$Postwt, anorexia$Treat)

tapply(control$Prewt, control$Postwt, mean)

#Ans: Although both the treatments had a significant difference in weights, 
#the Family Treatment (FT) group saw the most significant difference from the control group in weight with a p-value of 0.0021.

#----------

#File coop in package MASS has information on the concentration of the substance Analyte (variable Conc). 
#We want to know if there are differences in concentration of the substance across different labs (variable Lab).

#a. Which test do you use? Justify your answer.

coop
summary(coop$Lab)
hist(coop$Conc)
shapiro.test(coop$Conc)

#Ans: Based on visual inspection and Shapiro Wilk Normality Test, the distribution is not normal. 
#Therefore, a non-parametric test must be used. Since we are testing for differences in concentration of Analyte among more than 2 groups, 
#Kruskal-Wallis and Pairwise Wilcox Tests are proposed.

#b. Are there any differences across labs?

kruskal.test(coop$Conc ~ coop$Lab)

# Overall, there seems to be significant difference in concentration of Analyte among the labs.

pairwise.wilcox.test(coop$Conc, coop$Lab)

#Conclusion
#Upon running a pairwise comparison of the groups with a Pairwise Wilcox Test, 
#it can be seen that there are groups with varying degrees of significance of differences.

#c. If so, discuss the pattern.
#Ans: Lab 4 seems to be the only group which is displaying the difference of Analyte concentration among 6 labs. 
#In fact, no other lab shows any significant difference except Lab 4. 
#Based on this pattern, it can be assumed that there might be something odd with Lab 4 like an issue with measuring equipment, method of measurement etc. 

#----------

#This question is also based on the file birthwt. 
#The binary variable low indicates whether a child was born with low birth weight (low=1) or not (low=0).
#We want to test whether the variables (1) maternal age (age), (2) maternal weight (lwt), and (3) smoking (smoke) 
#affect the probability of low birth weight.

#a. Run a logistic regression of low on the three factors above, plus all interactions. Paste the output representing the maximal model; 
#that's the regression before optimisation (i.e. the original model). What is the AIC of the maximal model?

birthwt
attach(birthwt)

model.bwt <- glm(low ~ age*lwt*smoke, binomial, data= birthwt)
summary(model.bwt)

#Ans: The AIC of this model is 235.32

#b. Now optimise the model using the function step. 
#Paste the resulting minimal adequate model (the optimised model). 
#Don't paste all stages of the step process!!! Only include the summary table with estimated intercepts and coefficients of the optimised model. 
#What is the AIC of the optimised model?

step(model.bwt)
summary(step(model.bwt))

#Ans: The AIC of the Optimised Model is 230.34

#c. Finally, interpret the meaning of all factors left in the optimal model (you don't need to interpret the intercept)

#Ans:
#b1 is significant (p-value= 0.0287)
#b2 is significant (p-value= 0.0372)

#Odds Ratio for Maternal Weight (lwt)= e^b1 = exp(-0.01332) = 0.9867683
#Odds Ratio for Smoking = e^b2 = exp(0.67667) = 1.967316

#Odds for lwt = e^a * e^b1 = 1.86265 * 0.9867683 = 1.838004
#Odds for smoke = e^a * e^b2 = 1.86265 * 1.967316 = 3.664421

#Probability = Odds / 1 + Odds
#Probability lwt = 1.838004/ 2.838004 = 0.6476397
#Probability smoke = 3.664421/ 4.664421 = 0.7856111

#Conclusions:
#Low Maternal Weight increases the risk of low weight at birth by 65.76%.
#Smoking increases the risk of low weight at birth by 78.6%.


