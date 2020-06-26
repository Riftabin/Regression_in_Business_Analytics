library(tidyverse)
library(plot3D)

# set working directory

#-----------P1---------------------------

d1 <- read.csv("data1.csv", header = T)
#View(d1)
head(d1)
glimpse(d1)

#EDA
ggplot(d1)+
  geom_point(aes(x = DJIA, y = S_and_P), stroke = 3, color = 'blue')+
  ggtitle("DJIA vs. S&P 500")+
  theme_classic()

#1a
linReg1 <- lm(S_and_P ~  DJIA , d1)
linReg1$coefficients

summary(linReg1)

ggplot(d1)+
  geom_point(aes(x = DJIA, y = S_and_P), stroke = 3, color = 'blue')+
  # geom_abline(intercept = linReg1$coefficients[1], slope =   linReg1$coefficients[2],lwd = 1.5,color = 'red')+
  geom_smooth(aes(x = DJIA, y = S_and_P), method = 'lm', lwd = 1.5, color = 'red')+ 
  ggtitle("The Regression line with confidence interval")+
  theme_classic()


#1b
confint(linReg1, level = 0.95)

#1c
confint(linReg1, level = 0.95)

#1d
S_and_P.pred <- predict(linReg1, newdata = data.frame(DJIA = 13500))
S_and_P.pred


#-----------P2---------------------------

d2 <- read.csv("data2.csv", header = T)
#View(d2)
head(d2)
glimpse(d2)

#EDA
ggplot(d2)+
  geom_point(aes(x = Television_Adertising,
                 y = Weekly_Gross_Revenue), 
             stroke = 3, color = 'blue')+
  ggtitle("Television Advertising vs. Weekly Gross Revenue") +
  theme_classic()

#/
ggplot(d2)+
  geom_point(aes(x = Newspaper_Advertising,
                 y = Weekly_Gross_Revenue), 
             stroke = 3, color = 'darkgreen')+
  ggtitle("Newspaper Advertising vs. Weekly Gross Revenue") +
  theme_classic()


#2a
linReg2 <- lm(Weekly_Gross_Revenue ~  Television_Adertising , d2)
linReg2$coefficients

ggplot(d2)+
  geom_point(aes(x = Television_Adertising, y = Weekly_Gross_Revenue), 
             stroke = 3, color = 'blue')+
  geom_smooth(aes(x = Television_Adertising, y = Weekly_Gross_Revenue), 
              method = 'lm', lwd = 1.5, color = 'red')+ 
  ggtitle("The Regression line of Weekly_Gross_Revenue ~ Television_Adertising with confidence interval")+
  theme_classic()


summary(linReg2)

#2b

d2$ypredtv <- predict(linReg2, newdata = d2)

ggplot(d2)+
  geom_point(aes(x = Television_Adertising, y = Weekly_Gross_Revenue),
             stroke = 2.5, color = 'blue') +
  geom_point(aes(x = Television_Adertising, y = ypredtv),
             stroke = 3, color = 'red')+
  ggtitle('Weekly_Gross_Revenue - actual and model predicted')+
  theme_classic()

d2 <- d2[, 1:4]


#2c
linReg3 <- lm(Weekly_Gross_Revenue ~  Television_Adertising +
                Newspaper_Advertising , d2)
linReg3$coefficients

d2$ypred_tv_np <- predict(linReg3, newdata = d2)
x <- d2$Television_Adertising
y <- d2$Newspaper_Advertising
za <- d2$Weekly_Gross_Revenue
zb <- d2$ypred_tv_np

par(mfrow = c(1, 2))
scatter3D(x, y, za, colvar = NULL, bty = "g",
          col = "blue", pch = 16, cex = 2,
          phi = 10,
          xlab = 'TV Adv',
          ylab = 'Newspaper Adv',
          zlab = 'Weekly_Gross_Revenue',
          main = '*Actual* ~Weekly_Gross_Revenue')


scatter3D(x, y, zb, colvar = NULL, bty = "g",
          col = "red", pch = 16, cex = 2,
          phi = 10,
          xlab = 'TV Adv',
          ylab = 'Newspaper Adv',
          zlab = 'Weekly_Gross_Revenue',
          main = '*Predicted* ~Weekly_Gross_Revenue')
#/
par(mfrow = c(1, 1))
scatter3D(x, y, zb, colvar = NULL, bty = "g", type = "l",
          col = "red", lwd = 3.5,
          phi = 10,
          xlab = 'TV Adv',
          ylab = 'Newspaper Adv',
          zlab = 'Weekly_Gross_Revenue',
          main = 'The Regression Line')
d2 <- d2[, 1:4]


summary(linReg3)


#2e
linReg4 <- lm(Weekly_Gross_Revenue ~  Newspaper_Advertising , d2)
summary(linReg4)

ggplot(d2)+
  geom_point(aes(x = Newspaper_Advertising, y = Weekly_Gross_Revenue), 
             stroke = 3, color = 'darkgreen')+
  geom_smooth(aes(x = Newspaper_Advertising, y = Weekly_Gross_Revenue), 
              method = 'lm', lwd = 1.5, color = 'red')+ 
  ggtitle("The Regression line of Weekly_Gross_Revenue ~ Newspaper_Advertising with confidence interval")+
  theme_classic()


















