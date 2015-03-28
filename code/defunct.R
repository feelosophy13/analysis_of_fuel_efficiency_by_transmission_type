#### exploratory plotting
pairs(mtc, col = mtc$am + 1, 
      main = 'Pair-wise Scatter Plot')

plot(mtc$disp, mtc$mpg,
     pch = 21, bg = mtc$am + 1)

plot(mtc$hp, mtc$mpg,
     pch = 21, bg = mtc$am + 1)

plot(mtc$wt, mtc$mpg,
     pch = 21, bg = mtc$am + 1)



#### model building
## include all three highly correlated variables 
model0 <- lm(mpg ~ disp + hp + wt, data = mtc)
summary(model0)

## remove disp (since most it is redundant after including hp and wt)
model1 <- lm(mpg ~ wt + hp, data = mtc)
summary(model1)

## include ony wt
model2 <- lm(mpg ~ wt, data = mtc)  # better than model3
summary(model2)

## include only hp
model3 <- lm(mpg ~ hp, data = mtc)
summary(model3)

## include wt with am
model4 <- lm(mpg ~ wt * am, data = mtc)  # better than model5
summary(model4)

## include hp with am
model5 <- lm(mpg ~ hp * am, data = mtc)
summary(model5)

## include wt * am and hp * am
model6 <- lm(mpg ~ wt * am + hp * am, data = mtc)
summary(model6)

## include wt * am and hp (remove interaction between hp and am since it's not stat. sig.)
model7 <- lm(mpg ~ wt * am + hp, data = mtc)
summary(model7)



## build second-order model (one that contains interaction)
model_int <- lm(mpg ~ wt_centered * am, data = mtc)
summary(model_int)  # 81.5% of variance in data explained by the model

## model interpretation
# expected mpg of an automatic-transmission vehicle with an average weight: 19.236 (stat. sig.)
# expected change in mpg per 1-unit (1000 lb) change in weight: -3.786 (stat. sig.)
# expected change in mpg from automatic-transmission cars to manual-transmission cars (not stat. sig.)
# expected change in slope in regression models from automatic-transmission cars to manual-transmission cars (stat. sig.)

