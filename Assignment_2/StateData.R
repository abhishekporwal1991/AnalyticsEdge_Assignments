data(state)
statedata = cbind(data.frame(state.x77),state.abb,state.area,state.center,state.division,state.name,state.region)
str(statedata)
plot(statedata$x,statedata$y)
tapply(statedata$HS.Grad,statedata$state.region,mean,na.rm=TRUE)
boxplot(statedata$Murder ~ statedata$state.region)
outlier = subset(statedata,state.region == "Northeast")
outlier$state.abb
outlier$Murder
LifeExp = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata)
summary(LifeExp)
plot(statedata$Income, statedata$Life.Exp)
cor(statedata$Income, statedata$Life.Exp)
LifeExp = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost, data = statedata)
summary(LifeExp)
LifeExp = lm(Life.Exp ~ Population + Income + Murder + HS.Grad + Frost, data = statedata)
summary(LifeExp)
LifeExp = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(LifeExp)
LifeExp = lm(Life.Exp ~ Murder + HS.Grad + Frost, data = statedata)
summary(LifeExp)
LifeExp = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(LifeExp)
PredLife = predict(LifeExp)
sort(PredLife)
which.min(statedata$state.name)
statedata[1]
statedata$state.name[1]
which.min(statedata$Life.Exp)
statedata$state.name[40]
which.max(statedata$Life.Exp)
statedata$state.name[11]
error = statedata$Life.Exp - PredLife
error
sort(error)