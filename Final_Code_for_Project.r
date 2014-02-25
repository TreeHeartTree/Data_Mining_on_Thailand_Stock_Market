library(clusterSim)
x = read.csv("raw data2.csv",header=T)
summary(x)

x<-x[1:164,]
x <- data.Normalization(x, "n5")
y<-x[,248]
x.train=x[1:125,1:247]
y.train=y[1:125]
x.test=x[126:164,1:247]
y.test=y[126:164]

summary(x.train)
summary(y.train)
summary(x.test)
summary(y.test)

par(mfrow=c(4,4))
for (i in 1:16) {boxplot(x.train[,i], main=names(x.train[i]), type="l")}

#Assessing outliers#
par(mfrow=c(3,4))
for (i in 1:length(x.train)) {
        boxplot(x.train[,i], main=names(x.train[i]), type="l")

}

par(mfrow=c(4,4))
for (i in 1:16) {boxplot(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 17:32) {boxplot(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 33:48) {boxplot(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 49:64) {boxplot(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 65:80) {boxplot(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 81:96) {boxplot(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 97:112) {boxplot(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 113:128) {boxplot(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 129:144) {boxplot(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 145:160) {boxplot(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 161:176) {boxplot(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 177:193) {boxplot(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 177:192) {boxplot(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 177:192) {boxplot(x.train[,i], main=names(x.train[i]), type="l")}


#Eliminate outlie


#check normality

par(mfrow=c(4,4))
for (i in 1:length(x.train.1)) {
        qqnorm(x.train.1[,i], main=names(x.train[i]))

}
par(mfrow=c(4,4))
for (i in 1:16) {qqnorm(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 17:32) {qqnorm(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 33:48) {qqnorm((x.train[,i], main=names(x.train[i]), type="l")}
for (i in 49:64) {qqnorm(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 65:80) {qqnorm(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 81:96) {qqnorm(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 97:112) {qqnorm(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 113:128) {qqnorm(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 129:144) {qqnorm(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 145:160) {qqnorm(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 161:176) {qqnorm(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 177:192) {qqnorm(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 193:208) {qqnorm(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 209:224) {qqnorm(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 225:240) {qqnorm(x.train[,i], main=names(x.train[i]), type="l")}
for (i in 241:256) {qqnorm(x.train[,i], main=names(x.train[i]), type="l")}

#stepwise#

fit <- lm(y.train ~ .,data=x.train[1:100])
step <- stepAIC(fit, direction="both")
summary(step)
fit <- lm(y.train ~ .,data=x.train[101:200])
step <- stepAIC(fit, direction="both")
summary(step)
fit <- lm(y.train ~ .,data=x.train[201:247])
step <- stepAIC(fit, direction="both")
summary(step)


#Significant variable
x.train.step=data.frame(x.train[38],x.train[88],x.train[100],x.train[101],x.train[104],x.train[106],x.train[108],x.train[109],x.train[110],x.train[111],x.train[113],x.train[114],x.train[115],x.train[116],x.train[117],x.train[118],x.train[119],x.train[123],x.train[124],x.train[125],x.train[127],x.train[130],x.train[131],x.train[135],x.train[143],x.train[144],x.train[145],x.train[146],x.train[149],x.train[150],x.train[152],x.train[156],x.train[161],x.train[168],x.train[169],x.train[172],x.train[175],x.train[177],x.train[180],x.train[181],x.train[182],x.train[183],x.train[184],x.train[187],x.train[188],x.train[189],x.train[190],x.train[191],x.train[192])
x.test.step=data.frame(x.test[38],x.test[88],x.test[100],x.test[101],x.test[104],x.test[106],x.test[108],x.test[109],x.test[110],x.test[111],x.test[113],x.test[114],x.test[115],x.test[116],x.test[117],x.test[118],x.test[119],x.test[123],x.test[124],x.test[125],x.test[127],x.test[130],x.test[131],x.test[135],x.test[143],x.test[144],x.test[145],x.test[146],x.test[149],x.test[150],x.test[152],x.test[156],x.test[161],x.test[168],x.test[169],x.test[172],x.test[175],x.test[177],x.test[180],x.test[181],x.test[182],x.test[183],x.test[184],x.test[187],x.test[188],x.test[189],x.test[190],x.test[191],x.test[192])



#optimum --> number of factor
library(nFactors)
ev <- eigen(cor(x.train.step)) # get eigenvalues
ap <- parallel(subject=nrow(x.train.step),var=ncol(x.train.step),
  rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

#factor analysis 

library(psych)
library(GPArotation)
fit <- principal(x.train.step, nfactors=3, rotate="varimax")
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
fit$scores # the principal components
biplot(fit)

#regression train#
ftdata <-data.frame(fit$scores)
fitl <- lm(y.train ~ ., data=ftdata)
summary(fitl)

par(mfrow=c(2,2))
plot(fit)

plot(y.train, type="l",col="1")
lines(fitl$fitted.values,col="2")
c<-cbind(y.train, fitl$fitted.values)
fl<-as.matrix(fit$weights)
xm<-as.matrix(x.test.step)
z<-xm %*% fl


ftdatatest <-data.frame(z)
y.pred <- predict(fitl,ftdatatest)
summary((y.pred - y.test)^2)
p<-cbind(y.test,y.pred)
library(SDMTools)

library(hydroGOF)
rmse(y.pred, y.test)

# diagnostic plots 
 layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
 plot(fitl)
