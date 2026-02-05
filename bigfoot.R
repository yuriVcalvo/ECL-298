library(terra)
library(rspat)
bf <- spat_data("bigfoot")

plot(bf[,1:2],col="red",cex=0.3)
wrld <- world()
lines(wrld)
wc <-worldclim_global("bio",res=10)
plot(wc[[c(1,12)]],nr=2)
bfc <- extract(wc, bf[,1:2])
bfc <- bfc[,-1]
plot(bfc[ ,"wc2.1_10m_bio_1"], bfc[, "wc2.1_10m_bio_12"], col="red",
     xlab="Annual mean temperature (°C)", ylab="Annual precipitation (mm)")
ext_bf <- ext(vect(bf[, 1:2])) + 1
ext_bf

set.seed(0)
window(wc) <- ext_bf
bg <- spatSample(wc, 5000, "random", na.rm=TRUE, xy=TRUE)
head(bg)

plot(bg[, c("x", "y")])
bg <- bg[,-c(1:2)]
plot(bg[,1], bg[,12], xlab="Annual mean temperature (°C)",
     ylab="Annual precipitation (mm)", cex=.8)
points(bfc[,1], bfc[,12], col="red", cex=.6, pch="+")
legend("topleft", c("observed", "background"), col=c("red", "black"), pch=c("+", "o"), pt.cex=c(.6, .8))

bfe <- bfc[bf[,1] > -102, ]
#western points
bfw <- bfc[bf[,1] <= -102, ]
dw <- rbind(cbind(pa=1, bfw), cbind(pa=0, bg))
de <- rbind(cbind(pa=1, bfe), cbind(pa=0, bg))
dw <- data.frame(dw)
de <- data.frame(na.omit(de))
dim(dw)
## [1] 6224   20
dim(de)
## [1] 6866   20
library(rpart)
cart <- rpart(pa~., data=dw)
printcp(cart)
plotcp(cart)
cart <- rpart(pa~., data=dw, cp=0.02)
library(rpart.plot)
rpart.plot(cart, uniform=TRUE, main="Regression Tree")

x <- predict(wc, cart)
x <- mask(x, wc[[1]])
x <- round(x, 2)
plot(x, type="class", plg=list(x="bottomleft"))
set.seed(123)
i <- sample(nrow(dw), 0.2 * nrow(dw))
test <- dw[i,]
train <- dw[-i,]
fpa <- as.factor(train[, 'pa'])
library(randomForest)
crf <- randomForest(train[, 2:ncol(train)], fpa)
crf
varImpPlot(crf)
trf <- tuneRF(train[, 2:ncol(train)], train[, "pa"])
mt <- trf[which.min(trf[,2]), 1]
mt
rrf <- randomForest(train[, 2:ncol(train)], train[, "pa"], mtry=mt, ntree=250)
plot(rrf)
rp <- predict(wc, rrf, na.rm=TRUE)
plot(rp)
library(predicts)
eva <- pa_evaluate(predict(rrf, test[test$pa==1, ]), predict(rrf, test[test$pa==0, ]))
eva
plot(eva, "ROC")
par(mfrow=c(1,2))
plot(eva, "boxplot")
plot(eva, "density")
tr <- eva@thresholds
tr
plot(rp > tr$max_spec_sens)
rc <- predict(wc, crf, na.rm=TRUE)
plot(rc)
rc2 <- predict(wc, crf, type="prob", na.rm=TRUE)
plot(rc2, 2)
eva2 <- pa_evaluate(predict(rrf, de[de$pa==1, ]), predict(rrf, de[de$pa==0, ]))
eva2
par(mfrow=c(1,2))
plot(eva2, "ROC")
plot(eva2, "boxplot")
plot(rc)
points(bf[,1:2], cex=.25)
window(wc) <- NULL
pm <- predict(wc, rrf, na.rm=TRUE)
plot(pm)
lines(wrld)

