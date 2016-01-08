phoneme <-  read.table("~/stat852/data/phoneme.csv", header=TRUE, sep=",", na.strings=" ")

gbm.miss.train <- rep(0,5)
nnet.miss.train <- rep(0,5)
svm.miss.train <- rep(0,5)

gbm.miss.test <- rep(0,5)
nnet.miss.test <- rep(0,5)
svm.miss.test <- rep(0,5)

gbm.miss.test.re <- rep(0,5)
nnet.miss.test.re <- rep(0,5)
svm.miss.test.re <- rep(0,5)

set.seed(67982193)


resamp <-  read.table("~/stat852/hw9/resamp_max.csv", header=TRUE, sep=",", na.strings=" ")

for(i in 1:5)
{
    x.r  <- phoneme[resamp[i,],2:257]
    y.r  <- phoneme[resamp[i,],258]

    x.p  <- phoneme[-unique(resamp[i,]),2:257]
    y.p  <- phoneme[-unique(resamp[i,]),258]

    x.1 <- rescale.set1(x.r,x.r)
    y.1 <- class.ind(phoneme[resamp,258])
    x.2 <- rescale.set1(x.r,x.p)
    y.2 <- class.ind(phoneme[-unique(resamp),258])

    library(MASS)
    phoneme.boost <- gbm(data=data.frame(x.r,class=y.r), class ~., distribution="multinomial", 
                         n.trees=2000, interaction.depth=siz,verbose=FALSE, shrinkage=sh,
                         bag.fraction=rati, cv.folds=0,n.cores=48)

    pred.boost.test <-  predict(phoneme.boost, newdata=x.p, n.trees=2000, type="response")
    pred.boost.train <-  predict(phoneme.boost, newdata=x.r, n.trees=2000, type="response")

    class.boost.test <- kernels[apply(pred.boost.test[,,1], 1, which.max)]
    class.boost.train <- kernels[apply(pred.boost.train[,,1], 1, which.max)]

    mspe.boost  <- mean(ifelse(class.boost.test  == y.p, yes=0, no=1))
    smse.boost  <- mean(ifelse(class.boost.train == y.r, yes=0, no=1))

    phoneme.svm <- svm(data=data.frame(x.r,type=y.r), type ~ ., kernel="polynomial", gamma=gammap, cost=costo, degree=degreo, coef0 = coefo)


    pred.svm.train <- predict(phoneme.svm, newdata=x.r)
    smse.svm  <- mean(ifelse(pred.svm.train == y.r, yes=0, no=1))

    pred.svm.test <- predict(phoneme.svm, newdata=x.p)
    mspe.svm  <- mean(ifelse(pred.svm.test == y.p, yes=0, no=1))


    nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=s, decay=d, maxit=500, trace=FALSE,softmax=TRUE)

    pred.nn.test <- predict(nn,newdata = x.2,type="class")
    pred.nn.train <- predict(nn,newdata = x.1,type="class")

    smse.nn <-  mean(ifelse(pred.nn.train == y.r, yes=0, no=1))
    mspe.nn <-  mean(ifelse(pred.nn.test == y.p, yes=0, no=1))

    gbm.miss.train[i] <- smse.boost 
    nnet.miss.train[i] <- smse.nn
    svm.miss.train[i] <- smse.svm


    gbm.miss.test[i] <- mspe.gbm
    nnet.miss.test[i] <- mspe.nn
    svm.miss.test[i] <- mspe.svm

}

train_2.mse <- cbind(gbm = gbm.miss.train, nnet = nnet.miss.train, svm = svm.miss.train)

test_2.mse <- cbind(gbm = gbm.miss.test, nnet = nnet.miss.test, svm = svm.miss.test)

train_1.mse <- read.table("~/stat852/hw9/train.csv", header=TRUE, sep=",", na.strings=" ")

test_1.mse <- read.table("~/stat852/hw9/test.csv", header=TRUE, sep=",", na.strings=" ")

train.mse <- merge(train_1.mse,train_2.mse)

test.mse <- merge(test_1.mse,test_2.mse)

row_min <- apply(test.mse,1,min)

test.mse.re <- test.mse / row_min

ColumnMean <- data.frame(colMeans(train.mse),colMeans(test.mse),colMeans(test.mse.re))

quartz(h=7,w=12, title = "training error rate")

boxplot(train.mse)

quartz(h=7,w=12, title = "test error rate")

boxplot(test.mse)

quartz(h=7,w=12, title = "relative test error rate")

boxplot(test.mse.re)
