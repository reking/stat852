library(gbm)

wheat<-  read.table("~/stat852/data/wheat.csv", header=TRUE, sep=",", na.strings=" ")

wheat$class <- as.numeric(wheat$class)

colnames(wheat)[2] <- "classnum"

wheat$type <- as.factor(wheat$type)
library(MASS)
set.seed(67982193)
perm <- sample(x=nrow(wheat))
set1 <- wheat[which(perm<=200),-1]
set2 <- wheat[which(perm>200),-1]


kernels <- cbind("Healthy", "Scab", "Sprout")

num<- cbind(0.3,0.5,0.8,1.0)
Tree.siz <- cbind(1,5,10,20)
Shrink <- cbind(0.001, 0.01, 0.1)
sample.ratio <- cbind(0.25,0.5,0.75)

iter=10
wheat.val <- matrix(NA,nrow = length(num)*length(Tree.siz)*length(Shrink)*length(sample.ratio),ncol=iter+4)

for(i in 1:iter)
{
  
  rice <- runif(1,0,1)
  set.seed(rice * 10000000)
  set1$bina <- ifelse(runif(n=nrow(set1))>0.66, yes=2, no=1)
  y.r <- set1[which(set1$bina==1),7]
  x.r <- set1[which(set1$bina==1),1:6]
  
  y.p <- set1[which(set1$bina==2),7]
  x.p <- set1[which(set1$bina==2),1:6]
  
#  resamp <- sample.int(n=nrow(x.1), size=0.66 * nrow(x.1), replace=FALSE)
#  x.r <- x.1[resamp,]
#  y.r <- y.1[resamp]
#  x.p <- x.1[-unique(resamp),]
#  y.p <- y.1[-unique(resamp)]
  
  
  
  ii = 1
  for(siz in Tree.siz)
    for(sh in Shrink)
      for(rati in sample.ratio)
      {
        wheat.boost <- gbm(data=data.frame(x.r,class=y.r), class ~., distribution="multinomial", 
                             n.trees=5000, interaction.depth=siz,verbose=FALSE, shrinkage=sh,
                             bag.fraction=rati, cv.folds=0,n.cores=16)
        
        wheat.val[ii,1:4] <- c(siz,sh,rati,num[1]*10000)
        wheat.val[ii+1,1:4] <- c(siz,sh,rati,num[2]*10000)
        wheat.val[ii+2,1:4] <- c(siz,sh,rati,num[3]*10000)
        wheat.val[ii+3,1:4] <- c(siz,sh,rati,num[4]*10000)
        




      
        pred.mul.val.1  <-  predict(wheat.boost, newdata=x.p, n.trees=num[1]*5000, type="response")
        pred.mul.val.2  <-  predict(wheat.boost, newdata=x.p, n.trees=num[2]*5000, type="response")
        pred.mul.val.3  <-  predict(wheat.boost, newdata=x.p, n.trees=num[3]*5000, type="response")
        pred.mul.val.4  <-  predict(wheat.boost, newdata=x.p, n.trees=num[4]*5000, type="response")

        class.mul.val.1 <- kernels[apply(pred.mul.val.1[,,1], 1, which.max)]
        class.mul.val.2 <- kernels[apply(pred.mul.val.2[,,1], 1, which.max)]
        class.mul.val.3 <- kernels[apply(pred.mul.val.3[,,1], 1, which.max)]
        class.mul.val.4 <- kernels[apply(pred.mul.val.4[,,1], 1, which.max)]

     wheat.val[ii,i+4]     <- mean(ifelse(class.mul.val.1 == y.p, yes=0, no=1))
     wheat.val[ii+1,i+4]   <- mean(ifelse(class.mul.val.2 == y.p, yes=0, no=1))
     wheat.val[ii+2,i+4]   <- mean(ifelse(class.mul.val.3 == y.p, yes=0, no=1))
     wheat.val[ii+3,i+4]   <- mean(ifelse(class.mul.val.4 == y.p, yes=0, no=1))


       ii <- ii + 4
      }
  
  
      print("iteration finished once!")
  Mean_val <- rowMeans(wheat.val[,-c(1,2,3,4)])
  best_index <- which.min(Mean_val)
  best_para <- wheat.val[best_index,1:4]
}


jj = 1

for(j in 1:4)
{  
  mm <- jj + 26
  siz.dec <- paste(set1.val[jj:mm,1],wheat.val[jj:mm,2],wheat.val[jj:mm,3],wheat.val[jj:mm,4])
  x11(h=7,w=12,pointsize=12)
  boxplot.matrix(x=sqrt(set1.val[jj:mm,-c(1,2,3,4)]), use.cols=FALSE, names=siz.dec)
  jj=jj+27
}


##### Best Parameters 5.0e+00 1.0e-03 2.5e-01 1.0e+04

gbm(data=data.frame(x.2,Rings=y.2), Rings ~ ., distribution="gaussian", 
    n.trees=10000, interaction.depth=5,verbose=FALSE, shrinkage=0.001,
    bag.fraction=0.25, cv.folds=0,n.cores=16)




