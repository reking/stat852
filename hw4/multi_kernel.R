




prostate <-  read.table("C:/Users/Administrator/Downloads/reking-stat852-a4c82c6b9d87/data/Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)



library(leaps)
# 3D plot to show surface more clearly.
library(rgl)  
open3d()
plot3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")

library(mgcv)








#############################################################################
# Multivariate Splines using the gam function of mgcv.
# Air Quality data
#############################################################################

# Loess smoother, degree 1, span=.75
lo.1.75 <- loess(data=prostate,lpsa ~ lcavol + lweight, degree=1) 

#  Then creating matrix of estimated means by direct calculation.  
#   Matrix form required by contour()  

x1 <- seq(from=-2, to=5, by=.01)
xy1 <- expand.grid(lcavol=seq(from=-2, to=7, by=.01), 
                               lweight=seq(from=2, to=7, by=.01))
surface <- predict(lo.1.75, newdata=xy1)

# Plots of results
win.graph(h=7,w=12,pointsize=12)
par(mfrow=c(1,2))
persp(x = seq(from=-2, to=7, by=.01), y = seq(from=2, to=7, by=.01), 
      z = surface, xlab="lcavol", ylab="lweight", 
      zlab="lpsa", main="Loess, Degree 1, Span=.75 Contours")
contour(x = seq(from=-2, to=7, by=.01), y =  seq(from=2, to=7, by=.01),
        z = surface, xlab="lcavol", ylab="lweight", 
        main="Loess, Degree 1, Span=.75 Contours")

# 3D plot.
library(rgl)  
open3d()
persp3d(x = seq(from=-2, to=7, by=.1), y = seq(from=2, to=7, by=.1), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")


###############################################################################
#Repeat with longer span
#############################################################################

# Loess smoother, degree 1, span=1
lo.1.1 <- loess(data=prostate, lpsa ~ lcavol + lweight, span=1, degree=1) 

#  Then creating matrix of estimated means by direct calculation.  Matrix form required by contour()  

x1 <- seq(from=-2, to=5, by=.01)
xy1 <- expand.grid(lcavol=seq(from=-2, to=7, by=.01), 
                   lweight=seq(from=2, to=7, by=.01))
surface <- predict(lo.1.1, newdata=xy1)

# Plots of results
win.graph(h=7,w=12,pointsize=12)
par(mfrow=c(1,2))
persp(x = seq(from=-2, to=7, by=.01), y = seq(from=2, to=7, by=.01), 
      z = surface, xlab="lcavol", ylab="lweight", 
      zlab="lpsa", main="Loess, Degree 1, Span=.75 Contours")
contour(x = seq(from=-2, to=7, by=.01), y =  seq(from=2, to=7, by=.01),
        z = surface, xlab="lcavol", ylab="lweight", 
        main="Loess, Degree 1, Span=.75 Contours")

# 3D plot.
library(rgl)  
open3d()
persp3d(x = seq(from=-2, to=7, by=.1), y = seq(from=2, to=7, by=.1), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")
###############################################################################
#Repeat with higher degree
#############################################################################

# Loess smoother, degree 2, span=0.75
lo.2.1 <- loess(data=prostate, lpsa ~ lcavol + lweight, degree=2) 

#  Then creating matrix of estimated means by direct calculation.  Matrix form required by contour()  


x1 <- seq(from=-2, to=7, by=.01)
xy1 <- expand.grid(lcavol=seq(from=-2, to=7, by=.01), 
                   lweight=seq(from=2, to=7, by=.01))
surface <- predict(lo.1.1, newdata=xy1)

# Plots of results
win.graph(h=7,w=12,pointsize=12)
par(mfrow=c(1,2))
persp(x = seq(from=-2, to=7, by=.01), y = seq(from=2, to=7, by=.01), 
      z = surface, xlab="lcavol", ylab="lweight", 
      zlab="lpsa", main="Loess, Degree 1, Span=.75 Contours")
contour(x = seq(from=-2, to=7, by=.01), y =  seq(from=2, to=7, by=.01),
        z = surface, xlab="lcavol", ylab="lweight", 
        main="Loess, Degree 1, Span=.75 Contours")

# 3D plot.
library(rgl)  
open3d()
persp3d(x = seq(from=-2, to=7, by=.1), y = seq(from=2, to=7, by=.1), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")
