# Data Visualization 1
#install.packages("ggplot2")
library(ggplot2)

Data <- data("diamonds")
x <- 1:10
# qplot(x=,y=,data=diamonds)
qplot(x=price,data = diamonds)

qplot(x=price,y=carat,data=diamonds)

v <- diamonds$carat
qplot(v)

qplot(diamonds$price,diamonds$carat)
diam
qplot(x=price,y=carat,
      color=color,
      data=diamonds)
str(diamonds)

cor.test(as.integer(diamonds$clarity),diamonds$depth)
qplot(as.integer(diamonds$clarity,diamonds$depth))



str(diamonds$cut)

qplot(x=price,y=carat,
      color=color,
      data=diamonds,
      geom = "point",
      shape=cut)

str(mtcars$am)
mtcars$new_per <- ifelse(mtcars$am==1,"Auto","Manual")
qplot(mpg,hp,data=mtcars,
      color=new_per,shape=as.factor(cyl),size=I(3),
      alpha=I(0.3))



qplot(x=price,data=diamonds,geom="histogram",fill=color,col=I("black"))

qplot(x=price,data=diamonds,geom="density",fill=color,col=I("black"),alpha=I(0.5))
