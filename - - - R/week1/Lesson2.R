# Знакомство с libraries
# Знакомство с графиками (ggplot2)
# Виды графиков
# qplot

library(ggplot2)

x <- 1:10; y = x*x
qplot(x,y)
qplot(x, y, geom=c("point", "line"))
qplot(mpg, wt, data=mtcars)
qplot(mpg, wt, data = mtcars, geom = c("point", "smooth")
qplot(mpg, wt, data = mtcars, color = factor(cyl),
      geom=c("point", "smooth")
      
qplot(mpg, wt, data = mtcars, colour = cyl)

df <- mtcars
df[,'cyl'] <- as.factor(df[,'cyl'])
qplot(mpg, wt, data = df, colour = cyl)
qplot(mpg, wt, data = df, colour = cyl,
      geom=c("point", "line"))
      
qplot(mpg, wt, data=df, colour= factor(cyl))

qplot(mpg, wt, data = mtcars, size = mpg)
qplot(mpg, wt, data = mtcars, shape = factor(cyl))

qplot(mpg, wt, data = mtcars, label = rownames(mtcars), 
      geom=c("point", "text"),
      hjust=0, vjust=0)

head(PlantGrowth)

x <- "1"
y <- rnorm(100)
qplot(x, y, geom="boxplot")

qplot(group, weight, data = PlantGrowth, 
      geom=c("boxplot"))
qplot(group, weight, data = PlantGrowth, 
      geom=c("dotplot"), 
      stackdir = "center", binaxis = "y")
qplot(group, weight, data = PlantGrowth, 
      geom=c("violin"), trim = FALSE)
      
qplot(group, weight, data = PlantGrowth, 
      geom=c("boxplot", "jitter"), fill = group)
qplot(group, weight, data = PlantGrowth, 
      geom = "dotplot", stackdir = "center", binaxis = "y",
      color = group, fill = group)
      
      
# Гистограммы
set.seed(1234)
mydata = data.frame(
        sex = factor(rep(c("F", "M"), each=200)),
        weight = c(rnorm(200, 55), rnorm(200, 58)))
head(mydata)

qplot(weight, data = mydata, geom = "histogram")
qplot(weight, data = mydata, geom = "histogram",
    fill = sex)
    
qplot(weight, data = mydata, geom = "density")
qplot(weight, data = mydata, geom = "density",
    color = sex, linetype = sex)
    
    
qplot(weight, data = mydata, geom = "density",
      xlab = "Weight (kg)", ylab = "Density", 
      main = "Density plot of Weight")
