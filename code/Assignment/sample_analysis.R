
#selection effect
#ref http://d.hatena.ne.jp/yoschi0831/20130205/1360073249

N <- 1000
theta1 <- 50
theta2 <- 0.8 # true correlation
sigma <- 49
C <- 60
set.seed(1)
y1 <- theta1 + rnorm(N,0,sqrt(100))
y2 <- theta2 * y1 + rnorm(N,0,sqrt(sigma))
#pcol <-  if(y1 >= C) "red" else "grey"
pcol <- ifelse(y1 >= C, "navy", "grey")
data <- data.frame("y1"=y1,"y2"=y2)

# plot
plot(y1,y2,type="p",col=pcol,xlim=c(0,100),tlim=c(0,100))
par(new=T)
abline(v=C,col="black")

# subtract of observational data
#y11 <- subset(data,y2>=C)$y1
#y21 <- subset(data,y2>=C)$y2
y11 <- subset(data,y1>=C)$y1
y21 <- subset(data,y1>=C)$y2

# mitameno-correlation
cor1 <- cor(y11,y21,method="pearson")

# correlation coefficient from observational data
ylm <- lm(y21~-1 + y11,data = data)
summary(ylm)
theta2_es <- coefficients(ylm)

# real coefficient
cor2 <- (theta2_es * var(y1))/(sqrt(var(y1)) * sqrt(theta2_es^2 * var(y1) + sigma))


# data plot
plot(y1,y2, type = "p",col = pcol, xlim = c(0,100), ylim = c(0,100))
par(new = T)
abline(v = C, col = "black")
