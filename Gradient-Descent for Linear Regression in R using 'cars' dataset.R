#Bu çözümde cars data setini kullandým.
head(cars)
attach(cars)
str(cars)

x <- speed
y <- dist

###: Test
lm.fit <- lm(dist ~ speed)
summary(lm.fit)
plot(dist~speed , main="Linear Regression")
abline(lm.fit, col = "red")

# p value 0.05’ten küçük bir sayý, Ho hipotezimiz reddedildi.
# Arabanýn aldýðý yol(dist) ve hýz(speed) arasýnda anlamlý bir iliþki bulundu.
# error:observed edilen y deðerleri - predicted edilen y deðerleri

# set up the cost function for least square linear regression:
cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}

# learning rate and iterasyon sayýsý
alpha <- 0.01
num_iters <- 1000

# sonuçlarý saklamak adýna list ve double oluþturduk.
cost_history <- double(num_iters)
theta_history <- list(num_iters)

# boþ matris oluþtur, baþlangýç katsayýlarý
theta <- matrix(c(0,0), nrow=2)

# 1 tane sütun ekle intercept katsayýsý için
X <- cbind(1, matrix(x))

# Gradient Descent
for (i in 1:num_iters) {
  error <- (X %*% theta - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X, y, theta)
  theta_history[[i]] <- theta
}

# plot data and converging fit
plot(dist~speed , main="Linear Regression")
for (i in c(1,3,6,10,14,seq(20,num_iters,by=10))) {
  abline(coef=theta_history[[i]], col=rgb(0.8,0,0,0.3))
}
#giderek yaklaþýyor.

# check out the trajectory of the cost function
cost_history[seq(1,num_iters, by=100)]
plot(cost_history, type='l', col='blue', lwd=2, main='Cost function', ylab='cost', xlab='Iterations')

#katsayýlar
print(theta)
