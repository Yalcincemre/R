#Bu çözümde denklemimizi ve deðiþkenlerimizi belirlemiþ olalým.

library(ggplot2)

set.seed(37)
x <- rnorm(n=100, mean=5, sd=1)
e <- rnorm(n=100, mean=0, sd=1)
y <- e + 2*x 

# lm fnk ile elde ettiðimiz deðerleri bir bakalým
lm <- lm(y ~ x)
newdata <- data.frame(x=c(3.5,7))
predict(lm, newdata, interval="none") 

# show data
data = data.frame(x=x, y=y)
g <- ggplot(data, aes(x=x, y=y))  + 
  geom_point(alpha=1/3, size=4) +
  geom_smooth(method="lm", se=F, col="steelblue") +
  labs(title = "Linear Regression – Demo data")

# Add a column of ones to x
X <- matrix(c(rep(1,length(x)),x), ncol=2)
head(X)

# Initialize 
theta <- c(0, 0)
iterations <- 1500

# to be precise pick alpha=0.0002
alpha <- 0.0001 # for difference on plot

#gradientDescent fnk
gradientDescent <- function(X, y, theta, alpha, num_iters){
m <- length(y);  
J_history = rep(0, num_iters);
for (iter in 1:num_iters){
predictions <-  X %*% theta;
updates = t(X) %*% (predictions - y);
theta = theta - alpha * (1/m) * updates;
J_history[iter] <- computeCost(X, y, theta);
  }
  list("theta" = theta, "J_history" = J_history)  
}

#cost fnk yani Squared error function
computeCost <- function (X, y, theta){
# number of training examples
m <- length(y);
# need to return
J <- 0;
  
predictions <-  X %*% theta;
sqerrors = (predictions - y)^2;
J = 1/(2*m)* sum(sqerrors);
  
  J
}

# run gradient descent
result <- gradientDescent(X, y, theta, alpha, iterations);
theta <- result$theta
print("theta found:");print(theta)

# data with prediction
data = data.frame(x=x, y=y, test = X%*%theta)

ggplot(data, aes(x=x, y=y, test=test))  + 
geom_point(alpha=1/3, size=4) +
stat_smooth(method = "lm", formula = test ~ x, size = 1, se = FALSE,
              aes(color="Gradient Descent")) +
geom_smooth(method="lm", se=F, aes(color="Training set")) +
scale_colour_manual(name="Method", values=c("red", "steelblue")) +
theme(legend.position = "bottom") +
labs(title = "Gradient Descent – Results")

data <- data.frame(x=seq(1, length(result$J_history)),
                   y=result$J_history)
ggplot(data, aes(x=x, y=y)) +
  geom_line() +
  labs(title="Gradient descent iterations",
       x="Iterations", y="Cost J")

predict1 <- c(1, 3.5) %*% theta
predict2 <- c(1, 7) %*% theta

theta <- c(0, 0)
iterations <- 1500
alpha <- 0.0002 # set alpha more precisely
result <- gradientDescent(X, y, theta, alpha, iterations);
matrix(c(1, 1, 3.5, 7), ncol=2) %*% result$theta

#Tahminimize çok yakýn bir deðer çýkýyor.

