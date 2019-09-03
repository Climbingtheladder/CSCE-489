X1 = c(3, 2, 4, 1, 2, 4, 4)
X2 = c(4, 2, 4, 4, 1, 3, 1)
colors = c("red", "red", "red", "red", "blue", "blue", "blue")
plot(X1, X2, col = colors, xlim = c(0, 5), ylim = c(0, 5))

abline(0, 1, lty =2)
abline(-0.5, 1)
abline(-1, 1,lty =2)

points(c(1), c(3), col = c("blue"))
