## Ryan Elmore
## Random Walks
## October 2019

gen_random_walk <- function() {
  c(0, cumsum(rnorm(T)))
}

T <- 500
k <- 250

png(filename = "fig/one.png", width = 1.5*480, height = 1.5*480)
set.seed(3937123)
plot(-T:T, -T:T, axes = FALSE, ann = FALSE, type = "n")
values <- replicate(k, gen_random_walk())
gC <- round(runif(T, 1, 162))
for (i in 1:k) {
  m <- matrix(c(0:T, values[, i]), nc = 2)
  theta <- runif(1, 0, 2*pi)
  G <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)),
              nr = 2, byrow = F)
  tmp <- t(G %*% t(m))
  cutoff <- sample(1:T, 1)
  lines(tmp[1:cutoff, 1], tmp[1:cutoff, 2], lwd = 0.75, 
        col = rgb(10, gC[i], 10, alpha = runif(1, 50, 150), max = 255))
}
dev.off()
