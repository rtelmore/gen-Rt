## Ryan Elmore
## Random Walks
## October 2019
T <- 200
k <- 500
N <- 20

png(filename = "fig/two.png", width = 1.5*480, height = 1.5*480)
set.seed(3937123)
starts <- matrix(runif(2*N, -400, 400), nc = 2)
plot(-500:500, -500:500, axes = FALSE, ann = FALSE, type = "n")
# values <- replicate(k, gen_random_walk())
for(j in 1:N){
  gC <- round(runif(k, 1, 180))
  for (i in 1:k) {
    m <- matrix(c(0:T, c(0, cumsum(rnorm(T)))), nc = 2)
    theta <- runif(1, 0, 2*pi)
    G <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)),
                nr = 2, byrow = F)
    tmp <- t(G %*% t(m))
    cutoff <- sample(1:T, 1)
    lines(starts[j, 1] + tmp[1:cutoff, 1], 
          starts[j, 2] + tmp[1:cutoff, 2], lwd = 0.75, 
          col = rgb(0, gC[i], 0, alpha = runif(1, 50, 150), max = 255))
  }
}
dev.off()
