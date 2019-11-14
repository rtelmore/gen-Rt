## Ryan Elmore
## Random Walks
## October 2019
T <- 200
k <- 2500

png(filename = "fig/rw-1-du.png", units = "in", res = 300,
    width = 16, height = 16)
set.seed(393712)
plot(-T:T, -T:T, axes = FALSE, ann = FALSE, type = "n")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
     col = "#e3d9ae", density = -100)
# col = "#f0f0f0")
# values <- replicate(k, gen_random_walk())
gC <- round(runif(k, 50, 140))
# gC <- round(runif(k, 1, 180)) # Green
for (i in 1:k) {
  m <- matrix(c(0:T, c(0, cumsum(rnorm(T)))), nc = 2)
  theta <- runif(1, 0, 2*pi)
  G <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)),
              nr = 2, byrow = F)
  tmp <- t(G %*% t(m))
  cutoff <- sample(1:T, 1)
  lines(tmp[1:cutoff, 1], tmp[1:cutoff, 2], lwd = 1, 
        col = rgb(gC[i], 35, 50, alpha = runif(1, 50, 150), max = 255))
  # col = rgb(0, gC[i], 0, alpha = runif(1, 50, 150), max = 255))
}
dev.off()
