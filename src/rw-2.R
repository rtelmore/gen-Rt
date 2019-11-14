## Ryan Elmore
## Random Walks
## October 2019
k <- 100
N <- 50
B <- 500
T <- round(runif(N, 50, 200))

png(filename = "fig/rw-du-1.png", width = 1.5*480, height = 1.5*480)
set.seed(1215313)
starts <- matrix(runif(2*N, -400, 400), nc = 2)
plot(-B:B, -B:B, axes = FALSE, ann = FALSE, type = "n")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
     col = "#f0f0f0")

# values <- replicate(k, gen_random_walk())
for(j in 1:N){
  gC <- round(runif(k, 70, 160))
  for (i in 1:k) {
    m <- matrix(c(0:T[j], c(0, cumsum(rnorm(T[j])))), nc = 2)
    theta <- runif(1, 0, 2*pi)
    G <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)),
                nr = 2, byrow = F)
    tmp <- t(G %*% t(m))
    cutoff <- sample(1:T[j], 1)
    lines(starts[j, 1] + tmp[1:cutoff, 1], 
          starts[j, 2] + tmp[1:cutoff, 2], lwd = 0.75, 
          col = rgb(gC[i], 35, 50, alpha = runif(1, 30, 150), max = 255))
  }
}
dev.off()
# col = rgb(0, gC[i], 40, alpha = runif(1, 50, 150), max = 255))
    