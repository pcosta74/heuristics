# Traveling Salesman Routes and Distances
TSP.EDGES <- matrix(
  c( 0, 2, 9,10,
     1, 0, 6, 4,
    15, 7, 0, 8,
     6, 3,12, 0),
  ncol=4, nrow=4, byrow=TRUE 
)

#
# Initialization method
#
tsp.init <- function(items) {
  s0 <- c(1)
  ai <- c(2:nrow(items))
  list(Scurr=s0,A.items=ai)
}

#
# Available items
#
tsp.available <- function(av.items) {
  length(av.items) > 0
}

#
# Complete
#
tsp.complete <- function(Scurr, items) {
  return(Scurr[1] == 1 && Scurr[length(Scurr)] == 1 && 
         all(seq(nrow(items)) %in% Scurr))
}

#
# Objective function
#
tsp.nearest.neighbour <- function(Scurr, items) {
  a.items <- setdiff(seq(nrow(items)), Scurr)
  
  diag(items) <- Inf

  curpos <- Scurr[length(Scurr)]
  client <- which(items[curpos,] == min(items[curpos, a.items]))
  
  Scurr   <- c(Scurr, client)
  a.items <- a.items[a.items != client]
  
  list(Scurr=Scurr,A.items=a.items) 
}

#
# Objective function
#
tsp.cheapest.insertion <- function(Scurr, items) {
  a.items <- setdiff(seq(nrow(items)), Scurr)
  
  if(length(a.items) > 0) {
    Scurr    <- c(Scurr, 1)
    n.curr   <- length(Scurr)
    Sbest    <- NULL
    bst.cost <- Inf

    for(p in 2:n.curr) {
      for(i in a.items) {
        Sngh <- c(Scurr[1:(p-1)], i, Scurr[(p):n.curr])
        Scst <- sum(sapply(2:(n.curr+1), function(n,s,e) e[s[n-1],s[n]], Sngh, items))
        if(Scst < bst.cost) {
          bst.cost <- Scst
          Sbest <- Sngh 
        }
      }
    }

    Scurr   <- Sbest[-length(Sbest)]
    a.items <- setdiff(a.items, Scurr)
  }
  
  list(Scurr=Scurr,A.items=a.items) 
}

#
# Compare two solutions in terms of length
#
tsp.best.route <- function(S0, S1, items) {
  stopifnot((n0 <- length(S0)) == (n1 <- length(S1)))

  sum0 <- sum(sapply(2:n0, function(n,s,e) e[s[n-1],s[n]], S0, items))
  sum1 <- sum(sapply(2:n1, function(n,s,e) e[s[n-1],s[n]], S1, items))
  
  sign(sum1 - sum0)
}

