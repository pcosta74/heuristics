# Knapsack items
KS.ITEMS <- data.frame(
  Objects=c('Tent','Sleeping Bag','Canned Food','Bread','Pickaxe','Smoked Ham'),
  Weight=c(10,5,10,1,4,2),
  Utility=c(10,30,15,15,20,10)
)

# Knapsack capacity
KS.CAPACITY <- 20

#
# Initialization method
#
ks.init <- function(items) {
  s0 <- rep(0, nrow(items))
  ai <- 1 - s0
  list(Scurr=s0,A.items=ai)
}

#
# Available items
#
ks.available <- function(av.items) {
  any(av.items > 0)
}

#
# Constraints
#
ks.constraints <- function(Scurr, items) {
  sum(items$Weight * Scurr) <= KS.CAPACITY
}

#
# Objective function
#
ks.max.utility <- function(Scurr, items) {
  sc <- Scurr
  ai <- 1 - Scurr
  
  b  <- which.max(items$Utility * ai)
  sc[b] <- 1
  ai[b] <- 0
  
  list(Scurr=sc,A.items=ai)  
}

#
# Objective function
#
ks.ks.min.weight <- function(Scurr, items) {
  sc <- Scurr
  ai <- 1 - Scurr
  
  b  <- which.min(items$Weight * (1/ai))
  sc[b] <- 1
  ai[b] <- 0
  
  list(Scurr=sc,A.items=ai)  
}

#
# High Priority index
#
ks.high.priority <- function(Scurr, items) {
  sc <- Scurr
  ai <- 1 - Scurr
  
  b  <- which.max((items$Utility/items$Weight) * ai)
  sc[b] <- 1
  ai[b] <- 0
  
  list(Scurr=sc,A.items=ai) 
}

#
# Compare two solutions in terms of utility
#
ks.best.utility <- function(S0, S1, items) {
  sign(sum(S1*items$Utility) - sum(S0*items$Utility))
}
