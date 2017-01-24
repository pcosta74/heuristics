#
# Local Search
#
localsearch <- function(items, S0, NGHD, SEARCH) {
  Scurr <- S0
  
  while(TRUE) {
    Nghd  <- NGHD(Scurr)
    Sbest <- SEARCH(Scurr, Nghd)
    
    if(identical(Scurr, Sbest)) { break }
    Scurr <- Sbest    
  }
  
  return(Scurr)
}

#
# First-improving strategy: Stop searching as soon as a solution is found 
# in the neighbourhood that is better than Scurr 
#
first.improving <- function(Scurr, Nghd, BEST) {
  for(n in seq_along(Nghd)) {
    Sngh <- Nghd[[n]]
    if(BEST(Scurr, Sngh) == 1) {
      Scurr <- Sngh
      break
    }    
  }
  return(Scurr)
}

#
# Best-improving strategy: find the best solution in the neighbourhood 
# that is better than Scurr 
#
best.improving <- function(Scurr, Nghd, BEST) {
  Sbest <- Scurr
  for(n in seq_along(Nghd)) {
    Sngh <- Nghd[[n]]
    if(BEST(Sbest, Sngh) == 1) {
      Sbest <- Sngh
    }    
  }
  return(Sbest)
}