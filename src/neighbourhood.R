#====================
# public
#====================

#
# Generate neighbourhood
#
neighbourhood <- function(x, FUN) {
  stopifnot(is.vector(x))

  Nghd <- list()
  
  max   <- length(x)
  depth <- length(names(formals(FUN))[-1])  
  count <- rep(1, depth)
  index <- depth;
  exit  <- FALSE
  
  while (!exit) {
    try({
      ngh  <- do.call(FUN,append(list(x),as.list(count)))
      Nghd <- c(Nghd, list(ngh))
    }, silent=TRUE)
    
    count[depth] <- count[depth] + 1;
    while (count[index] == max+1) {
      count[index] <- 1
      index <- index - 1
      if (index == 0) { 
        exit <- TRUE
        break
      }
      count[index] <- count[index]+1;
    }
    index <- depth;
  }
  
  return(unique(Nghd))
}


#
# Interchanges neighbourhood
# Move: swap two different elements 
#
interchanges <- function(x, p0, p1) {
  .stopifnot.valid.perm(x, p0, p1)
  
  s     <- x[p1]
  x[p1] <- x[p0]
  x[p0] <- s
  
  return(x)
}

#
# Interchanges neighbourhood
# Move: swap two different adjacent elements 
#
adj.interchanges <- function(x, p0, p1) {
  stopifnot(abs(p0-p1)==1)
  return(interchanges(x, p0, p1))
}

#
# Insertions neighbourhood
# Move: remove one element (at p0) and insert into a different position (at p1)
#
insertions <- function(x, p0, p1) {
  .stopifnot.valid.perm(x, p0, p1)

  n <- length(x)
  if(p0 < p1) {
    if(p1 == n) {
      x <- c(x,x[p0])
    } else {
      x <- c(x[1:p1], x[p0], x[(p1+1):n])
    }
    return(x[-p0])
  } else {
    if(p1==1) {
      x <- c(x[p0],x)
    } else {
      x <- c(x[1:(p1-1)], x[p0], x[p1:n])
    }
    return(x[-(p0+1)])
  }
}

#
# Bitflip neighbourhood
# Move: change the value of one element
#
bitflip <- function(x, p) {
  .stopifnot.valid.binenc(x, p)
  x[p] <- 1-x[p]
  return(x)
}


#====================
# private
#====================

# Binary encoding validation
.stopifnot.valid.binenc <- function(x, p) {
  stopifnot(is.vector(x))
  stopifnot(all(x %in% c(0,1)))

  stopifnot(is.numeric(p))
  stopifnot(1 <= p & p <= length(x))
}

# Permutation validation
.stopifnot.valid.perm <- function(x, p0, p1) {
  stopifnot(is.vector(x))
  stopifnot(is.numeric(p0))
  stopifnot(is.numeric(p1))
  stopifnot(1 <= p0 & p0 <= length(x))
  stopifnot(1 <= p1 & p1 <= length(x))
  stopifnot(p0 != p1)
}