.__FILE__ <- (function() {
  attr(body(sys.function()), "srcfile")
})()$filename
.__DIR__ <- dirname(.__FILE__)
.__SRC__ <- file.path(dirname(.__DIR__),'src',basename(.__FILE__))

library(assertthat)
source(.__SRC__)

# 
test.neighbourhood <- function() {
  S0 <- c(1,2,3)
  n  <- length(S0)

  E1 <- list(
    c(2,1,3),
    c(1,3,2)
  )
  N1 <- neighbourhood(S0, adj.interchanges)
  assert_that(all(N1 %in% E1))
  assert_that(length(N1) == n-1)

  E2 <- list(
    c(2,1,3),
    c(3,2,1),
    c(1,3,2)
  )
  N2 <- neighbourhood(S0, interchanges)
  assert_that(all(N2 %in% E2))
  assert_that(length(N2) == n*(n-1)/2)
  
  E3 <- list(
    c(2,1,3),
    c(2,3,1),
    c(1,3,2),
    c(3,1,2)  
  )
  N3 <- neighbourhood(S0, insertions)
  assert_that(all(N3 %in% E3))
  assert_that(length(N3) == (n-1)^2)
  
  S0 <- c(0,0,0)
  E4 <- list(
    c(1,0,0),
    c(0,1,0),
    c(0,0,1)
  )
  N4 <- neighbourhood(S0, bitflip)
  assert_that(all(N4 %in% E4))
  assert_that(length(N4) == n)
}

#
test.interchanges <- function() {
  S0 <- c(5,1,3,6,2,7,4)
  
  E1 <- c(5,6,3,1,2,7,4)
  ngh1 <- interchanges(S0,2,4)
  assert_that(all(ngh1 == E1))

  E2 <- c(5,1,4,6,2,7,3)
  ngh2 <- interchanges(S0,3,7)
  assert_that(all(ngh2 == E2))
}

#
test.adj.interchanges <- function() {
  S0 <- c(5,1,3,6,2,7,4)
  
  E1 <- c(1,5,3,6,2,7,4)
  ngh1 <- adj.interchanges(S0,1,2)
  assert_that(all(ngh1 == E1))
  
  E2 <- c(5,1,3,2,6,7,4)
  ngh2 <- adj.interchanges(S0,4,5)
  assert_that(all(ngh2 == E2))
}

#
test.insertions <- function() {
  S0 <- c(5,1,3,6,2,7,4)

  E1 <- c(1,3,6,5,2,7,4)
  ngh1 <- insertions(S0,1,4)
  assert_that(all(ngh1 == E1))

  E2 <- c(1,3,6,2,7,4,5)
  ngh2 <- insertions(S0,1,7)
  assert_that(all(ngh2 == E2))
    
  E3 <- c(5,7,1,3,6,2,4)
  ngh3 <- insertions(S0,6,2)
  assert_that(all(ngh3 == E3))
  
  E4 <- c(4,5,1,3,6,2,7)
  ngh4 <- insertions(S0,7,1)
  assert_that(all(ngh4 == E4))
}

#
test.bitflip <- function() {
  S0 <- c(1,1,0,1,0,0,1)
  
  E1 <- c(1,0,0,1,0,0,1)
  ngh1 <- bitflip(S0,2)
  assert_that(all(ngh1 == E1))
  
  E2 <- c(1,1,0,1,1,0,1)
  ngh2 <- bitflip(S0,5)
  assert_that(all(ngh2 == E2))
}


#
# Run tests
#
run <- function() {
  test.interchanges()
  test.adj.interchanges()
  test.insertions()
  test.bitflip()
  test.neighbourhood()
}