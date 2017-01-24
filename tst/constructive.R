.__FILE__ <- (function() {
  attr(body(sys.function()), "srcfile")
})()$filename
.__DIR__ <- dirname(.__FILE__)
.__SRC__ <- file.path(dirname(.__DIR__),'src',basename(.__FILE__))
.__DT1__ <- file.path(dirname(.__DIR__),'src','knapsack.R')
.__DT2__ <- file.path(dirname(.__DIR__),'src','traveling_salesman.R')

library(assertthat)
source(.__SRC__)
source(.__DT1__)
source(.__DT2__)

#
test.constructive.ks <- function() {
  ks.complete <- function(...) { !ks.constraints(...) }
  
  E1 <- c(0,1,1,1,1,0)
  S1 <- constructive(KS.ITEMS, INIT=ks.init, SEL.ITEM=ks.max.utility,
                     IS.AVAIL=ks.available, COMPLETE=ks.complete)
  assert_that(identical(S1, E1))
  assert_that(ks.constraints(S1, KS.ITEMS))
  
  E2 <- c(0,1,0,1,1,1)
  S2 <- constructive(KS.ITEMS, INIT=ks.init, SEL.ITEM=ks.min.weight,
                     IS.AVAIL=ks.available, COMPLETE=ks.complete)
  assert_that(identical(S2, E2))
  assert_that(ks.constraints(S2, KS.ITEMS))
  
  E3 <- c(0,1,0,1,1,1)  
  S3 <- constructive(KS.ITEMS, INIT=ks.init, SEL.ITEM=ks.high.priority,
                     IS.AVAIL=ks.available, COMPLETE=ks.complete)
  assert_that(identical(S3, E3))
  assert_that(ks.constraints(S3, KS.ITEMS))
}

#
test.constructive.tsp <- function() {
  E1 <- c(1,2,4,3)
  S1 <- constructive(TSP.EDGES, INIT=tsp.init, SEL.ITEM=tsp.nearest.neighbour,
                     IS.AVAIL=tsp.available, COMPLETE=tsp.complete)
  assert_that(identical(S1,E1))
  assert_that(tsp.complete(c(S1,1), TSP.EDGES))
  
  E2 <- c(1,2,3,4)
  S2 <- constructive(TSP.EDGES, INIT=tsp.init, SEL.ITEM=tsp.cheapest.insertion,
                     IS.AVAIL=tsp.available, COMPLETE=tsp.complete)
  assert_that(identical(S2,E2))
  assert_that(tsp.complete(c(S2,1), TSP.EDGES))
}

#
# Run tests
#
run <- function() {
  test.constructive.ks()
  test.constructive.tsp()
}