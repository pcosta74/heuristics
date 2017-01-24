.__FILE__ <- (function() {
  attr(body(sys.function()), "srcfile")
})()$filename
.__DIR__ <- dirname(.__FILE__)
.__SRC__ <- file.path(dirname(.__DIR__),'src',basename(.__FILE__))
.__NGH__ <- file.path(dirname(.__DIR__),'src','neighbourhood.R')
.__DT1__ <- file.path(dirname(.__DIR__),'src','knapsack.R')
.__DT2__ <- file.path(dirname(.__DIR__),'src','traveling_salesman.R')

library(assertthat)
source(.__SRC__)
source(.__NGH__)
source(.__DT1__)
source(.__DT2__)

#
test.localsearch.ks <- function() {
  ks.bst.util <- function(S0,S1) { ks.best.utility(S0, S1, KS.ITEMS) }
  ks.filter   <- function(Scurr) { ks.constraints(Scurr, KS.ITEMS) }
  ks.bf.nghd  <- function(Scurr) { Filter(ks.filter,neighbourhood(Scurr, bitflip)) }

  ls.frst.util <- function(Scurr, Nghd) { first.improving(Scurr, Nghd, ks.bst.util)}
  ls.best.util <- function(Scurr, Nghd) { best.improving(Scurr, Nghd, ks.bst.util)}

  S0 <- c(0,1,0,1,1,0)
  
  E1 <- c(1,1,0,1,1,0)
  S1 <- localsearch(KS.ITEMS, S0, NGHD=ks.bf.nghd, SEARCH=ls.frst.util)
  assert_that(identical(S1, E1))
  assert_that(ks.constraints(S1, KS.ITEMS))
  
  E2 <- c(0,1,1,1,1,0)
  S2 <- localsearch(KS.ITEMS, S0, NGHD=ks.bf.nghd, SEARCH=ls.best.util)
  assert_that(identical(S2, E2))
  assert_that(ks.constraints(S2, KS.ITEMS))
}

#
test.localsearch.tsp <- function() {
  tsp.filter   <- function(Scurr) { tsp.complete(c(Scurr, 1), TSP.EDGES) }
  tsp.adj.nghd <- function(Scurr) { Filter(tsp.filter, neighbourhood(Scurr, adj.interchanges)) }
  tsp.ntr.nghd <- function(Scurr) { Filter(tsp.filter, neighbourhood(Scurr, interchanges)) }
  tsp.ins.nghd <- function(Scurr) { Filter(tsp.filter, neighbourhood(Scurr, insertions)) }
  
  tsp.bst.route <- function(S0,S1) { tsp.best.route(S0, S1, TSP.EDGES) }
  tsp.chpst.ins <- function(S0,S1) { tsp.cheapest.insertion(S0, S1, TSP.EDGES) }
  
  ls.frst.route <- function(Scurr, Nghd) { first.improving(Scurr, Nghd, tsp.bst.route)}
  ls.best.route <- function(Scurr, Nghd) { best.improving(Scurr, Nghd, tsp.bst.route)}
  
  S0 <- c(1,3,4,2)
  
  E1 <- c(1,4,3,2)
  S1 <- localsearch(TSP.EDGES, S0, NGHD=tsp.adj.nghd, SEARCH=ls.frst.route)
  assert_that(identical(S1, E1))
  assert_that(tsp.complete(c(S1,1), TSP.EDGES))

  E2 <- c(1,4,3,2)
  S2 <- localsearch(TSP.EDGES, S0, NGHD=tsp.adj.nghd, SEARCH=ls.best.route)
  assert_that(identical(S2, E2))
  assert_that(tsp.complete(c(S2,1), TSP.EDGES))
  
  E3 <- c(1,4,3,2)
  S3 <- localsearch(TSP.EDGES, S0, NGHD=tsp.ntr.nghd, SEARCH=ls.frst.route)
  assert_that(identical(S3, E3))
  assert_that(tsp.complete(c(S3,1), TSP.EDGES))

  # E4 <- c(1,4,3,2)
  # S4 <- localsearch(TSP.EDGES, S0, NGHD=tsp.ntr.nghd, SEARCH=ls.best.util)
  # assert_that(identical(S4, E4))
  # assert_that(tsp.complete(c(S4,1), TSP.EDGES))
}

#
# Run tests
#
run <- function() {
  test.localsearch.ks()
  test.localsearch.tsp()
}