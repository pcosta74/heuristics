#
# Constructive heuristic
#
constructive <- function(items, INIT, IS.AVAIL, COMPLETE, SEL.ITEM) {
  rslt <- INIT(items)
  Scurr   <- rslt$Scurr
  a.items <- rslt$A.items
  
  while(TRUE) {
    rslt <- SEL.ITEM(Scurr, items)
    Spart   <- rslt$Scurr
    a.items <- rslt$A.items
    
    if(COMPLETE(Spart, items)) { break }
    Scurr <- Spart
    
    if(!IS.AVAIL(a.items)) { break }
  }
  
  return(Scurr)
}