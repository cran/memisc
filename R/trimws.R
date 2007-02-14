trimws <- function(x,left=TRUE,right=TRUE){
  res <- x
  if(left)
    res <- sub('^[[:space:]]+', '',res)
  if(right)
    res <- sub('[[:space:]]+$', '',res)
  res
}