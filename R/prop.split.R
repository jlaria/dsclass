prop.split = function(N, p){
  idx = sample(1:N)
  split.idx = c(0, ceiling(cumsum(p)*N))
  lapply(1:length(p), function(i){idx[(split.idx[i]+1):split.idx[i+1]]})
}
