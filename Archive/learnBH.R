pvalues<-c(0.01,0.001, 0.05, 0.20, 0.15, 0.15)
ranks<-rank(pvalues, ties.method = "last")
p_m_over_k<-pvalues*length(pvalues)/ranks
for (r in length(pvalues):1) {
  print(p_m_over_k[ranks>=r])
}
pvalues_adj<-c()
for (i in 1:length(pvalues)) {
  
  # find the rank
  tmp_rank<-ranks[i]
  
  # get all the p_m_over_k that are greater or equal to this rank
  # and get the min value
  pvalues_adj<-c(pvalues_adj, min(1,min(p_m_over_k[ranks>=tmp_rank])))
}
print(pvalues_adj)
