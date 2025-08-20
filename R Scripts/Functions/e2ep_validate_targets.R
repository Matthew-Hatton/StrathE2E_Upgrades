e2ep_validate_targets <- function(model,results){
  opt <- results[["final.year.outputs"]][["opt_results"]]
  
  # keep only the targets flagged for use
  opt_used <- subset(opt, Use1_0 == 1)
  
  # compute ratio: Model / Observed
  opt_used$ratio <- opt_used$Model_data / opt_used$Annual_measure
  
  # add row num
  opt_used$Excel_row <- as.numeric(rownames(opt_used)) + 1
  
  # flag extreme ratios
  extreme_targets <- subset(opt_used, abs(ratio) > 10 | abs(ratio) < 0.1)
  
  # sort
  extreme_targets <- extreme_targets[order(-abs(extreme_targets$ratio)), ]
  
  return(extreme_targets)
}