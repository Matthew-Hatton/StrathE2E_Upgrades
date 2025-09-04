## Function to scale maximum uptake rates to help diagnose issues when fitting. Can also be used to create time series
## plots with scaled uptakes by setting plot = TRUE and providing and out.dir. 
e2ep_run_pcurve <- function(model.name,model.variant,nyears,guild,Maximum_uptake_scalar,out.dir = NULL,plot = FALSE){
  library(StrathE2EPolar)
  model <- e2ep_read(model.name = paste0(model.name),model.variant = paste0(model.variant))
  if (guild == "PHYTOPLANKTON") {
    model[["data"]][["fitted.parameters"]][["u_phyt"]] <- model[["data"]][["fitted.parameters"]][["u_phyt"]] * Maximum_uptake_scalar
    results <- e2ep_run(model = model,nyears = nyears)
    gross_production <- mean(tail(results[["aggregates"]][["phytgrossprod"]],n = 365))
  }else if (guild == "DEMERSAL"){
    model[["data"]][["fitted.parameters"]][["u_fishd"]] <- model[["data"]][["fitted.parameters"]][["u_fishd"]] * Maximum_uptake_scalar
    results <- e2ep_run(model = model,nyears = nyears)
    gross_production <- mean(tail(results[["aggregates"]][["dfishgrossprod"]],n = 365))
  }else if (guild == "PLANKTIV"){
    model[["data"]][["fitted.parameters"]][["u_fishp"]] <- model[["data"]][["fitted.parameters"]][["u_fishp"]] * Maximum_uptake_scalar
    results <- e2ep_run(model = model,nyears = nyears)
    gross_production <- mean(tail(results[["aggregates"]][["pfishgrossprod"]],n = 365))
  }else if (guild == "OMNI_ZOO"){
    model[["data"]][["fitted.parameters"]][["u_omni"]] <- model[["data"]][["fitted.parameters"]][["u_omni"]] * Maximum_uptake_scalar
    results <- e2ep_run(model = model,nyears = nyears)
    gross_production <- mean(tail(results[["aggregates"]][["omnigrossprod"]],n = 365))
  }else if (guild == "CARN_ZOO"){
    model[["data"]][["fitted.parameters"]][["u_carn"]] <- model[["data"]][["fitted.parameters"]][["u_carn"]] * Maximum_uptake_scalar
    results <- e2ep_run(model = model,nyears = nyears)
    gross_production <- mean(tail(results[["aggregates"]][["carngrossprod"]],n = 365))
  }
  else if (guild == "PHYT_PIN"){
    model[["data"]][["fitted.parameters"]][["u_phyt"]] <- model[["data"]][["fitted.parameters"]][["u_phyt"]] * Maximum_uptake_scalar
    model[["data"]][["fitted.parameters"]][["u_seal"]] <- model[["data"]][["fitted.parameters"]][["u_seal"]] * Maximum_uptake_scalar
    results <- e2ep_run(model = model,nyears = nyears)
    gross_production <- mean(tail(results[["aggregates"]][["sealgrossprod"]],n = 365))
  }
  else{
    warning("Please enter a valid guild. PHYTOPLANKTON, PLANKTIV, DEMERSAL, OMNI_ZOO, CARN_ZOO")
    stop()
  }
  if (plot == TRUE) {
    filename <- paste0(out.dir,as.numeric(round(Maximum_uptake_scalar,digits = 3)),"_time_series_",guild,".png")
    png(filename, width = 800, height = 600, res = 120)
    e2ep_plot_ts(model = model, results = results,selection = "ECO")
    title(paste0(round(Maximum_uptake_scalar,digits=3)),cex.main = 0.8, font.main= 1,col.main = "red",line = -4.3)
    
    dev.off()
  }
  return(
    data.frame(
      guild = guild,
      scalar = Maximum_uptake_scalar,
      gross_production = gross_production
    )
  )
}