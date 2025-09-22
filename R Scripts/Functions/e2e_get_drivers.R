## add option to save file.
## make sure to add warning to top of climate matching function
## which gets you to run this one first
e2e_get_drivers <- function(model.name,model.variant,save = TRUE){
  model <- e2e_read(model.name = model.name,
                    model.variant = model.variant)
  physics <- model$data$physics.drivers
  chemistry <- model$data$chemistry.drivers
  physics_names <- data.frame(Driver = colnames(physics)[-1], # don't want month col
                              Driver.Type = "Physics")
  chemistry_names <- data.frame(Driver = colnames(chemistry)[-1],
                                Driver.Type = "Chemistry")
  names <- rbind(physics_names,chemistry_names)
  driver_longnames <- c( # Including zones allows for more user control - better to have as separate column though? 
    "Sea Surface Light",
    "Shallow Offshore Suspended Particulate Matter",
    "Shallow Inshore Suspended Particulate Matter",
    "Shallow Offshore temperature",
    "Deep temperature",
    "Shallow Inshore temperature",
    "River volume",
    "log k vertical",
    "Mixed layer scale",
    "Deep Shallow Offshore upwelling",
    "Shallow Offshore deep downwelling",
    "Shallow Offshore inflow",
    "Deep inflow",
    "Shallow Inshore inflow",
    "Shallow Inshore outflow",
    "Shallow Offshore to Shallow Inshore flow",
    "Habitat Disturbance (s1)",
    "Habitat Disturbance (s2)",
    "Habitat Disturbance (s3)",
    "Habitat Disturbance (d1)",
    "Habitat Disturbance (d2)",
    "Habitat Disturbance (d3)",
    "Inshore wave height",
    "Deep Offshore log k vertical",
    "Deep Offshore mixed layer scale",
    "Deep Offshore deep upwelling",
    "Deep deep downwelling",
    "Shallow Offshore nitrate",
    "Shallow Offshore ammonia",
    "Shallow Offshore phytoplankton",
    "Shallow Offshore detritus",
    "Deep nitrate",
    "Deep ammonia",
    "Deep phytoplankton",
    "Deep detritus",
    "Shallow Inshore nitrate",
    "Shallow Inshore ammonia",
    "Shallow Inshore phytoplankton",
    "Shallow Inshore detritus",
    "River nitrate",
    "River ammonia",
    "River detritus",
    "Shallow Offshore atmospheric nitrate",
    "Shallow Offshore atmospheric ammonia",
    "Shallow Inshore atmospheric nitrate",
    "Shallow Inshore atmospheric ammonia",
    "Shallow Inshore other nitrate",
    "Shallow Inshore other ammonia",
    "Shallow Offshore other nitrate",
    "Shallow Offshore other ammonia",
    "Deep Offshore nitrate",
    "Deep Offshore ammonia",
    "Deep Offshore detritus"
  )
  
  ben.groups <- seq(1,length(driver_longnames)) # temp until I get default groupings from Ben
  drivers <- data.frame(Driver = names$Driver,
                        Longname = driver_longnames,
                        Driver.Type = names$Driver.Type,
                        Grouping = ben.groups)
  if (save) {
    write.csv(drivers,paste0(model[["setup"]][["model.path"]],"/Param/Climate_Matching_",model[["setup"]][["model.name"]],"_",model[["setup"]][["model.variant"]],".csv"),
              row.names = F)
  }
  return(drivers)
}

drivers <- e2e_get_drivers(model.name = "NORTH_SEA",
                           model.variant = "1970-1999")
