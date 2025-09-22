## Function needs tweaking to add ice in for polar model - don't have any polar models currently
## on this machine
e2e_get_drivers <- function(model.name,model.variant){
  model <- e2e_read(model.name = model.name,
                    model.variant = model.variant)
  physics <- model$data$physics.drivers
  chemistry <- model$data$chemistry.drivers
  physics_names <- data.frame(Driver = colnames(physics)[-1], # don't want month col
                              Driver.Type = "Physics")
  chemistry_names <- data.frame(Driver = colnames(chemistry)[-1],
                                Driver.Type = "Chemistry")
  names <- rbind(physics_names,chemistry_names)
  driver_longnames <- c(
    "Sea Surface Light",
    "Shallow Offshore logespm",
    "Shallow Inshore logespm",
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
  
  return(data.frame(Driver = names$Driver,
                    Longnames = driver_longnames,
                    Driver.Type = names$Driver.Type))
}
