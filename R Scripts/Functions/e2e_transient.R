library(tidyverse)
library(lubridate)
library(StrathE2E2)

model <- e2e_read(model.name = "North_Sea",model.variant = "2003-2013")
# model <- e2e_read(model.name = "Norwegian_Basin_MA", model.variant = "2010-2015-CNRM-ssp126")
## Read in all the StrathE2E Driving data

## MISSING ##
My_scale <- readRDS("./Objects/Mission Atlantic Transients/Domain_NS.rds")
##############

My_volumes <- readRDS("./Objects/Mission Atlantic Transients/TS.rds") 

My_light <- readRDS("./Objects/Mission Atlantic Transients/light.rds")                                                      # Order to match template

My_H_Flows <- readRDS("./Objects/Mission Atlantic Transients/H-Flows.rds")                                                        # Order by month to match template

My_V_Flows <- readRDS("./Objects/Mission Atlantic Transients/vertical diffusivity.rds")

My_Waves <- readRDS("./Objects/Mission Atlantic Transients/Significant wave height.rds")

My_atmosphere <- readRDS("./Objects/Mission Atlantic Transients/Atmospheric N deposition.rds")

My_SPM <- readRDS("./Objects/Mission Atlantic Transients/Suspended particulate matter.rds")

My_Rivers <- readRDS("./Objects/Mission Atlantic Transients/NE River input.rds")

master <- list(All_Results = list(),
               Flow_Matrices = list(),
               Biomasses = list(),
               Network_Indicators = list(),
               Initial_Conditions = list(),
               Flows = list()) #How are we going to save all of this?

e2e_transient <- function(model,nyears,transient_years,ssp,forcing,
                          My_scale,My_volumes,My_light,My_H_Flows,My_V_Flows,My_Waves,My_atmosphere,My_SPM){
  Temp_scale <- My_scale %>%                          # Calculate the volume of the three zones
    sf::st_drop_geometry() %>% 
    mutate(S = c(T, T),
           D = c(F, T)) %>% 
    gather(key = "slab_layer", value = "Exists", S, D) %>% 
    filter(Exists == T) %>%
    mutate(Elevation = c(Elevation[1], -60, Elevation[3] + 60)) %>% 
    mutate(Volume = area * abs(Elevation)) %>% 
    dplyr::select(Shore, slab_layer, Volume)
  
  for (year in transient_years) {
    
    ## big debugger
    forcing <- "GFDL"
    ssp = "ssp126"
    year <- 2010
    ##
    
    Temp_volumes <- My_volumes %>% 
      filter(Year == year & SSP == SSP & Forcing == Forcing) %>% 
      group_by(Compartment, Month) %>%                                          # By compartment and month
      # not sure on groupings here - not the same as in NM
      summarise(across(c(Diatoms_avg,Other_phytoplankton_avg,Detritus_avg,Temperature_avg), mean, na.rm = T)) %>%         # Average across years for multiple columns
      ungroup() %>% 
      arrange(Month)
    
    Temp_light <- My_light %>% 
      filter(Year == year &  (SSP %in% c("hist",ssp)) & Forcing == forcing)
    
    Temp_H_Flows <- My_H_Flows %>% 
      filter(Year == year & (SSP %in% c("hist",ssp)) & Forcing == forcing) %>%    # Import data%>%                                     # Limit to reference period
      group_by(across(-c(Year, Flow))) %>%                                      # Group over everything except year and variable of interest
      summarise(Flow = mean(Flow, na.rm = T)) %>%                               # Average flows by month over years
      ungroup() %>% 
      left_join(Temp_scale,by = join_by(Shore,slab_layer)) %>%                                                   # Attach compartment volumes
      mutate(Flow = Flow/Volume) %>%                                            # Scale flows by compartment volume
      mutate(Flow = abs(Flow * 86400)) %>%                                      # Multiply for total daily from per second, and correct sign for "out" flows
      arrange(Month) 
    
    Temp_V_Flows <- My_V_Flows %>%
      filter(Year == year & (SSP %in% c("hist",ssp)) & Forcing == forcing) %>%                                 # Limit to reference period
      group_by(Month) %>% 
      summarise(V_diff = mean(Vertical_diffusivity, na.rm = T)) %>% 
      ungroup() %>% 
      arrange(Month) 

    if (year > max(lubridate::year(My_Waves$Date))) {
      warning(paste0("Waves data only goes to ",max(lubridate::year(My_Waves$Date)-1),". Wave data will be held constant at ",max(lubridate::year(My_Waves$Date))-1," values."))
      Temp_Waves <- My_Waves %>% 
        mutate(Year = lubridate::year(Date),
               Month = lubridate::month(Date)) %>%
        filter(Year == max(lubridate::year(My_Waves$Date))-1) %>% 
        arrange(Month) %>% 
        group_by(Month) %>% 
        summarise(mean_height = mean(Waves))# Arrange to match template
    } else{
      Temp_Waves <- My_Waves %>% 
        mutate(Year = lubridate::year(Date),
               Month = lubridate::month(Date)) %>%
        filter(Year == year) %>% 
        arrange(Month) %>% 
        group_by(Month) %>% 
        summarise(mean_height = mean(Waves))# Arrange to match template
    }
    
    Temp_atmosphere <- My_atmosphere %>% 
      filter(Year == year & (SSP %in% c("hist",ssp))) %>% 
      group_by(Month, Oxidation_state, Shore,  Year) %>%
      summarise(Measured = sum(Measured, na.rm = T)) %>%                                              # Sum across deposition states
      summarise(Measured = mean(Measured, na.rm = T)) %>%                                             # Average over years
      ungroup() %>%
      pivot_wider(names_from = c(Shore, Oxidation_state), values_from = Measured) %>%                     # Spread to match template
      arrange(Month)

    Temp_SPM <- My_SPM %>%
      filter(Year == year) %>%                                     # Limit to reference period
      group_by(Shore, Month) %>%
      summarise(SPM = mean(SPM, na.rm = T)) %>%                                 # Average by month across years
      ungroup() %>%
      arrange(Month) 
    
    Temp_rivers <- My_Rivers %>%
      filter(Year == year) %>%                                     # Limit to reference period
      mutate(Month = lubridate::month(.$Date)) %>% 
      group_by(Month) %>%
      summarise(Runoff = mean(Runoff, na.rm = T)) %>%                           # Average by month across years
      ungroup() %>%
      arrange(as.numeric(Month))
    
    Physics_template <- model[["data"]][["physics.drivers"]] # get template
    
    Physics_template$sslight <-  Temp_light$Measured
    Physics_template$so_logespm <- log(filter(Temp_SPM,Shore == "Offshore")$SPM)
    Physics_template$si_logespm <- log(filter(Temp_SPM,Shore == "Inshore")$SPM)
    Physics_template$so_temp <- filter(Temp_volumes, Compartment == "Offshore S")$Temperature_avg
    Physics_template$d_temp <- filter(Temp_volumes, Compartment == "Offshore D")$Temperature_avg
    Physics_template$si_temp <- filter(Temp_volumes, Compartment == "Inshore S")$Temperature_avg
    Physics_template$rivervol <- Temp_Rivers$Runoff / filter(Temp_scale, Shore == "Inshore")$Volume
    Physics_template$logkvert <- log10(Temp_V_Flows$V_diff)
    Physics_template$mixlscale <- Physics_template$mixlscale
    Physics_template$upwelling <- 0
    Physics_template$so_inflow <- filter(Temp_H_Flows, slab_layer == "S", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow
    Physics_template$d_inflow <- filter(Temp_H_Flows, slab_layer == "D", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow
    Physics_template$si_inflow <- filter(Temp_H_Flows, slab_layer == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "In")$Flow
    Physics_template$si_outflow <- filter(Temp_H_Flows, slab_layer == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "Out")$Flow
    Physics_template$so_si_flow <- filter(Temp_H_Flows, slab_layer == "S", Shore == "Offshore", Neighbour == "Inshore", Direction == "Out")$Flow
    Physics_template$Inshore_waveheight <- Temp_Waves$mean_height
    
    # Replace with new drivers
    model[["data"]][["physics.drivers"]] <- Physics_template
    
    Boundary_template <- model[["data"]][["chemistry.drivers"]] # get chemistry template
    
    Boundary_new <- mutate(Boundary_template,
                           so_nitrate = My_boundary_data$SO_DIN * (1-filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion),
                           so_ammonia = My_boundary_data$SO_DIN * filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion,
                           so_phyt = My_boundary_data$SO_Phytoplankton,
                           so_detritus = My_boundary_data$SO_Detritus,
                           d_nitrate =  My_boundary_data$D_DIN * (1-filter(My_DIN_fix, Depth_layer == "Deep")$Proportion),
                           d_ammonia = My_boundary_data$D_DIN * filter(My_DIN_fix, Depth_layer == "Deep")$Proportion,
                           d_phyt = My_boundary_data$D_Phytoplankton,
                           d_detritus = My_boundary_data$D_Detritus,
                           si_nitrate = My_boundary_data$SI_DIN * (1-filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion),
                           si_ammonia = My_boundary_data$SI_DIN * filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion,
                           si_phyt = My_boundary_data$SI_Phytoplankton,
                           si_detritus = My_boundary_data$SI_Detritus,
                           rivnitrate = NO3_boundary$Nitrate,
                           rivammonia = NH4_boundary$Ammonia,
                           rivdetritus = 0,
                           so_atmnitrate = My_atmosphere$Offshore_O,
                           so_atmammonia = My_atmosphere$Offshore_R,
                           si_atmnitrate = My_atmosphere$Inshore_O,
                           si_atmammonia = My_atmosphere$Inshore_R,
                           si_othernitrate = 0,
                           si_otherammonia = 0)
    
    results <- tryCatch({
      e2ep_run(model = model, nyears = 1)
    }, error = function(e) {
      message("\n An error occurred during e2e_run: ", e$message,"\n Error occured at i = ",i,". Year = ",transient_years[i])
      return(master)
    })
    
    #Extract I.C
    init_con <- e2ep_extract_start(model = model,results = results,
                                   csv.output = F)
    
    #Reinsert I.C
    model[["data"]][["initial.state"]][1:nrow(init_con)] <- e2ep_extract_start(model = model,results = results,
                                                                               csv.output = F)[,1]
    
    # Pull everything we need
    master[[paste0(year)]][["All_results"]] <- results
    master[[paste0(year)]][["Initial_Conditions"]] <- init_con
  }
  return(master)
}



