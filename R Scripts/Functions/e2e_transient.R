e2e_transient <- function(model,transient_years,ssp,forcing,
                          My_scale,My_volumes,My_light,My_H_Flows,My_V_Flows,My_V_Diff,My_Waves,My_atmosphere,My_SPM,My_boundary,My_overhang,My_overhang_diffusivity,My_overhang_exchanges){
  options(dplyr.summarise.inform = FALSE) # Turn off dplyr warnings
  master <- list() #How are we going to save all of this?
  
  Temp_scale <- My_scale %>%                          # Calculate the volume of the three zones
    sf::st_drop_geometry() %>% 
    mutate(S = T,
           D = case_when(Shore == "Inshore" ~ F,
                         Shore == "Offshore" ~ T)) %>% 
    gather(key = "slab_layer", value = "Exists", S, D) %>% 
    filter(Exists == T) %>%
    mutate(Elevation = case_when(Shore == "Inshore" ~ Elevation,
                                 Shore == "Offshore" & slab_layer == "D" ~ Elevation + SDepth,
                                 Shore == "Offshore" & slab_layer == "S" ~ -SDepth,)) %>%
    mutate(Volume = area * abs(Elevation)) %>% 
    dplyr::select(Shore, slab_layer, Volume)
  
  for (y in transient_years) {
    # big debugger
    # forcing <- "GFDL"
    # ssp = "ssp126"
    # y <- 2013
    ##
    message(paste0("Year = ",y))
    Temp_volumes <- My_volumes %>% 
      filter(Year == y &  (SSP %in% c("hist",ssp)) & Forcing == forcing) %>% 
      group_by(Compartment, Month) %>%                                          # By compartment and month
      # not sure on groupings here - not the same as in NM
      summarise(across(c(Diatoms_avg,Other_phytoplankton_avg,Detritus_avg,Temperature_avg), mean, na.rm = T)) %>%         # Average across years for multiple columns
      ungroup() %>% 
      arrange(Month)
    
    Temp_light <- My_light %>% 
      filter(Year == y &  (SSP %in% c("hist",ssp)) & Forcing == forcing)
    
    Temp_H_Flows <- My_H_Flows %>% 
      filter(Year == y & (SSP %in% c("hist",ssp)) & Forcing == forcing) %>%    # Import data%>%                                     # Limit to reference period
      group_by(across(-c(Year, Forcing, SSP,Flow))) %>%                                      # Group over everything except year, run, and variable of interest
      summarise(Flow = mean(Flow, na.rm = T)) %>%                               # Average flows by month over years
      ungroup() %>% 
      group_by(Shore, slab_layer, Neighbour) %>%                                # Add in missing months
      complete(Month, Direction, fill = list(Flow = 0)) %>%                     # By making sure all months are represented in both directions
      ungroup() %>% 
      left_join(Temp_scale) %>%                                                   # Attach compartment volumes
      mutate(Flow = Flow/Volume) %>%                                            # Scale flows by compartment volume
      mutate(Flow = abs(Flow * 86400)) %>%                                      # Multiply for total daily from per second, and correct sign for "out" flows
      arrange(Month)
    
    Temp_V_Flows <- My_V_Flows %>% 
      filter(Year == y & (SSP %in% c("hist",ssp)) & Forcing == forcing) %>%
      group_by(Month) %>%
      summarise(Upwelling = mean(Upwelling, na.rm = T),
                Downwelling = mean(Downwelling, na.rm = T)) %>%
      ungroup() %>%
      mutate(Upwelling = Upwelling/filter(Temp_scale, Shore == "Offshore" & slab_layer == "S")$Volume,
             Downwelling = Downwelling/filter(Temp_scale, slab_layer == "D")$Volume) %>% # Scale flows by compartment volume
      mutate(Upwelling = Upwelling * 86400,
             Downwelling = Downwelling * 86400) %>%                             # Multiply for total daily from per second
      arrange(Month)
    
    Temp_V_Diff <- My_V_Diff %>%
      filter(Year == y & (SSP %in% c("hist",ssp)) & Forcing == forcing) %>%                                 # Limit to reference period
      group_by(Month) %>% 
      summarise(V_diff = mean(Vertical_diffusivity, na.rm = T)) %>% 
      ungroup() %>% 
      arrange(Month) 

    if (y > max(lubridate::year(My_Waves$Date))) {
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
        filter(Year == y) %>% 
        arrange(Month) %>% 
        group_by(Month) %>% 
        summarise(mean_height = mean(Waves))# Arrange to match template
    }
    
    Temp_atmosphere <- My_atmosphere %>% 
      filter(Year == y & (SSP %in% c("hist",ssp))) %>% 
      group_by(Month, Oxidation_state, Shore,  Year) %>%
      summarise(Measured = sum(Measured, na.rm = T)) %>%                                              # Sum across deposition states
      summarise(Measured = mean(Measured, na.rm = T)) %>%                                             # Average over ys
      ungroup() %>%
      pivot_wider(names_from = c(Shore, Oxidation_state), values_from = Measured) %>%                     # Spread to match template
      arrange(Month)

    Temp_SPM <- My_SPM %>%
      filter(Year == y) %>%                                     # Limit to reference period
      group_by(Shore, Month) %>%
      summarise(SPM = mean(SPM, na.rm = T)) %>%                                 # Average by month across ys
      ungroup() %>%
      arrange(Month) 
    
    Temp_rivers <- My_Rivers %>%
      filter(Year == y) %>%                                     # Limit to reference period
      mutate(Month = lubridate::month(.$Date)) %>% 
      group_by(Month) %>%
      summarise(Runoff = mean(Runoff, na.rm = T)) %>%                           # Average by month across ys
      ungroup() %>%
      arrange(as.numeric(Month))
    
    Temp_river_N <- My_Rivers %>%   
      filter(Year == y & (SSP %in% c("hist",ssp))) %>%                        # Limit to outputs from a specific run and time
      mutate(Month = lubridate::month(Date)) %>% 
      group_by(Month) %>%                                                                        # Average across ys
      summarise(NO3 = mean(NO3, na.rm = T),
                NH4 = mean(NH4, na.rm = T)) %>%  
      ungroup() %>% 
      arrange(Month) 
    
    Temp_boundary <- My_boundary %>%
      filter(Year == y & (SSP %in% c("hist",ssp))) %>% 
      group_by(Month) %>%                                                 # Average across ys
      summarise(across(SO_NO3:D_phyt, ~ mean(.x, na.rm = T))) %>% 
      ungroup() %>% 
      arrange(Month)
    
    Temp_overhang <- My_overhang %>%
      filter(Year == y & (SSP %in% c("hist",ssp)) & Direction == "Upwelling") %>%                            # Limit to reference period
      group_by(Month) %>%                                                                      # Average across ys
      summarise(NO3 = mean(NO3, na.rm = T),
                NH4 = mean(NH4, na.rm = T),
                Detritus = mean(Detritus, na.rm = T)) %>%
      ungroup() %>%
      arrange(Month)
    
    Temp_overhang_diffusivity <- My_overhang_diffusivity %>% 
      filter(Year == y & (SSP %in% c("hist",ssp))) %>% 
      group_by(Month) %>%
      summarise(V_diff = mean(Vertical_diffusivity, na.rm = T)) %>%
      ungroup() %>%
      arrange(Month)
    
    Temp_overhang_exchanges <- My_overhang_exchanges %>% 
      filter(Year == y & (SSP %in% c("hist",ssp))) %>% 
      group_by(Month, Direction) %>%                                            # Group by flow and time step
      summarise(Flow = mean(Vertical_velocity, na.rm = T)) %>%                  # Average flows by month over ys
      ungroup() %>%
      mutate(Shore = "Offshore", slab_layer = "D") %>%
      left_join(Temp_scale) %>%                                                   # Attach compartment volumes
      mutate(Flow = Flow/Volume) %>%                                            # Scale flows by compartment volume
      mutate(Flow = Flow * 86400) %>%                                           # Multiply for total daily from per second
      arrange(Month)
    
    
    Physics_template <- model[["data"]][["physics.drivers"]] # get template
    
    Physics_new <- mutate(Physics_template,
                          SLight = Temp_light$Light,
                          ## Flows, should be proportions of volume per day
                          SO_OceanIN = filter(Temp_H_Flows, slab_layer == "S", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow,
                          D_OceanIN = filter(Temp_H_Flows, slab_layer == "D", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow,
                          SI_OceanIN = filter(Temp_H_Flows, slab_layer == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "In")$Flow,
                          SI_OceanOUT = filter(Temp_H_Flows, slab_layer == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "Out")$Flow,
                          SO_SI_flow = filter(Temp_H_Flows, slab_layer == "S", Shore == "Offshore", Neighbour == "Inshore", Direction == "Out")$Flow,
                          # ## log e transformed suspended particulate matter concentration in zones
                          SO_LogeSPM = log(filter(Temp_SPM, Shore == "Offshore")$SPM),
                          SI_LogeSPM = log(filter(Temp_SPM, Shore == "Inshore")$SPM),
                          ## Temperatures in volumes for each zone
                          SO_temp = filter(Temp_volumes, Compartment == "Offshore S")$Temperature_avg,
                          D_temp = filter(Temp_volumes, Compartment == "Offshore D")$Temperature_avg,
                          SI_temp = filter(Temp_volumes, Compartment == "Inshore S")$Temperature_avg,
                          ## River inflow,
                          Rivervol_SI = Temp_rivers$Runoff / filter(Temp_scale, Shore == "Inshore")$Volume, # Scale as proportion of inshore volume
                          ## Vertical diffusivity
                          log10Kvert = log10(Temp_V_Diff$V_diff),
                          ## Daily proportion disturbed by natural bed shear stress
                          # habS1_pdist = filter(My_Stress, Shore == "Inshore", Habitat == "Silt")$Disturbance,
                          # habS2_pdist = filter(My_Stress, Shore == "Inshore", Habitat == "Sand")$Disturbance,
                          # habS3_pdist = filter(My_Stress, Shore == "Inshore", Habitat == "Gravel")$Disturbance,
                          # habD1_pdist = filter(My_Stress, Shore == "Offshore", Habitat == "Silt")$Disturbance,
                          # habD2_pdist = filter(My_Stress, Shore == "Offshore", Habitat == "Sand")$Disturbance,
                          # habD3_pdist = filter(My_Stress, Shore == "Offshore", Habitat == "Gravel")$Disturbance,
                          # ## Monthly mean significant wave height inshore
                          Inshore_waveheight = Temp_Waves$mean_height,
                          # ## Overhang variables
                          D_SO_upwelling = Temp_V_Flows$Upwelling,
                          SO_D_downwelling = Temp_V_Flows$Downwelling,
                          DO_log10Kvert = log10(Temp_overhang_diffusivity$V_diff),
                          DO_mixLscale = 0.9,
                          DO_D_upwelling = filter(Temp_overhang_exchanges, Direction == "Upwelling")$Flow,
                          D_DO_downwelling = filter(Temp_overhang_exchanges, Direction == "Downwelling")$Flow
    )


    # Replace with new drivers
    model[["data"]][["physics.drivers"]] <- Physics_new
    
    Boundary_template <- model[["data"]][["chemistry.drivers"]] # get chemistry template
    
    Boundary_new <- mutate(Boundary_template,
                           SO_nitrate = Temp_boundary$SO_NO3,
                           SO_ammonia = Temp_boundary$SO_NH4,
                           SO_phyt = Temp_boundary$SO_phyt,
                           SO_detritus = Temp_boundary$SO_Detritus,
                           D_nitrate = Temp_boundary$D_NO3, 
                           D_ammonia = Temp_boundary$D_NH4, 
                           D_phyt = Temp_boundary$D_phyt,
                           D_detritus = Temp_boundary$D_Detritus,
                           SI_nitrate = Temp_boundary$SI_NO3,
                           SI_ammonia = Temp_boundary$SI_NH4,
                           SI_phyt = Temp_boundary$SI_phyt, 
                           SI_detritus = Temp_boundary$SI_Detritus,
                           ## Rivers
                           RIV_nitrate = Temp_river_N$NO3,     
                           RIV_ammonia = Temp_river_N$NH4,          
                           RIV_detritus = 0,
                           ## Atmosphere, daily deposition as monthly averages
                           SO_ATM_nitrate_flux = Temp_atmosphere$Offshore_O,
                           SO_ATM_ammonia_flux = Temp_atmosphere$Offshore_R,
                           SI_ATM_nitrate_flux = Temp_atmosphere$Inshore_O,
                           SI_ATM_ammonia_flux = Temp_atmosphere$Inshore_R, 
                           SI_other_nitrate_flux = 0,   # Can be used for scenarios
                           SI_other_ammonia_flux = 0,
                           SO_other_nitrate_flux = 0,   # Can be used for scenarios
                           SO_other_ammonia_flux = 0,
                           # ## Overhang
                           DO_nitrate = Temp_overhang$NO3,
                           DO_ammonia	= Temp_overhang$NH4,
                           DO_detritus = Temp_overhang$Detritus
    ) 
    
    model[["data"]][["chemistry.drivers"]] <- Boundary_new
   
    results <- tryCatch({
      if (y == transient_years[1]) {
        e2e_run(model = model, nyears = 1)
      } else{
      e2e_run(model = model, nyears = 1)
      }
    }, error = function(e) {
      message("\n An error occurred during e2e_run: ", e$message,"\n Error occured year = ",y)
      return(master)
    })
    
    #Extract I.C
    init_con <- e2e_extract_start(model = model,results = results,
                                   csv.output = F)
    
    #Reinsert I.C
    model[["data"]][["initial.state"]][1:nrow(init_con)] <- e2e_extract_start(model = model,results = results,
                                                                               csv.output = F)[,1]
    
    
    # Pull everything we need
    master[[paste0(forcing,".",ssp)]][[paste0(y)]][["All_results"]] <- results
    master[[paste0(forcing,".",ssp)]][[paste0(y)]][["Initial_Conditions"]] <- init_con
  }
  return(master)
}
