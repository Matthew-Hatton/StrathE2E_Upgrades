library(StrathE2EPolar)

name_map <- c(
  # Primary production
  "Obs_TAPP" = "Total phyt.",
  "Obs_IAPP" = "Total ice-algae.",
  "Obs_NP" = "New primary phyt.",
  "Obs_KelpP" = "Kelp carbon",
  
  # Zooplankton & fish
  "Obs_OmnizooP" = "Omniv.zooplankton",
  "Obs_CarnzooP" = "Carniv.zooplankton",
  "Obs_PFishP" = "Planktiv.fish",
  "Obs_DFishP" = "Demersal fish",
  
  # Benthos
  "Obs_BensuspP" = "Susp/dep.benthos",
  "Obs_BencarnP" = "Carn/scav.benthos",
  
  # Birds and mammals
  "Obs_birdP" = "Seabird",
  "Obs_sealP" = "Seal",
  "Obs_cetaP" = "Cetacean",
  "Obs_bearP" = "Maritime mammals",
  
  # Larvae
  "Obs_maxbenthslar" = "Susp/dep.benthos larv",
  "Obs_maxbenthclar" = "Carn/scav.benthos larv",
  
  # Consumption
  "Obs_Conpfishfish" = "Pel.fish by fish",
  "Obs_Condfishfish" = "Dem.fish by fish",
  "Obs_Conzoofish" = "Zooplankton by fish",
  "Obs_Conzoocarnz" = "Meso-zoo by carniv.zoo.",
  "Obs_Conbenfish" = "Benthos by fish",
  "Obs_Contotal_bird" = "Total by birds",
  
  # Bird diet proportions
  "Obs_Proppfishbird" = "Plank.fish in bird diet",
  "Obs_Propdfishbird" = "Dem.fish in bird diet",
  "Obs_Propmfishbird" = "Mig.fish in bird diet",
  "Obs_Propdiscbird" = "Disc. in bird diet",
  
  # Seal diet
  "Obs_Contotal_seal" = "Total by seals",
  "Obs_Proppfishseal" = "Plank.fish in seal diet",
  "Obs_Propdfishseal" = "Dem.fish in seal diet",
  "Obs_Propmfishseal" = "Mig.fish in seal diet",
  
  # Cetaceans
  "Obs_Contotal_ceta" = "Total by cetaceans",
  "Obs_Proppfishceta" = "Plank.fish in cet. diet",
  "Obs_Propdfishceta" = "Dem.fish in cet. diet",
  "Obs_Propmfishceta" = "Mig.fish in cet. diet",
  "Obs_Propzooceta" = "Zooplank. in cet. diet",
  
  # Maritime mammals
  "Obs_Contotal_bear" = "Total by marit. mam.",
  "Obs_Propsealbear" = "Pinnipeds in marit. mam. diet",
  "Obs_Propcetabear" = "Cetaceans in marit. mam. diet",
  
  # Landings
  "Obs_Pland_livewt" = "Plank.fish landings",
  "Obs_Dland_livewt" = "Dem.fish landings",
  "Obs_Mland_livewt" = "Mig.fish landings",
  "Obs_Bsland_livewt" = "Susp/dep.benthos landings",
  "Obs_Bcland_livewt" = "Carn/scav.benthos landings",
  "Obs_Zcland_livewt" = "Pel.invert. landings",
  "Obs_Slland_livewt" = "Pinniped landings",
  "Obs_Ctland_livewt" = "Cetacean landings",
  "Obs_Kland_livewt" = "Kelp harvest",
  
  # P/B ratios
  "Obs_kelp_pb" = "Kelp P/B",
  "Obs_benslar_pb" = "Susp/dep.benthos larv. P/B",
  "Obs_benclar_pb" = "Carn/scav.benthos larv. P/B",
  "Obs_bens_pb" = "Susp/dep.benthos P/B",
  "Obs_benc_pb" = "Carn/scav.benthos P/B",
  "Obs_omni_pb" = "Omniv.zooplankton P/B",
  "Obs_carn_pb" = "Carniv.zooplankton P/B",
  "Obs_fishplar_pb" = "Plank.fish larvae P/B",
  "Obs_fishdlar_pb" = "Dem.fish larvae P/B",
  "Obs_fishp_pb" = "Plank.fish P/B",
  "Obs_fishd_pb" = "Dem.fish P/B",
  "Obs_fishm_pb" = "Mig.fish P/B",
  "Obs_bird_pb" = "Bird P/B",
  "Obs_seal_pb" = "Seal P/B",
  "Obs_ceta_pb" = "Cetacean P/B",
  "Obs_bear_pb" = "Maritime mam. P/B",
  
  # Misc
  "Obs_exud_C_kelp" = "Prop. kelp prod. exuded",
  "Obs_kelp_NC" = "Kelp N/C ratio",
  "Obs_WCSedDenitrif" = "WC sed. denitrification",
  "Obs_IceSnowDenitrif" = "Ice snow denitrification",
  "Obs_Dfdiscardp" = "Dem.fish discard/catch",
  "Obs_Denitrif" = "Denitrification",
  
  # Nutrients
  "Obs_snow_ammonia" = "Snow ammonia",
  "Obs_ice_ammonia" = "Ice ammonia",
  "Obs_s_x_ammonia" = "Sand porewater ammonia",
  "Obs_d_x_ammonia" = "Mud porewater ammonia",
  "Obs_snow_nitrate" = "Snow nitrate",
  "Obs_ice_nitrate" = "Ice nitrate",
  "Obs_s_x_nitrate" = "Sand porewater nitrate",
  "Obs_d_x_nitrate" = "Mud porewater nitrate",
  "Obs_s_x_TON" = "Sand %TON",
  "Obs_d_x_TON" = "Mud %TON",
  
  # Seasonal nutrients
  "Obs_NDJF_s_chl" = "Winter surf.chlorophyll",
  "Obs_MJJA_s_chl" = "Summer surf.chlorophyll",
  "Obs_NDJF_s_nitrate" = "Winter surf.nitrate",
  "Obs_MJJA_s_nitrate" = "Summer surf.nitrate",
  "Obs_NDJF_d_nitrate" = "Winter deep nitrate",
  "Obs_MJJA_d_nitrate" = "Summer deep nitrate",
  "Obs_NDJF_s_ammonia" = "Winter surf.ammonia",
  "Obs_MJJA_s_ammonia" = "Summer surf.ammonia",
  "Obs_NDJF_d_ammonia" = "Winter deep ammonia",
  "Obs_MJJA_d_ammonia" = "Summer deep ammonia",
  "Obs_AMJJAS_offshore_ice_alg" = "Summer chlorophyll offshore sea ice",
  "Obs_AMJJAS_inshore_ice_alg" = "Summer chlorophyll inshore sea ice",
  
  # Ratios
  "Obs_carn_io_ratio" = "Carniv.zooplankton",
  "Obs_omni_io_ratio" = "Omniv.zooplankton",
  "Obs_phyt_io_ratio" = "Surf.phytoplankton",
  "Obs_nit_io_ratio" = "Surf.nitrate",
  "Obs_amm_io_ratio" = "Surf.ammonia",
  "Obs_pfish_io_ratio" = "Plank.fish",
  "Obs_dfish_io_ratio" = "Dem.fish",
  
  # Bycatch
  "Obs_birddisc" = "Bird by-catch",
  "Obs_sealdisc" = "Seal by-catch",
  "Obs_cetadisc" = "Cetacean by-catch",
  
  # Kelp
  "Obs_kelp_beachcast" = "Kelp beach-cast"
)

assign_group <- function(original_names, map) {
  out <- map[original_names]
  out[is.na(out)] <- "Unknown"
  return(out)
}

# assign


# Define all groups
group_map <- c(
  # ---- Annual production rates ----
  "Obs_KelpP" = "Annual production rates",
  "Obs_IAPP" = "Annual production rates",
  "Obs_TAPP" = "Annual production rates",
  "Obs_NP" = "Annual production rates",
  "Obs_WCSedDenitrif" = "Annual production rates",
  "Obs_IceSnowDenitrif" = "Annual production rates",
  "Obs_Denitrif" = "Annual production rates",
  "Obs_OmnizooP" = "Annual production rates",
  "Obs_CarnzooP" = "Annual production rates",
  "Obs_BensuspP" = "Annual production rates",
  "Obs_BencarnP" = "Annual production rates",
  "Obs_PFishP" = "Annual production rates",
  "Obs_DFishP" = "Annual production rates",
  "Obs_birdP" = "Annual production rates",
  "Obs_sealP" = "Annual production rates",
  "Obs_cetaP" = "Annual production rates",
  "Obs_bearP" = "Annual production rates",
  "Obs_maxbenthslar" = "Annual production rates",
  "Obs_maxbenthclar" = "Annual production rates",
  
  # ---- Annual fishery landings and by-catch ----
  "Obs_Pland_livewt" = "Annual fishery landings and by-catch",
  "Obs_Dland_livewt" = "Annual fishery landings and by-catch",
  "Obs_Mland_livewt" = "Annual fishery landings and by-catch",
  "Obs_Bsland_livewt" = "Annual fishery landings and by-catch",
  "Obs_Bcland_livewt" = "Annual fishery landings and by-catch",
  "Obs_Zcland_livewt" = "Annual fishery landings and by-catch",
  "Obs_Slland_livewt" = "Annual fishery landings and by-catch",
  "Obs_Ctland_livewt" = "Annual fishery landings and by-catch",
  "Obs_Kland_livewt" = "Annual fishery landings and by-catch",
  "Obs_cetadisc" = "Annual fishery landings and by-catch",
  "Obs_sealdisc" = "Annual fishery landings and by-catch",
  "Obs_birddisc" = "Annual fishery landings and by-catch",
  "Obs_Dfdiscardp" = "Annual fishery landings and by-catch",
  
  # ---- Annual consumption rates ----
  "Obs_Conzoocarnz" = "Annual consumption rates",
  "Obs_Conzoofish" = "Annual consumption rates",
  "Obs_Conbenfish" = "Annual consumption rates",
  "Obs_Conpfishfish" = "Annual consumption rates",
  "Obs_Condfishfish" = "Annual consumption rates",
  "Obs_Contotal_bird" = "Annual consumption rates",
  "Obs_Contotal_seal" = "Annual consumption rates",
  "Obs_Contotal_ceta" = "Annual consumption rates",
  "Obs_Contotal_bear" = "Annual consumption rates",
  "Obs_Proppfishbird" = "Annual consumption rates",
  "Obs_Propdfishbird" = "Annual consumption rates",
  "Obs_Propmfishbird" = "Annual consumption rates",
  "Obs_Propdiscbird" = "Annual consumption rates",
  "Obs_Proppfishseal" = "Annual consumption rates",
  "Obs_Propdfishseal" = "Annual consumption rates",
  "Obs_Propmfishseal" = "Annual consumption rates",
  "Obs_Proppfishceta" = "Annual consumption rates",
  "Obs_Propdfishceta" = "Annual consumption rates",
  "Obs_Propmfishceta" = "Annual consumption rates",
  "Obs_Propzooceta" = "Annual consumption rates",
  "Obs_Propsealbear" = "Annual consumption rates",
  "Obs_Propcetabear" = "Annual consumption rates",
  
  # ---- Annual PB and other ratios ----
  "Obs_kelp_pb" = "Annual P/B and other ratios",
  "Obs_kelp_NC" = "Annual P/B and other ratios",
  "Obs_exud_C_kelp" = "Annual P/B and other ratios",
  "Obs_kelp_beachcast" = "Annual P/B and other ratios",
  "Obs_omni_pb" = "Annual P/B and other ratios",
  "Obs_benslar_pb" = "Annual P/B and other ratios",
  "Obs_benclar_pb" = "Annual P/B and other ratios",
  "Obs_fishplar_pb" = "Annual P/B and other ratios",
  "Obs_fishdlar_pb" = "Annual P/B and other ratios",
  "Obs_carn_pb" = "Annual P/B and other ratios",
  "Obs_bens_pb" = "Annual P/B and other ratios",
  "Obs_benc_pb" = "Annual P/B and other ratios",
  "Obs_fishp_pb" = "Annual P/B and other ratios",
  "Obs_fishd_pb" = "Annual P/B and other ratios",
  "Obs_fishm_pb" = "Annual P/B and other ratios",
  "Obs_bird_pb" = "Annual P/B and other ratios",
  "Obs_seal_pb" = "Annual P/B and other ratios",
  "Obs_ceta_pb" = "Annual P/B and other ratios",
  "Obs_bear_pb" = "Annual P/B and other ratios",
  
  # ---- Average nutrient concentrations ----
  "Obs_snow_ammonia" = "Average nutrient concentrations",
  "Obs_ice_ammonia" = "Average nutrient concentrations",
  "Obs_s_x_ammonia" = "Average nutrient concentrations",
  "Obs_d_x_ammonia" = "Average nutrient concentrations",
  "Obs_snow_nitrate" = "Average nutrient concentrations",
  "Obs_ice_nitrate" = "Average nutrient concentrations",
  "Obs_s_x_nitrate" = "Average nutrient concentrations",
  "Obs_d_x_nitrate" = "Average nutrient concentrations",
  "Obs_s_x_TON" = "Average nutrient concentrations",
  "Obs_d_x_TON" = "Average nutrient concentrations",
  "Obs_NDJF_s_chl" = "Average nutrient concentrations",
  "Obs_MJJA_s_chl" = "Average nutrient concentrations",
  "Obs_NDJF_s_nitrate" = "Average nutrient concentrations",
  "Obs_MJJA_s_nitrate" = "Average nutrient concentrations",
  "Obs_NDJF_d_nitrate" = "Average nutrient concentrations",
  "Obs_MJJA_d_nitrate" = "Average nutrient concentrations",
  "Obs_NDJF_s_ammonia" = "Average nutrient concentrations",
  "Obs_MJJA_s_ammonia" = "Average nutrient concentrations",
  "Obs_NDJF_d_ammonia" = "Average nutrient concentrations",
  "Obs_MJJA_d_ammonia" = "Average nutrient concentrations",
  "Obs_AMJJAS_offshore_ice_alg" = "Average nutrient concentrations",
  "Obs_AMJJAS_inshore_ice_alg" = "Average nutrient concentrations",
  
  # ---- Inshore:offshore ratios ----
  "Obs_amm_io_ratio" = "Inshore:offshore ratios",
  "Obs_nit_io_ratio" = "Inshore:offshore ratios",
  "Obs_phyt_io_ratio" = "Inshore:offshore ratios",
  "Obs_omni_io_ratio" = "Inshore:offshore ratios",
  "Obs_carn_io_ratio" = "Inshore:offshore ratios",
  "Obs_pfish_io_ratio" = "Inshore:offshore ratios",
  "Obs_dfish_io_ratio" = "Inshore:offshore ratios",
  "Obs_bird_io_ratio" = "Inshore:offshore ratios",
  "Obs_seal_io_ratio" = "Inshore:offshore ratios",
  "Obs_ceta_io_ratio" = "Inshore:offshore ratios",
  "Obs_bear_io_ratio" = "Inshore:offshore ratios"
)






family_target_comparison_plot <- function(
    combined_models_df,
    model_1,
    model_2,
    family,
    xlabel = "",
    m1_2_offset = 0.5,
    error_end_width = 0.3,
    transformation_scale = function(x) x) {
  combined_models_df <- combined_models_df[combined_models_df$family == family, ]
  combined_models_df <- combined_models_df[!is.na(combined_models_df$Annual_measure), ]
  
  vars <- unique(combined_models_df$var_long_names)
  for (var in vars) {
    if (nrow(combined_models_df[combined_models_df$var_long_names == var & is.finite(combined_models_df$Annual_measure), ]) < 2) {
      vars[vars == var] <- NA
    }
  }
  vars <- vars[!is.na(vars)]
  if (length(vars) < 1) {
    plot.new()
    text(0.5, 0.5, paste0("No data available for ", family), cex = 1.2)
    
    return(invisible(NULL))
  }
  
  combined_models_df <- combined_models_df[combined_models_df$var_long_names %in% vars, ]
  
  df <- expand.grid(var_long_names = vars, model = unique(combined_models_df$model))
  df <- merge(df, combined_models_df, by = c("var_long_names", "model"), all.x = TRUE)
  df <- df[order(df$var_long_names), ]
  
  df$transformed_mean <- transformation_scale(df$Annual_measure)
  df$transformed_sd <- transformation_scale(df$SD_of_measure)
  df$transformed_mean <- ifelse(df$transformed_mean == -Inf, 0, df$transformed_mean)
  df$transformed_sd <- ifelse(df$transformed_sd == -Inf, 0, df$transformed_sd)
  
  base_pos <- 1:length(vars) * 2
  pos_model1 <- base_pos - m1_2_offset
  pos_model2 <- base_pos + m1_2_offset
  
  model1_df <- df[df$model == model_1, ]
  model2_df <- df[df$model == model_2, ]
  
  # xlims = range(df$transformed_mean - df$transformed_sd, df$transformed_mean + df$transformed_sd, na.rm=TRUE)
  xlims <- c(
    if (min(df$transformed_mean, na.rm = TRUE) < 0) {
      min(df$transformed_mean, na.rm = TRUE) + (0.1 * min(df$transformed_mean, na.rm = TRUE))
    } else {
      min(df$transformed_mean, na.rm = TRUE) - (0.1 * min(df$transformed_mean, na.rm = TRUE))
    },
    max(df$transformed_mean, na.rm = TRUE) + (0.1 * max(df$transformed_mean, na.rm = TRUE))
  )
  
  # par(mar = c(6, 8, 4, 2))
  plot(
    1,
    type = "n",
    ylim = c(0, length(vars) * 2 + 1),
    xlim = xlims,
    yaxt = "n",
    ylab = "",
    xlab = ""
  )
  mtext(xlabel, cex = 0.75, side = 1, line = 2)
  boxplot(
    transformed_mean ~ var_long_names,
    data = model1_df,
    at = pos_model1,
    outwex = 0.6,
    border = "red",
    add = TRUE,
    horizontal = TRUE,
    yaxt = "n"
  )
  boxplot(
    transformed_mean ~ var_long_names,
    data = model2_df,
    at = pos_model2,
    outwex = 0.6,
    col = "black",
    add = TRUE,
    horizontal = TRUE,
    yaxt = "n"
  )
  
  
  axis(2, at = base_pos, labels = vars, las = 2)
  
  segments(
    model1_df$transformed_mean - model1_df$transformed_sd,
    pos_model1,
    model1_df$transformed_mean + model1_df$transformed_sd,
    pos_model1
  )
  segments(
    model2_df$transformed_mean - model2_df$transformed_sd,
    pos_model2,
    model2_df$transformed_mean + model2_df$transformed_sd,
    pos_model2
  )
  
  segments(
    model1_df$transformed_mean - model1_df$transformed_sd,
    as.numeric(pos_model1) - error_end_width,
    model1_df$transformed_mean - model1_df$transformed_sd,
    as.numeric(pos_model1) + error_end_width
  )
  segments(
    model1_df$transformed_mean + model1_df$transformed_sd,
    as.numeric(pos_model1) - error_end_width,
    model1_df$transformed_mean + model1_df$transformed_sd,
    as.numeric(pos_model1) + error_end_width
  )
  segments(
    model2_df$transformed_mean - model2_df$transformed_sd,
    as.numeric(pos_model2) - error_end_width,
    model2_df$transformed_mean - model2_df$transformed_sd,
    as.numeric(pos_model2) + error_end_width
  )
  segments(
    model2_df$transformed_mean + model2_df$transformed_sd,
    as.numeric(pos_model2) - error_end_width,
    model2_df$transformed_mean + model2_df$transformed_sd,
    as.numeric(pos_model2) + error_end_width
  )
}

e2ep_compare_annual_obs_models <- function(
    model_1,
    model_2,
    g_map = group_map,
    n_map = name_map) {
  family_labels <- c(
    "Annual production rates" = bquote("Production log"[10] ~ "mMN.m"^-2 * ".y"^-1),
    "Annual fishery landings and by-catch" = bquote("Catch log"[10] ~ "mMN.m"^-2 * ".y"^-1),
    "Annual consumption rates" = bquote("Consumption log"[10] ~ "mMN.m"^-2 * ".y"^-1),
    "Annual P/B and other ratios" = bquote("log"[10] ~ "Annual ratio"),
    "Average nutrient concentrations" = bquote("Conc. log"[10] ~ "mMN.m"^-3 ~ "or % by weight"),
    "Inshore:offshore ratios" = bquote("Inshore:offshore ratios")
  )
  family_scales <- c(
    "Annual production rates" = log10,
    "Annual fishery landings and by-catch" = log10,
    "Annual consumption rates" = log10,
    "Annual P/B and other ratios" = log10,
    "Average nutrient concentrations" = log10,
    "Inshore:offshore ratios" = function(x) x
  )
  
  family_info <- vector("list", length(family_labels))
  names(family_info) <- names(family_labels)
  for (family in names(family_info)) {
    family_info[[family]] <- c(
      name = family,
      label = unname(family_labels[family]),
      scale = unname(family_scales[family])
    )
  }
  
  m1_target <- StrathE2EPolar:::read_annual_target_data(model_1$setup$model.path)
  m1_name <- model_1$setup$model.name
  m1_target$model <- m1_name
  
  m2_target <- StrathE2EPolar:::read_annual_target_data(model_2$setup$model.path)
  m2_name <- model_2$setup$model.name
  m2_target$model <- m2_name
  
  m1_2_target <- rbind(m1_target, m2_target)
  
  m1_2_target$var_long_names <- assign_group(m1_2_target$Name, n_map)
  m1_2_target$family <- assign_group(m1_2_target$Name, g_map)
  
  par(mfrow = c(3, 2), mar = c(4, 10.5, 1, 1))
  
  invisible(lapply(
    family_info,
    function(family) {
      family_target_comparison_plot(
        combined_models_df = m1_2_target,
        model_1 = m1_name,
        model_2 = m2_name,
        family = family$name,
        xlabel = family$label,
        transformation_scale = family$scale
      )
    }
  ))
  
  legend(grconvertX(0.5, "ndc", "user"), grconvertY(0.08, "ndc", "user"),
         m1_name,
         fill = "red", ncol = 1, bty = "n", xpd = NA
  )
  legend(grconvertX(0.5, "ndc", "user"), grconvertY(0.05, "ndc", "user"),
         m2_name,
         fill = "black", ncol = 1, bty = "n", xpd = NA
  )
}
