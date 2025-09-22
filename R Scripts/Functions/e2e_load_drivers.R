e2e_load_drivers <- function(model_base_path) {
  #' Load all possible physics and chemistry driving values from model variants.
  #'
  #' @description This function creates a master dataframe containing all variants of
  #' physics and chemistry drivers. (Note that some drivers do not vary across model variants).
  #'
  #' @param model_base_path character. Base model folder that contains variant subfolders.
  #' @return Longform dataframe containing all possible variant driving data for chemistry and physics variables.
  #'
  #' @references StrathE2E2
  #' @examples
  #' load_all_possible_drivers("Models/South_Africa_MA/", "South_Africa_MA")
  
  # --- discover variants ---
  possible_variants <- dir(model_base_path)
  possible_variants <- unique(stringr::str_extract(possible_variants, "\\d{4}-\\d{4}"))
  possible_variants <- possible_variants[!is.na(possible_variants)]
  if (length(possible_variants) == 0) stop("No variant folders found under the provided model base path.")
  
  # --- master skeleton ---
  master_forcings <- expand.grid(
    Month = 1:12,
    variant = possible_variants,
    stringsAsFactors = FALSE
  )
  
  # --- inspect one variant to get column names (physics / chemistry naming isn't always consistent) ---
  first_driving <- list.files(file.path(model_base_path, possible_variants[1], "Driving"),
                              full.names = TRUE)
  phys_file <- first_driving[grepl("physics", first_driving, ignore.case = TRUE)][1]
  chem_file <- first_driving[grepl("chemistry", first_driving, ignore.case = TRUE)][1]
  if (is.na(phys_file) || is.na(chem_file)) stop("Couldn't find physics/chemistry files in first variant")
  
  se2e_physics_names <- setdiff(colnames(read.csv(phys_file, stringsAsFactors = FALSE)), "Month")
  se2e_chemistry_names <- setdiff(colnames(read.csv(chem_file, stringsAsFactors = FALSE)), "Month")
  driver_cols <- unique(c(se2e_physics_names, se2e_chemistry_names))
  
  # --- create numeric placeholders (force double due to rows_update logical conversion) ---
  var_names <- as.data.frame(matrix(NA_real_, nrow = nrow(master_forcings), ncol = length(driver_cols)))
  colnames(var_names) <- driver_cols
  master_forcings <- cbind(master_forcings, var_names)
  
  # --- loop through variants and update rows ---
  for (variant in possible_variants) {
    drivers <- list.files(file.path(model_base_path, variant, "Driving"), full.names = TRUE)
    phys_file <- drivers[grepl("physics", drivers, ignore.case = TRUE)]
    chem_file <- drivers[grepl("chemistry", drivers, ignore.case = TRUE)] # pull only physics and chemistry - naming inconsistencies
    
    if (!is.na(phys_file)) {
      physics <- read.csv(phys_file, stringsAsFactors = FALSE)
      physics$variant <- variant
      physics$Month <- as.integer(physics$Month)
      
      phys_cols <- intersect(names(physics), driver_cols)
      phys_cols <- setdiff(phys_cols, c("Month", "variant"))
      }
      
      master_forcings <- dplyr::rows_update(master_forcings, physics, by = c("Month", "variant"))
    
    if (!is.na(chem_file)) {
      chemistry <- read.csv(chem_file, stringsAsFactors = FALSE)
      chemistry$variant <- variant
      chemistry$Month <- as.integer(chemistry$Month)
      
      chem_cols <- intersect(names(chemistry), driver_cols)
      chem_cols <- setdiff(chem_cols, c("Month", "variant"))
      }
      
      master_forcings <- dplyr::rows_update(master_forcings, chemistry, by = c("Month", "variant"))
    }
  
  return(master_forcings)
}
