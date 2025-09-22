#
# e2e_read.R
#
#' Read all the configuration, driving and parameter files for a designated model and compile the data into a list object.
#'
#' This function is the key point of entry to the package. It reads all the csv files containing the configuration, driving and parameter values which define
#' a model for a particular region and compiles them into a single R-list object. This data object forms the basis for almost all other functions in the package.
#'
#' The input csv files are specified in the MODEL_SETUP.csv file located
#' in the Model/variant folder specified by the argument models.path in a e2e_read() call, or from the extdata/Models folder
#' in the package installation. The models supplied with the package are two variants (1970-1999 and 2003-2013) of a fully
#' parameterised and documented model of the North Sea.
#'
#' Starting from a baseline model configuration defined in the MODEL_SETUP.csv file, any of the terms in the R-list objcet created by e2e_read() can be modified by coding
#' to produce an unlimited range of scenario configurations representing, for example, changes in the physical environment, changes in the composition or activity of the
#' fishing fleets, or any combination of these. The power of this approach is that large numbers of scenarios can be coded in standard R and simulated without any need
#' for manual editing of input files.
#'
#' @param model.name Name of model to read (no default).
#' @param model.variant Variant of the model to be read (no default).
#' @param models.path Relative path from the current working directory to a folder containing a library of model configurations (typically "Folder/Models"). Setting models.path="" is valid. Default models.path=NULL, meaning read a North Sea model setup from the package folder extdata/Models.
#' @param results.path Relative path from the current working directory to a folder for writing and reading model output files (e.g. "E2E_results"). Setting results.path="" is valid. Model-specific sub-folders will be assigned and if necessary auto-created according to the model name and variant. Default results.path=NULL, meaning write/read to/from a temporary directory.
#' @param results.subdir Subdirectory of "working_directory/results.path/model_name/model_variant" to be created if required. (Default="", meaning no subdirectory will be created).
#' @param model.ident Identifier text appended to output file names (e.g. OFFSHORE_model_annualresults-TEXT.csv instead of just OFFSHORE_model_annualresults.csv). (Default="base").
#' @param quiet Logical. If FALSE then see on-screen information on indvidual parameter files as they are read (default=TRUE).
#' @param silent Logical. If FALSE then see on-screen information on model and results paths (default=FALSE). If set TRUE then this over-rides any quiet=FALSE setting and forces quiet=TRUE.
#'
#' @return R-list object containing all of the model configuration data.
#'
#' @seealso \code{\link{e2e_ls}}, \code{\link{e2e_copy}}, \code{\link{e2e_run}}
#'
#' @export
#
#' @examples
#' # Load the 1970-1999 version of the North Sea model supplied with the package; outputs go
#' # to a temporary results folder; default model.ident setting.
#' # Default screen information settings - on-screen parameter file information suppressed but  
#' # path information enabled:
#'     model <- e2e_read("North_Sea", "1970-1999")
#'
#' # User-specified model.ident; on-screen parameter file and path information enabaled:
#'     model <- e2e_read("North_Sea", "1970-1999",model.ident="baseline",quiet=FALSE)
#'
#' # All on-screen information suppressed even though quiet=FALSE:
#'     model <- e2e_read("North_Sea", "1970-1999",model.ident="baseline",quiet=FALSE,silent=TRUE)
#'
#' # View details of the structure and contents of the model list-object:
#'     str(model,max.level=1)
#'     str(model,max.level=2)
#'
#' # Dummy example to illustrate loading a user defined model from a user workspace
#' # ... substitute your own path details for e.g. "Folder/Models":
#' #     model<-e2e_read("Modelname", "Variantname",
#' #                      models.path="Folder/Models",
#' #                      model.ident="demo")
#'
#' # Dummy example to illustrate loading a user defined model from a user workspace and
#' # sending the model outputs to a specified folder ... substitute your own path details
#' # for the dummy paths shown here 
#' # for "Folder/Models" and "Folder/results":
#' #     model<-e2e_read("Modelname", "Variantname",
#' #                      models.path="Folder/Models",
#' #                      results.path="Folder/results",
#' #                      model.ident="demo")
#'
#' # Create a new scenario version of the North Sea/1970-1999 model by increasing the
#' # activity rate of gear 1 (pelagic trawls and seines) by a factor of 2:
#'     model <- e2e_read("North_Sea", "1970-1999")
#'     scenario_model <- model
#'     scenario_model$data$fleet.model$gear_mult[1] <- 2
#' # Set a new identifier for any csv outputs:
#'     scenario_model$setup$model.ident <- "gear1x2"
#'
#' # Run the baseline model for 2 years:
#'     results<-e2e_run(model,nyears=2)
#' # Visualise the time series of output from the baseline model run:
#'     e2e_plot_ts(model,results,selection="ECO")
#'
#' \donttest{
#' # Run the scenario model for 20 years:
#'     scenario_results<-e2e_run(scenario_model,nyears=20)
#' # Visualise the time series of output from the scenario model run
#' # in a new graphics window:
#'     dev.new()
#'     e2e_plot_ts(scenario_model,scenario_results,selection="ECO")
#' }
#'
#'
#
# ---------------------------------------------------------------------
# |                                                                   |
# | Authors: Mike Heath, Ian Thurlbeck, Benjamin Grier, Matthew Hatton|
# | Department of Mathematics and Statistics                          |
# | University of Strathclyde, Glasgow                                |
# |                                                                   |
# | Date of this version: September 2025                              |
# |                                                                   |
# ---------------------------------------------------------------------

e2e_read <- function(model.name, model.variant, models.path=NULL, results.path=NULL, results.subdir="", model.ident="base", quiet=TRUE, silent=FALSE) {
  
  oo <- options()
  on.exit(options(oo))
  
  if(silent==TRUE) quiet<-TRUE
  
  pkg.env$quiet	<- quiet			# quietens down read/write notes
  pkg.env$silent	<- silent			# silences all notes
  
  if(! pkg.env$silent){
    message("Current working directory is... ")
    message("'",getwd(),"'\n")
  }
  
  packagemodel<-FALSE
  if(is.null(models.path)) packagemodel<-TRUE
  
  models.path <- remove.dirsep(models.path)	# remove any trailing '/'
  results.path <- remove.dirsep(results.path)	# remove any trailing '/'
  
  if( ! is.null(models.path)) {
    models.path<-makepath(getwd(),models.path)
    if (! dir.exists(models.path)) {
      mesg <- paste0("Error: could not find the model path '", models.path, "' !\n")
      stop(mesg)
    }
  }
  
  if( ! is.null(results.path)) {
    results.path<-makepath(getwd(),results.path)
    if (! dir.exists(results.path)) {
      mesg <- paste0("Error: could not find the results path '", results.path, "' !\n")
      stop(mesg)
    }
  }
  
  if( is.null(results.path)){
    results.path<-tempdir()
    if (! pkg.env$silent){
      message("No 'results.path' specified so any csv data requested") 
      message("will be directed to/from the temporary directory...")
      message("'",results.path,"'")
      message("")
    }
  }
  
  read.only	<- (is.null(models.path))						# read only unless input path is specified
  
  model.path	<- get.variant.path(model.name, model.variant, models.path)		# full path to either the system model or the user specified one:
  #	resultsdir	<- makepath(MODEL_RESULTS_DIR, model.name, model.variant, results.subdir)	# results/<model>/<variant>/<subdir>
  resultsdir	<- makepath(results.path, model.name, model.variant, results.subdir)	# results/<model>/<variant>/<subdir>
  matching <- makepath(model.path,"Param/Climate_Matching_",model.name,model.variant)
  
  setup <- list(
    read.only	= read.only,
    model.name	= model.name,		# e.g. "NorthSea"
    model.variant	= model.variant,	# e.g. "2000-2013"
    model.ident	= model.ident,		# "free-text"
    model.subdir	= results.subdir,	# "free-text"
    model.path	= model.path,		# e.g. "C:/Users/username/Documents/Folder/Models/NorthSea/2000-2013"
    resultsdir	= resultsdir,		# e.g. "C:/Users/username/Documents/results/NorthSea/2000-2013" ...
    matching = matching
    )
  
  if (! pkg.env$quiet) message("Loading model   : ", model.path)
  
  read.model.setup(model.path)			# e.g. Models/Model/Variant/MODEL_SETUP.csv
  
  # read model inputs:
  physical.parameters	<- read_physical_parameters(model.path)
  fixed.parameters	<- read_fixed_parameters(model.path)
  physics.drivers		<- read_physics_drivers(model.path)
  chemistry.drivers	<- read_chemistry_drivers(model.path)
  biological.events	<- read_biological_event_timings(model.path)
  fitted.parameters	<- read_fitted_parameters(model.path)
  initial.state		<- read_initial_state(model.path)
  fleet.model		<- read_fishing_fleet_model(model.path, physical.parameters)
  biology.scenarios       <- read_biology_scenarios(model.path)
  
  # data slot:
  data <- list(
    # read:
    fixed.parameters	= fixed.parameters,
    fitted.parameters	= fitted.parameters,
    physical.parameters	= physical.parameters,
    physics.drivers		= physics.drivers,
    chemistry.drivers	= chemistry.drivers,
    biological.events	= biological.events,
    fleet.model		= fleet.model,
    initial.state		= initial.state,
    biology.scenarios       = biology.scenarios
  )
  
  model <- list(
    setup		= setup,
    data		= data
  )
  
  if (! pkg.env$silent){
    message("Model setup and parameters gathered from ...") 
    if(packagemodel==FALSE) message("'",model$setup$model.path,"'")
    if(packagemodel==TRUE) message("StrathE2E2 package folder")
  }
  
  if (! pkg.env$silent){
    message("Model results will be directed to/from ...")   
    message("'",model$setup$resultsdir,"'\n")
  }
  
  model
}

