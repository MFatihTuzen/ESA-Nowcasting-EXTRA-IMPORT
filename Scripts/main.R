
# Nowcasting algorithm of EXTRA IMPORT 

get_nowcast <- function(nowcast_period){

# Create Directories ------------------------------------------------------

if(!dir.exists(paste0("Data/",nowcast_period))){
  dir.create(paste0("Data/",nowcast_period))
}

if(!dir.exists(paste0("Results/",nowcast_period))){
  dir.create(paste0("Results/",nowcast_period))
}

# Source R Codes ----------------------------------------------------------

source("./scripts/global.R", local = TRUE)
source("./scripts/1.utilities.R", local = TRUE)

if(format(Sys.Date(), "%Y-%m") == nowcast_period)
{
  
source("./scripts/2.data_import.R", local = TRUE)
source("./scripts/3.data_preparation.R", local = TRUE)
cat(paste0("Data Preparation Process Completed\n"))
  
}

source("./scripts/4.tramo_model_mv1.R", local = TRUE)
cat(paste0("TRAMO MODEL MULTIVARIATE 1 Process Completed\n"))
source("./scripts/5.tramo_model_uv.R", local = TRUE)
cat(paste0("TRAMO MODEL UNIVARIATE Process Completed\n"))
source("./scripts/6.tramo_model_average.R", local = TRUE)
cat(paste0("TRAMO MODEL AVERAGE Process Completed\n"))
source("./scripts/7.tramo_model_mv2.R", local = TRUE)
cat(paste0("TRAMO MODEL MULTIVARIATE 2 Process Completed\n"))
source("./scripts/8.export_results.R", local = TRUE)
}
