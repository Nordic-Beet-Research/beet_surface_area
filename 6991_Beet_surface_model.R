############################################
############################################
##
## 699.1 Beet size - surface area model
##
## ML with TensorFlow
##
## This project uses R 4.1.1 (2021-08-10) 
## with package snapshot date 2021-10-18
##
############################################
############################################

############################################
# Setup

{
  # -------------------------------------------
  snapshot_date = "2021-10-18"
  options("repos" = paste0("https://mran.revolutionanalytics.com/snapshot/", snapshot_date))
  # -------------------------------------------
  
  # -------------------------------------------
  # sink options
  options(width = 150)
  # rJava memory option
  options(java.parameters = "-Xmx8000m")
  # -------------------------------------------
  
  # R packages
  # -------------------------------------------
  Rpackages_version = c("tidyr_1.1.4", "ggplot2_3.3.5"
                        )
  path_Rpackages = "C:/R packages_411"
  # -------------------------------------------
  
  # version check and load packages
  # -------------------------------------------
  # R version check
  if(sessionInfo()$R.version$version.string != "R version 4.1.1 (2021-08-10)") stop("R.version must be 4.1.1 (2021-08-10)")
  
  # install packages
  Rpack = sapply(strsplit(Rpackages_version, "_", fixed = T), FUN = function(x) x[1])
  Rpack_version = sapply(strsplit(Rpackages_version, "_", fixed = T), FUN = function(x) x[2])
  if(!all(Rpack %in% list.files(path_Rpackages))){
    loadRpackages <- Rpack[!Rpack %in% list.files(path_Rpackages)]
    for(i in loadRpackages) install.packages(i, lib = path_Rpackages, repos = options("repos"), dependencies = T)
  }
  
  # load packages
  for(i in Rpack) eval(parse(text = paste0("library(", i, ", lib.loc = '", path_Rpackages, "')")))
}

############################################

widest_yx <- 1/3

widest_y_d <- 12
widest_y_r <- widest_y_d/2
widest_z_d <- 10
widest_z_r <- widest_z_d/2
top_x_r <- widest_y_r * widest_yx
length <- 22
widest_x_r <- length - top_x_r
tip_y_d <- 3
tip_y_r <- tip_y_d/2
tip_z_d <- 2
tip_z_r <- tip_z_d/2

density <- 1.1
weight <- 1200
volume <- weight/density

# Volume of the top is taken as half of the volume of an ellipsoid (4/3.pi.abc, where a,b,c are the radius on each axis)
top_volume <- 0.5*pi*widest_y_r*widest_z_r*top_x_r
main_volume <- volume - top_volume

# area of the top is taken as half the area of an ellipsoid, approximated using Knud Thomsen's formula
p <- 1.6075
top_area <- 0.5 * 4 * pi *(((widest_y_r*widest_z_r)^p+(widest_y_r*top_x_r)^p+(top_x_r*widest_z_r)^p)/3)^(1/p)

# area of the main body of the beet
#??????????