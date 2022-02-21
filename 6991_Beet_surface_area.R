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
  snapshot_date = "2021-11-01"
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
  Rpackages_version = c("tidyr_1.1.4", "ggplot2_3.3.5",
                        "readxl_1.3.1", "dplyr_1.0.7",
                        "writexl_1.4.0"
                        )
  path_Rpackages = "C:/R packages_412"
  # ------------------------------------------
  
  # version check and load packages
  # -------------------------------------------
  # R version check
  if(sessionInfo()$R.version$version.string != "R version 4.1.2 (2021-11-01)") stop("R.version must be 4.1.2 (2021-11-01)")
  
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

# READ IN FIELD DATA
dat_in <- data.frame(read_excel("RawData_Beet_Size_and_Damage.xlsx", sheet="Rawdata_beet", col_names=T,skip=0))

# Define model fixed parameters
slo <- 0.8
den <- 1.1
top <- 2    # ratio of mean(r_y_xt, r_z_xt):x_top

r01_a <- c(1,1,2,3,2)
r01_b <- c(2,0,0,0,1)
r12_a <- c(1,1,1,1,1)
r12_b <- c(1,1,1,1,1)
  
# Calculate

dat_in <- dat_in %>% mutate(y_x0 = diameter_max/2)
dat_in <- dat_in %>% mutate(y_x1 = y_x0*slo)
dat_in <- dat_in %>% mutate(y_x3 = rotspetsbrot_max/2)
dat_in <- dat_in %>% mutate(y_x2 = y_x3/slo)

dat_in <- dat_in %>% mutate(z_x0 = diameter_min/2)
dat_in <- dat_in %>% mutate(z_x1 = z_x0*slo)
dat_in <- dat_in %>% mutate(z_x3 = rotspetsbrot_min/2)
dat_in <- dat_in %>% mutate(z_x2 = z_x3/slo)

dat_in <- dat_in %>% rowwise %>% mutate(yz_x0 = mean(c(y_x0, z_x0)))
dat_in <- dat_in %>% rowwise %>% mutate(yz_x1 = mean(c(y_x1, z_x1)))
dat_in <- dat_in %>% rowwise %>% mutate(yz_x2 = mean(c(y_x2, z_x2)))
dat_in <- dat_in %>% rowwise %>% mutate(yz_x3 = mean(c(y_x3, z_x3)))

dat_in <- dat_in %>% mutate(xt = yz_x0/top)
dat_in <- dat_in %>% mutate(x0 = 0)
dat_in <- dat_in %>% mutate(x3 = längd - xt)

dat_in <- drop_na(dat_in, xt)

dat_in <- dat_in %>% mutate(v_tot = vikt/den)
dat_in <- dat_in %>% mutate(v_top = (4/3*pi*xt*yz_x0*yz_x0)/2)
dat_in <- dat_in %>% mutate(v_x0x3_1 = v_tot - v_top)

fn_diff <- function(r, r01_a, r01_b, r12_a, r12_b, x0, x3, yz_x0, yz_x1, yz_x2, yz_x3, v_x0x3_1) {
  r01 <- r01_a*r^r01_b
  r12 <- r12_a*r^r12_b
  r23 <- 1
  r_tot <- r01 + r12 + r23
  
  x01 <- r01/r_tot*(x3-x0)
  x12 <- r12/r_tot*(x3-x0)
  x23 <- r23/r_tot*(x3-x0)

  v_x0x1 <- 1/3*pi*(x01)*(yz_x0^2+yz_x1^2+yz_x0*yz_x1)
  v_x1x2 <- 1/3*pi*(x12)*(yz_x1^2+yz_x2^2+yz_x1*yz_x2)
  v_x2x3 <- 1/3*pi*(x23)*(yz_x2^2+yz_x3^2+yz_x2*yz_x3)
  v_x0x3_2 <- v_x0x1 + v_x1x2 + v_x2x3
  
  diff <- (v_x0x3_1 - v_x0x3_2)^2
}

for(i in 1:length(r01_a)){
  dat_in <- dat_in %>%
    rowwise() %>%
    mutate(r = optimize(fn_diff, c(0, 100), r01_a = r01_a[i], r01_b = r01_b[i], r12_a = r12_a[i], r12_b = r12_b[i], x0 = x0, x3 = x3, yz_x0 = yz_x0, yz_x1 = yz_x1, yz_x2 = yz_x2, yz_x3 = yz_x3, v_x0x3_1 = v_x0x3_1)$minimum)
  
  names(dat_in)[names(dat_in) == "r"] <- paste0("r_",i)
}

fn_sa_xtx3 <- function(r, r01_a, r01_b, r12_a, r12_b, xt, x0, x3, yz_x0, yz_x1, yz_x2, yz_x3, v_x0x3_1) {
  r01 <- r01_a*r^r01_b
  r12 <- r12_a*r^r12_b
  r23 <- 1
  r_tot <- r01 + r12 + r23
  
  x01 <- r01/r_tot*(x3-x0)
  x12 <- r12/r_tot*(x3-x0)
  x23 <- r23/r_tot*(x3-x0)
  
  sa_xtx0 <- (4*pi*(((yz_x0*yz_x0)^1.6+(yz_x0*xt)^1.6+(yz_x0*xt)^1.6)/3)^(1/1.6))/2
  sa_x0x1 <- pi*(yz_x0 + yz_x1)*((yz_x0 - yz_x1)^2+x01^2)^0.5
  sa_x1x2 <- pi*(yz_x1 + yz_x2)*((yz_x1 - yz_x2)^2+x12^2)^0.5
  sa_x2x3 <- pi*(yz_x2 + yz_x3)*((yz_x2 - yz_x3)^2+x23^2)^0.5 + pi*(yz_x3)^2
  sa_xtx3 <- sa_xtx0 + sa_x0x1 + sa_x1x2 + sa_x2x3
  
  sa_xtx3
}

# NEED TO MANUALLY CHANGE THE # AT: i = #L, sa_# =, and r= r_#
i=5L
dat_in <- dat_in %>%
    rowwise() %>%
    mutate(sa_5 = fn_sa_xtx3(r = r_5, r01_a = r01_a[i], r01_b = r01_b[i], r12_a = r12_a[i], r12_b = r12_b[i], xt= xt, x0 = x0, x3 = x3, yz_x0 = yz_x0, yz_x1 = yz_x1, yz_x2 = yz_x2, yz_x3 = yz_x3, v_x0x3_1 = v_x0x3_1))

# columns (i.e. model) mean surface areas
sum_tab <- dat_in %>%
  group_by() %>%
  summarise(sa_m1 = mean(sa_1), sa_m2 = mean(sa_2), sa_m3 = mean(sa_3), 
            sa_m4 = mean(sa_4), sa_m5 = mean(sa_5), wei = mean(vikt), 
            sa_wei = mean(sa_1)/mean(vikt)) 
  
# columns (i.e. model) mean surface areas
sum_tab_2 <- dat_in %>%
  group_by(r_s_l) %>%
  summarise(sa_m1 = mean(sa_1), sa_m2 = mean(sa_2), sa_m3 = mean(sa_3), 
            sa_m4 = mean(sa_4), sa_m5 = mean(sa_5), wei = mean(vikt), 
            sa_wei = mean(sa_1)/mean(vikt)) 

dat_in <- drop_na(dat_in, ytaskador)

sum_tab_3 <- dat_in %>%
  group_by(r_s_l) %>%
  summarise(yta_sa = mean(ytaskador)/mean(sa_1))

write_xlsx(path = "sum_tabs.xlsx", list(models = sum_tab, models_by_size = sum_tab_2, skador_by_size = sum_tab_3))
