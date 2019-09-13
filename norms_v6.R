

# Calculation of differences in norms for QIDs and DIMs.

# Developers: Oleksandr Romanchenko
# v5.0 - 28/6/2019 

# uncomment the line below and run the code if you don't have these packages installed
# install.packages(c("readxl", "dplyr", "openxlsx", "tidyverse"))

library(readxl)
library(dplyr)
library(openxlsx) 
library(tidyverse)

# clear the workspace, plots and console
rm(list = ls())
if(!is.null(dev.list())) dev.off()
cat("\014")

# --------- loading the data (UPDATE THE PATH HERE) ---------

# the very first data used to create script
# path <- "C:/Users/oleksandr-romanchenk/Desktop/R/EE tasks/Agnieszka/norms"

# project: TERNIUM
# path <- "//plwaw61ns1a.emea.mrshmc.com/mcwawshared$/GPC/06_GOST/01_ADMIN/2018/Sprint_ER/Documentation/NORMS/2. Ternium"
path <- "C:/Users/oleksandr-romanchenk/Desktop/R/EE tasks/Agnieszka/norms/7. Shiloh"

favper_file <- "fav_R_results.csv"
norms_file <- "Norms_S4_export.xlsx"
Theme_file_export <- "S4 Theme file export.csv"

favper <- read.csv(paste0(path, "/", favper_file), stringsAsFactors = FALSE)
norms_Theme <- read.csv(paste0(path, "/", Theme_file_export))
norms_QID <- read_excel(paste0(path, "/", norms_file), sheet = "Questions")
norms <- read_excel(paste0(path, "/", norms_file), sheet = "Norms")
norms_DIM <- read_excel(paste0(path, "/", norms_file), sheet = "Dimensions")
colnames(norms_DIM) [colnames(norms_DIM) == "value"] <- "Value"

# column "Value" has been read as character type so we need to convert it to numeric
norms_QID$Value <- as.numeric(norms_QID$Value)
norms_DIM$Value <- as.numeric(norms_DIM$Value)

# selecting separately parts wit QID and DIM
favper_QID <- favper %>% select(starts_with("QID"))
favper_DIM <- favper %>% select(starts_with("DIM"))

favper_QID[] <- lapply(favper_QID, function(x) as.numeric(x))
favper_DIM[] <- lapply(favper_DIM, function(x) as.numeric(x))

# ------------------------------------------------------------------------------
# ---------------------------- preparing the data ------------------------------
# ------------------------------------------------------------------------------
numbers_QID <- as.numeric(str_extract(colnames(favper_QID), "\\-*\\d+\\.*\\d*"))

  # creating "not in" operator
  `%!in%` = Negate(`%in%`)

# creating lists with number of dataframes equal to number of norms
results_QID <- list()
results_DIM <- list()
results_diff <- list()

for (i in 1:nrow(norms)) {
  
  selected_QID <- favper_QID %>% select_if(numbers_QID %in% norms_QID$QuestionId[norms_QID$NormId == i])
  diff <- favper_QID %>% select_if(numbers_QID %!in% norms_QID$QuestionId[norms_QID$NormId == i])
  diff[] <- "-"
  
  name_QID <- paste("result_diff_QID", i, sep = "_")
  name_DIM <- paste("result_diff_DIM", i, sep = "_")
  name_diff <- paste("result_diff", i, sep = "_")
  results_QID[[name_QID]] <- selected_QID
  results_DIM[[name_DIM]] <- favper_DIM
  results_diff[[name_diff]] <- diff
}

# -------------------------------------------------------------------------------
# --------------------- calculating the difference for QIDs ---------------------
# -------------------------------------------------------------------------------

# iterating through all dataframes
for (i in 1:nrow(norms)) {           # i = number of norms
  selected_QID <- favper_QID %>% select_if(numbers_QID %in% norms_QID$QuestionId[norms_QID$NormId == i])
  numbers_selected_QID <- as.numeric(str_extract(colnames(selected_QID), "\\-*\\d+\\.*\\d*"))
  
  for (j in 1:ncol(selected_QID)) {  # j = number of questions which exist in norms
    
    results_QID[[i]][j] <- selected_QID[j] -                                                 # norm                          
      norms_QID$Value[which(norms_QID$QuestionId == numbers_selected_QID[j] & norms_QID$NormId == i)]
  }
}

# --------------------------------------------------------------------------------
# --------------------- calculating the DIMs differences -------------------------
# --------------------------------------------------------------------------------
for (i in 1:nrow(norms)) {           # i = number of norms
  for (j in 1:ncol(favper_DIM)) {    # j = number of dimensions
    
    # selecting all questions for the current dimension
    itemId <- norms_Theme$itemid[norms_Theme$DimensionID == j]
                                                         
    if (all(itemId %in% norms_QID$QuestionId[norms_QID$NormId == i])) {                     # norm
      mean_norm <- mean(norms_QID$Value[(norms_QID$QuestionId %in% itemId) & norms_QID$NormId == i])
      results_DIM[[i]][j] <- round((favper_DIM[j] - mean_norm), 0)
    } else {
      results_DIM[[i]][j] <- "-"
    }
  }
}

# ---------------------------------------------------------------------------------
# ------------- making final merges and saving output as excel file ---------------
# ---------------------------------------------------------------------------------
setwd(path)

results <- list()
wb <- createWorkbook()

info <- favper %>% select(-starts_with("QID"), -starts_with("DIM"))

for (i in 1:nrow(norms)) {

  norm <- paste("norm", i, sep = "_")
  results[[norm]] <- cbind(info, results_QID[[i]], results_diff[[i]], results_DIM[[i]])

  # replacing all missing values with "-"
  results <- lapply(results, function(x) replace(x, is.na(x), "-"))

  addWorksheet(wb, norm)
  writeData(wb, norm, x = as.data.frame(results[[i]]), withFilter = FALSE)
}

# saving final file
saveWorkbook(wb, "difference.xlsx", overwrite = TRUE)

