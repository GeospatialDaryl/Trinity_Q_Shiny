library(readr)

#
#     Lewiston Stage-Discharge Curve
#
Lewiston_Stage_Dishcarge_20191018 <- read_csv("tblInputs/Lewiston_Stage_Dishcarge_20191018.txt")
#View(Lewiston_Stage_Dishcarge_20191018)
Lew_stg_Q <- as_tibble(Lewiston_Stage_Dishcarge_20191018)
rm(Lewiston_Stage_Dishcarge_20191018)
names(Lew_stg_Q)


