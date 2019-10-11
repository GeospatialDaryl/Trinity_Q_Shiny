library(readr)

#
#   0.  Lewiston Stage-Discharge Curve
#
Lewiston_Stage_Dishcarge_20191018 <- read_csv("tblInputs/Lewiston_Stage_Dishcarge_20191018.txt")
#View(Lewiston_Stage_Dishcarge_20191018)
Lew_stg_Q <- as_tibble(Lewiston_Stage_Dishcarge_20191018)
rm(Lewiston_Stage_Dishcarge_20191018)
# all are in feet
names(Lew_stg_Q) <- c("Stage","Shift_ft","Q")

fnStg_from_Q <- approxfun(Lew_stg_Q$Q,Lew_stg_Q$Stage)  
fnQ_from_Stage <- approxfun(Lew_stg_Q$Stage,Lew_stg_Q$Q)
#
#   fnStg_from_Q      Stage from Q
#   fnQ_from_Stage    Q from Stage

#
#   1.  Historic Water Year Classifications 19
#
histWYClasses <- read_csv("tblInputs/tblHistYearlyWYClassifications.txt", 
                          col_types = cols(`Year Type` = col_factor(levels = c("Normal", 
                                                                               "Wet", "Dry", "Crit.Dry", "Ex.Wet"))))
names(histWYClasses) <- c("HY","Total_Volume","YearType")
dHistWYClasses <- as.data.frame(histWYClasses)
gHistWYClasses <- group_by(dHistWYClasses, YearType)

#
#   2.  Coffee Creek Gage - Daily
#
Trinity_CoffeeCreek_Import <- read_delim("tblInputs/Trinity_CoffeeCreek_Import.txt", 
                                         +     "\t", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                                                                             +         Q = col_double(), Site = col_integer()), 
                                         +     trim_ws = TRUE)
c("AGENCY","Site","YMD","CoffeeCreek.Q","Status") -> names(Trinity_CoffeeCreek_Import)
CCk.Q <- Trinity_CoffeeCreek_Import[,c(3,4)]
tAllQ2 <- full_join(tAllQ, CCk.Q, by="YMD")
tAllQ <- tAllQ2




