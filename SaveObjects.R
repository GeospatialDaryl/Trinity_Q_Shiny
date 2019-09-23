#  Save Data Objects
#  "Z:/Programs/SHC/2_Projects/092_TrinityDischarg_Shiny/Trinity_Q_Shiny"
dirProjHome <- "Z:/Programs/SHC/2_Projects/092_TrinityDischarg_Shiny/Trinity_Q_Shiny"
listSave <- c(tAllQ,histQ,ROD_Hydrographs,summaryHY)
save(listSave, file = "ProjectObjects/basicData.RData")
