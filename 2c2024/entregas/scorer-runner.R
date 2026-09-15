# Ambiente limpio
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
#cargo codigo y corro tests
script_R <- "~/Desktop/codigo-Rolando-Guglielmino-Frau-bis.R"
source(script_R, echo=TRUE)
source("~/Git/iecd/2c2024/entregas/tp-final-tests-scorer.R", echo=TRUE)