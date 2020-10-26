library("timevis")
library("readxl")
 
mytime<- read_excel("timing.xlsx")
timevis(mytime)

sessionInfo()

