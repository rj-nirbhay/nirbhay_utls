# update R from R studio

#mac
install.packages('devtools') #assuming it is not already installed
library(devtools)
install_github('andreacirilloac/updateR')
library(updateR)
updateR(admin_password = ****)
# windows
install.packages("installr")
library(installr)
updateR()
