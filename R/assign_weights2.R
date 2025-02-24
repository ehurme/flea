# L M H =  1 2 3

order <- sample(1:3, size = 3)
# M L H
percentages <- c(0.03, 0.07, 0.12)
percentages[order]

bat_mass <- 19.4

housings <- data.frame(id = c(0,1,2,3,5,7,9,11),
                       weight = c(0.44, 0.71, 0.90, 1.23, 1.61, 1.93, 2.43, 2.95))

bat_mass*percentages[order] - 0.34
# 7 2 0

x = 1
get_housing <- function(x){
  idx <- which.min(abs(housings$weight - x))
  return(housings$id[idx])
}
get_housing(mass*percentages)

# 0 - 0.44
# 1 - 0.71
# 2 - 0.90
# 3 - 1.23
# 5 - 1.61
# 7 - 1.93
# 9 - 2.43
# 11 - 2.95
