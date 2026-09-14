library(knitr)
library(dplyr)
library(ggplot2)

# Define a custom function to convert DMS to decimal degrees for multiple coordinates
dms_to_decimal_multiple <- function(dms_coordinates) {
  decimal_coordinates <- numeric(length(dms_coordinates))

  for (i in seq_along(dms_coordinates)) {
    dms <- gsub("[°'’\"NnSsEeWw]", "", dms_coordinates[i], perl = TRUE)
    parts <- strsplit(dms, "\\s+|\\.")
    degrees <- as.numeric(parts[[1]][1])
    minutes <- as.numeric(parts[[1]][2])
    seconds <- as.numeric(parts[[1]][3])

    sign <- 1
    if (grepl("[SsWw]", dms_coordinates[i]))
      sign <- -1

    decimal_coordinates[i] <- sign * (degrees + minutes/60 + seconds/3600)
  }

  return(decimal_coordinates)
}

n <- 6

# from 0 to ...
max_x <- 3000
max_y <- 3000

set.seed(123456)
ports <- data.frame(id = 1:n, x = runif(n, max = max_x), y = runif(n, max = max_y))

latitude_dms <- c("03° 46' 48.16'' N","06° 05' 785'' S","7° 11' 48'' S",
                  "03° -02' -30'' S","05° 07' 0.23'' S","00° 53' 10.04'' S")
longitude_dms <- c("098° 42' 09.69'' E","106° 52' 921'' E","112° 43' 58'' E",
                   "03° -02' -30'' S","119° 24.52' E", "131° 16' 06'' E")

# Convert latitude from DMS to decimal degrees
latitude_decimal <- dms_to_decimal_multiple(latitude_dms)

# Convert longitude from DMS to decimal degrees
longitude_decimal <- dms_to_decimal_multiple(longitude_dms)


ports$x <- longitude_decimal
ports$y <- latitude_decimal
ggplot(ports, aes(x, y)) +
  geom_point()

distance <- matrix(c(0,1064,1488,1430,1708,2807,
                     1064,0,438,614,806,2102,
                     1488,438,0,328,520,1816,
                     1430,614,328,0,353,1577,
                     1708,806,520,353,0,1375,
                     2807,2102,1816,1577,1375,0),ncol = 6)

dist_fun <- function(i, j) {
  vapply(seq_along(i), function(k) distance[i[k], j[k]], numeric(1L))
}

library(ompr)
model <- MIPModel() %>%
  # we create a variable that is 1 iff we travel from city i to j
  add_variable(x[i, j], i = 1:n, j = 1:n,
               type = "integer", lb = 0, ub = 1) %>%

  # a helper variable for the MTZ formulation of the tsp
  add_variable(u[i], i = 1:n, lb = 1, ub = n) %>%

  # minimize travel distance
  set_objective(sum_expr(dist_fun(i, j) * x[i, j], i = 1:n, j = 1:n), "min") %>%

  # you cannot go to the same city
  set_bounds(x[i, i], ub = 0, i = 1:n) %>%

  # leave each city
  add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n) %>%
  #
  # visit each city
  add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 1:n) %>%

  # ensure no subtours (arc constraints)
  add_constraint(u[i] >= 2, i = 2:n) %>%
  add_constraint(u[i] - u[j] + 1 <= (n - 1) * (1 - x[i, j]), i = 2:n, j = 2:n)

model
## Mixed integer linear optimization problem
## Variables:
##   Continuous: 6
##   Integer: 36
##   Binary: 0
## Model sense: minimize
## Constraints: 42

library(ompr.roi)
library(ROI.plugin.glpk)
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
# <SOLVER MSG>  ----
#   GLPK Simplex Optimizer 5.0
# 42 rows, 42 columns, 142 non-zeros
# 0: obj =   0.000000000e+00 inf =   1.700e+01 (17)
# 19: obj =   5.706000000e+03 inf =   4.441e-16 (0)
# *    26: obj =   5.691600000e+03 inf =   3.109e-16 (0)
# OPTIMAL LP SOLUTION FOUND
# GLPK Integer Optimizer 5.0
# 42 rows, 42 columns, 142 non-zeros
# 36 integer variables, 30 of which are binary
# Integer optimization begins...
# Long-step dual simplex will be used
# +    26: mip =     not found yet >=              -inf        (1; 0)
# +    51: >>>>>   6.476000000e+03 >=   5.721000000e+03  11.7% (6; 0)
# +    71: >>>>>   6.365000000e+03 >=   5.869000000e+03   7.8% (5; 4)
# +    98: mip =   6.365000000e+03 >=     tree is empty   0.0% (0; 17)
# INTEGER OPTIMAL SOLUTION FOUND
# <!SOLVER MSG> ----

solution <- get_solution(result, x[i, j]) %>%
  filter(value > 0)
kable(head(solution, 3))

paths <- select(solution, i, j) %>%
  rename(from = i, to = j) %>%
  mutate(trip_id = row_number()) %>%
  tidyr::gather(property, idx_val, from:to) %>%
  mutate(idx_val = as.integer(idx_val)) %>%
  inner_join(ports, by = c("idx_val" = "id"))
kable(head(arrange(paths, trip_id), 4))


row_names <- c("Belawan","Tanjung Priok","Tanjung Perak","Banjarmasin","Makassar","Sorong")

ggplot(ports, aes(x, y)) +
  geom_point() +
  geom_line(data = paths, aes(group = trip_id)) +
  ggtitle(paste0("Optimal route with cost: ", round(objective_value(result), 2)))+
  geom_text(aes(label = row_names), size = 4, color = "black", position = position_nudge(y = 0.2))

# Display the result
solution # We can calculate the optimal cost with multiple the distance with values and sum it.
paths # We can see the sequence of the path here
