library(knitr)
library(dplyr)
library(ggplot2)

n <- 6

# from 0 to ...
max_x <- 3000
max_y <- 3000

set.seed(123456)
cities <- data.frame(id = 1:n, x = runif(n, max = max_x), y = runif(n, max = max_y))
ggplot(cities, aes(x, y)) +
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
##<SOLVER MSG>  ----
##GLPK Simplex Optimizer, v4.65
## 42 rows, 42 columns, 142 non-zeros
## 0 : obj =   0.000000000e+00 inf =   1.700e+01 (17)
## 19: obj =   5.706000000e+03 inf =   4.441e-16 (0)
## *    26: obj =   5.691600000e+03 inf =   3.109e-16 (0)
## OPTIMAL LP SOLUTION FOUND
## GLPK Integer Optimizer, v4.65
## 42 rows, 42 columns, 142 non-zeros
## 36 integer variables, 30 of which are binary
## Integer optimization begins...
## Long-step dual simplex will be used
## +    26: mip =     not found yet >=              -inf        (1; 0)
## +    51: >>>>>   6.476000000e+03 >=   5.721000000e+03  11.7% (6; 0)
## +    71: >>>>>   6.365000000e+03 >=   5.869000000e+03   7.8% (5; 4)
## +    98: mip =   6.365000000e+03 >=     tree is empty   0.0% (0; 17)
## INTEGER OPTIMAL SOLUTION FOUND
## <!SOLVER MSG> ----

solution <- get_solution(result, x[i, j]) %>%
  filter(value > 0)
kable(head(solution, 3))

paths <- select(solution, i, j) %>%
  rename(from = i, to = j) %>%
  mutate(trip_id = row_number()) %>%
  tidyr::gather(property, idx_val, from:to) %>%
  mutate(idx_val = as.integer(idx_val)) %>%
  inner_join(cities, by = c("idx_val" = "id"))
kable(head(arrange(paths, trip_id), 4))


row_names <- c("Belawan","Tanjung Priok","Tanjung Perak","Banjarmasin","Makassar","Sorong")

ggplot(cities, aes(x, y)) +
  geom_point() +
  geom_line(data = paths, aes(group = trip_id)) +
  ggtitle(paste0("Optimal route with cost: ", round(objective_value(result), 2)))+
  geom_text(aes(label = row_names), size = 4, color = "black", position = position_nudge(y = 0.2))

# Display the result
solution # We can calculate the optimal cost with multiple the distance with values and sum it.
paths # We can see the sequence of the path here


