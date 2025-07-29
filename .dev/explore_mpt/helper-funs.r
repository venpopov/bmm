## Utility funs
logit <- function(p) {
  log(p) - log1p(-p)
}
inv_logit <- function(x) {
  1 / (1 + exp(-x))
}

## Transforming wide matrix MPT data to long data.frame
to_long_mpt_data <- function(data = matrix(), branch_names = character(ncol(data)))  {
  data |> 
    as_tibble() |> 
    rename_with(\(x) branch_names) |> 
    mutate(id = as.factor(1:n())) |> 
    pivot_longer(
      cols = !id, 
      names_to = c("resp_type","condition"), 
      names_pattern = "(.*)_(.*)",
      values_to = "count"
    ) |> 
    pivot_wider(names_from = resp_type, values_from = count) |> 
    mutate(n = rowSums(across(where(is.numeric))))
}
