
# -------- end of sample size estimator



conditionalPanel(
  condition = "input.type_of_app == 'confint_est'",
  
  
  # ------ Enter in values for confidence interval estimation
  numericInput("num_seed_pool", "Number of Seed Pools:",min=1, max=1000),
  numericInput("num_seed_per_pool", "Number of Seeds per Pool:", min=1, max=10000),
  numericInput("pop_size", "Population Size:", min=1, max=100000),
  numericInput("num_dev_pools", "Number Of Deviant Pools", min=1, max=1000),
)
