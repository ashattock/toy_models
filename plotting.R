############################################################
# PLOTTING
#
# Plotting functions in one place.
############################################################

# ---------------------------------------------------------
# Plot model outcomes
# ---------------------------------------------------------
plot_model_results = function() {
  
  message(" > Plotting model outcomes")
  
  # Load from individual-based model
  results_ibm = readRDS("results_ibm.rds") %>%
    mutate(model = "ibm")
  
  # Load from population-based model
  results_sir = readRDS("results_sir.rds") %>%
    mutate(model = "sir")
  
  # Concatenate results from the two models
  results_dt = rbind(results_ibm, results_sir)
  
  # ---- Assess model states ----
  
  # Plot datatable - disease states
  state_dt = results_dt %>%
    select(time, s, i, r, model) %>%
    # Melt to tidy format...
    pivot_longer(cols = -c(time, model), 
                 names_to = "state") %>%
    # Set plotting order (S, I, R)....
    mutate(state = fct_inorder(state)) %>%
    arrange(model, state, time) %>%
    as.data.table()
  
  # Plot disease states over time
  g1 = ggplot(state_dt) + 
    aes(x = time, 
        y = value, 
        colour = state) + 
    geom_line() + 
    # Facet by model...
    facet_wrap(
      facets = vars(model))
  
  # Save figure to file
  save_fig(g1, "Disease states")
  
  # ---- Key metrics -----
  
  # Plot datatable - prevalence
  metrics_dt = results_dt %>%
    select(-s, -i, -r) %>%
    # Melt to tidy format...
    pivot_longer(cols = -c(time, model), 
                 names_to = "metric") %>%
    arrange(model, metric, time) %>%
    as.data.table()
  
  # Plot prevalence states over time
  g2 = ggplot(metrics_dt) + 
    aes(x = time, 
        y = value, 
        linetype = model) + 
    geom_line() + 
    # Facet by metric...
    facet_wrap(
      facets = vars(metric), 
      scales = "free_y")
  
  # Save figure to file
  save_fig(g2, "Epi metrics")
}

# ---------------------------------------------------------
# Wrapper function to save files
# ---------------------------------------------------------
save_fig = function(g, file) {
  
  # Figure properties
  format     = "png"
  width      = 9
  height     = 6
  units      = "in" # Units of figures sizes
  resolution = 300  # Plotting resolution (in dpi)
  
  # Save figure to file
  ggsave(
    filename = paste(file, format, sep = "."), 
    plot     = g, 
    device   = format, 
    dpi      = resolution, 
    width    = width, 
    height   = height, 
    units    = units)
}

