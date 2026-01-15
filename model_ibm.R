############################################################
# MODEL IBM
#
# A toy individual-based model.
############################################################

# ---------------------------------------------------------
# Run a single model simulation of the IBM for given parameters
# ---------------------------------------------------------
model_ibm = function(p) {
  
  message(" > Running individual-based model")
  
  # Start timer
  time_taken = tic()
  
  # Set random number generator seed if you want
  set.seed(100)
  
  # ---- Initial set up ----
  
  # Randomly sample ages - would want to inform this with demographic data
  # ages = sample.int(90, p$N, replace = T)
  
  # Preallocate vectors
  ppl = data.table(
    id    = 1 : p$N, 
    # age   = ages, 
    state = "S",
    when  = as.integer(NA))  # When change in disease state occured
    # count = as.integer(0))   # Count of number of previous infections
  
  # Who is initially infected
  init_inf_id = sample.int(p$N, p$i0)
  
  # Start those people infected
  ppl[init_inf_id, state := "I"]
  ppl[init_inf_id, when  := 0]
  # ppl[init_inf_id, count := count + 1]
  
  # ---- Create network ----
  
  # NOTES: 
  #  - For a 'static' network, call this once outside the main time loop
  #  - For a 'dynamic' network, call this on each time step (might be a little slow)
  
  # Simple one liner
  network = create_network(p)
  
  # ---- Main model loop ----
  
  # Initite a progress bar
  pb = start_progress_bar(p$time)
  
  # Preallocate list to store results as we go
  results = list()
  
  # Iterate through daily time steps
  for (t in 1 : p$time) {
    
    # Create copy of main people datatable
    ppl_t = data.table::copy(ppl)
    
    # ---- Susceptible to infected ----
    
    # Contacts to assess for possible infection transmission
    exposures = network %>%
      filter(from %in% ppl[state == "I", id], 
             to   %in% ppl[state == "S", id])
    
    # Identify those becoming infected in this time step
    transmission_id = exposures %>%
      # Probability of transmission in each contact...
      mutate(prob = p$beta, 
             rand = runif(n()), 
             transmission = rand < prob) %>% 
      # Only interested in transmission events...
      filter(transmission == TRUE) %>%
      # Only consider first transmission if multiple (guarantee uniqueness)... 
      slice_head(n = 1, by = to) %>%
      pull(to)
      
    # Give those people an infection (in the following timestep)
    ppl_t[transmission_id, state := "I"]
    ppl_t[transmission_id, when  := t]
    # ppl_t[transmission_id, count := count + 1]
    
    # ---- People recovering ----
    
    # Identify which people are due to recover
    recover_id = ppl %>%
      filter(state == "I" & t >= when + p$duration_infection) %>%
      pull(id)
    
    # Mark those people as recovered
    ppl_t[recover_id, state := "R"]
    ppl_t[recover_id, when  := t]
    
    # ---- People losing immunity ----
    
    # Identify in which people immunity has waned
    waned_id = ppl %>%
      filter(state == "R" & t >= when + p$duration_immunity) %>%
      pull(id)
    
    # Mark those people as recovered
    ppl_t[waned_id, state := "S"]
    ppl_t[waned_id, when  := NA]
    
    # ---- Store key epi metrics ----
    
    # Calculate prevalence
    prevalence = sum(ppl$state == "I") / p$N
    
    # Incidence has already been calculated
    incidence = length(transmission_id)
    
    # Store number in each disease state
    results[[t]] = data.table(
      time = t, 
      s = sum(ppl$state == "S"), 
      i = sum(ppl$state == "I"), 
      r = sum(ppl$state == "R"), 
      prevalence = prevalence,
      incidence  = incidence)
    
    # Update ppl datatable
    ppl = data.table::copy(ppl_t)
    
    # Update progress bar
    pb$tick()
  }
  
  # Concatenate results
  results_dt = rbindlist(results)
  
  # Save results to file
  saveRDS(results_dt, "results_ibm.rds")
  
  # Finish timer and report time
  toc(time_taken)
}

# ---------------------------------------------------------
# Create random network
# ---------------------------------------------------------
create_network = function(p) {
  
  # Number of nodes (people) and edges (contacts)
  n_edges = round(p$N * p$n / 2)
  
  # Generate simple network
  network = play_gnm(
    n = p$N, 
    m = n_edges, 
    directed = FALSE)
  
  # Format edges into a datatable - this is what we really need
  network_df = network %>% 
    activate(edges) %>% 
    as.data.table()
  
  # Repeat pair-wise so all contacts are double directed
  reverse_df = data.table(
    from = network_df$to,
    to   = network_df$from)
  
  # Bind into single datatable
  network = rbind(network_df, reverse_df)
  
  return(network)
}

