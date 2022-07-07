#' Treatment encoding
#'
#' Generate a treatment encoding based on the values on the vector treatment. This is usually the first step in most
#' the procedures on this package.
#'
#' @param treatment A vector.
#' @param sort_treat A logical.
#' @return A dataframe with the original treatment encoding on column 'original' and the new treatment encoding on
#'         column 'new'.
#' @export
generateEncoding = function(treatment, sort_treat = T) {
  # Remove missing values from treatment (for now)
  treatment_clean = treatment[!is.na(treatment)]

  # Identify treatments and sort them
  treatment_list = unique(treatment_clean)
  if (sort_treat) {
    treatment_list = treatment_list[order(treatment_list)]
  }

  # Find number of treatments
  n_treatments = length(treatment_list)

  # Generate a new treatment encoding
  new_treatment_labels = 1:n_treatments

  # Save encoding
  treatment_encoding = data.frame(list('original' = treatment_list,
                                       'new' = new_treatment_labels))
  return(treatment_encoding)
}


#' Decode treatment labels
#'
#' Transform treatment vector values based on the treatment_encoding dataframe from 'new' to 'original'.
#'
#' @param treatment A vector.
#' @param treatment_encoding A dataframe.
#' @return A vector with the treatment transformed based on values of the column 'original' in the
#'         treatment_encoding dataframe.
#' @export
decodeLabels = function(treatment, treatment_encoding) {
  # Define encoding function (for single value)
  decodeLabel = function(value, encoding) {
    if (is.na(value)) {
      return(NA)
    }
    output = encoding[encoding$new == value, 'original']
    return(output)
  }


  # Transform treatment labels based on encoding
  output = sapply(as.character(treatment), decodeLabel, encoding = treatment_encoding)
  names(output)<- NULL
  return(output)
}

#' Encode treatment labels
#'
#' Transform treatment vector values based on the treatment_encoding dataframe from 'original' to 'new'.
#'
#' @param treatment A vector.
#' @param treatment_encoding A dataframe.
#' @return A vector with the treatment transformed based on values of the column 'new' in the
#'         treatment_encoding dataframe.
#' @export
encodeLabels = function(treatment, treatment_encoding) {
  # Define encoding function (for single value)
  encodeLabel = function(value, encoding) {
    if (is.na(value)) {
      return(NA)
    }
    output = encoding[encoding$original == value, 'new']
    return(output)
  }

  # Transform treatment labels based on encoding
  new_treatment = sapply(treatment, encodeLabel, encoding = treatment_encoding)

  output = as.numeric(new_treatment)
  return(output)
}


#' Bayesian update for a Binomial outcome
#'
#' Obtain the posterior of the parameter theta for an outcome distributed as a Binomial with prior Beta.
#'
#' @param treatment A vector.
#' @param outcome A vector.
#' @param alpha0 A vector.
#' @param beta0 A vector.
#' @return A list with the distribution and parameters of the posterior distribution of theta.
#' @export
bayesianUpdateBernoulli = function(treatment, outcome, alpha0 = 1, beta0 = 1) {

  #TODO: write a handle so users can specify a "common" prior (like uniform, jeffreys,...)
  #TODO: describe inputs better

  # Calculate alpha and beta of the posterior distribution
  alpha <- alpha0 + tapply(outcome, treatment, sum, default = 0)
  beta <- beta0 + tapply(1 - outcome, treatment, sum, default = 0)

  # Fix names of vectors to reflect that each value corresponds to a treatment
  names(alpha) <- paste0("treat", names(alpha))
  names(beta) <- paste0("treat", names(beta))

  # Store results in a dictionary
  dist_cache <- list(distribution = "beta",
                parameters = list(alpha = alpha,
                                  beta = beta),
                n_treatment = length(unique(treatment)))

  return(dist_cache)
}

#' Bayesian update for a Poisson outcome
#'
#' Obtain the posterior of the parameter theta for an outcome distributed as poisson with prior Gamma.
#'
#' @param treatment A vector.
#' @param outcome A vector.
#' @param alpha0 A vector.
#' @param beta0 A vector.
#' @return A list with the distribution and parameters of the posterior distribution of theta.
#' @export
bayesianUpdatePoisson = function(treatment, outcome, alpha0 = 1, beta0 = 0) {

  # Calculate alpha and beta of the posterior distribution
  alpha <- alpha0 + tapply(outcome, treatment, sum, default = 0)
  beta <- beta0 + tapply(outcome, treatment, length, default = 0)

  # Fix names of vectors to reflect that each value corresponds to a treatment
  names(alpha) <- paste0("treat", names(alpha))
  names(beta) <- paste0("treat", names(beta))

  # Store results in a dictionary
  dist_cache <- list(distribution = "gamma",
                     parameters = list(alpha = alpha,
                                       beta = beta),
                     n_treatment = length(unique(treatment)))

  return(dist_cache)
}


#' Sample from a posterior distribution.
#'
#' Sample from a posterior distribution.
#'
#' @param dist_cache A list with the structure of a bayesianUpdateBinomial output.
#' @param sample_resolution A number.
#' @return A matrix with the samples drawn from the posterior distribution.
#'
#' @export
sampleDistribution = function(dist_cache, sample_resolution = 50000) {
  #TODO: describe inputs better

  # Extract number of treatments
  n_treatment <- dist_cache$n_treatment

  if (dist_cache$distribution == "gamma") {

    # Read information from dist_cache
    alpha <- dist_cache$parameters$alpha
    beta <- dist_cache$parameters$beta

    #TODO: this may consume too much memory, it would be better to do it by batches
    # Initiation of theta_matrix, where we will store draws from the beta distribution
    theta_matrix <- matrix(NA, nrow = n_treatment, ncol = sample_resolution)

    # Draw from a beta distribution
    for (i in 1:n_treatment){
      theta_matrix[i,] <- stats::rgamma(sample_resolution, shape = alpha[i], rate = beta[i])
    }

    return(theta_matrix)

  }
  else if (dist_cache$distribution == "beta") {

    # Read information from dist_cache
    alpha <- dist_cache$parameters$alpha
    beta <- dist_cache$parameters$beta

    #TODO: this may consume too much memory, it would be better to do it by batches
    # Initiation of theta_matrix, where we will store draws from the beta distribution
    theta_matrix <- matrix(NA, nrow = n_treatment, ncol = sample_resolution)

    # Draw from a beta distribution
    for (i in 1:n_treatment){
      theta_matrix[i,] <- stats::rbeta(sample_resolution, alpha[i], beta[i])
    }

    return(theta_matrix)

  }
  else {
    stop("The specified distribution is not recognized. If you think the distribution should be included, please contact us.")
  }
}


#' Thompson sampling
#'
#' Use Thompson sampling to find the proportion shares of each treatment.
#'
#' @param theta_matrix A matrix.
#' @param treatment_cost A vector.
#' @return A vector with the proportion shares, obtained through Thompson sampling, for each treatment.
#'
#' @export
thompsonSampling = function(theta_matrix, treatment_cost = NA) {
  #TODO: describe inputs better
  #TODO: wrong treatment_cost dimension
  #TODO: normalization of treatment_cost?
  #TODO: work case when prob(d1) = 1

  # Extract number of treatments and sample resolution
  n_treatment <- dim(theta_matrix)[1]
  sample_resolution <- dim(theta_matrix)[2]

  # Get treatment names
  treat_names <- paste0("treat", 1:n_treatment)

  if (is.na(treatment_cost)) {
    # Set treatment cost equal to a zero vector if it has not being specified.
    treatment_cost = rep(0,n_treatment)
  }

  for (i in 1:n_treatment){
    # Subtract treatment cost from drawn thetas
    theta_matrix[i,] <- theta_matrix[i,] - treatment_cost[i]
  }

  # Find the maximum (between treatments) for each sample draw.
  max_theta <- sapply(1:sample_resolution, function(x) which.max(theta_matrix[, x]))
  max_theta_factor <- factor(max_theta, levels = 1:n_treatment)

  # Generate proportions
  prop_shares <- table(max_theta_factor) / sample_resolution
  names(prop_shares) <- treat_names

  return(prop_shares)
}

#' Exploration sampling
#'
#' Get exploration sampling shares by modifying the proportion shares obtained through Thompson sampling.
#'
#' @param prop_shares A vector.
#' @return A vector with the proportion shares, obtained through exploration sampling, for each treatment.
#'
#' @export
explorationSampling = function(prop_shares) {
  # TODO: check if sum(p-shares) = 1

  # Common case: 0 < p-shares < 0
  if (max(prop_shares) < 1 && min(prop_shares) >= 0) {
    explore_shares = prop_shares * (1 - prop_shares) # Based on stationary distribution for modified Thompson
    explore_shares = explore_shares / sum(explore_shares)

    return(explore_shares)
  }
  # Case when one p-share == 1
  else if (max(prop_shares) == 1){
    explore_shares = prop_shares

    return(explore_shares)
  }
  else {
    stop("Each value of prop_shares should be bounded between 0 and 1")
  }
}

#' Full exploration sampling method
#'
#' Get exploration sampling shares from a sample of outcomes and treatments.
#'
#' @param treatment A vector.
#' @param outcome A vector.
#' @param alpha0 A vector.
#' @param beta0 A vector.
#' @param sample_resolution A number.
#' @param treatment_cost A vector.
#' @return A vector with the proportion shares, obtained through exploration sampling, for each treatment.
#'
#' @export
BinomialExplorationSampling = function(treatment, outcome, alpha0 = 1, beta0 = 1, sample_resolution = 100000, treatment_cost = NA) {

  # Bayesian update for the Binomial case
  dist_cache = bayesianUpdateBinomial(treatment, outcome, alpha0, beta0)

  # Sample from the Beta posterior
  theta_matrix = sampleDistribution(dist_cache, sample_resolution)

  # Thompson sampling
  prop_shares = thompsonSampling(theta_matrix, treatment_cost)

  # Exploration sampling adjustment
  explore_shares = explorationSampling(prop_shares)

  return(explore_shares)
}


#' Sample based on proportions
#'
#' Generate a sample of size sample_size based on the proportions in the vector shares.
#'
#' @param shares A vector.
#' @param sample_size A number.
#' @return A vector with the new sample based on the proportions in the vector shares.
#'
#' @export
proportionalAssignment = function(shares, sample_size) {

  # Number of treatments
  n_treatment = length(shares)

  # Initial allocation based on the expected value
  initial_allocation = floor(sample_size * shares)

  # Remaining observations in (real)
  real_remainder = sample_size * shares - initial_allocation

  # Remaining observations (integers)
  units_remaining = sample_size - sum(initial_allocation)

  if (units_remaining > 0) {
    # If there are units remaining, assign each treatment according to the rounded down average count
    #   and fill units remaining randomly
    ordered_allocation = c(rep(1:n_treatment, initial_allocation),
                           sample(1:n_treatment, size = units_remaining, replace = T, prob = real_remainder))
  }
  else {
    # If there are no units remaining, assign each treatment according to the rounded down average count
    ordered_allocation = rep(1:n_treatment, initial_allocation)
  }

  # Shuffle treatment allocations
  final_allocation = sample(ordered_allocation)

  return(final_allocation)
}

#' Sample based on proportions and stratification
#'
#' Generate a sample of size sample_size based on the proportions in the vector shares and stratified
#' based on the stratify variable.
#'
#' @param shares A vector.
#' @param stratify A vector at observation level with the strata.
#' @return A vector with the new stratified sample based on the proportions in the vector shares.
#'
#' @export
proportionalAssignmentStratified = function(shares, stratify) {

  # Number of treatments
  n_treatment = length(shares)

  # Strata
  strata = unique(stratify)
  n_strata = length(strata)

  # Output storage
  output = vector(mode='numeric', length = length(stratify))
  output[1:length(stratify)] = NA

  for (i in strata) {
    sample_size = sum(stratify == i)

    # Initial allocation based on the expected value
    initial_allocation = floor(sample_size * shares)

    # Remaining observations in (real)
    real_remainder = sample_size * shares - initial_allocation

    # Remaining observations (integers)
    units_remaining = sample_size - sum(initial_allocation)

    if (units_remaining > 0) {
      # If there are units remaining, assign each treatment according to the rounded down average count
      #   and fill units remaining randomly
      ordered_allocation = c(rep(1:n_treatment, initial_allocation),
                             sample(1:n_treatment, size = units_remaining, replace = T, prob = real_remainder))
    }
    else {
      # If there are no units remaining, assign each treatment according to the rounded down average count
      ordered_allocation = rep(1:n_treatment, initial_allocation)
    }

    # Shuffle treatment allocations
    if (length(ordered_allocation) == 1) {
      final_allocation = ordered_allocation
    }
    else {
      final_allocation = sample(ordered_allocation)
    }

    # Storage
    output[stratify == i] = final_allocation
  }

  return(output)
}

#' Expected regret
#'
#' Generate the expected policy regret of the sample allocation based on p-shares and true treatment effects.
#'
#' @param shares A vector.
#' @param true_treatment_effects A vector.
#' @return A number equal to the expected policy regret.
expectedRegret = function(shares, true_treatment_effects) {
  # TODO: check that shares and true_treatment_effects have the same length
  # TODO: add testing for this function

  # Find optimal treatment effect
  optimal_treatment_effect = max(true_treatment_effects)

  # Get the policy regret or loss for choosing an incorrect treatment
  policy_regret = optimal_treatment_effect - true_treatment_effects

  # Find the expected regret
  expected_regret = sum(policy_regret * shares)

  return(expected_regret)
}

#' Expected regret feasible
#'
#' Generate the expected policy regret without the theoretical or oracle treatment effects.
#'
#' @param theta_matrix A matrix
expectedRegretFeasible = function(theta_matrix) {
  # TODO: check that shares and true_treatment_effects have the same length
  # TODO: add testing for this function

  # Find optimal treatment effect
  max_theta = apply(theta_matrix, 1, max)

  # Get the policy regret or loss for choosing an incorrect treatment
  regret = max_theta - theta_matrix

  # Find the expected regret
  expected_regret = apply(regret, 2, mean)

  return(expected_regret)
}

#' Optimal treatment selected
#'
#' Find whether the optimal treatment has the highest proportion share.
#'
#' @param shares A vector.
#' @param true_treatment_effects A vector.
#' @return 1 if the highest proportion treatment is the optimal treatment, 0 otherwise.
optimalSelected = function(shares, true_treatment_effects) {
  # TODO: check that shares and true_treatment_effects have the same length
  # TODO: add testing for this function

  optimal_selected = which.max(true_treatment_effects) == which.max(shares)

  return(as.integer(optimal_selected))
}

#' In-sample regret
#'
#' Generate the in-sample regret of the sample allocation based on p-shares and true treatment effects.
#'
#' @param treatment A vector.
#' @param true_treatment_effects A vector.
#' @param sample_size A scalar
#' @return A number equal to the expected policy regret.
inSampleRegret = function(treatment, true_treatment_effects, sample_size) {
  # TODO: THIS IS INCORRECT
  # TODO: check that shares and true_treatment_effects have the same length
  # TODO: add testing for this function

  # Find the optimal treatment
  optimal_treatment_effect = max(true_treatment_effects)

  # Find the treatment effect associated to each observation
  obs_treatment_effect = sapply(treatment, function(x) true_treatment_effects[x])

  # Find the average regret or loss for choosing an incorrect treatment per observation
  insample_regret = mean(optimal_treatment_effect - obs_treatment_effect)

  return(insample_regret)
}


#' Simulate a binomial outcome
#'
#' Simulate a single binomial outcome based on a vector of true treatment effects.
#'
#' @param single_treatment A number between 1 and the number of treatments.
#' @param true_theta An ordered vector with the probabilities of success of each treatment. Its length must be equal to the number of treatments.
#' @return A simulated binomial outcome.
generateBinomialOutcome = function(single_treatment, true_theta) {
  if (single_treatment < 1 || length(true_theta) < single_treatment) {

    stop(paste('The size of the vector true_theta should be eaqual to the number of treatments.',
               'The number single_treatment should be bounded between 1 and the number of treatments'))

  }

  outcome = stats::rbinom(n = 1, size = 1, prob = true_theta[single_treatment])

  # Outdated alternative:
  #   outcome = sample(c(1,0), 1, prob = c(true_theta[single_treatment], 1-true_theta[single_treatment]))

  return(outcome)
}


#' Product of 1 Beta pdf and k-1 Beta cdfs
#'
#' Calculate the product of 1 Beta pdf and k-1 Beta cdfs, where k is the number of treatments. The selected treatment that characterizes the
#' Beta pdf is declared with the argument index. The k-1 beta cdfs are calculated using the distributions of the remaining treatments.
#'
#' @param x Value at which the pdfs and cdfs are evaluated.
#' @param dist_cache List containing the characteristics of the distributions of treatments.
#' @param index Scalar denoting the treatment associated to the beta pdf.
#' @return product of the beta pdf and k-1 beta cdfs.
cdfProductBeta <- function(x, dist_cache, index){
  # Number of treatments
  n_treatment = dist_cache$n_treatment

  # Start with the beta pdf of the selected treatment index
  output = stats::dbeta(x, dist_cache$parameters$alpha[[index]], dist_cache$parameters$beta[[index]])
  for (i in 1:n_treatment) {
    # Do not include the cdf of the selected treatment index
    if (i == index) next

    # Multiply the pdf with the cdf of the remaining treatments
    output = output * stats::pbeta(x, dist_cache$parameters$alpha[[i]], dist_cache$parameters$beta[[i]])
  }
  return(output)
}


#' Analytical Thompson shares
#'
#' Get the analytical Thompson shares based on the characteristics of the distributions of treatments.
#'
#' @param dist_cache List containing the characteristics of the distributions of treatments.
#' @return List that contains (i) the analytical Thompson shares and (ii) the absolute error associated to the numerical integration procedure.
#' TODO: be clear about the distribution. Only implemented for bernoulli.
analyticalThompsonSampling <- function(dist_cache) {
  # Number of treatments
  n_treatment = dist_cache$n_treatment

  # Treatment names
  treat_names <- paste0("treat", 1:n_treatment)

  # Storage
  analytical_thompson_shares = vector(mode = 'numeric', length = n_treatment)
  analytical_thompson_shares_error = vector(mode = 'numeric', length = n_treatment)

  for (i in 1:n_treatment) {
    if (dist_cache$distribution == 'beta') {
      # Integral of cdfProductBeta for the range 0<x<1
      temp_integral = stats::integrate(cdfProductBeta, dist_cache, i, lower = 0, upper = 1, rel.tol =.Machine$double.eps^0.5)
    } else {
      #TODO: implement for other distributions
      stop('Distribution not recognized')
    }
    # Store results
    analytical_thompson_shares[i] = temp_integral$value
    analytical_thompson_shares_error[i] = temp_integral$abs.error
  }
  # Rename output vectors
  names(analytical_thompson_shares) = treat_names
  names(analytical_thompson_shares_error) = treat_names

  # Storage
  output = list('shares' = analytical_thompson_shares,
                'abs_error' = analytical_thompson_shares_error)
  return(output)
}

#' Difference between Thompson shares and analytical Thompson shares
#'
#' Get the difference between Thompson shares and analytical Thompson shares.
#'
#' @param thompson_shares Vector containing Thompson shares.
#' @param analytical_thompson_shares List containing the analytical Thompson shares and their associated absolute error.
#' @return Minimum difference between Thomson shares and analytical Thompson shares.
evaluateDiffThompsonShares <-function(thompson_shares, analytical_thompson_shares) {
  n_treatment = length(thompson_shares)
  differences = abs(thompson_shares - analytical_thompson_shares$shares)
  tol = 7e-03

  if (any(differences > tol)) {
    warning(paste('There is a difference between thompson shares and analytical thompson shares greater than', tol, '-', max(differences)))
  }
  return(differences)
}


#' Outcome table for a Binomial outcome variable
#'
#' Generate the outcome table for a Binomial outcome table. Columns are:
#' - Treatment
#' - Treatment label
#' - Success
#' - Attempts
#' - Average outcome
#' - Mean of the posterior distribution
#' - Standard deviation of the posterior distribution
#' - Thompson shares
#'
#' @param history_df a dataframe.
#' @param dist_cache A list.
#' @param thompson_pshares A vector.
#' @param treatment_encoding A dataframe.
#' @param wave_index A scalar.
#' @return A dataframe showing main outcomes for a Binomial outcome variable
#' @export
generateBinomialOutcomeTable = function(history_df, dist_cache, thompson_pshares, treatment_encoding, wave_index) {
  #TODO: add support for different distributions. Maybe create a new handle function?

  # Fix treatment format so dplyr recognizes all the treatments.
  history_df$treatment = as.factor(history_df$treatment)

  # Collapse at wave and treatment level. Generate mean, sum and count.
  temp_sum = history_df %>%
    group_by(wave, treatment, .drop = FALSE) %>%
    summarize(mean = mean(outcome), sum = sum(outcome), count = dplyr::n())

  # Collapse at treatment level. Generate cumsum, cumcount and cumavg.
  treat_sum = temp_sum %>%
    group_by(treatment, .drop = FALSE) %>%
    mutate(cumusum = cumsum(sum), cumucount = cumsum(count), cumuavg = cumsum(sum)/cumsum(count)) %>%
    as.data.frame()

  # Keep only info about wave == wave_index
  treat_sum = treat_sum[treat_sum$wave == wave_index, ]

  # Find distribution mean and sd
  alphas = dist_cache$parameters$alpha
  betas = dist_cache$parameters$beta

  dist_mean = alphas/(alphas+betas)
  names(dist_mean) = NULL

  dist_sd = sqrt(alphas*betas/((alphas+betas)^2*(alphas+betas+1)))
  names(dist_sd) = NULL

  # Fix format of thompson_pshares
  thompson_prop = c(thompson_pshares)
  names(thompson_prop) = NULL

  # Build output
  output = data.frame('treatment' = treat_sum$treatment,
                      'treatment_label' = decodeLabels(treat_sum$treatment, treatment_encoding),
                      'success' = treat_sum$cumusum,
                      'attempt' = treat_sum$cumucount,
                      'avg' = treat_sum$cumuavg,
                      'post_mean' = dist_mean,
                      'post_sd' = dist_sd,
                      'thompson_prop' = thompson_prop)
  return(output)
}


treatmentAllocationDescription = function(treatment, treatment_encoding, stratify = NULL) {
  n_treatment = dim(treatment_encoding)[1]
  treatment = factor(treatment, levels = 1:n_treatment)
  if (is.null(stratify)) {
    output = table(treatment)/length(treatment)
  }
  else {
    n_strata = length(unique(stratify))
    output = matrix(NA, nrow = n_strata, ncol = n_treatment)
    for (i in 1:n_strata) {
      output[i,] = as.vector(table(treatment[stratify==i])/length(treatment[stratify==i]))
    }
  }
  return(output)
}

#' Bayesian update for the general
#'
#' Obtain the posterior of the parameter theta for an outcome distributed as a Binomial with prior Beta.
#'
#' @param treatment A vector.
#' @param outcome A vector.
#' @param alpha0 A vector.
#' @param beta0 A vector.
#' @return A list with the distribution and parameters of the posterior distribution of theta.
#' @export
bayesianBinomial = function(treatment, outcome, max_trials = 1, include_constant = TRUE) {

  #TODO: report Rhat from model$stan_summary

  treatment_unique = sort(unique(treatment))
  treatment_matrix = matrix(NA, nrow = length(outcome), ncol = length(unique(treatment)))
  cont = 1
  for (i in treatment_unique) {
    treatment_matrix[,cont] = as.numeric(treatment == i)
    cont = cont + 1
  }
  treatment_matrix = as.data.frame(treatment_matrix)
  colnames(treatment_matrix) = paste0("x", treatment_unique)

  if (include_constant) {
    formula_string = paste0("outcome ~ ", paste0(colnames(treatment_matrix)[1:length(treatment_unique)-1], collapse = "+"))
  } else {
    formula_string = paste0("outcome ~ -1+", paste0(colnames(treatment_matrix), collapse = "+"))
  }

  data_list = cbind(treatment_matrix,
                    data.frame(outcome = outcome,
                               max_trials = rep(max_trials, length(outcome))))

  model = stan_glm(formula = formula_string,
                   data = data_list,
                   family = binomial(link = "logit"),
                   weights = max_trials,
                   iter = 1500,
                   warmup = 700,
                   chains = 3,
                   seed = 1234,
                   refresh = 500)

  posterior_draws = as.matrix(model)

  return(posterior_draws)
}

#' Bayesian update for the general
#'
#' Obtain the posterior of the parameter theta for an outcome distributed as a Binomial with prior Beta.
#'
#' @param treatment A vector.
#' @param outcome A vector.
#' @param alpha0 A vector.
#' @param beta0 A vector.
#' @return A list with the distribution and parameters of the posterior distribution of theta.
#' @export
bayesianNormal = function(treatment, outcome, include_constant = TRUE) {

  #TODO: report Rhat from model$stan_summary

  treatment_unique = sort(unique(treatment))
  treatment_matrix = matrix(NA, nrow = length(outcome), ncol = length(unique(treatment)))
  cont = 1
  for (i in treatment_unique) {
    treatment_matrix[,cont] = as.numeric(treatment == i)
    cont = cont + 1
  }
  treatment_matrix = as.data.frame(treatment_matrix)
  colnames(treatment_matrix) = paste0("x", treatment_unique)

  if (include_constant) {
    formula_string = paste0("outcome ~ ", paste0(colnames(treatment_matrix)[1:length(treatment_unique)-1], collapse = "+"))
  } else {
    formula_string = paste0("outcome ~ -1+", paste0(colnames(treatment_matrix), collapse = "+"))
  }

  data_list = cbind(treatment_matrix,
                    data.frame(outcome = outcome))

  model = stan_glm(formula = formula_string,
                   data = data_list,
                   family = gaussian(link = "identity"),
                   iter = 1500,
                   warmup = 700,
                   chains = 3,
                   seed = 1234,
                   refresh = 500)

  posterior_draws = as.matrix(model)[, colnames(treatment_matrix)]

  return(posterior_draws)
}






