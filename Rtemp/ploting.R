#TODO: Documentation!

#' Plot Exploration Sampling shares
#'
#' Plot evolution of Exploration Sampling shares across waves.
#'
#' @param exploration_props_df A dataframe with information about the exploration sampling across waves.
#' @param font_size A scalar for the font size.
#' @return A ggplot of the evolution of Exploration Sampling shares.
plotProportionalShares = function(exploration_props_df, font_size = 20) {
  # TODO: change plot when one of the treatments has a p-share == 1.
  ggplot(exploration_props_df, aes(x = wave, y = prop, group = factor(treatment_label), colour = factor(treatment_label))) +
    geom_line(size = 1) +
    geom_point() +
    theme_minimal(base_size = font_size) +
    scale_x_continuous(breaks = c(1,2,3), limits = c(1,3)) +
    scale_y_continuous(breaks = seq(0,0.5,0.1)) +
    labs(y = 'Exploration sampling shares', x = 'Wave', color = 'Treatment')
}

#' Plot average outcomes
#'
#' Plot evolution of average outcomes (no bayesian estimation) across waves.
#'
#' @param history_df A dataframe with the complete sample at observation level.
#' @param treatment_encoding A dataframe with the treatment encoding.
#' @param n_waves A scalar for the number of waves.
#' @param y_limits A vector with two values for the limits of y in the plot.
#' @param cumulative A logical. True if the quantities are cumulative.
#' @param font_size A scalar for the font size.
#' @return A ggplot of the evolution of average outcomes across waves.
plotAverageOutcome = function(history_df, n_waves, cumulative = T, font_size = 20, date_wave = F) {
  if (date_wave){
    # Fix treatment format so dplyr recognizes all the treatments.
    history_df$treatment = as.factor(history_df$treatment)
    history_df$wave = as.Date(history_df$wave, "%m/%d/%Y")

    df_sum <- history_df %>%
      group_by(wave, treatment, .drop = FALSE) %>%
      summarize(mean = mean(outcome), sum = sum(outcome), count = dplyr::n())

    df_sum <- df_sum %>%
      group_by(treatment, .drop = FALSE) %>%
      mutate(cumusum = cumsum(sum), cumucount = cumsum(count), cumuavg = cumsum(sum)/cumsum(count)) %>%
      as.data.frame()

    #df_sum['treatment_label'] = decodeLabels(df_sum$treatment, treatment_encoding)

    if (cumulative) {
      ggplot(df_sum, aes(x = wave,  y = cumuavg, group = factor(treatment), colour = factor(treatment))) +
        geom_line(size = 1) +
        geom_point() +
        #theme_minimal(base_size = font_size) +
        scale_fill_brewer(palette="Set1") +
        labs(y = 'Average outcome (cumulative)', x = 'Wave', color = 'Treatment')
    }
    else {
      ggplot(df_sum, aes(x = wave,  y = mean, group = factor(treatment), colour = factor(treatment))) +
        geom_line(size = 1) +
        geom_point() +
        theme_minimal(base_size = font_size) +
        labs(y = 'Average outcome (cumulative)', x = 'Wave', color = 'Treatment')
    }
  }
  else {

    # Fix treatment format so dplyr recognizes all the treatments.
    history_df$treatment = as.factor(history_df$treatment)

    df_sum <- history_df %>%
      group_by(wave, treatment, .drop = FALSE) %>%
      summarize(mean = mean(outcome), sum = sum(outcome), count = dplyr::n())

    df_sum <- df_sum %>%
      group_by(treatment, .drop = FALSE) %>%
      mutate(cumusum = cumsum(sum), cumucount = cumsum(count), cumuavg = cumsum(sum)/cumsum(count)) %>%
      as.data.frame()

    #df_sum['treatment_label'] = decodeLabels(df_sum$treatment, treatment_encoding)

    if (cumulative) {
      ggplot(df_sum, aes(x = wave,  y = cumuavg, group = factor(treatment), colour = factor(treatment))) +
        geom_line(size = 1) +
        geom_point() +
        #theme_minimal(base_size = font_size) +
        scale_fill_brewer(palette="Set1") +
        scale_x_continuous(breaks = 1:n_waves, limits = c(1,n_waves)) +
        labs(y = 'Average outcome (cumulative)', x = 'Wave', color = 'Treatment')
    }
    else {
      ggplot(df_sum, aes(x = wave,  y = mean, group = factor(treatment), colour = factor(treatment))) +
        geom_line(size = 1) +
        geom_point() +
        theme_minimal(base_size = font_size) +
        scale_x_continuous(breaks = seq(1,n_waves), limits = c(1,n_waves)) +
        labs(y = 'Average outcome (cumulative)', x = 'Wave', color = 'Treatment')
    }
  }
}


#' Plot posterior distributions
#'
#' Plot posterior density distributions for a single wave.
#'
#' @param theta_matrix A matrix from dist_cache with the theta drawns from the posterioir distribution.
#' @param treatment_encoding A dataframe with the treatment encoding.
#' @param font_size A scalar for the font size.
#' @return A ggplot of posterior density distributions for a single wave.
plotPosteriorDistributions = function(theta_matrix, treatment_encoding, x_label, font_size = 20) {
  n_treatment = dim(theta_matrix)[1]
  theta_data = as.data.frame(t(theta_matrix))
  names(theta_data) = decodeLabels(1:n_treatment, treatment_encoding)

  theta_melt = reshape2::melt(theta_data)
  names(theta_melt) = c('Treatment', 'Theta')

  ggplot(theta_melt, aes(x = Theta, fill= Treatment)) +
    geom_density(alpha=0.5, color = NA) +
    #theme_minimal(base_size = font_size) +
    scale_fill_brewer(palette="Set1") +
    labs(y = "Posterior Density", x = x_label, color = 'Treatments')
}


#' Plot posterior mean and HPI
#'
#' Plot the evolution of the posterior mean and the 95% Highest Density Interval (HPI) across waves.
#'
#' @param summary_posterior A dataframe with information of the posterior distributions across waves.
#' @param n_waves A scalar for the number of waves.
#' @param y_limits A vector with two values for the limits of y in the plot.
#' @param font_size A scalar for the font size.
#' @return A ggplot of the evolution of the posterior means and HPIs across waves.
plotPosteriorMean = function(summary_posterior, n_waves, font_size = 20, HPI = TRUE) {
  if (HPI) {
    ggplot(summary_posterior, aes(x = wave,  y = mean, group = factor(treatment_label), colour = factor(treatment_label))) +
      geom_line(size = 1) +
      geom_point() +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(treatment_label)), alpha=0.3) +
      theme_minimal(base_size = font_size) +
      scale_x_continuous(breaks = seq(1,n_waves), limits = c(1,n_waves)) +
      labs(y = 'Average outcome (cumulative)', x = 'Wave', color = 'Treatment', fill = "Treatment")
    }

  if (!HPI) {
      ggplot(summary_posterior, aes(x = wave,  y = mean, group = factor(treatment_label), colour = factor(treatment_label))) +
        geom_line(size = 1) +
        geom_point() +
        theme_minimal(base_size = font_size) +
        scale_x_continuous(breaks = seq(1,n_waves), limits = c(1,n_waves)) +
        labs(y = 'Average outcome (cumulative)', x = 'Wave', color = 'Treatment', fill = "Treatment")
    }
}

#' Plot average outcome (bar plot)
#'
#' Generate a bar plot of the average outcome of each treatment at each wave (not cumulative).
#'
#' @param history_df A dataframe with the complete sample at observation level.
#' @param treatment_encoding A dataframe with the treatment encoding.
#' @param font_size A scalar for the font size.
#' @return A ggplot (bar plot) of the average outcomes at each wave (not cumulative).
plotAverageOutcomePerWave = function(history_df, treatment_encoding, font_size = 20){
  history_df$treatment = as.factor(history_df$treatment)

  df_sum <- history_df %>%
    group_by(wave, treatment, .drop = FALSE) %>%
    summarize(mean = mean(outcome), sum = sum(outcome), count = dplyr::n())

  df_sum <- df_sum %>%
    group_by(treatment, .drop = FALSE) %>%
    mutate(cumusum = cumsum(sum), cumucount = cumsum(count), cumuavg = cumsum(sum)/cumsum(count)) %>%
    as.data.frame()

  df_sum['treatment_label'] = decodeLabels(df_sum$treatment, treatment_encoding)
  melt_df_sum = reshape2::melt(df_sum[c('wave', 'treatment_label', 'mean')],
                               value_name = 'mean', id.vars = c('wave', 'treatment_label'))

  ggplot(melt_df_sum, aes(x = wave, y = value, fill = factor(treatment_label))) +
    geom_bar(stat="identity",position="dodge") +
    theme_minimal(base_size = font_size) +
    labs(y = 'Average outcome per wave (not cumulative)', x = 'Wave', fill = 'Treatment')
}

