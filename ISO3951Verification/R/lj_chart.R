# Add plotting function
levey_jennings <- function(df, metric, labs = TRUE){

  # Subset down to required fields
  df <- df %>% dplyr::select(sample_num, rna_seq_lib, rna_seq_cohort, metric)

  # Select the mean and standard deviation for the selected metric
  metric_mean <- mean(df[[metric]])
  metric_sd <- sd(df[[metric]])

  # Add labels to outliers
  metric_threshold_high <- metric_mean + (2 * metric_sd)
  metric_threshold_low <- metric_mean - (2 * metric_sd)
  df$metric_label <- ifelse(((df[,metric] >= metric_threshold_high) |
                               (df[,metric] <= metric_threshold_low)),
                            yes = df$rna_seq_lib,
                            no = NA_character_)

  # Add the main aesthetics
  # Then add in the lines for mean, and +/- 1 and 2 standard deviations
  p <- ggplot(df, aes(x = sample_num,
                      y = df[,metric],
                      label = metric_label,
                      colour = rna_seq_cohort)) +
    geom_point(size = 3) + scale_color_brewer(palette = 'Dark2') +
    geom_hline(yintercept = metric_mean) +
    geom_hline(yintercept = metric_mean + metric_sd, linetype = 2) +
    geom_hline(yintercept = metric_mean - metric_sd, linetype = 2) +
    geom_hline(yintercept = metric_mean + (2*metric_sd), linetype = 3) +
    geom_hline(yintercept = metric_mean - (2*metric_sd), linetype = 3) +
    scale_y_continuous() +
    geom_label_repel(box.padding = unit(0.5, "lines"),
                     na.rm = TRUE)
  # Add verbose labels - not used for grid plots
  if (labs) {
    p <- p + labs(title = metric,
                  subtitle = "All RNA-Seq cohorts, dashed lines show +/- 1 and 2 standard deviations",
                  x = "Library Number",
                  y = metric,
                  caption = format(Sys.time(), '%Y-%m-%d'),
                  colour = "RNA-Seq cohort")
  } else {
    p <- p + labs(x = "Library Number", y = metric)
  }
  return(p)
}
