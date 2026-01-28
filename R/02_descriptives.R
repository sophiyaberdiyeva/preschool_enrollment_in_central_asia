# Code written by Claude Sonnet 4.5

# Additional Utility Functions for Chi-Square and Cramer's V Analysis
# Companion script with extra visualization and analysis options

# Function to create a correlation-style plot (clustering similar variables)

plot_cramers_clustered <- function(results) {
  
  # Replace NA with 0 for clustering
  cramers_for_clustering <- results$cramers_v
  cramers_for_clustering[is.na(cramers_for_clustering)] <- 0
  
  # Perform hierarchical clustering
  dist_matrix <- as.dist(1 - cramers_for_clustering)
  hc <- hclust(dist_matrix, method = "average")
  
  # Reorder matrix based on clustering
  var_order <- hc$order
  cramers_ordered <- results$cramers_v[var_order, var_order]
  
  # Create melted data - convert matrix to long format
  cramers_melt <- data.frame(
    Var1 = rep(rownames(cramers_ordered), each = ncol(cramers_ordered)),
    Var2 = rep(colnames(cramers_ordered), times = nrow(cramers_ordered)),
    CramersV = as.vector(cramers_ordered)
  )
  # Remove NA values
  cramers_melt <- cramers_melt[!is.na(cramers_melt$CramersV), ]
  
  # Plot
  ggplot(cramers_melt, aes(x = Var1, y = Var2, fill = CramersV)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradient2(
      low = "white", 
      mid = "#fee08b", 
      high = "#d73027",
      midpoint = 0.3,
      limit = c(0, 1),
      na.value = "grey90",
      name = "Cramer's V"
    ) +
    theme_minimal() +
    labs(
      title = "Cramer's V Heatmap (Clustered)",
      subtitle = "Variables ordered by hierarchical clustering",
      x = "", y = ""
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7),
      axis.text.y = element_text(size = 7),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 9),
      legend.position = "right",
      panel.grid = element_blank()
    ) +
    coord_fixed()
}

# Function to create network plot of strong associations
plot_association_network <- function(results, min_cramers_v = 0.3) {
  
  # Get upper triangle to avoid duplicates
  n_vars <- nrow(results$cramers_v)
  edges <- data.frame()
  
  for (i in 1:(n_vars - 1)) {
    for (j in (i + 1):n_vars) {
      v <- results$cramers_v[i, j]
      if (!is.na(v) && v >= min_cramers_v) {
        edges <- rbind(edges, data.frame(
          from = rownames(results$cramers_v)[i],
          to = colnames(results$cramers_v)[j],
          weight = v
        ))
      }
    }
  }
  
  if (nrow(edges) == 0) {
    cat("No associations meet the minimum Cramer's V threshold.\n")
    return(NULL)
  }
  
  # Create graph
  g <- graph_from_data_frame(edges, directed = FALSE)
  E(g)$width <- E(g)$weight * 5
  E(g)$color <- rgb(1, 0.4, 0.3, alpha = E(g)$weight)
  
  # Layout
  layout <- layout_with_fr(g)
  
  # Plot
  plot(g, 
       layout = layout,
       vertex.size = 15,
       vertex.color = "#4393c3",
       vertex.label.cex = 0.8,
       vertex.label.color = "black",
       edge.curved = 0.2,
       main = sprintf("Association Network (Cramer's V >= %.2f)", min_cramers_v))
  
  return(g)
}

# Function to get top associations
get_top_associations <- function(results, n = 10) {
  
  # Get upper triangle indices
  n_vars <- nrow(results$cramers_v)
  indices <- which(upper.tri(results$cramers_v), arr.ind = TRUE)
  
  # Create data frame
  assoc_df <- data.frame(
    Var1 = rownames(results$cramers_v)[indices[, 1]],
    Var2 = colnames(results$cramers_v)[indices[, 2]],
    CramersV = results$cramers_v[indices],
    ChiSquare = results$chi_sq_stats[indices],
    PValue = results$p_values[indices]
  )
  
  # Remove NA and sort
  assoc_df <- assoc_df[!is.na(assoc_df$CramersV), ]
  assoc_df <- assoc_df[order(-assoc_df$CramersV), ]
  
  # Return top n
  head(assoc_df, n)
}

# Function to create summary statistics
summary_stats <- function(results) {
  
  cat("\n=== Association Analysis Summary ===\n\n")
  
  # Overall statistics
  n_vars <- nrow(results$cramers_v)
  n_possible <- n_vars * (n_vars - 1) / 2
  n_computed <- sum(!is.na(results$p_values[upper.tri(results$p_values)]))
  n_significant <- results$n_significant
  
  cat(sprintf("Total variables: %d\n", n_vars))
  cat(sprintf("Possible pairs: %d\n", n_possible))
  cat(sprintf("Computed pairs: %d\n", n_computed))
  cat(sprintf("Significant pairs: %d (%.1f%%)\n", 
              n_significant, 
              100 * n_significant / n_computed))
  cat(sprintf("Adjusted p-value threshold: %.2e\n", results$adjusted_p_threshold))
  
  # Cramer's V distribution for significant associations
  sig_cramers <- results$cramers_v[!is.na(results$cramers_v)]
  
  if (length(sig_cramers) > 0) {
    cat("\n--- Cramer's V Statistics (Significant Associations) ---\n")
    cat(sprintf("Mean: %.3f\n", mean(sig_cramers)))
    cat(sprintf("Median: %.3f\n", median(sig_cramers)))
    cat(sprintf("Min: %.3f\n", min(sig_cramers)))
    cat(sprintf("Max: %.3f\n", max(sig_cramers)))
    
    # Effect size categories (Cohen's guidelines adapted)
    weak <- sum(sig_cramers < 0.1)
    small <- sum(sig_cramers >= 0.1 & sig_cramers < 0.3)
    medium <- sum(sig_cramers >= 0.3 & sig_cramers < 0.5)
    large <- sum(sig_cramers >= 0.5)
    
    cat("\n--- Effect Size Distribution ---\n")
    cat(sprintf("Weak (< 0.1): %d (%.1f%%)\n", weak, 100 * weak / length(sig_cramers)))
    cat(sprintf("Small (0.1-0.3): %d (%.1f%%)\n", small, 100 * small / length(sig_cramers)))
    cat(sprintf("Medium (0.3-0.5): %d (%.1f%%)\n", medium, 100 * medium / length(sig_cramers)))
    cat(sprintf("Large (>= 0.5): %d (%.1f%%)\n", large, 100 * large / length(sig_cramers)))
  }
  
  cat("\n")
}

# Function for parallel processing (for very large datasets)
pairwise_chi_cramers_parallel <- function(df, p_threshold = 0.05, 
                                          use_bonferroni = TRUE, n_cores = NULL) {
  
  if (!require("parallel")) {
    stop("parallel package required for this function")
  }
  
  if (is.null(n_cores)) {
    n_cores <- detectCores() - 1
  }
  
  cat_vars <- names(df)
  n_vars <- length(cat_vars)
  n_pairs <- n_vars * (n_vars - 1) / 2
  
  if (use_bonferroni) {
    adjusted_p <- p_threshold / n_pairs
  } else {
    adjusted_p <- p_threshold
  }
  
  # Generate all pairs
  pairs <- combn(n_vars, 2, simplify = FALSE)
  
  # Define function for single pair
  process_pair <- function(pair_idx) {
    i <- pairs[[pair_idx]][1]
    j <- pairs[[pair_idx]][2]
    
    valid_rows <- complete.cases(df[, c(cat_vars[i], cat_vars[j])])
    
    if (sum(valid_rows) < 5) {
      return(NULL)
    }
    
    contingency_table <- table(df[valid_rows, cat_vars[i]], 
                               df[valid_rows, cat_vars[j]])
    
    if (any(dim(contingency_table) < 2)) {
      return(NULL)
    }
    
    chi_test <- suppressWarnings(
      tryCatch(
        chisq.test(contingency_table, correct = FALSE),
        error = function(e) NULL
      )
    )
    
    if (is.null(chi_test)) {
      return(NULL)
    }
    
    result <- list(
      i = i, j = j,
      p_value = chi_test$p.value,
      chi_sq = chi_test$statistic
    )
    
    if (chi_test$p.value < adjusted_p) {
      n <- sum(contingency_table)
      min_dim <- min(nrow(contingency_table) - 1, ncol(contingency_table) - 1)
      result$cramers_v <- sqrt(chi_test$statistic / (n * min_dim))
    } else {
      result$cramers_v <- NA
    }
    
    return(result)
  }
  
  # Process in parallel
  cat(sprintf("Processing %d pairs using %d cores...\n", n_pairs, n_cores))
  cl <- makeCluster(n_cores)
  clusterExport(cl, c("df", "cat_vars", "adjusted_p"), envir = environment())
  
  results_list <- parLapply(cl, 1:length(pairs), process_pair)
  stopCluster(cl)
  
  # Convert to matrices
  p_values <- matrix(NA, n_vars, n_vars, dimnames = list(cat_vars, cat_vars))
  chi_sq_stats <- matrix(NA, n_vars, n_vars, dimnames = list(cat_vars, cat_vars))
  cramers_v_matrix <- matrix(NA, n_vars, n_vars, dimnames = list(cat_vars, cat_vars))
  
  for (res in results_list) {
    if (!is.null(res)) {
      p_values[res$i, res$j] <- p_values[res$j, res$i] <- res$p_value
      chi_sq_stats[res$i, res$j] <- chi_sq_stats[res$j, res$i] <- res$chi_sq
      cramers_v_matrix[res$i, res$j] <- cramers_v_matrix[res$j, res$i] <- res$cramers_v
    }
  }
  
  n_significant <- sum(p_values < adjusted_p, na.rm = TRUE) / 2
  cat(sprintf("Found %d significant pairs\n", n_significant))
  
  list(
    p_values = p_values,
    chi_sq_stats = chi_sq_stats,
    cramers_v = cramers_v_matrix,
    adjusted_p_threshold = adjusted_p,
    n_significant = n_significant
  )
}


# Efficient Pairwise Chi-Square and Cramer's V Analysis
# Description: Computes pairwise chi-square tests and Cramer's V for categorical variables

# Function to calculate Cramer's V
cramers_v <- function(chi_sq, n, df) {
  # Cramer's V = sqrt(chi_sq / (n * min(r-1, c-1)))
  sqrt(chi_sq / (n * df))
}

# Main function for efficient pairwise analysis
pairwise_chi_cramers <- function(df, p_threshold = 0.05, use_bonferroni = TRUE) {
  
  # Get categorical variable names
  cat_vars <- names(df)
  n_vars <- length(cat_vars)
  n_pairs <- n_vars * (n_vars - 1) / 2
  
  # Adjust p-value threshold if using Bonferroni correction
  if (use_bonferroni) {
    adjusted_p <- p_threshold / n_pairs
    cat(sprintf("Using Bonferroni correction: adjusted p-value = %.2e\n", adjusted_p))
  } else {
    adjusted_p <- p_threshold
  }
  
  # Initialize matrices for results
  p_values <- matrix(NA, n_vars, n_vars, dimnames = list(cat_vars, cat_vars))
  chi_sq_stats <- matrix(NA, n_vars, n_vars, dimnames = list(cat_vars, cat_vars))
  cramers_v_matrix <- matrix(NA, n_vars, n_vars, dimnames = list(cat_vars, cat_vars))
  
  # Progress tracking
  cat(sprintf("Computing %d pairwise comparisons...\n", n_pairs))
  pb <- txtProgressBar(min = 0, max = n_pairs, style = 3)
  counter <- 0
  
  # Pairwise comparisons (only upper triangle to avoid redundancy)
  for (i in 1:(n_vars - 1)) {
    for (j in (i + 1):n_vars) {
      
      counter <- counter + 1
      setTxtProgressBar(pb, counter)
      
      # Create contingency table
      # Remove rows with NA in either variable for this pair
      valid_rows <- complete.cases(df[, c(cat_vars[i], cat_vars[j])])
      
      if (sum(valid_rows) < 5) {
        # Skip if too few observations
        next
      }
      
      contingency_table <- table(df[valid_rows, cat_vars[i]], 
                                 df[valid_rows, cat_vars[j]])
      
      # Skip if table is too sparse
      if (any(dim(contingency_table) < 2)) {
        next
      }
      
      # Perform chi-square test
      # Use suppressWarnings to handle expected count warnings
      chi_test <- suppressWarnings(
        tryCatch(
          chisq.test(contingency_table, correct = FALSE),
          error = function(e) NULL
        )
      )
      
      if (is.null(chi_test)) {
        next
      }
      
      # Store chi-square results (symmetric)
      p_values[i, j] <- p_values[j, i] <- chi_test$p.value
      chi_sq_stats[i, j] <- chi_sq_stats[j, i] <- chi_test$statistic
      
      # Calculate Cramer's V only if significant
      if (chi_test$p.value < adjusted_p) {
        n <- sum(contingency_table)
        min_dim <- min(nrow(contingency_table) - 1, ncol(contingency_table) - 1)
        
        v <- cramers_v(chi_test$statistic, n, min_dim)
        cramers_v_matrix[i, j] <- cramers_v_matrix[j, i] <- v
      }
    }
  }
  
  close(pb)
  
  # Count significant pairs
  n_significant <- sum(p_values < adjusted_p, na.rm = TRUE) / 2
  cat(sprintf("\nFound %d significant pairs (p < %.2e)\n", n_significant, adjusted_p))
  
  # Return results
  list(
    p_values = p_values,
    chi_sq_stats = chi_sq_stats,
    cramers_v = cramers_v_matrix,
    adjusted_p_threshold = adjusted_p,
    n_significant = n_significant
  )
}

# Function to create enhanced heatmap
# Function to create enhanced heatmap
plot_cramers_heatmap <- function(results, title = "Cramer's V for Significant Associations",
                                 show_values = FALSE, text_size = 3, 
                                 text_angle_x = 90, text_size_x = 5, text_size_y = 5) {
  
  # Prepare data for ggplot - convert matrix to long format
  cramers_matrix <- results$cramers_v
  
  # Get variable names in order
  var_names <- rownames(cramers_matrix)
  
  cramers_melt <- data.frame(
    Var1 = rep(rownames(cramers_matrix), each = ncol(cramers_matrix)),
    Var2 = rep(colnames(cramers_matrix), times = nrow(cramers_matrix)),
    CramersV = as.vector(cramers_matrix)
  )
  # Remove NA values
  cramers_melt <- cramers_melt[!is.na(cramers_melt$CramersV), ]
  
  # Set factor levels to control ordering
  # X-axis: left to right (normal order)
  cramers_melt$Var1 <- factor(cramers_melt$Var1, levels = var_names)
  # Y-axis: top to bottom (reverse order for display)
  cramers_melt$Var2 <- factor(cramers_melt$Var2, levels = rev(var_names))
  
  # Create heatmap
  p <- ggplot(cramers_melt, aes(x = Var1, y = Var2, fill = CramersV)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradient2(
      low = "white", 
      mid = "#fee08b", 
      high = "#d73027",
      midpoint = 0.3,
      limit = c(0, 1),
      na.value = "grey90",
      name = "Cramer's V"
    ) +
    theme_minimal() +
    labs(
      title = title,
      subtitle = sprintf("Showing %d significant associations (p < %.2e)", 
                         results$n_significant, 
                         results$adjusted_p_threshold),
      x = "", y = ""
    ) +
    theme(
      # X-axis labels: 90 degrees, centered vertically
      axis.text.x = element_text(angle = text_angle_x, hjust = 1, vjust = 0.5, 
                                 size = text_size_x, margin = margin(t = 5)),
      # Y-axis labels: horizontal, right-aligned
      axis.text.y = element_text(size = text_size_y, hjust = 1, 
                                 margin = margin(r = 5)),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      legend.position = "right",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 9),
      panel.grid = element_blank(),
      plot.margin = margin(15, 10, 15, 10)
    ) +
    coord_fixed()
  
  # Add text values if requested
  if (show_values) {
    p <- p + geom_text(aes(label = sprintf("%.2f", CramersV)), 
                       size = text_size, na.rm = TRUE)
  }
  
  return(p)
}
