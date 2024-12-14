#' Draw PCA Plot with Density and Box Plots
#'
#' This function creates a comprehensive PCA plot with density distributions and box plots
#' for visualizing group differences in multivariate data.
#'
#' @param df A data frame containing the variables for PCA analysis
#' @param var_names A character vector specifying the names of variables to be used in PCA
#' @param Group A character vector specifying the levels of groups
#' @param group_labels Optional character vector for custom group labels
#' @param colors Optional vector of colors for groups
#' @param x_limits Optional numeric vector of length 2 specifying x-axis limits
#' @param y_limits Optional numeric vector of length 2 specifying y-axis limits
#' @param show_stats Logical, whether to show PERMANOVA results (default: TRUE)
#' @param point_size Numeric, size of points in the plot (default: 3)
#' @param top_n_vars Numeric, number of top contributing variables to show (default: 5, 0 for no variables)
#'
#' @return A ggplot object containing the combined PCA plot
#' @export
#'
#' @importFrom ggplot2 ggplot geom_point geom_segment geom_hline aes
#' @importFrom ggplot2 geom_vline geom_path geom_polygon scale_color_manual
#' @importFrom ggplot2 scale_fill_manual labs scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 theme theme_minimal element_blank element_line annotate
#' @importFrom ggplot2 geom_density geom_boxplot coord_flip xlab ylab unit arrow theme_classic
#' @importFrom dplyr mutate select filter arrange row_number all_of desc
#' @importFrom magrittr %>%
#' @importFrom vegan rda adonis2 vegdist
#' @importFrom ggpubr theme_classic2 stat_compare_means get_legend
#' @importFrom ggrepel geom_text_repel
#' @importFrom patchwork plot_layout
#' @importFrom ellipse ellipse
#' @importFrom grDevices colorRampPalette
#' @importFrom stats cov
#' @importFrom utils combn
#'
#' @examples
#' \dontrun{
#' # Create example data
#' set.seed(123)
#' df_example <- data.frame(
#'   SampleID = paste0("Sample_", sprintf("%03d", 1:75)),
#'   Group = rep(c("G1", "G2", "G3", "G4", "G5"), each = 15),
#'   DOC = c(
#'     rnorm(15, 100, 15), rnorm(15, 90, 15), rnorm(15, 110, 15),
#'     rnorm(15, 95, 15), rnorm(15, 105, 15)
#'   ),
#'   MBC = c(
#'     rnorm(15, 200, 30), rnorm(15, 180, 30), rnorm(15, 220, 30),
#'     rnorm(15, 190, 30), rnorm(15, 210, 30)
#'   ),
#'   TDN = c(
#'     rnorm(15, 25, 4), rnorm(15, 23, 4), rnorm(15, 27, 4),
#'     rnorm(15, 24, 4), rnorm(15, 26, 4)
#'   ),
#'   AP = c(
#'     rnorm(15, 50, 8), rnorm(15, 45, 8), rnorm(15, 55, 8),
#'     rnorm(15, 48, 8), rnorm(15, 52, 8)
#'   ),
#'   NAG = c(
#'     rnorm(15, 30, 6), rnorm(15, 28, 6), rnorm(15, 32, 6),
#'     rnorm(15, 29, 6), rnorm(15, 31, 6)
#'   ),
#'   BG = c(
#'     rnorm(15, 80, 12), rnorm(15, 75, 12), rnorm(15, 85, 12),
#'     rnorm(15, 78, 12), rnorm(15, 82, 12)
#'   ),
#'   LAP = c(
#'     rnorm(15, 40, 6), rnorm(15, 37, 6), rnorm(15, 43, 6),
#'     rnorm(15, 39, 6), rnorm(15, 41, 6)
#'   ),
#'   TC = c(
#'     rnorm(15, 150, 20), rnorm(15, 140, 20), rnorm(15, 160, 20),
#'     rnorm(15, 145, 20), rnorm(15, 155, 20)
#'   ),
#'   TN = c(
#'     rnorm(15, 15, 3), rnorm(15, 14, 3), rnorm(15, 16, 3),
#'     rnorm(15, 14.5, 3), rnorm(15, 15.5, 3)
#'   ),
#'   TP = c(
#'     rnorm(15, 3, 0.6), rnorm(15, 2.8, 0.6), rnorm(15, 3.2, 0.6),
#'     rnorm(15, 2.9, 0.6), rnorm(15, 3.1, 0.6)
#'   )
#' )
#'
#' # Use function
#' pca_plot <- draw_pca(
#'   df = df_example,
#'   var_names = c("DOC", "MBC", "TDN", "AP", "NAG", "BG", "LAP", "TC", "TN", "TP"),
#'   Group = c("G1", "G2", "G3", "G4", "G5"),
#'   group_labels = c("Control", "Treatment 1", "Treatment 2", "Treatment 3", "Treatment 4"),
#'   top_n_vars = 3,
#'   point_size = 1,
#'   show_stats = TRUE,
#'   y_limits = c(-2,2),
#'   x_limits = c(-2,2)
#' )
#'
#' # Save plot
#' ggsave("PCA_plot.png", pca_plot, width = 6, height = 6, limitsize = FALSE)
#' }
draw_pca <- function(df, var_names, Group, group_labels = NULL, 
                    colors = NULL,
                    x_limits = NULL, 
                    y_limits = NULL,
                    show_stats = TRUE,
                    point_size = 3,
                    top_n_vars = 5) {
  
  # Auto-generate colors
  if(is.null(colors)) {
    colors <- grDevices::colorRampPalette(c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF", 
                                "#D62728FF", "#9467BDFF"))(length(Group))
  }
  
  # Process group labels
  if(!is.null(group_labels)) {
    df$Group <- factor(df$Group, 
                      levels = Group,
                      labels = group_labels)
  } else {
    df$Group <- factor(df$Group, levels = Group)
  }
  
  # Perform PCA analysis
  pca <- summary(vegan::rda(dplyr::select(df, dplyr::all_of(var_names)), scale=T))
  
  # Extract sample coordinates
  sites <- data.frame(pca$sites) %>%
    dplyr::mutate(Group = df$Group)
  
  # Calculate data ranges
  x_range <- range(sites$PC1)
  y_range <- range(sites$PC2)
  
  # Auto-calculate limits if not provided
  if(is.null(x_limits)) {
    x_margin <- diff(x_range) * 0.2
    x_limits <- c(x_range[1] - x_margin, x_range[2] + x_margin)
  }
  if(is.null(y_limits)) {
    y_margin <- diff(y_range) * 0.2
    y_limits <- c(y_range[1] - y_margin, y_range[2] + y_margin)
  }
  
  # Extract species scores
  species <- data.frame(pca$species) %>%
    dplyr::mutate(func = factor(rownames(pca$species), 
                        levels = var_names,
                        labels = var_names))
  
  # Calculate variable contributions
  var_contrib <- data.frame(
    var = var_names,
    contrib = sqrt(species$PC1^2 + species$PC2^2)
  ) %>%
    dplyr::arrange(dplyr::desc(contrib)) %>%
    dplyr::mutate(rank = dplyr::row_number())
  
  # Filter variables based on top_n_vars
  if(top_n_vars > 0) {
    top_vars <- var_contrib$var[1:min(top_n_vars, nrow(var_contrib))]
    species_filtered <- species %>%
      dplyr::filter(func %in% top_vars)
  }
  
  # Perform PERMANOVA analysis
  adonis_result <- vegan::adonis2(vegan::vegdist(dplyr::select(df, dplyr::all_of(var_names)), 
                                  method="bray") ~ Group, data=df)
  
  # Calculate ellipse data
  ellipse_data <- lapply(levels(df$Group), function(grp) {
    subset <- dplyr::filter(sites, Group == grp)
    mean_data <- colMeans(subset[, c("PC1", "PC2")])
    cov_matrix <- stats::cov(subset[, c("PC1", "PC2")])
    ellipse_points <- ellipse::ellipse(cov_matrix, centre = mean_data, level = 0.95)
    data.frame(Group = grp, ellipse_points)
  })
  ellipse_data <- do.call(rbind, ellipse_data)
  
  # Set annotation position to top right corner
  annotation_x <- x_limits[2]
  annotation_y <- y_limits[2]
  
  # Create main plot
  p1 <- ggplot2::ggplot() +
    ggplot2::geom_point(data = sites, 
               ggplot2::aes(x = PC1, y = PC2, fill = Group), 
               size = point_size, color = "transparent", shape = 21)
  
  # Add arrows and labels if top_n_vars > 0
  if(top_n_vars > 0) {
    p1 <- p1 +
      ggplot2::geom_segment(data = species_filtered, 
                  ggplot2::aes(x = 0, y = 0, xend = -1.25 * PC1, 
                      yend = 1.25 * PC2),
                  arrow = ggplot2::arrow(angle = 22.5, 
                                       length = ggplot2::unit(0.25, "cm"), 
                                       type = "closed")) +
      ggrepel::geom_text_repel(data = species_filtered, 
                      ggplot2::aes(x = -1.275 * PC1, y = 1.275 * PC2, 
                          label = func), 
                      size = 3.8)
  }
  
  p1 <- p1 +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    ggplot2::geom_path(data = ellipse_data, 
              ggplot2::aes(x = PC1, y = PC2, group = Group, 
                  color = Group),
              show.legend = FALSE, linetype = "dashed") +
    ggplot2::geom_polygon(data = ellipse_data,
                ggplot2::aes(x = PC1, y = PC2, group = Group, 
                    fill = Group),
                alpha = 0.2) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(x = sprintf("PC1 (%.2f%%)", pca$cont$importance[2,1] * 100),
         y = sprintf("PC2 (%.2f%%)", pca$cont$importance[2,2] * 100)) +
    ggplot2::scale_x_continuous(limits = x_limits) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggpubr::theme_classic2() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(color = "black"),
          axis.ticks = ggplot2::element_blank())
  
  # Add PERMANOVA results if show_stats is TRUE
  if(show_stats) {
    p1 <- p1 + 
      ggplot2::annotate("text", 
               label = sprintf("PERMANOVA\nR^2 = %.3f\np = %.3f", 
                             adonis_result$R2[1], 
                             adonis_result$`Pr(>F)`[1]),
               x = annotation_x,
               y = annotation_y,
               size = 3.5,
               hjust = 1,
               vjust = 1)
  }
  
  # Extract legend and create legend-free version of main plot
  legend <- ggpubr::get_legend(p1)
  p11 <- p1 + ggplot2::theme(legend.position = "none")
  
  # Create density plots
  p2 <- ggplot2::ggplot(data = sites) +
    ggplot2::geom_density(ggplot2::aes(x = PC1, fill = Group), alpha = 0.2,
                 color = 'black', position = 'identity', show.legend = FALSE) +
    ggplot2::scale_fill_manual(values=colors) +
    ggplot2::scale_x_continuous(limits = x_limits) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank())
  
  p3 <- ggplot2::ggplot(data = sites) +
    ggplot2::geom_density(ggplot2::aes(x = PC2, fill = Group), alpha = 0.2,
                 color = 'black', position = 'identity', show.legend = FALSE) +
    ggplot2::scale_fill_manual(values=colors) +
    ggplot2::scale_x_continuous(limits = y_limits) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank()) +
    ggplot2::coord_flip()
  
  # Prepare data for box plots
  otu.pca.data <- data.frame(
    PC1 = sites$PC1,
    PC2 = sites$PC2,
    group = sites$Group
  )
  
  # Generate pairwise comparisons
  my_comparisons <- utils::combn(levels(df$Group), 2, simplify = FALSE)
  
  # Create box plots
  p4 <- ggplot2::ggplot(otu.pca.data, ggplot2::aes(x = group, y = PC1, 
                                 colour = group)) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          legend.position = "none",
          axis.ticks = ggplot2::element_blank()) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::coord_flip() +
    ggpubr::stat_compare_means(comparisons = my_comparisons, label = "p.signif")
  
  p5 <- ggplot2::ggplot(otu.pca.data, ggplot2::aes(x = group, y = PC2, 
                                 colour = group)) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          legend.position = "none",
          axis.ticks = ggplot2::element_blank()) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggpubr::stat_compare_means(comparisons = my_comparisons, label = "p.signif")
  
  # Combine all plots
  design <- "166
            266
            345"
  
  p <- p4 + p2 + p11 + p3 + p5 + legend + 
    patchwork::plot_layout(design = design, widths=c(5,1,1), heights=c(1,1,5))
  
  return(p)
}