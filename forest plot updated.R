library(forestplot)

# Extract odds ratios and confidence intervals
coefs <- exp(coef(logfit_final))           # Convert log-odds → odds ratio
conf_ints <- exp(confint(logfit_final))    # Convert CI → odds ratio scale

plot_coefs <- data.frame(
  var = names(coefs)[-1],
  OR = coefs[-1],
  l_conf_int = conf_ints[-1, 1],
  u_conf_int = conf_ints[-1, 2]
)

# Flag significance (if CI does not cross 1)
plot_coefs$signif <- plot_coefs$l_conf_int > 1 | plot_coefs$u_conf_int < 1

# Add asterisks for significant variables
plot_coefs$var <- ifelse(plot_coefs$signif,
                         paste0(plot_coefs$var, " *"),
                         plot_coefs$var)

# Format OR + CI for table
plot_coefs$OR_ci <- sprintf("%.2f (%.2f, %.2f)",
                            plot_coefs$OR,
                            plot_coefs$l_conf_int,
                            plot_coefs$u_conf_int)

# Build label text (header + data)
tabletext <- rbind(
  c("Variable", "Odds Ratio (95% CI)"),
  cbind(plot_coefs$var, plot_coefs$OR_ci)
)

# Logical vector for summary rows (header = TRUE)
is_summary <- c(TRUE, rep(FALSE, nrow(plot_coefs)))

# --- DISPLAY IN RSTUDIO ---
forestplot(
  labeltext = tabletext,
  mean  = c(NA, plot_coefs$OR),
  lower = c(NA, plot_coefs$l_conf_int),
  upper = c(NA, plot_coefs$u_conf_int),
  zero = 1,                        # For odds ratios, null value = 1
  xlog = TRUE,                     # Log scale
  cex = 1,
  lineheight = "auto",
  fn.ci_norm = fpDrawNormalCI,
  boxsize = 0.1,
  col = fpColors(box = "darkgreen", line = "forestgreen", summary = "darkgreen"),
  xlab = "Odds Ratio (log scale)",
  is.summary = is_summary,
  title = "Forest Plot of Final Logistic Regression Model",
  txt_gp = fpTxtGp(
    label = gpar(cex = 0.9),
    ticks = gpar(cex = 0.8),
    xlab = gpar(cex = 1),
    title = gpar(cex = 1.2, fontface = "bold")
  )
)

# --- SAVE COPY AS PNG ---
if (!dir.exists("Plots")) dir.create("Plots")  # create folder if missing

png("Plots/05a-log_reg_OR_forestplot.png", width = 800, height = 600, res = 100)
forestplot(
  labeltext = tabletext,
  mean  = c(NA, plot_coefs$OR),
  lower = c(NA, plot_coefs$l_conf_int),
  upper = c(NA, plot_coefs$u_conf_int),
  zero = 1,
  xlog = TRUE,
  cex = 1,
  lineheight = "auto",
  fn.ci_norm = fpDrawNormalCI,
  boxsize = 0.1,
  col = fpColors(box = "darkgreen", line = "forestgreen", summary = "darkgreen"),
  xlab = "Odds Ratio (log scale)",
  is.summary = is_summary,
  title = "Forest Plot of Final Logistic Regression Model",
  txt_gp = fpTxtGp(
    label = gpar(cex = 0.9),
    ticks = gpar(cex = 0.8),
    xlab = gpar(cex = 1),
    title = gpar(cex = 1.2, fontface = "bold")
  )
)
dev.off()

# Optional: open the saved image automatically
browseURL("Plots/05a-log_reg_OR_forestplot.png")
getwd()



