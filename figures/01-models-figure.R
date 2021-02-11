#===============================================================================
# 01-models-figure.R
# Purpose: To replicate Figure 1 of the paper, where we report the results of 
#          a set of 5 statistical models    
# Article: "The (null) effects of happiness on affective polarization, conspiracy 
#           endorsement, and deep fake recognition : Evidence from five survey 
#           experiments in three countries"
# Journal: Political Behavior
# Year: 2020
# Authors: Xudong Yu, Magdalena Wojcieszak, Seungsu Lee, Andreu Casas, Rachid Azrout, Tomasz Gackowski
#===============================================================================

# PACKAGES
#===============================================================================
# - install packages if needed
installed.packages("broom")
installed.packages("dplyr")
installed.packages("ggplot2")
# - load packages
library(broom)
library(dplyr)
library(ggplot2)
library(extrafont)
loadfonts()

# DATA
#===============================================================================
# - load all the estimated models and save the coefficents and SEs into a single
#   table
model_list <- list.files("./models/")
model_table <- NULL
for (m in model_list) {
  # - load the model
  print(load(paste0("./models/", m)))
  # - pull data used to estimate the model
  model_data <- model_fit$model
  # - calculate standard deviation of responses for those in the control group
  control_sd <- sd(model_data[which(grepl("Control", model_data$condition)), 1])
  # - pull coefficients and SE for the treatment variables
  new_rows <- tidy(model_fit) %>%
    rename(condition = term) %>%
    mutate(pe  = estimate,
           # - adjusted t-score for judging significance, due to running many
           #   tests, and adjusted following FDR (False Discovery Rate) procedure
           lwr = estimate - (2.4 * std.error),
           upr = estimate + (2.4 * std.error)) %>%
    # - normalize coefficients by dividing them by the standard deviation
    mutate(pe_std = pe / control_sd,
           lwr_std = lwr / control_sd,
           upr_std = upr / control_sd) %>% 
    # - keep only the variables indicating the standardized effect
    dplyr::select(condition, pe_std, lwr_std, upr_std) %>%
    # - remove results for the intercept and controls
    filter(!(condition %in% c("(Intercept)", "age", "conditionControlW"))) %>%
    # - add information about the model
    mutate(study = strsplit(m, split = "_")[[1]][1],
           outcome = gsub(".rda", "", 
                          paste0(strsplit(m, split = "_")[[1]][-1],
                                 collapse = "_")))
  # - save results for this model into the main dataset with all model info
  model_table <- rbind(model_table, new_rows)
}

# DATA WRANGLING
#===============================================================================
# - provide human-readable labels to conditions, studies, and outcomes
plot_db <- model_table %>%
  mutate(condition = recode(condition,
                            `conditionHappyP` = "Happy photos",
                            `conditionHappyQ` = "Happy questions",
                            `conditionHappyW` = "Happy writing",
                            `condition_comHappyP` = "Happy photos",
                            `condition_comHappyQ` = "Happy questions",
                            `condition_comHappyW` = "Happy writing",
                            `conditionANGW` = "Angry writing",
                            `conditionANXW` = "Anxiety writing"),
         study = paste0("Study ", gsub("[a-z]", "", study)),
         outcome = recode(outcome,
                          `ap_index` = "Affective \npolarization",
                          `conspiracy` = "Beliefs in \nconspiracy theories",
                          `ft_feminist` = "Feeling thermometer \ntowards feminists",
                          `ft_immigrant` = "Feeling thermometer \ntowards immigrants",
                          `ft_neonazi` = "Feeling thermometer \ntowards neo-Nazis",
                          `ft_nationalist` = "Feeling thermometer \ntowards nationalists",
                          `video` = "Believing a \ndeep fake video"
                          ))


# PLOT
#===============================================================================
pdf("./plots/models-figure.pdf",
    width = 11.5, height = 5, family = "NYTFranklin Light")
ggplot(plot_db,
       aes(x = condition, y = pe_std)) +
  geom_hline(yintercept = 0, color = "gray60") +
  geom_point() +
  geom_segment(aes(x = condition, xend = condition, 
                   y = lwr_std, yend = upr_std)) +
  scale_y_continuous("Treatment effect (in standard deviation change)", 
                     limits = c(-0.4, 0.5)) +
  scale_x_discrete("") +
  coord_flip() + 
  facet_grid(study ~ outcome, scales = "free") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
        strip.background = element_rect(color = "black", fill = "white"),
        panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10))
dev.off()  

plot_db <- plot_db %>% mutate(significant = ifelse(sign(lwr_std) == sign(upr_std), 1, 0))
plot_db %>% filter(significant == 1)
