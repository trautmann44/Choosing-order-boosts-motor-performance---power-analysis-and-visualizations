packages_needed <- c('writexl', 'readxl', 'dplyr', 'tidyr', 'ggplot2', 
                     'tidyverse', 'lme4', 'faux', 'lmerTest', 'emmeans', 
                     'purrr', 'MASS', 'faux', 'MuMIn', 'performance', 
                     'webshot2', 'rempsyc', 'flextable', 'officer', 
                     'flexplot', 'ggbreak')

lapply(packages_needed, FUN = require, character.only = T)


# Create a function to retrieve package information
get_package_info <- function(package_name) {
  if (requireNamespace(package_name, quietly = TRUE)) {
    pkg_desc <- packageDescription(package_name)
    
    package_info <- data.frame(
      Package = package_name,
      Description = ifelse("Title" %in% names(pkg_desc), pkg_desc[["Title"]], NA),
      Version = ifelse("Version" %in% names(pkg_desc), pkg_desc[["Version"]], NA)
    )
    return(package_info)
  } else {
    return(NULL)
  }
}

# Get information for each loaded package
package_info <- lapply(packages_needed, get_package_info)

# Filter out NULL values (packages that weren't loaded)
package_info <- Filter(Negate(is.null), package_info)

# Combine the information into a single data frame
package_info_df <- do.call(rbind, package_info)

# Make a table
writexl::write_xlsx(package_info_df, "package_info_A5.xlsx")
packages <- read_excel("package_info_A5.xlsx")

print(packages)
packages %>% as_tibble() %>% print(n = nrow(packages))

# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ----------------------------------------------------- TASK 1 Figure of Stops ------------------------------------------------- #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #

data_long <- data %>%
  pivot_longer(
    cols = matches("^(BT|PT|RT)_Tr\\d+"),
    names_to = c("phase", "trial"),
    names_pattern = "(BT|PT|RT)_(Tr\\d+)",
    values_to = "stops"
  )

data_long$Comments <- NULL

# ------------------------------------------------------------------------------------------- #
# ------------------------------------------------------------------------------------------- #
# ------------------------------------------------------------------------------------------- #

data_summary <- data_long %>%
  group_by(Group, phase, trial) %>%
  summarise(total_stops = sum(stops, na.rm = TRUE), .groups = "drop")


data_summary <- data_summary %>%
  dplyr::mutate(
    trial_num = as.numeric(sub("Tr", "", trial)),   # extract number
    Group = factor(Group, levels = c("EE", "Con")),
    trial = factor(trial, levels = paste0("Tr", sort(unique(trial_num))))  # ordered factor
  )

data_summary$phase_trial <- paste(data_summary$phase, data_summary$trial, sep = "")

# ------------------------------------------------------------------------------------------- #
# ------------------------------------------------------------------------------------------- #

(plot_all3 <- data_summary %>%
    ggplot(aes(x = phase_trial, y = total_stops, fill = Group)) +
    
    # --- draw custom lines only for 1–14 --- #
    
    geom_hline(
      yintercept = 0:12,      
      color = "gray80",
      linewidth = 0.2
    ) +
    
    # --- bars and labels --- #
    
    geom_bar(stat = "identity", position = position_dodge(width = 1.0)) +
    labs(
      title = "",
      x = "Trial",
      y = "Number of task interruptions",
      fill = "Group"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "right",        # put whole legend above plot
      legend.direction = "vertical",
      legend.box = "vertical",        # stack title above keys
      legend.box.just = "center",     # center the legend box in the plot area
      legend.key.width = unit(0.6, "cm"),
      axis.text.x = element_text(hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      axis.title.y = element_text(margin = margin(r = 15), hjust = 0.2),
      axis.title.x = element_text(margin = margin(t = 15))
    ) + labs(fill='') + 
    
    # ------------------------ vertical separators ----------------------- #
    
    geom_segment(
      aes(x = 0.5, xend = 0.5, y = 0, yend = 12),
      linetype = "dashed",
      color = "gray80", linewidth = 0.4
    ) + annotate("text", x = 2.5, y = 12.6, label = "Block 1", size = 3.0) +
    annotate("text", x = 2.5, y = 14.1, label = "BT", size = 3.0, fontface = 2) +
    geom_segment(aes(x = 0.5, xend = 4.35, y = 13.5, yend = 13.5), linewidth = 0.1) +
    geom_segment(aes(x = 0.5, xend = 0.5, y = 13.5, yend = 12.5), linewidth = 0.1) +
    geom_segment(aes(x = 4.35, xend = 4.35, y = 13.5, yend = 12.5), linewidth = 0.1) +
    
    geom_segment(
      aes(x = 4.5, xend = 4.5, y = 0, yend = 12),
      linetype = "dashed",
      color = "gray80", linewidth = 0.4
    ) + annotate("text", x = 6.5, y = 12.6, label = "Block 2", size = 3.0) +
    annotate("text", x = 14.5, y = 14.1, label = "PT", size = 3.0, fontface = 2) +
    geom_segment(aes(x = 4.65, xend = 24.35, y = 13.5, yend = 13.5), linewidth = 0.1) +
    geom_segment(aes(x = 4.65, xend = 4.65, y = 13.5, yend = 12.5), linewidth = 0.1) +
    geom_segment(aes(x = 24.35, xend = 24.35, y = 13.5, yend = 12.5), linewidth = 0.1) +
    
    
    geom_segment(
      aes(x = 8.5, xend = 8.5, y = 0, yend = 12),
      linetype = "dashed",
      color = "gray80", linewidth = 0.4
    ) + annotate("text", x = 10.5, y = 12.6, label = "Block 3", size = 3.0) +
    
    geom_segment(
      aes(x = 12.5, xend = 12.5, y = 0, yend = 12),
      linetype = "dashed",
      color = "gray80", linewidth = 0.4
    ) + annotate("text", x = 14.5, y = 12.6, label = "Block 4", size = 3.0) +
    
    geom_segment(
      aes(x = 16.5, xend = 16.5, y = 0, yend = 12),
      linetype = "dashed",
      color = "gray80", linewidth = 0.4
    ) + annotate("text", x = 18.5, y = 12.6, label = "Block 5", size = 3.0) +
    
    geom_segment(
      aes(x = 20.5, xend = 20.5, y = 0, yend = 12),
      linetype = "dashed",
      color = "gray80", linewidth = 0.4
    ) + annotate("text", x = 22.5, y = 12.6, label = "Block 6", size = 3.0) +
    
    geom_segment(
      aes(x = 24.5, xend = 24.5, y = 0, yend = 12),
      linetype = "dashed",
      color = "gray80", linewidth = 0.4
    ) + annotate("text", x = 28.5, y = 12.6, label = "Block 7", size = 3.0) +
    annotate("text", x = 28.5, y = 14.1, label = "RT", size = 3.0, fontface = 2) +
    geom_segment(aes(x = 24.65, xend = 32.5, y = 13.5, yend = 13.5), linewidth = 0.1) +
    geom_segment(aes(x = 24.65, xend = 24.65, y = 13.5, yend = 12.5), linewidth = 0.1) +
    geom_segment(aes(x = 32.5, xend = 32.5, y = 13.5, yend = 12.5), linewidth = 0.1) +
    
    geom_segment(
      aes(x = 32.5, xend = 32.5, y = 0, yend = 12),
      linetype = "dashed",
      color = "gray80", linewidth = 0.4
    ) +
    
    geom_segment(aes(x = 0, xend = 33, y = 0, yend = 0)) +
    geom_segment(aes(x = 0, xend = 0, y = 0, yend = 12)) +
    
    # --- Phases and trials --- #
    
    scale_y_continuous(limits = c(0,16), breaks = c(0:12), expand = expansion(mult = c(0, 0.02))) +
    scale_x_discrete(limits = c(
      "BTTr1","BTTr2","BTTr3","BTTr4",
      "PTTr1","PTTr2","PTTr3","PTTr4","PTTr5","PTTr6","PTTr7","PTTr8",
      "PTTr9","PTTr10","PTTr11","PTTr12","PTTr13","PTTr14","PTTr15","PTTr16",
      "PTTr17","PTTr18","PTTr19","PTTr20",
      "RTTr1","RTTr2","RTTr3","RTTr4","RTTr5","RTTr6","RTTr7","RTTr8"
    ),labels = c("BTTr1" = "1", 
                 "BTTr2" = "2", 
                 "BTTr3" = "3",
                 "BTTr4" = "4",
                 "PTTr1" = "5", 
                 "PTTr2" = "6", 
                 "PTTr3" = "7",
                 "PTTr4" = "8",
                 "PTTr5" = "9",
                 "PTTr6" = "10",
                 "PTTr7" = "11",
                 "PTTr8" = "12",
                 "PTTr9" = "13",
                 "PTTr10" = "14",
                 "PTTr11" = "15",
                 "PTTr12" = "16",
                 "PTTr13" = "17",
                 "PTTr14" = "18",
                 "PTTr15" = "19",
                 "PTTr16" = "20",
                 "PTTr17" = "21",
                 "PTTr18" = "22",
                 "PTTr19" = "23",
                 "PTTr20" = "24",
                 "RTTr1" = "25", 
                 "RTTr2" = "26", 
                 "RTTr3" = "27",
                 "RTTr4" = "28",
                 "RTTr5" = "29",
                 "RTTr6" = "30",
                 "RTTr7" = "31",
                 "RTTr8" = "32"), expand = expansion(add = c(0.85, 0.85))) + 
    scale_fill_manual(values = c("EE" = "#3F3D73",
                                 "Con" = "#D8C3A5")))

# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ----------------------------------------------------- TASK 2 Sensitivity analysis -------------------------------------------- #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #

model_1 <- lmerTest::lmer(Score_Execution ~ 1 + Group + Phase + Group : Phase + (1 + Group + Phase | ID), 
                          data = data_model_1, control = lmerControl('bobyqa'))
# `control = lmerControl('bobyqa)` = Control of Mixed Model Fitting

performance::check_model(model_1)

# ------------------------------------------------------------------------------------------------------------------------------ #

(model_results <- summary(model_1))
model_results$sigma
performance::performance(model_1)


model_results_coeff <- model_results$coefficients
round(model_results_coeff, digits = 5)

# extract random effects from the model
(model_results_random <- lme4::ranef(model_1))
cor(model_results_random$ID$GroupEE, model_results_random$ID$`(Intercept)`)

# extract random effects' covariance matrix from the model
model_results_random_matrix <- lme4::VarCorr(model_1)
model_results_random_matrix$ID


# ------------------------------------------------------------------------------------------------------------------------------ #

MuMIn::r.squaredGLMM(model_1)

# ------------------------------------------------------------------------------------------------------------------------------ #

table(data_model_1$Group)
table(data_model_1$Score_Execution)
ggplot2::ggplot(data = data_model_1, aes(x = Score_Execution)) + geom_density(fill = "darkred")


# ------------------------------------------------------------------------------------------------------------------------------ #

(post_hoc <- emmeans::emmeans(model_1, specs = pairwise ~ Group:Phase))

# ------------------------------------------------------------------------------------------------------------------------------ #


(VIF_model_1 <- car::vif(model_1))

(VIF_model_1_performance <- performance::check_collinearity(model_1, component = "all"))

# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# --------------------------- Scores Execution ~ 1 + Group + Phase + Group × Phase + (1 | ID Participant) ---------------------- #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #


# set fixed effect parameters
Score_beta_0 <- 0.356 # intercept; i.e., the grand mean of PPO

# ---------------------------------------------------------------------------------- #

Score_beta_EE <- -0.006 # slope; i.e, effect of category EE/Con 
Score_beta_PT <- -0.021 # slope; i.e, effect of phase PT
Score_beta_RT <- -0.092 # slope; i.e, effect of phase RT
Score_beta_EE_PT <- -0.060 # slope; i.e, effect of interaction EE PT
Score_beta_EE_RT <- -0.052 # slope; i.e, effect of interaction EE RT

# set random effect parameters
Score_tau_0 <- 0.0961 # by-subject random intercept sd
Score_omega_0 <- 0.0582 # by-item random intercept sd (EE and Con are different)

# set more random effect and error parameters
Score_tau_1 <- 0.075 # by-subject random slope sd
Score_rho <- -0.18 # correlation between random intercept and slope
Score_sigma <- 0.061 # residual (error) sd (twice the size of the by-subject random intercept SD)

# simulating the sampling process

# set number of subjects and items
Score_n_subj <- 36 # number of subjects

# ////////////////////////////////// #

Phase_BT <- 4 # number of phase BT 
Phase_PT <- 20 # number of phase PT 
Phase_RT <- 8 # number of phase RT 
Group_EE <- 576 # number of EE measurements
Group_Con <- 576 # number of Con measurements

# simulate a sample of items
# total number of items = Phase_BT + Phase_PT + Phase_RT
Score_items <- data.frame(
  Score_item_id = seq_len(Phase_BT + Phase_PT + Phase_RT),
  Phase = rep(c("BT", "PT", "RT"), c(Phase_BT, Phase_PT, Phase_RT)),
  Group = rep(c("EE", "Con"), c(Group_EE, Group_Con)),
  Score_O_0i = rnorm(Phase_BT + Phase_PT + Phase_RT, mean = 0, sd = Score_omega_0)
)


# effect-code category
Score_items$Score_X_i <- dplyr::recode(Score_items$Phase, "BT" = 0, "PT" = -0.021, "RT" = -0.052)

Score_items$Score_X_ii <- dplyr::recode(Score_items$Group, "EE" = -0.006, "Con" = 0)

# simulate a sample of subjects
# calculate random intercept / random slope covariance
Score_covar <- Score_rho * Score_tau_0 * Score_tau_1

# put values into variance-covariance matrix
Score_cov_mx  <- matrix(
  c(Score_tau_0^2, Score_covar,
    Score_covar, Score_tau_1^2),
  nrow = 2, byrow = TRUE)


# generate the by-subject random effects
Score_subject_rfx <- MASS::mvrnorm(n = Score_n_subj,
                                 mu = c(Score_T_0s = 0, Score_T_1s = 0),
                                 Sigma = Score_cov_mx)

# combine with subject IDs
Score_subjects <- data.frame(Score_subj_id = seq_len(Score_n_subj),
                             Score_subject_rfx)



# simulate a sample of subjects
# sample from a multivariate random distribution 
Score_subjects <- faux::rnorm_multi(
  n = Score_n_subj, 
  mu = 0, # means for random effects are always 0
  sd = c(Score_tau_0, Score_tau_1), # set SDs
  r = Score_rho, # set correlation, see ?faux::rnorm_multi
  varnames = c("Score_T_0s", "Score_T_1s")
)


# add subject IDs
Score_subjects$Score_subj_id <- seq_len(Score_n_subj)


# cross subject and item IDs; add an error term
# nrow(.) is the number of rows in the table
Score_trials <- crossing(Score_subjects, Score_items[, setdiff(names(Score_items), c("Group", "Score_X_ii"))]) %>%
  mutate(Score_e_si = rnorm(nrow(.), mean = 0, sd = Score_sigma),
         Group = Score_items$Group, 
         Score_X_ii = Score_items$Score_X_ii) %>%
  dplyr::select(Score_subj_id, Score_item_id, Phase, Group, Score_X_i, Score_X_ii, everything())


Score_trials <- Score_trials[order(Score_trials$Score_subj_id, decreasing = F),]


# calculate the response variable
dat_sim_Score <- Score_trials %>%
  mutate(Score = Score_beta_0 + Score_T_0s + Score_O_0i + 
           (Score_beta_EE + Score_beta_PT + Score_beta_RT + Score_beta_EE_PT + Score_beta_EE_RT + 
              Score_T_1s) * 
           Score_X_i + Score_X_ii + Score_e_si) %>%
  dplyr::select(Score_subj_id, Score_item_id, Phase, Group, Score_X_i, Score_X_ii, Score)

dat_sim_Score$Trial <- rep(c(1:4, 1:20, 1:8), length.out = nrow(dat_sim_Score))

View(dat_sim_Score)


data_simulated <- dat_sim_Score[, c(1, 4, 3, 8, 7)]

View(data_simulated)

colnames(data_simulated)[colnames(data_simulated) == "Score_subj_id"] ="ID"
colnames(data_simulated)[colnames(data_simulated) == "Score"] ="Score_Execution"

str(data_simulated)

# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# --------------------------- Checking equivalence of the data_simulated and the original data_model_1 ------------------------- #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #

model_1 <- lmerTest::lmer(Score_Execution ~ 1 + Group + Phase + Group : Phase + (1 | ID), data = data_model_1)

summary(model_1)

model_results <- summary(model_1)

model_results$coefficients

# ------------------------------------------------------------------------------------------------------------------------------ #

model_1_simulated <- lmerTest::lmer(Score_Execution ~ 1 + Group + Phase + Group : Phase + (1 | ID), data = data_simulated)

summary(model_1_simulated)

model_results_simulated <- summary(model_1_simulated)

model_results_simulated$coefficients

# ------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------ power analysis -------------------------------------------------- #
# ------------------------------------------------------------------------------------------------------------------------------ #

# ----------------------------------------------------- simulated data set function -------------------------------------------- #

# Run sensitivity for 20, 36, 40, 60, 80, 100, and 120 participants. The number of measurements (rows) should be N * 32,
# where N = number of participants, and 32 = number of trials in all three phases. Then, for the EE should be (N * 32) / 2, 
# and the same for Control group.


# ----------------------------------------------------------------- #
# ----------------------------- 20 -------------------------------- # 
# ----------------------------------------------------------------- #

# number of rows
(20*32) / 2

my_sim_data_20 <- function(
    n_subj = 20, # number of subjects
    n_BT = 4,  # number of BT
    n_PT = 20, # number of PT
    n_RT = 8, # number of RT 
    n_group_EE = 320, # number of EE measurements
    n_group_Con = 320, # number of Con measurements
    beta_0 = 0.356,
    beta_EE = -0.006, # slope; i.e, effect of category EE/Con 
    beta_PT = -0.021, # slope; i.e, effect of phase PT
    beta_RT = -0.092, # slope; i.e, effect of phase RT
    beta_EE_PT = -0.060, # slope; i.e, effect of interaction EE PT
    beta_EE_RT = -0.052, # slope; i.e, effect of interaction EE RT
    omega_0 = 0.0582, # by-item random intercept sd (EE and Con are different)
    tau_0 = 0.0961, # by-subject random intercept sd
    tau_1 = 0.075, # by-subject random slope sd
    rho = -0.18, # correlation between intercept and slope
    sigma = 0.061) { # residual (standard deviation)
  
  items <- data.frame(
    item_ID = seq_len(n_BT + n_PT + n_RT),
    Phase = rep(c("BT", "PT", "RT"), c(n_BT, n_PT, n_RT)),
    Group = rep(c("EE", "Con"), c(n_group_EE, n_group_Con)),
    X_i = rep(c(0, -0.021, -0.052), c(n_BT, n_PT, n_RT)),
    X_ii = rep(c(0, -0.006), c(n_group_Con, n_group_EE)),
    O_0i = rnorm(n = n_BT + n_PT + n_RT, mean = 0, sd = omega_0))
  
  # variance-covariance matrix
  cov_mx  <- matrix(
    c(tau_0^2, rho * tau_0 * tau_1, rho * tau_0 * tau_1, tau_1^2),
    nrow = 2, byrow = TRUE)
  
  subjects <- data.frame(subj_ID = seq_len(n_subj),
                         MASS::mvrnorm(n = n_subj,
                                       mu = c(T_0s = 0, T_1s = 0),
                                       Sigma = cov_mx))
  
  crossing(subjects, items[, setdiff(names(items), c("Group", "X_ii"))]) %>%
    mutate(e_si = rnorm(nrow(.), mean = 0, sd = sigma),
           Score = beta_0 + T_0s + O_0i + (beta_EE + beta_PT + beta_RT + beta_EE_PT + beta_EE_RT + T_1s) * X_i * items$X_ii + e_si,
           Group = items$Group, 
           X_ii = items$X_ii) %>%
    dplyr::select(subj_ID, item_ID, Phase, Group, Score, X_i, X_ii, everything())
}

my_sim_data_20()

# ----------------------------------------------------------------- #
# ----------------------------- 36 -------------------------------- # 
# ----------------------------------------------------------------- #

# number of rows
(36*32) / 2

my_sim_data_36 <- function(
    n_subj = 36, # number of subjects
    n_BT = 4,  # number of BT
    n_PT = 20, # number of PT
    n_RT = 8, # number of RT 
    n_group_EE = 576, # number of EE measurements
    n_group_Con = 576, # number of Con measurements
    beta_0 = 0.356,
    beta_EE = -0.006, # slope; i.e, effect of category EE/Con 
    beta_PT = -0.021, # slope; i.e, effect of phase PT
    beta_RT = -0.092, # slope; i.e, effect of phase RT
    beta_EE_PT = -0.060, # slope; i.e, effect of interaction EE PT
    beta_EE_RT = -0.052, # slope; i.e, effect of interaction EE RT
    omega_0 = 0.0582, # by-item random intercept sd (EE and Con are different)
    tau_0 = 0.0961, # by-subject random intercept sd
    tau_1 = 0.075, # by-subject random slope sd
    rho = -0.18, # correlation between intercept and slope
    sigma = 0.061) { # residual (standard deviation)
  
  items <- data.frame(
    item_ID = seq_len(n_BT + n_PT + n_RT),
    Phase = rep(c("BT", "PT", "RT"), c(n_BT, n_PT, n_RT)),
    Group = rep(c("EE", "Con"), c(n_group_EE, n_group_Con)),
    X_i = rep(c(0, -0.021, -0.052), c(n_BT, n_PT, n_RT)),
    X_ii = rep(c(0, -0.006), c(n_group_Con, n_group_EE)),
    O_0i = rnorm(n = n_BT + n_PT + n_RT, mean = 0, sd = omega_0))
  
  # variance-covariance matrix
  cov_mx  <- matrix(
    c(tau_0^2, rho * tau_0 * tau_1, rho * tau_0 * tau_1, tau_1^2),
    nrow = 2, byrow = TRUE)
  
  subjects <- data.frame(subj_ID = seq_len(n_subj),
                         MASS::mvrnorm(n = n_subj,
                                       mu = c(T_0s = 0, T_1s = 0),
                                       Sigma = cov_mx))
  
  crossing(subjects, items[, setdiff(names(items), c("Group", "X_ii"))]) %>%
    mutate(e_si = rnorm(nrow(.), mean = 0, sd = sigma),
           Score = beta_0 + T_0s + O_0i + (beta_EE + beta_PT + beta_RT + beta_EE_PT + beta_EE_RT + T_1s) * X_i * items$X_ii + e_si,
           Group = items$Group, 
           X_ii = items$X_ii) %>%
    dplyr::select(subj_ID, item_ID, Phase, Group, Score, X_i, X_ii, everything())
}

my_sim_data_36()


# ----------------------------------------------------------------- #
# ----------------------------- 40 -------------------------------- # 
# ----------------------------------------------------------------- #

# number of rows
(40*32) / 2

my_sim_data_40 <- function(
    n_subj = 40, # number of subjects
    n_BT = 4,  # number of BT
    n_PT = 20, # number of PT
    n_RT = 8, # number of RT 
    n_group_EE = 640, # number of EE measurements
    n_group_Con = 640, # number of Con measurements
    beta_0 = 0.356,
    beta_EE = -0.006, # slope; i.e, effect of category EE/Con 
    beta_PT = -0.021, # slope; i.e, effect of phase PT
    beta_RT = -0.092, # slope; i.e, effect of phase RT
    beta_EE_PT = -0.060, # slope; i.e, effect of interaction EE PT
    beta_EE_RT = -0.052, # slope; i.e, effect of interaction EE RT
    omega_0 = 0.0582, # by-item random intercept sd (EE and Con are different)
    tau_0 = 0.0961, # by-subject random intercept sd
    tau_1 = 0.075, # by-subject random slope sd
    rho = -0.18, # correlation between intercept and slope
    sigma = 0.061) { # residual (standard deviation)
  
  items <- data.frame(
    item_ID = seq_len(n_BT + n_PT + n_RT),
    Phase = rep(c("BT", "PT", "RT"), c(n_BT, n_PT, n_RT)),
    Group = rep(c("EE", "Con"), c(n_group_EE, n_group_Con)),
    X_i = rep(c(0, -0.021, -0.052), c(n_BT, n_PT, n_RT)),
    X_ii = rep(c(0, -0.006), c(n_group_Con, n_group_EE)),
    O_0i = rnorm(n = n_BT + n_PT + n_RT, mean = 0, sd = omega_0))
  
  # variance-covariance matrix
  cov_mx  <- matrix(
    c(tau_0^2, rho * tau_0 * tau_1, rho * tau_0 * tau_1, tau_1^2),
    nrow = 2, byrow = TRUE)
  
  subjects <- data.frame(subj_ID = seq_len(n_subj),
                         MASS::mvrnorm(n = n_subj,
                                       mu = c(T_0s = 0, T_1s = 0),
                                       Sigma = cov_mx))
  
  crossing(subjects, items[, setdiff(names(items), c("Group", "X_ii"))]) %>%
    mutate(e_si = rnorm(nrow(.), mean = 0, sd = sigma),
           Score = beta_0 + T_0s + O_0i + (beta_EE + beta_PT + beta_RT + beta_EE_PT + beta_EE_RT + T_1s) * X_i * items$X_ii + e_si,
           Group = items$Group, 
           X_ii = items$X_ii) %>%
    dplyr::select(subj_ID, item_ID, Phase, Group, Score, X_i, X_ii, everything())
}

my_sim_data_40()

# ----------------------------------------------------------------- #
# ----------------------------- 60 -------------------------------- # 
# ----------------------------------------------------------------- #

# number of rows
(60*32) / 2

my_sim_data_60 <- function(
    n_subj = 60, # number of subjects
    n_BT = 4,  # number of BT
    n_PT = 20, # number of PT
    n_RT = 8, # number of RT 
    n_group_EE = 960, # number of EE measurements
    n_group_Con = 960, # number of Con measurements
    beta_0 = 0.356,
    beta_EE = -0.006, # slope; i.e, effect of category EE/Con 
    beta_PT = -0.021, # slope; i.e, effect of phase PT
    beta_RT = -0.092, # slope; i.e, effect of phase RT
    beta_EE_PT = -0.060, # slope; i.e, effect of interaction EE PT
    beta_EE_RT = -0.052, # slope; i.e, effect of interaction EE RT
    omega_0 = 0.0582, # by-item random intercept sd (EE and Con are different)
    tau_0 = 0.0961, # by-subject random intercept sd
    tau_1 = 0.075, # by-subject random slope sd
    rho = -0.18, # correlation between intercept and slope
    sigma = 0.061) { # residual (standard deviation)
  
  items <- data.frame(
    item_ID = seq_len(n_BT + n_PT + n_RT),
    Phase = rep(c("BT", "PT", "RT"), c(n_BT, n_PT, n_RT)),
    Group = rep(c("EE", "Con"), c(n_group_EE, n_group_Con)),
    X_i = rep(c(0, -0.021, -0.052), c(n_BT, n_PT, n_RT)),
    X_ii = rep(c(0, -0.006), c(n_group_Con, n_group_EE)),
    O_0i = rnorm(n = n_BT + n_PT + n_RT, mean = 0, sd = omega_0))
  
  # variance-covariance matrix
  cov_mx  <- matrix(
    c(tau_0^2, rho * tau_0 * tau_1, rho * tau_0 * tau_1, tau_1^2),
    nrow = 2, byrow = TRUE)
  
  subjects <- data.frame(subj_ID = seq_len(n_subj),
                         MASS::mvrnorm(n = n_subj,
                                       mu = c(T_0s = 0, T_1s = 0),
                                       Sigma = cov_mx))
  
  crossing(subjects, items[, setdiff(names(items), c("Group", "X_ii"))]) %>%
    mutate(e_si = rnorm(nrow(.), mean = 0, sd = sigma),
           Score = beta_0 + T_0s + O_0i + (beta_EE + beta_PT + beta_RT + beta_EE_PT + beta_EE_RT + T_1s) * X_i * items$X_ii + e_si,
           Group = items$Group, 
           X_ii = items$X_ii) %>%
    dplyr::select(subj_ID, item_ID, Phase, Group, Score, X_i, X_ii, everything())
}

my_sim_data_60()

# ----------------------------------------------------------------- #
# ----------------------------- 80 -------------------------------- # 
# ----------------------------------------------------------------- #

# number of rows
(80*32) / 2

my_sim_data_80 <- function(
    n_subj = 80, # number of subjects
    n_BT = 4,  # number of BT
    n_PT = 20, # number of PT
    n_RT = 8, # number of RT 
    n_group_EE = 1280, # number of EE measurements
    n_group_Con = 1280, # number of Con measurements
    beta_0 = 0.356,
    beta_EE = -0.006, # slope; i.e, effect of category EE/Con 
    beta_PT = -0.021, # slope; i.e, effect of phase PT
    beta_RT = -0.092, # slope; i.e, effect of phase RT
    beta_EE_PT = -0.060, # slope; i.e, effect of interaction EE PT
    beta_EE_RT = -0.052, # slope; i.e, effect of interaction EE RT
    omega_0 = 0.0582, # by-item random intercept sd (EE and Con are different)
    tau_0 = 0.0961, # by-subject random intercept sd
    tau_1 = 0.075, # by-subject random slope sd
    rho = -0.18, # correlation between intercept and slope
    sigma = 0.061) { # residual (standard deviation)
  
  items <- data.frame(
    item_ID = seq_len(n_BT + n_PT + n_RT),
    Phase = rep(c("BT", "PT", "RT"), c(n_BT, n_PT, n_RT)),
    Group = rep(c("EE", "Con"), c(n_group_EE, n_group_Con)),
    X_i = rep(c(0, -0.021, -0.052), c(n_BT, n_PT, n_RT)),
    X_ii = rep(c(0, -0.006), c(n_group_Con, n_group_EE)),
    O_0i = rnorm(n = n_BT + n_PT + n_RT, mean = 0, sd = omega_0))
  
  # variance-covariance matrix
  cov_mx  <- matrix(
    c(tau_0^2, rho * tau_0 * tau_1, rho * tau_0 * tau_1, tau_1^2),
    nrow = 2, byrow = TRUE)
  
  subjects <- data.frame(subj_ID = seq_len(n_subj),
                         MASS::mvrnorm(n = n_subj,
                                       mu = c(T_0s = 0, T_1s = 0),
                                       Sigma = cov_mx))
  
  crossing(subjects, items[, setdiff(names(items), c("Group", "X_ii"))]) %>%
    mutate(e_si = rnorm(nrow(.), mean = 0, sd = sigma),
           Score = beta_0 + T_0s + O_0i + (beta_EE + beta_PT + beta_RT + beta_EE_PT + beta_EE_RT + T_1s) * X_i * items$X_ii + e_si,
           Group = items$Group, 
           X_ii = items$X_ii) %>%
    dplyr::select(subj_ID, item_ID, Phase, Group, Score, X_i, X_ii, everything())
}

my_sim_data_80()

# ----------------------------------------------------------------- #
# ----------------------------- 100 ------------------------------- # 
# ----------------------------------------------------------------- #

# number of rows
(100*32) / 2

my_sim_data_100 <- function(
    n_subj = 100, # number of subjects
    n_BT = 4,  # number of BT
    n_PT = 20, # number of PT
    n_RT = 8, # number of RT 
    n_group_EE = 1600, # number of EE measurements
    n_group_Con = 1600, # number of Con measurements
    beta_0 = 0.356,
    beta_EE = -0.006, # slope; i.e, effect of category EE/Con 
    beta_PT = -0.021, # slope; i.e, effect of phase PT
    beta_RT = -0.092, # slope; i.e, effect of phase RT
    beta_EE_PT = -0.060, # slope; i.e, effect of interaction EE PT
    beta_EE_RT = -0.052, # slope; i.e, effect of interaction EE RT
    omega_0 = 0.0582, # by-item random intercept sd (EE and Con are different)
    tau_0 = 0.0961, # by-subject random intercept sd
    tau_1 = 0.075, # by-subject random slope sd
    rho = -0.18, # correlation between intercept and slope
    sigma = 0.061) { # residual (standard deviation)
  
  items <- data.frame(
    item_ID = seq_len(n_BT + n_PT + n_RT),
    Phase = rep(c("BT", "PT", "RT"), c(n_BT, n_PT, n_RT)),
    Group = rep(c("EE", "Con"), c(n_group_EE, n_group_Con)),
    X_i = rep(c(0, -0.021, -0.052), c(n_BT, n_PT, n_RT)),
    X_ii = rep(c(0, -0.006), c(n_group_Con, n_group_EE)),
    O_0i = rnorm(n = n_BT + n_PT + n_RT, mean = 0, sd = omega_0))
  
  # variance-covariance matrix
  cov_mx  <- matrix(
    c(tau_0^2, rho * tau_0 * tau_1, rho * tau_0 * tau_1, tau_1^2),
    nrow = 2, byrow = TRUE)
  
  subjects <- data.frame(subj_ID = seq_len(n_subj),
                         MASS::mvrnorm(n = n_subj,
                                       mu = c(T_0s = 0, T_1s = 0),
                                       Sigma = cov_mx))
  
  crossing(subjects, items[, setdiff(names(items), c("Group", "X_ii"))]) %>%
    mutate(e_si = rnorm(nrow(.), mean = 0, sd = sigma),
           Score = beta_0 + T_0s + O_0i + (beta_EE + beta_PT + beta_RT + beta_EE_PT + beta_EE_RT + T_1s) * X_i * items$X_ii + e_si,
           Group = items$Group, 
           X_ii = items$X_ii) %>%
    dplyr::select(subj_ID, item_ID, Phase, Group, Score, X_i, X_ii, everything())
}

my_sim_data_100()

# ----------------------------------------------------------------- #
# ----------------------------- 120 ------------------------------- # 
# ----------------------------------------------------------------- #

# number of rows
(120*32) / 2

my_sim_data_120 <- function(
    n_subj = 120, # number of subjects
    n_BT = 4,  # number of BT
    n_PT = 20, # number of PT
    n_RT = 8, # number of RT 
    n_group_EE = 1920, # number of EE measurements
    n_group_Con = 1920, # number of Con measurements
    beta_0 = 0.356,
    beta_EE = -0.006, # slope; i.e, effect of category EE/Con 
    beta_PT = -0.021, # slope; i.e, effect of phase PT
    beta_RT = -0.092, # slope; i.e, effect of phase RT
    beta_EE_PT = -0.060, # slope; i.e, effect of interaction EE PT
    beta_EE_RT = -0.052, # slope; i.e, effect of interaction EE RT
    omega_0 = 0.0582, # by-item random intercept sd (EE and Con are different)
    tau_0 = 0.0961, # by-subject random intercept sd
    tau_1 = 0.075, # by-subject random slope sd
    rho = -0.18, # correlation between intercept and slope
    sigma = 0.061) { # residual (standard deviation)
  
  items <- data.frame(
    item_ID = seq_len(n_BT + n_PT + n_RT),
    Phase = rep(c("BT", "PT", "RT"), c(n_BT, n_PT, n_RT)),
    Group = rep(c("EE", "Con"), c(n_group_EE, n_group_Con)),
    X_i = rep(c(0, -0.021, -0.052), c(n_BT, n_PT, n_RT)),
    X_ii = rep(c(0, -0.006), c(n_group_Con, n_group_EE)),
    O_0i = rnorm(n = n_BT + n_PT + n_RT, mean = 0, sd = omega_0))
  
  # variance-covariance matrix
  cov_mx  <- matrix(
    c(tau_0^2, rho * tau_0 * tau_1, rho * tau_0 * tau_1, tau_1^2),
    nrow = 2, byrow = TRUE)
  
  subjects <- data.frame(subj_ID = seq_len(n_subj),
                         MASS::mvrnorm(n = n_subj,
                                       mu = c(T_0s = 0, T_1s = 0),
                                       Sigma = cov_mx))
  
  crossing(subjects, items[, setdiff(names(items), c("Group", "X_ii"))]) %>%
    mutate(e_si = rnorm(nrow(.), mean = 0, sd = sigma),
           Score = beta_0 + T_0s + O_0i + (beta_EE + beta_PT + beta_RT + beta_EE_PT + beta_EE_RT + T_1s) * X_i * items$X_ii + e_si,
           Group = items$Group, 
           X_ii = items$X_ii) %>%
    dplyr::select(subj_ID, item_ID, Phase, Group, Score, X_i, X_ii, everything())
}

my_sim_data_120()

# ----------------------------------------------------------------- #
# ----------------------------------------------------------------- # 
# ----------------------------------------------------------------- #

# ------------------------------------------------- single run function of data set -------------------------------------------- #

# ----------------------------------------------------------------- #
# ------------------------------ 20 ------------------------------- # 
# ----------------------------------------------------------------- #

single_run_model_1_20 <- function(...) {
  dat_sim <- my_sim_data_20(...)
  mod_sim <- lmerTest::lmer(Score ~ 1 + Group + Phase + Group : Phase + (1 | subj_ID), data = dat_sim)
  table <- broom.mixed::tidy(mod_sim, effects = "fixed", conf.int = T)
  table <- as.data.frame(table)
  table <- table %>%
    dplyr::mutate(CI_includes_zero = ifelse(conf.low <= 0 & conf.high >= 0, 0, 1))
  table <- table %>%
    dplyr::mutate(CI_includes_zero_B = ifelse(conf.low <= 0 & conf.high >= 0, "Yes", "Not"))
  print(table)
}

single_run_model_1_20()

# ----------------------------------------------------------------- #
# ------------------------------ 36 ------------------------------- # 
# ----------------------------------------------------------------- #

single_run_model_1_36 <- function(...) {
  dat_sim <- my_sim_data_36(...)
  mod_sim <- lmerTest::lmer(Score ~ 1 + Group + Phase + Group : Phase + (1 | subj_ID), data = dat_sim)
  table <- broom.mixed::tidy(mod_sim, effects = "fixed", conf.int = T)
  table <- as.data.frame(table)
  table <- table %>%
    dplyr::mutate(CI_includes_zero = ifelse(conf.low <= 0 & conf.high >= 0, 0, 1))
  table <- table %>%
    dplyr::mutate(CI_includes_zero_B = ifelse(conf.low <= 0 & conf.high >= 0, "Yes", "Not"))
  print(table)
}

single_run_model_1_36()

# ----------------------------------------------------------------- #
# ------------------------------ 40 ------------------------------- # 
# ----------------------------------------------------------------- #

single_run_model_1_40 <- function(...) {
  dat_sim <- my_sim_data_40(...)
  mod_sim <- lmerTest::lmer(Score ~ 1 + Group + Phase + Group : Phase + (1 | subj_ID), data = dat_sim)
  table <- broom.mixed::tidy(mod_sim, effects = "fixed", conf.int = T)
  table <- as.data.frame(table)
  table <- table %>%
    dplyr::mutate(CI_includes_zero = ifelse(conf.low <= 0 & conf.high >= 0, 0, 1))
  table <- table %>%
    dplyr::mutate(CI_includes_zero_B = ifelse(conf.low <= 0 & conf.high >= 0, "Yes", "Not"))
  print(table)
}

single_run_model_1_40()

# ----------------------------------------------------------------- #
# ------------------------------ 60 ------------------------------- # 
# ----------------------------------------------------------------- #

single_run_model_1_60 <- function(...) {
  dat_sim <- my_sim_data_60(...)
  mod_sim <- lmerTest::lmer(Score ~ 1 + Group + Phase + Group : Phase + (1 | subj_ID), data = dat_sim)
  table <- broom.mixed::tidy(mod_sim, effects = "fixed", conf.int = T)
  table <- as.data.frame(table)
  table <- table %>%
    dplyr::mutate(CI_includes_zero = ifelse(conf.low <= 0 & conf.high >= 0, 0, 1))
  table <- table %>%
    dplyr::mutate(CI_includes_zero_B = ifelse(conf.low <= 0 & conf.high >= 0, "Yes", "Not"))
  print(table)
}

single_run_model_1_60()

# ----------------------------------------------------------------- #
# ------------------------------ 80 ------------------------------- # 
# ----------------------------------------------------------------- #

single_run_model_1_80 <- function(...) {
  dat_sim <- my_sim_data_80(...)
  mod_sim <- lmerTest::lmer(Score ~ 1 + Group + Phase + Group : Phase + (1 | subj_ID), data = dat_sim)
  table <- broom.mixed::tidy(mod_sim, effects = "fixed", conf.int = T)
  table <- as.data.frame(table)
  table <- table %>%
    dplyr::mutate(CI_includes_zero = ifelse(conf.low <= 0 & conf.high >= 0, 0, 1))
  table <- table %>%
    dplyr::mutate(CI_includes_zero_B = ifelse(conf.low <= 0 & conf.high >= 0, "Yes", "Not"))
  print(table)
}

single_run_model_1_80()

# ----------------------------------------------------------------- #
# ----------------------------- 100 ------------------------------- # 
# ----------------------------------------------------------------- #

single_run_model_1_100 <- function(...) {
  dat_sim <- my_sim_data_100(...)
  mod_sim <- lmerTest::lmer(Score ~ 1 + Group + Phase + Group : Phase + (1 | subj_ID), data = dat_sim)
  table <- broom.mixed::tidy(mod_sim, effects = "fixed", conf.int = T)
  table <- as.data.frame(table)
  table <- table %>%
    dplyr::mutate(CI_includes_zero = ifelse(conf.low <= 0 & conf.high >= 0, 0, 1))
  table <- table %>%
    dplyr::mutate(CI_includes_zero_B = ifelse(conf.low <= 0 & conf.high >= 0, "Yes", "Not"))
  print(table)
}

single_run_model_1_100()

# ----------------------------------------------------------------- #
# ----------------------------- 120 ------------------------------- # 
# ----------------------------------------------------------------- #

single_run_model_1_120 <- function(...) {
  dat_sim <- my_sim_data_120(...)
  mod_sim <- lmerTest::lmer(Score ~ 1 + Group + Phase + Group : Phase + (1 | subj_ID), data = dat_sim)
  table <- broom.mixed::tidy(mod_sim, effects = "fixed", conf.int = T)
  table <- as.data.frame(table)
  table <- table %>%
    dplyr::mutate(CI_includes_zero = ifelse(conf.low <= 0 & conf.high >= 0, 0, 1))
  table <- table %>%
    dplyr::mutate(CI_includes_zero_B = ifelse(conf.low <= 0 & conf.high >= 0, "Yes", "Not"))
  print(table)
}

single_run_model_1_120()

# ------------------------------------------------------- power analysis simulation -------------------------------------------- #

# ----------------------------------------------------------------- #
# ---------------------- number of simulations -------------------- #
# ----------------------------------------------------------------- #

n_sims <- 5000

# ----------------------------------------------------------------- #
# ----------------------------- power ----------------------------- #
# ----------------------------------------------------------------- #

simulations_model_1_20 <- purrr::map_df(1:n_sims, ~ single_run_model_1_20())
simulations_model_1_36 <- purrr::map_df(1:n_sims, ~ single_run_model_1_36())
simulations_model_1_40 <- purrr::map_df(1:n_sims, ~ single_run_model_1_40())
simulations_model_1_60 <- purrr::map_df(1:n_sims, ~ single_run_model_1_60())
simulations_model_1_80 <- purrr::map_df(1:n_sims, ~ single_run_model_1_80())
simulations_model_1_100 <- purrr::map_df(1:n_sims, ~ single_run_model_1_100())
simulations_model_1_120 <- purrr::map_df(1:n_sims, ~ single_run_model_1_120())


readr::write_csv(simulations_model_1_20, "E:/data/Statistics/Data/simulations_model_1_20.csv")
readr::write_csv(simulations_model_1_36, "E:/data/Statistics/Data/simulations_model_1_36.csv")
readr::write_csv(simulations_model_1_40, "E:/data/Statistics/Data/simulations_model_1_40.csv")
readr::write_csv(simulations_model_1_60, "E:/data/Statistics/Data/simulations_model_1_60.csv")
readr::write_csv(simulations_model_1_80, "E:/data/Statistics/Data/simulations_model_1_80.csv")
readr::write_csv(simulations_model_1_100, "E:/data/Statistics/Data/simulations_model_1_100.csv")
readr::write_csv(simulations_model_1_120, "E:/data/Statistics/Data/simulations_model_1_120.csv")


simulations_model_1_20 <- readr::read_csv("E:/data/Statistics/Data/simulations_model_1_20.csv")
simulations_model_1_36 <- readr::read_csv("E:/data/Statistics/Data/simulations_model_1_36.csv")
simulations_model_1_40 <- readr::read_csv("E:/data/Statistics/Data/simulations_model_1_40.csv")
simulations_model_1_60 <- readr::read_csv("E:/data/Statistics/Data/simulations_model_1_60.csv")
simulations_model_1_80 <- readr::read_csv("E:/data/Statistics/Data/simulations_model_1_80.csv")
simulations_model_1_100 <- readr::read_csv("E:/data/Statistics/Data/simulations_model_1_100.csv")
simulations_model_1_120 <- readr::read_csv("E:/data/Statistics/Data/simulations_model_1_120.csv")


simulations_model_1_20_results <- simulations_model_1_20 %>% 
  filter(effect == "fixed") %>%
  group_by(term) %>%
  summarize(
    mean_estimate = mean(estimate),
    mean_se = mean(std.error),
    power = mean(CI_includes_zero > 0),
    discernible_models = sum(CI_includes_zero > 0),
    power_B = mean(CI_includes_zero_B == "Not") * 100
  )

simulations_model_1_36_results <- simulations_model_1_36 %>% 
  filter(effect == "fixed") %>%
  group_by(term) %>%
  summarize(
    mean_estimate = mean(estimate),
    mean_se = mean(std.error),
    power = mean(CI_includes_zero > 0),
    discernible_models = sum(CI_includes_zero > 0),
    power_B = mean(CI_includes_zero_B == "Not") * 100
  )

simulations_model_1_40_results <- simulations_model_1_40 %>% 
  filter(effect == "fixed") %>%
  group_by(term) %>%
  summarize(
    mean_estimate = mean(estimate),
    mean_se = mean(std.error),
    power = mean(CI_includes_zero > 0),
    discernible_models = sum(CI_includes_zero > 0),
    power_B = mean(CI_includes_zero_B == "Not") * 100
  )

simulations_model_1_60_results <- simulations_model_1_60 %>% 
  filter(effect == "fixed") %>%
  group_by(term) %>%
  summarize(
    mean_estimate = mean(estimate),
    mean_se = mean(std.error),
    power = mean(CI_includes_zero > 0),
    discernible_models = sum(CI_includes_zero > 0),
    power_B = mean(CI_includes_zero_B == "Not") * 100
  )

simulations_model_1_80_results <- simulations_model_1_80 %>% 
  filter(effect == "fixed") %>%
  group_by(term) %>%
  summarize(
    mean_estimate = mean(estimate),
    mean_se = mean(std.error),
    power = mean(CI_includes_zero > 0),
    discernible_models = sum(CI_includes_zero > 0),
    power_B = mean(CI_includes_zero_B == "Not") * 100
  )

simulations_model_1_100_results <- simulations_model_1_100 %>% 
  filter(effect == "fixed") %>%
  group_by(term) %>%
  summarize(
    mean_estimate = mean(estimate),
    mean_se = mean(std.error),
    power = mean(CI_includes_zero > 0),
    discernible_models = sum(CI_includes_zero > 0),
    power_B = mean(CI_includes_zero_B == "Not") * 100
  )

simulations_model_1_120_results <- simulations_model_1_120 %>% 
  filter(effect == "fixed") %>%
  group_by(term) %>%
  summarize(
    mean_estimate = mean(estimate),
    mean_se = mean(std.error),
    power = mean(CI_includes_zero > 0),
    discernible_models = sum(CI_includes_zero > 0),
    power_B = mean(CI_includes_zero_B == "Not") * 100
  )

# ----------------------------------------------------- 95% CIs of all powers ---------------------------------------------------- #

(CI_20_group <- binom.test(simulations_model_1_20_results$discernible_models[2], n_sims)$conf.int)
(CI_20_group_PhasePT <- binom.test(simulations_model_1_20_results$discernible_models[3], n_sims)$conf.int)
(CI_20_group_PhaseRT <- binom.test(simulations_model_1_20_results$discernible_models[4], n_sims)$conf.int)
(CI_20_PhasePT <- binom.test(simulations_model_1_20_results$discernible_models[5], n_sims)$conf.int)
(CI_20_PhaseRT <- binom.test(simulations_model_1_20_results$discernible_models[6], n_sims)$conf.int)

(CI_36_group <- binom.test(simulations_model_1_36_results$discernible_models[2], n_sims)$conf.int)
(CI_36_group_PhasePT <- binom.test(simulations_model_1_36_results$discernible_models[3], n_sims)$conf.int)
(CI_36_group_PhaseRT <- binom.test(simulations_model_1_36_results$discernible_models[4], n_sims)$conf.int)
(CI_36_PhasePT <- binom.test(simulations_model_1_36_results$discernible_models[5], n_sims)$conf.int)
(CI_36_PhaseRT <- binom.test(simulations_model_1_36_results$discernible_models[6], n_sims)$conf.int)

(CI_40_group <- binom.test(simulations_model_1_40_results$discernible_models[2], n_sims)$conf.int)
(CI_40_group_PhasePT <- binom.test(simulations_model_1_40_results$discernible_models[3], n_sims)$conf.int)
(CI_40_group_PhaseRT <- binom.test(simulations_model_1_40_results$discernible_models[4], n_sims)$conf.int)
(CI_40_PhasePT <- binom.test(simulations_model_1_40_results$discernible_models[5], n_sims)$conf.int)
(CI_40_PhaseRT <- binom.test(simulations_model_1_40_results$discernible_models[6], n_sims)$conf.int)

(CI_60_group <- binom.test(simulations_model_1_60_results$discernible_models[2], n_sims)$conf.int)
(CI_60_group_PhasePT <- binom.test(simulations_model_1_60_results$discernible_models[3], n_sims)$conf.int)
(CI_60_group_PhaseRT <- binom.test(simulations_model_1_60_results$discernible_models[4], n_sims)$conf.int)
(CI_60_PhasePT <- binom.test(simulations_model_1_60_results$discernible_models[5], n_sims)$conf.int)
(CI_60_PhaseRT <- binom.test(simulations_model_1_60_results$discernible_models[6], n_sims)$conf.int)

(CI_80_group <- binom.test(simulations_model_1_80_results$discernible_models[2], n_sims)$conf.int)
(CI_80_group_PhasePT <- binom.test(simulations_model_1_80_results$discernible_models[3], n_sims)$conf.int)
(CI_80_group_PhaseRT <- binom.test(simulations_model_1_80_results$discernible_models[4], n_sims)$conf.int)
(CI_80_PhasePT <- binom.test(simulations_model_1_80_results$discernible_models[5], n_sims)$conf.int)
(CI_80_PhaseRT <- binom.test(simulations_model_1_80_results$discernible_models[6], n_sims)$conf.int)

(CI_100_group <- binom.test(simulations_model_1_100_results$discernible_models[2], n_sims)$conf.int)
(CI_100_group_PhasePT <- binom.test(simulations_model_1_100_results$discernible_models[3], n_sims)$conf.int)
(CI_100_group_PhaseRT <- binom.test(simulations_model_1_100_results$discernible_models[4], n_sims)$conf.int)
(CI_100_PhasePT <- binom.test(simulations_model_1_100_results$discernible_models[5], n_sims)$conf.int)
(CI_100_PhaseRT <- binom.test(simulations_model_1_100_results$discernible_models[6], n_sims)$conf.int)

(CI_120_group <- binom.test(simulations_model_1_120_results$discernible_models[2], n_sims)$conf.int)
(CI_120_group_PhasePT <- binom.test(simulations_model_1_120_results$discernible_models[3], n_sims)$conf.int)
(CI_120_group_PhaseRT <- binom.test(simulations_model_1_120_results$discernible_models[4], n_sims)$conf.int)
(CI_120_PhasePT <- binom.test(simulations_model_1_120_results$discernible_models[5], n_sims)$conf.int)
(CI_120_PhaseRT <- binom.test(simulations_model_1_120_results$discernible_models[6], n_sims)$conf.int)

CI_all <- rbind(CI_20_group, CI_20_PhasePT, CI_20_PhaseRT, CI_20_group_PhasePT, CI_20_group_PhaseRT,
                CI_36_group, CI_36_PhasePT, CI_36_PhaseRT, CI_36_group_PhasePT, CI_36_group_PhaseRT,
                CI_40_group, CI_40_PhasePT, CI_40_PhaseRT, CI_40_group_PhasePT, CI_40_group_PhaseRT,
                CI_60_group, CI_60_PhasePT, CI_60_PhaseRT, CI_60_group_PhasePT, CI_60_group_PhaseRT,
                CI_80_group, CI_80_PhasePT, CI_80_PhaseRT, CI_80_group_PhasePT, CI_80_group_PhaseRT,
                CI_100_group, CI_100_PhasePT, CI_100_PhaseRT, CI_100_group_PhasePT, CI_100_group_PhaseRT,
                CI_120_group, CI_120_PhasePT, CI_120_PhaseRT, CI_120_group_PhasePT, CI_120_group_PhaseRT)
print(CI_all)

# ----------------------------------------------------- Extracting power values -------------------------------------------------- #

(power_20_group <- simulations_model_1_20_results$power[2])
(power_20_group_PhasePT <- simulations_model_1_20_results$power[3])
(power_20_group_PhaseRT <- simulations_model_1_20_results$power[4])
(power_20_PhasePT <- simulations_model_1_20_results$power[5])
(power_20_PhaseRT <- simulations_model_1_20_results$power[6])

(power_36_group <- simulations_model_1_36_results$power[2])
(power_36_group_PhasePT <- simulations_model_1_36_results$power[3])
(power_36_group_PhaseRT <- simulations_model_1_36_results$power[4])
(power_36_PhasePT <- simulations_model_1_36_results$power[5])
(power_36_PhaseRT <- simulations_model_1_36_results$power[6])

(power_40_group <- simulations_model_1_40_results$power[2])
(power_40_group_PhasePT <- simulations_model_1_40_results$power[3])
(power_40_group_PhaseRT <- simulations_model_1_40_results$power[4])
(power_40_PhasePT <- simulations_model_1_40_results$power[5])
(power_40_PhaseRT <- simulations_model_1_40_results$power[6])

(power_60_group <- simulations_model_1_60_results$power[2])
(power_60_group_PhasePT <- simulations_model_1_60_results$power[3])
(power_60_group_PhaseRT <- simulations_model_1_60_results$power[4])
(power_60_PhasePT <- simulations_model_1_60_results$power[5])
(power_60_PhaseRT <- simulations_model_1_60_results$power[6])

(power_80_group <- simulations_model_1_80_results$power[2])
(power_80_group_PhasePT <- simulations_model_1_80_results$power[3])
(power_80_group_PhaseRT <- simulations_model_1_80_results$power[4])
(power_80_PhasePT <- simulations_model_1_80_results$power[5])
(power_80_PhaseRT <- simulations_model_1_80_results$power[6])

(power_100_group <- simulations_model_1_100_results$power[2])
(power_100_group_PhasePT <- simulations_model_1_100_results$power[3])
(power_100_group_PhaseRT <- simulations_model_1_100_results$power[4])
(power_100_PhasePT <- simulations_model_1_100_results$power[5])
(power_100_PhaseRT <- simulations_model_1_100_results$power[6])

(power_120_group <- simulations_model_1_120_results$power[2])
(power_120_group_PhasePT <- simulations_model_1_120_results$power[3])
(power_120_group_PhaseRT <- simulations_model_1_120_results$power[4])
(power_120_PhasePT <- simulations_model_1_120_results$power[5])
(power_120_PhaseRT <- simulations_model_1_120_results$power[6])

power_all <- rbind(power_20_group, power_20_PhasePT, power_20_PhaseRT, power_20_group_PhasePT, power_20_group_PhaseRT,
                   power_36_group, power_36_PhasePT, power_36_PhaseRT, power_36_group_PhasePT, power_36_group_PhaseRT,
                   power_40_group, power_40_PhasePT, power_40_PhaseRT, power_40_group_PhasePT, power_40_group_PhaseRT,
                   power_60_group, power_60_PhasePT, power_60_PhaseRT, power_60_group_PhasePT, power_60_group_PhaseRT,
                   power_80_group, power_80_PhasePT, power_80_PhaseRT, power_80_group_PhasePT, power_80_group_PhaseRT,
                   power_100_group, power_100_PhasePT, power_100_PhaseRT, power_100_group_PhasePT, power_100_group_PhaseRT,
                   power_120_group, power_120_PhasePT, power_120_PhaseRT, power_120_group_PhasePT, power_120_group_PhaseRT)
print(power_all)

power_and_CI <- cbind(power_all, CI_all)
print(power_and_CI)

power_and_CI <- as.data.frame(power_and_CI)

power_and_CI$Variable <- rownames(power_and_CI)
print(power_and_CI)

power_and_CI <- power_and_CI[,c(4, 1:3)]

power_and_CI <- power_and_CI %>%
  rename("power" = "V1",
         "CI_lower" = "V2",
         "CI_upper" = "V3")

print(power_and_CI)

power_and_CI_wider <- power_and_CI %>%
  extract(Variable, into = c("Sample_size", "Predictor"), regex = ".*_(\\d+)_(.*)")
str(power_and_CI_wider)
print(power_and_CI_wider)

print(power_and_CI_wider[power_and_CI_wider$Predictor == "group",
                         c("power", "CI_lower", "CI_upper")])

print(power_and_CI_wider[power_and_CI_wider$Predictor == "group_PhasePT",
                         c("power", "CI_lower", "CI_upper")])

print(power_and_CI_wider[power_and_CI_wider$Predictor == "group_PhaseRT",
                         c("power", "CI_lower", "CI_upper")])

print(power_and_CI_wider[power_and_CI_wider$Predictor == "PhasePT",
                         c("power", "CI_lower", "CI_upper")])

print(power_and_CI_wider[power_and_CI_wider$Predictor == "PhaseRT",
                         c("power", "CI_lower", "CI_upper")])


(plot_2 <- ggplot2::ggplot(data = power_and_CI_wider, 
                           aes(x = Sample_size, 
                               y = power, 
                               color = Predictor, 
                               group = Predictor)) + 
    geom_line(linewidth = 0.4) +
    geom_hline(
      yintercept = c(0, 0.02, 0.04, 0.06, 0,4, 0.5, 0.6, 0.7, 0.8),      
      color = "gray90",
      linewidth = 0.2
    ) +
    geom_segment(
      aes(x = 36, xend = 36, y = 0.00, yend = 0.80),
      linetype = 1,
      color = "gray50", linewidth = 0.4
    ) +
    geom_point(size = 0.8) +
    geom_line(linewidth = 0.4) +
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.9) + 
    labs(
      title = "",
      x = "Sample size",
      y = "Power",
      color = ""
    ) +
    scale_color_manual(
      breaks = c(
        "group",
        "PhasePT",
        "PhaseRT",
        "group_PhasePT",
        "group_PhaseRT"
      ),
      labels = c(
        "group" = "Group",
        "PhasePT" = "Phase PT",
        "PhaseRT" = "Phase RT",
        "group_PhasePT" = "Group x Phase PT",
        "group_PhaseRT" = "Group x Phase RT"
      ), 
      values = c(
        "group" = "#A053A1", 
        "PhasePT" = "#DB778F",
        "PhaseRT" = "#09A39A",
        "group_PhasePT" = "#E69F52",
        "group_PhaseRT" = "#5869C7"
      ),
    ) +  
    theme_classic(base_size = 13) +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(margin = margin(t = 15) , hjust = 0.34),
      axis.title.y = element_text(margin = margin(r = 15)),
      axis.text.x = element_text(size = 7),
      axis.text.y = element_text(size = 7),
      axis.ticks = element_blank(),
      axis.text.y.right  = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.line.y.right  = element_blank()
    ) +
    scale_y_break(
      breaks = c(0.06, 0.35),
      scales = c(1, 1)
    ) +
    scale_y_continuous(limits = c(0.0, 0.8), expand = c(0, 0)) +
    scale_x_continuous(limits = c(18, 120), breaks = c(20, 36, 40, 60, 80, 100, 120)) +
    
    geom_hline(
      yintercept = 0.06,
      linetype = "dashed",
      linewidth = 0.5) +
    geom_hline(
      yintercept = 0.35,
      linetype = "dashed",
      linewidth = 0.5))

# ------------------------------------------------------------------------------------------------------------------------------ #
# ---------------------------------------------------------- Sensitivity table ------------------------------------------------- #
# ------------------------------------------------------------------------------------------------------------------------------ #

power_and_CI_wider$`95%CI` <- paste0("[", 
                                     round(power_and_CI_wider$CI_lower, 
                                           digits = 3),
                                     ", ", 
                                     round(power_and_CI_wider$CI_upper, 
                                           digits = 3), 
                                     "]")

power_and_CI_wider <- power_and_CI_wider[, c(2,1,3,6)]

print(power_and_CI_wider)

power_and_CI_wider <- power_and_CI_wider %>%
  rename("Power" = "power",
         "N" = "Sample_size")

print(power_and_CI_wider)

power_and_CI_wider <- rempsyc::nice_table(power_and_CI_wider)

print(power_and_CI_wider)

class(power_and_CI_wider)

doc <- read_docx() |> 
  body_add_flextable(value = power_and_CI_wider) |> 
  body_add_par("", style = "Normal")

# Save the document
setwd("C:/Users/malir/Desktop/UNCE/")
print(doc, target = "power_and_CI_wider.docx")

# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ----------------------------------------------------- TASK 3 Figure of Model 1 ----------------------------------------------- #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #

data <- data %>%
  mutate(Group = factor(Group, levels = c("EE", "Con")))

# ------------------------------------------------------------------------------------------------------------------------------ #

# checking results in accordance with model
model_1 <- lmerTest::lmer(Score_Execution ~ 1 + Group + Phase + Group * Phase + (1 | ID), data = data_model_1)

summary(model_1)

emmeans::emmeans(model_1, specs = pairwise ~ Group * Phase)



# ------------------------------------------------------------------------------------------------------------------------------ #

summary_data_score <- data %>%
  group_by(Group, Phase) %>%
  summarise(mean_Score_Execution = mean(Score_Execution, na.rm = TRUE),
            sterr = sd(Score_Execution, na.rm = TRUE) / sqrt(n()),
            lower_ci = mean_Score_Execution - qt(0.975, df = n() - 1) * sterr,
            upper_ci = mean_Score_Execution + qt(0.975, df = n() - 1) * sterr)

# make a mean values of the trials for each participant

data_mean_phases <- data %>%
  group_by(ID, Phase, Group) %>% # group by participant and phase
  summarize(mean_execution = mean(Score_Execution, na.rm = TRUE)) %>% 
  ungroup()

# -------------------------------------- #

str(summary_data_score)
print(summary_data_score)

str(summary_data_score)

# Create the plot
(model_3_plot <- ggplot() +
    
    # 1. Individual participant data: gray points and connecting lines
    geom_line(
      data = data_mean_phases,
      aes(x = Phase, y = mean_execution, group = ID, color = Group),
      alpha = 0.2,
      linewidth = 0.3,
      position = position_dodge(width = 0.4)
    ) + # Connect time points per subject, colored by state
    
    geom_point(data = data_mean_phases, 
               aes(x = Phase, y = mean_execution, group = ID, color = Group), 
               size = 0.5, alpha = 0.3, position = position_dodge(width = 0.4)) +  # Individual data points
    
    # 2. Mean values with 95% confidence intervals
    geom_point(data = summary_data_score, 
               aes(x = Phase, y = mean_Score_Execution, color = Group), 
               size = 2, shape = 16, position = position_dodge(width = 0.4)) +  # Mean points
    geom_errorbar(data = summary_data_score, 
                  aes(x = Phase, ymin = lower_ci, ymax = upper_ci, color = Group), 
                  width = 0.2, linewidth = 0.7, position = position_dodge(width = 0.4)) +  # 95% CI error bars
    geom_line(data = summary_data_score, 
              aes(x = Phase, y = mean_Score_Execution, group = Group, color = Group), 
              linewidth = 1.0, position = position_dodge(width = 0.4)) +  # Line connecting means per state
    
    # Labels and theme
    xlab("Phase") +
    ylab("Execution deduction mean score") +
    #scale_x_continuous(breaks = c(0, 5, 7, 10)) +  # Ensure time points appear correctly
    scale_color_manual(
      name = "",
      values = c("EE" = "#3F3D73", "Con" = "#D8C3A5"),
      labels = c("EE", "Con")
    ) +
    scale_y_continuous(
      breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),   # Exact y-axis ticks you want
      limits = c(0, 0.7)               # Optional: Set min and max y-axis range
    ) +
    scale_x_discrete(expand = expansion(mult = c(0.3, 0.3)), labels = c("EE" = "Experimental", "Con" = "Control")) +
    theme_classic(base_size = 13) +
    #theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
          axis.title.x = element_text(margin = margin(t = 15)),
          axis.title.y = element_text(margin = margin(r = 15)),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()))

ggsave("E:/data/Statistics/Plots/A5_model_1.png", plot = model_3_plot, 
       width = 8.0, height = 6.0, dpi = 800)
  
ggsave("C:/Users/malir/OneDrive - Univerzita Karlova/Plocha/Statistics/Plots/A5_model_1.png", plot = model_3_plot, 
       width = 7.0, height = 6.0, dpi = 800)

# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ----------------------------------------------------- TASK 4 Figure of Model 2 ----------------------------------------------- #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------ #

data_model_2$Block <- as.factor(data_model_2$Block)

# checking results in accordance with model
model_2 <- lmerTest::lmer(Score_Execution ~ 1 + Group * Block + (1 | ID), data = data_model_2)
#model_2 <- lme4::lmer(Score_Execution ~ 1 + Group + Block + Group * Block + (1 | ID), data = data_model_2)

# -------------------------------------------------------------------------- #
# ------------------------------- Individual trials ------------------------ #
# -------------------------------------------------------------------------- #

data_model_2 <- data_model_2 %>%
  group_by(ID, Phase) %>%
  mutate(Trials_2 = paste0(Phase, "_Tr_", row_number())) %>%
  ungroup()

# ------------------------------------------------------------------------- #

data_model_2 <- data_model_2 %>%
  mutate(Group = factor(Group, levels = c("EE", "Con")))

summary_data_score_model_2 <- data_model_2 %>%
  group_by(Group, Trials_2) %>%
  summarise(mean_Score_Execution = mean(Score_Execution, na.rm = TRUE),
            sterr = sd(Score_Execution, na.rm = TRUE) / sqrt(n()),
            lower_ci = mean_Score_Execution - qt(0.975, df = n() - 1) * sterr,
            upper_ci = mean_Score_Execution + qt(0.975, df = n() - 1) * sterr)

# ------------------------------- Individual trials ------------------------ #

data_model_2$Trials_2 <- factor(
  data_model_2$Trials_2,
  levels = unique(data_model_2$Trials_2)
)

summary_data_score_model_2$Trials_2 <- factor(
  summary_data_score_model_2$Trials_2,
  levels = unique(data_model_2$Trials_2)
)

# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #  

(model_4_plot_2 <- ggplot() +
     
     # 1. Individual participant data: gray points and connecting lines
     geom_line(
       data = data_model_2,
       aes(x = Trials_2, y = Score_Execution, group = ID, color = Group),
       alpha = 0.2,
       linewidth = 0.3,
       position = position_dodge(width = 0.4)
     ) + # Connect time points per subject, colored by state
     
     geom_point(data = data_model_2, 
                aes(x = Trials_2, y = Score_Execution, group = ID, color = Group), 
                size = 0.5, alpha = 0.3, position = position_dodge(width = 0.4)) +  # Individual data points
     
     # 2. Mean values with 95% confidence intervals
     geom_point(data = summary_data_score_model_2, 
                aes(x = Trials_2, y = mean_Score_Execution, color = Group), 
                size = 2, shape = 16, position = position_dodge(width = 0.4)) +  # Mean points
     geom_errorbar(data = summary_data_score_model_2, 
                   aes(x = Trials_2, ymin = lower_ci, ymax = upper_ci, color = Group), 
                   width = 0.2, linewidth = 0.7, position = position_dodge(width = 0.4)) +  # 95% CI error bars
     geom_line(data = summary_data_score_model_2, 
               aes(x = Trials_2, y = mean_Score_Execution, group = Group, color = Group), 
               linewidth = 1.0, position = position_dodge(width = 0.4)) +  # Line connecting means per state
     
     # Labels and theme
     xlab("Trial") +
     ylab("Execution deduction mean score") +
     
     scale_color_manual(
       name = "",
       values = c("EE" = "#3F3D73", "Con" = "#D8C3A5"),
       labels = c("EE", "Con")
     ) +
     theme_classic(base_size = 13) +
     
     theme(
       panel.grid.major.y = element_blank(),
       panel.grid.minor.y = element_blank(),
       panel.grid.major.x = element_blank(),
       panel.grid.minor.x = element_blank(),
       strip.text = element_text(face = "bold"),
       legend.position = "right",        # put whole legend above plot
       legend.direction = "vertical",
       legend.box = "vertical",        # stack title above keys
       #legend.title = element_text(vjust = 0),       # center title relative to keys
       legend.box.just = "center",     # center the legend box in the plot area
       #legend.spacing.y = unit(0.55, "cm"),
       legend.key.width = unit(0.6, "cm"),
       axis.text.x = element_text(hjust = 1, size = 8),
       axis.text.y = element_text(size = 8),
       axis.title.y = element_text(margin = margin(r = 15)),
       axis.title.x = element_text(margin = margin(t = 15)),
       axis.ticks = element_blank(),
       axis.line.y = element_blank()
     ) +
  
    # -------------------------------------------------------------------- #
    # ------------------------ vertical separators ----------------------- #
    # -------------------------------------------------------------------- #
    

    geom_segment(
      aes(x = 0.5, xend = 0.5, y = 0, yend = 0.7),
      linetype = "dashed",
      color = "gray80", linewidth = 0.4
    ) + annotate("text", x = 2.5, y = 0.75, label = "Block 1", size = 3.0) +
    annotate("text", x = 2.5, y = 0.82, label = "BT", size = 3.0, fontface = 2) +
    geom_segment(aes(x = 0.5, xend = 4.35, y = 0.78, yend = 0.78), linewidth = 0.2) +
    geom_segment(aes(x = 0.5, xend = 0.5, y = 0.78, yend = 0.7), linewidth = 0.2) +
    geom_segment(aes(x = 4.35, xend = 4.35, y = 0.78, yend = 0.7), linewidth = 0.2) +
    
    geom_segment(
      aes(x = 4.5, xend = 4.5, y = 0, yend = 0.7),
      linetype = "dashed",
      color = "gray80", linewidth = 0.4
    ) + annotate("text", x = 6.5, y = 0.75, label = "Block 2", size = 3.0) +
    annotate("text", x = 14.5, y = 0.82, label = "PT", size = 3.0, fontface = 2) +
    geom_segment(aes(x = 4.65, xend = 24.35, y = 0.78, yend = 0.78), linewidth = 0.2) +
    geom_segment(aes(x = 4.65, xend = 4.65, y = 0.78, yend = 0.7), linewidth = 0.2) +
    geom_segment(aes(x = 24.35, xend = 24.35, y = 0.78, yend = 0.7), linewidth = 0.2) +
    
    
    geom_segment(
      aes(x = 8.5, xend = 8.5, y = 0, yend = 0.7),
      linetype = "dashed",
      color = "gray80", linewidth = 0.4
    ) + annotate("text", x = 10.5, y = 0.75, label = "Block 3", size = 3.0) +
    
    geom_segment(
      aes(x = 12.5, xend = 12.5, y = 0, yend = 0.7),
      linetype = "dashed",
      color = "gray80", linewidth = 0.4
    ) + annotate("text", x = 14.5, y = 0.75, label = "Block 4", size = 3.0) +
    
    geom_segment(
      aes(x = 16.5, xend = 16.5, y = 0, yend = 0.7),
      linetype = "dashed",
      color = "gray80", linewidth = 0.4
    ) + annotate("text", x = 18.5, y = 0.75, label = "Block 5", size = 3.0) +
    
    geom_segment(
      aes(x = 20.5, xend = 20.5, y = 0, yend = 0.7),
      linetype = "dashed",
      color = "gray80", linewidth = 0.4
    ) + annotate("text", x = 22.5, y = 0.75, label = "Block 6", size = 3.0) +
    
    geom_segment(
      aes(x = 24.5, xend = 24.5, y = 0, yend = 0.7),
      linetype = "dashed",
      color = "gray80", linewidth = 0.4
    ) + annotate("text", x = 28.5, y = 0.75, label = "Block 7", size = 3.0) +
    annotate("text", x = 28.5, y = 0.82, label = "RT", size = 3.0, fontface = 2) +
    geom_segment(aes(x = 24.65, xend = 32.5, y = 0.78, yend = 0.78), linewidth = 0.2) +
    geom_segment(aes(x = 24.65, xend = 24.65, y = 0.78, yend = 0.7), linewidth = 0.2) +
    geom_segment(aes(x = 32.5, xend = 32.5, y = 0.78, yend = 0.7), linewidth = 0.2) +
    
    geom_segment(
      aes(x = 32.5, xend = 32.5, y = 0, yend = 0.7),
      linetype = "dashed",
      color = "gray80", linewidth = 0.4
    ) +
    
    
    # ---------------------------- Trials -------------------------------- #
    
    scale_y_continuous(limits = c(0.0, 0.9), breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7), expand = expansion(mult = c(0.00, 0.00))) +
    scale_x_discrete(limits = c(
      "BT_Tr_1","BT_Tr_2","BT_Tr_3","BT_Tr_4",
      "PT_Tr_1","PT_Tr_2","PT_Tr_3","PT_Tr_4","PT_Tr_5","PT_Tr_6","PT_Tr_7","PT_Tr_8",
      "PT_Tr_9","PT_Tr_10","PT_Tr_11","PT_Tr_12","PT_Tr_13","PT_Tr_14","PT_Tr_15","PT_Tr_16",
      "PT_Tr_17","PT_Tr_18","PT_Tr_19","PT_Tr_20",
      "RT_Tr_1","RT_Tr_2","RT_Tr_3","RT_Tr_4","RT_Tr_5","RT_Tr_6","RT_Tr_7","RT_Tr_8"
    ),labels = c("BT_Tr_1" = "1", 
                 "BT_Tr_2" = "2", 
                 "BT_Tr_3" = "3",
                 "BT_Tr_4" = "4",
                 "PT_Tr_1" = "5", 
                 "PT_Tr_2" = "6", 
                 "PT_Tr_3" = "7",
                 "PT_Tr_4" = "8",
                 "PT_Tr_5" = "9",
                 "PT_Tr_6" = "10",
                 "PT_Tr_7" = "11",
                 "PT_Tr_8" = "12",
                 "PT_Tr_9" = "13",
                 "PT_Tr_10" = "14",
                 "PT_Tr_11" = "15",
                 "PT_Tr_12" = "16",
                 "PT_Tr_13" = "17",
                 "PT_Tr_14" = "18",
                 "PT_Tr_15" = "19",
                 "PT_Tr_16" = "20",
                 "PT_Tr_17" = "21",
                 "PT_Tr_18" = "22",
                 "PT_Tr_19" = "23",
                 "PT_Tr_20" = "24",
                 "RT_Tr_1" = "25", 
                 "RT_Tr_2" = "26", 
                 "RT_Tr_3" = "27",
                 "RT_Tr_4" = "28",
                 "RT_Tr_5" = "29",
                 "RT_Tr_6" = "30",
                 "RT_Tr_7" = "31",
                 "RT_Tr_8" = "32"), expand = expansion(add = c(0.85, 0.85))) + 
    geom_segment(aes(x = 0.0, xend = 0.0, y = 0.0, yend = 0.73), linewidth = 0.8))

