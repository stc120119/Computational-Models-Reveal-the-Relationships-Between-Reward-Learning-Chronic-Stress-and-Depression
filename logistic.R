
library(dplyr)

setwd('c://Users/MYQ20/Desktop/paper1/submit_depression and anxiety/code and data/prl_data/')

# 
file_list <- list.files(pattern = ".csv$")

# 
data_list <- lapply(file_list, read.csv, stringsAsFactors = FALSE)

nSubjects <- as.numeric(length(data_list))

#
subid_list <- vector(length = nSubjects)

for (t in 1:nSubjects) {
  
  id <- data_list[[t]][["participant"]]
  subid_list[t] <- id[1]
  
}


for (i in 1:nSubjects) {
  
# Step 1: 
reward_result <- data_list[[i]]$reward_result
selected_image <- data_list[[i]]$selected_image
data <- cbind(reward_result/10,selected_image)
data <- na.omit(data)
data <- as.data.frame(data)

# Step 2: 
data$reward_diff <- ifelse(data[,2] == "image1",
                           2*as.numeric(data[,1]) - 1,
                           1 - 2*as.numeric(data[,1]))

# Step 3: 
# 
for (m in 1:10) {
  data[[paste0("lagged_reward_diff_", m)]] <- c(rep(NA, m), head(data$reward_diff, -m))
}

# 
data <- data[11:nrow(data), ]

# Step 4: 
data$prev_selected_image <- c(NA, head(data$selected_image, -1))
data$last_choice_diff <- ifelse(data$prev_selected_image == 'image1', 1, -1)
data$last_choice_diff <- scale(data$last_choice_diff)

# 
data <- na.omit(data)

# Step 5: 
lagged_vars <- paste0("lagged_reward_diff_", 1:10)
formula <- as.formula(paste("selected_image_numeric ~", paste(c(lagged_vars, "last_choice_diff"), collapse = " + ")))

# 
data$selected_image_numeric <- ifelse(data$selected_image == "image1", 1, 0)

# 
model <- glm(formula, data = data, family = binomial(link = "logit"))
all_models[[i]] <- model
all_model_summ[[i]] <- summary(model)
}

# 
beta_values <- data.frame()

for (i in 1:nSubjects) {
  subid = subid_list[i]
  model <- all_models[[i]]
  # 
  coefficients <- coef(model)
  
  # 
  subject_betas <- data.frame(
    subid = subid,  # 
    term = names(coefficients),         # 
    beta_value = coefficients           # 
  )
  # 
  beta_values <- rbind(beta_values, subject_betas)
}

rownames(beta_values) <- NULL

library(tidyr)

beta_values <- beta_values %>%
  pivot_wider(
    names_from = subid,  # 
    values_from = beta_value, # 
    id_cols = term            #  
  )

# 
beta_stats <- beta_values %>%
  rowwise() %>%
  mutate(
    mean = mean(c_across(-term), na.rm = TRUE),  # 
    sd = sd(c_across(-term), na.rm = TRUE)      # 
  ) 

# 
beta_stats <- beta_stats[c(2:nrow(beta_stats), 1), ]
beta_values <- beta_values[c(2:nrow(beta_values), 1), ]
# 
print(beta_stats)

library(ggplot2)

# 
beta_stats <- beta_stats %>%
  mutate(se = sd / sqrt(ncol(beta_values) - 1))  # 

# 
# 
beta_stats$term <- c(
  "β_1", "β_2", "β_3", "β_4", "β_5", 
  "β_6", "β_7", "β_8", "β_9", "β_10", 
  "Inertia", "Bias"
)

beta_values$term <- c(
  "β_1", "β_2", "β_3", "β_4", "β_5", 
  "β_6", "β_7", "β_8", "β_9", "β_10", 
  "Inertia", "Bias"
)

# 
beta_stats$term <- factor(beta_stats$term, levels = beta_stats$term)
beta_values$term <- factor(beta_values$term, levels = beta_values$term)

for (i in 1:12) {
  lower_limit <- mean(as.numeric(unlist(beta_values[i, -1])), na.rm = TRUE) - 1 * sd(as.numeric(unlist(beta_values[i, -1])), na.rm = TRUE)
  upper_limit <- mean(as.numeric(unlist(beta_values[i, -1])), na.rm = TRUE) + 1 * sd(as.numeric(unlist(beta_values[i, -1])), na.rm = TRUE)
  beta_values[i,-1] <- ifelse(
    as.numeric(beta_values[i, -1]) >= lower_limit & 
      as.numeric(beta_values[i, -1]) <= upper_limit,
    beta_values[i, -1],
    NA  # 
  )
}

library(ggplot2)

# 
significance_data <- data.frame(
  term = c("β_1", "β_2", "Inertia"),  # 
  mean = c(beta_stats$mean[1], beta_stats$mean[2], beta_stats$mean[11]),  # 
  se = c(beta_stats$se[1], beta_stats$se[2], beta_stats$se[11]),  # 
  label = c("***", "***", "***")  # 
)

# 
significance_data <- significance_data %>%
  mutate(
    x = term ,  # 
    y = mean + se + 0.015  # 
  )

# 绘制改进版图形
ggplot(beta_stats, aes(x = term, y = mean, group = 1)) +
  geom_jitter(data = beta_values %>%
                pivot_longer(-term, names_to = "subid", values_to = "beta_value"),
              aes(x = term, y = beta_value),
              inherit.aes = FALSE,
              color = "#8FC0A9",  # 
              alpha = 0.5,        # 
              size = 2,           # 
              width = 0.05         # 
  )+
  geom_line(linewidth = 1, color = "#467F79") +  # 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.13, color = "#2E4F4A",linewidth = 0.8) +  # 
  geom_point(size = 3, color = "#2E4F4A") +  # 
  labs(x = "", y = "Beta", title = "") +
  scale_y_continuous(limits = c(-0.2, 2.5), 
                     breaks = seq(-0.2, 2.5, 0.5)) +
  theme_minimal() +  # 
  theme(
    axis.line = element_line(color = "black", linewidth = 1),  # 
    axis.text = element_text(size = 12,face = "bold"),  # 
    axis.title = element_text(size = 14,face = "bold"),  # 
    axis.ticks.x = element_line(color = "black", linewidth = 1),  # 
    axis.ticks.length = unit(0.2, "cm"),  # 
    panel.grid.major = element_blank(),  # 
    panel.grid.minor = element_blank(),  # 
    axis.text.x = element_text(angle = 45, hjust = 1)  # 
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +  # 
  geom_text(data = significance_data, 
            aes(x = x , y = y, label = label),  # 
            inherit.aes = FALSE, color = "#683611", size = 5,nudge_x = 0.2,fontface = "bold")  # 

  ggsave('lm_image.png',plot = last_plot(),
         width = 10, height = 6,  # 
         dpi = 300)  # 