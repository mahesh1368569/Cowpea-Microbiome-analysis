alpha_metrics_its = readxl::read_xlsx("Alpha_Diversity_its.xlsx")

shapiro.test(alpha_metrics_its$Chao1)

# Shapiro-Wilk test for Shannon
shapiro.test(alpha_metrics_its$Shannon)

# GLMM for Chao1
glmm_chao1_its <- lmer(Chao1 ~ Treatment * Drought_Stage * Genotype + (1|Replication), data = alpha_metrics_its)
glmm_shannon_its <- lmer(Shannon ~ Treatment * Drought_Stage * Genotype + (1|Replication), data = alpha_metrics_its)

summary(glmm_chao1)

anova_results_chao1_its <- anova(glmm_chao1_its)
anova_results_shannon_its <- anova(glmm_shannon_its)

anova_results_chao1_its
anova_results_shannon_its
# GLMM for Shannon
glmm_shannon <- lmer(Shannon ~ Group + (1|RandomFactor), data = data)
summary(glmm_shannon)

beta = readxl::read_xlsx("PCOA-Bray-ITS.xlsx")

beta_plot_genotype_its <- ggplot(beta, aes(x = Axis.1, y = Axis.2, color = Genotype)) +
  geom_point(size = 3) +   # Adjust point size as needed
  scale_color_manual(values = c("#faa","#9e76ab")) +  # Customize colors
  scale_shape_manual(values = c(16, 17)) +
  scale_fill_manual(values = c("#faa","#9e76ab")) +
  stat_ellipse(aes(fill = Genotype), level = 0.97, alpha = 0.2, geom = "polygon") +  # 95% confidence ellipses
  theme(legend.position = "right") + # Customize shapes
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +  # Horizontal axis line
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +  # Vertical axis line
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", size = 1),  # Ensure x and y axis lines are visible
    axis.ticks = element_line(color = "black", size = 1)  # Ensure axis ticks are visible
  ) +
  labs(x = "PC1 (34.7 %)",
       y = "PC2 (23.7 %)",
       title = "PCoA Plot - Genotype - Fungal communities")

beta_plot_genotype_its

ggsave("beta_plot_genotype-its.jpeg", plot = beta_plot_genotype_its, 
       width = 6, height = 4, units = "in", 
       dpi = 1000)

beta_plot_trt_its <- ggplot(beta, aes(x = Axis.1, y = Axis.2, color = Treatment)) +
  geom_point(size = 3) +   # Adjust point size as needed
  scale_color_manual(values = c("#c5b0d5", "#ffbb78")) +  # Customize colors
  scale_shape_manual(values = c(16, 17)) +
  scale_fill_manual(values = c("#c5b0d5", "#ffbb78")) +
  stat_ellipse(aes(fill = Treatment), level = 0.97, alpha = 0.2, geom = "polygon") +  # 95% confidence ellipses
  theme(legend.position = "right") + # Customize shapes
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +  # Horizontal axis line
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +  # Vertical axis line
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", size = 1),  # Ensure x and y axis lines are visible
    axis.ticks = element_line(color = "black", size = 1)  # Ensure axis ticks are visible
  ) +
  labs(x = "PC1 (34.7 %)",
       y = "PC2 (23.7 %)",
       title = "PCoA plot - Treatment - Fungal Communities")

beta_plot_trt_its

ggsave("beta_plot_trt_its.jpeg", plot = beta_plot_trt_its, 
       width = 6, height = 4, units = "in", 
       dpi = 1000)

beta_stage = readxl::read_xlsx("PCOA-Wg.xlsx")
# Ensure Drought_Stage is a factor with the correct order
beta$Drought_Stage <- factor(beta_stage$Drought_Stage, levels = c("V2-Stage", "V4-Stage", "R1-Stage", "R4-Stage"))

beta_plot_stage_its <- ggplot(beta, aes(x = Axis.1, y = Axis.2, color = Drought_Stage, fill = Drought_Stage)) +
  geom_point(size = 3, shape = 21) +  # Use shape = 21 for filled points
  scale_color_manual(values = c("#FFFFB3", "#B2DF8A", "#33A02C", "#006837")) +  # Gradual color change
  scale_fill_manual(values = c("#FFFFB3", "#B2DF8A", "#33A02C", "#006837")) +  # Matching fill for ellipses
  stat_ellipse(level = 0.95, alpha = 0.2, geom = "polygon") +  # 97% confidence ellipses with color fill
  theme(legend.position = "right") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +  # Horizontal axis line
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +  # Vertical axis line
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", size = 1),  # Ensure x and y axis lines are visible
    axis.ticks = element_line(color = "black", size = 1)  # Ensure axis ticks are visible
  ) +
  labs(x = "PC1 (34.7 %)",
       y = "PC2 (23.7 %)",
       title = "PCoA plot - Growth Stages - Fungal Communities")

beta_plot_stage_its 

ggsave("beta_plot_stage_its.jpeg", plot = beta_plot_stage_its, 
       width = 6, height = 4, units = "in", 
       dpi = 1000)

