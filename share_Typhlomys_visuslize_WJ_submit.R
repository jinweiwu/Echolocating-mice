# 00. readying -------------------------------------------------------------------------------------
# loading packages --
pacman::p_load(openxlsx, tidyverse, ggsci, ggpubr, ggbeeswarm)
setwd=("directory/data")
# 01. figure 1 -------------------------------------------------------------------------------------
# __1.1 Fig. 1A ------------------------------------------------------------------------------------
# picture --

# __1.2 Fig. 1B ------------------------------------------------------------------------------------
data <- read.xlsx("data_share_Typhlomys.xlsx", sheet = "figure_1BC")
p1 <- ggplot(data, aes(x = type, y = times)) + 
  geom_violin(aes(fill = type, color = type), width = 1.1, alpha = 0.2, lwd = 0.4) + 
  geom_boxplot(aes(fill = type, color = type), width = 0.42, outlier.shape = NA, alpha = 0.2, lwd = 0.2) + 
  geom_jitter(aes(color = type), width = 0.18, size = 1.2, alpha = 0.7, shape = 16) + 
  geom_segment(aes(x = 1, y = ggpp::as_npc(0.93), xend = 2, yend = ggpp::as_npc(0.93)), color = "#000000", linewidth = 0.4) + 
  annotate(geom = "text", x = 1.5, y = ggpp::as_npc(0.94), label = "***", size = (10*0.35), family = "sans") + 
  scale_x_discrete(labels = c("Unskilled mice    ", "    Skilled mice")) + 
  scale_y_continuous(limits = c(0, 24), breaks = c(0, 12, 24)) + 
  scale_fill_manual(values = c("#ED7D31", "#4472C4")) + 
  scale_color_manual(values = c("#ED7D31", "#4472C4")) + 
  labs(x = NULL, y = "Gnawing frequency") + 
  theme_bw(base_family = "sans") + 
  theme(legend.position = "none", 
        axis.text = element_text(size = 7, color = "#000000"), 
        axis.title = element_text(size = 8), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    ); p1

# __1.3 Fig. 1C ------------------------------------------------------------------------------------
p2 <- ggplot(data, aes(x = type, y = sum)) + 
  geom_violin(aes(fill = type, color = type), width = 1.1, alpha = 0.2, lwd = 0.4) + 
  geom_boxplot(aes(fill = type, color = type), width = 0.42, outlier.shape = NA, alpha = 0.2, lwd = 0.2) + 
  geom_jitter(aes(color = type), width = 0.18, size = 1.2, alpha = 0.7, shape = 16) + 
  geom_segment(aes(x = 1, y = ggpp::as_npc(0.93), xend = 2, yend = ggpp::as_npc(0.93)), color = "#000000", linewidth = 0.4) + 
  annotate(geom = "text", x = 1.5, y = ggpp::as_npc(0.94), label = "***", size = (10*0.35), family = "sans") + 
  scale_x_discrete(labels = c("Unskilled mice    ", "    Skilled mice")) + 
  scale_y_continuous(limits = c(0, 5000), breaks = c(0, 2500, 5000)) + 
  scale_fill_manual(values = c("#ED7D31", "#4472C4")) + 
  scale_color_manual(values = c("#ED7D31", "#4472C4")) + 
  labs(x = NULL, y = "Gnawing duration (s)") + 
  theme_bw(base_family = "sans") + 
  theme(legend.position = "none", 
        axis.text = element_text(size = 7, color = "#000000"), 
        axis.text.y = element_text(angle = 90, hjust = 0.7), 
        axis.title = element_text(size = 8), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    ); p2

# __1.4 Fig. 1D ------------------------------------------------------------------------------------
# picture --

# __1.5 Fig. 1E ------------------------------------------------------------------------------------
data <- read.xlsx("data_share_Typhlomys.xlsx", sheet = "figure_1EF")
p3 <- ggplot(data, aes(x = type, y = C_percent)) + 
  geom_jitter(aes(color = type), width = 0.18, size = 1.2, alpha = 0.7, shape = 16) + 
  geom_segment(aes(x = 1, y = ggpp::as_npc(0.93), xend = 2, yend = ggpp::as_npc(0.93)), color = "#000000", linewidth = 0.4) + 
  annotate(geom = "text", x = 1.5, y = ggpp::as_npc(0.94), label = "***", size = (10*0.35), family = "sans") + 
  scale_x_discrete(labels = c("Unskilled mice    ", "    Skilled mice")) + 
  scale_y_continuous(limits = c(0, 1.08), breaks = c(0, 0.5, 1)) + 
  scale_fill_manual(values = c("#ED7D31", "#4472C4")) + 
  scale_color_manual(values = c("#ED7D31", "#4472C4")) + 
  labs(x = NULL, y = "Thick hilum region (%)    ") + 
  theme_bw(base_family = "sans") + 
  theme(legend.position = "none", 
        axis.text = element_text(size = 7, color = "#000000"), 
        axis.title = element_text(size = 8), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    ); p3

# __1.6 Fig. 1F ------------------------------------------------------------------------------------
p4 <- ggplot(data, aes(x = type, y = max_point)) + 
  geom_jitter(aes(color = type), width = 0.18, size = 1.2, alpha = 0.7, shape = 16) + 
  geom_segment(aes(x = 1, y = ggpp::as_npc(0.93), xend = 2, yend = ggpp::as_npc(0.93)), color = "#000000", linewidth = 0.4) + 
  annotate(geom = "text", x = 1.5, y = ggpp::as_npc(0.94), label = "***", size = (10*0.35), family = "sans") + 
  scale_x_discrete(labels = c("Unskilled mice    ", "    Skilled mice")) + 
  scale_y_continuous(limits = c(0, 1.08), breaks = c(0, 0.5, 1)) + 
  scale_fill_manual(values = c("#ED7D31", "#4472C4")) + 
  scale_color_manual(values = c("#ED7D31", "#4472C4")) + 
  labs(x = NULL, y = "Specialization index (%)      ") + 
  theme_bw(base_family = "sans") + 
  theme(legend.position = "none", 
        axis.text = element_text(size = 7, color = "#000000"), 
        axis.title = element_text(size = 8), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    ); p4

ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, align = "hv")
ggsave(file = "save/fig_1_20260418.tiff", width = 9, height = 8, units = "cm", dpi = 300, limitsize = FALSE, bg = "#ffffff")
rm(data, p1, p2, p3, p4)

# 02. figure 2 -------------------------------------------------------------------------------------
# __2.1 Fig. 2A ------------------------------------------------------------------------------------
# picture --

# __2.2 Fig. 2B ------------------------------------------------------------------------------------
data <- read.xlsx("data_share_Typhlomys.xlsx", sheet = "figure_2B")

p1 <- ggplot(data, aes(x = adviser_gender, y = Attack_events)) + 
  geom_beeswarm(aes(color = color_status), cex = 3.6, size = 1.5, shape = 16, method = "center") + 
  geom_segment(aes(x = 1.5, y = ggpp::as_npc(0), xend = 1.5, yend = ggpp::as_npc(1)), linewidth = 0.34, linetype = 2, color = "#7f7f7f") + 
  annotate(geom = "text", x = 1, y = ggpp::as_npc(0.94), label = "0/13 (0%)", size = (7*0.35), family = "sans") + 
  annotate(geom = "text", x = 2, y = ggpp::as_npc(0.94), label = "6/18 (33.3%)", size = (7*0.35), family = "sans") + 
  scale_x_discrete(labels = c("S-Female", "S-Male")) + 
  scale_y_continuous(limits = c(-1, 30), breaks = c(0, 15, 30)) + 
  scale_fill_manual(values = c("#ED7D31", "#4472C4")) + 
  scale_color_manual(values = c("#ED7D31", "#4472C4")) + 
  labs(x = NULL, y = "Attack frequency") + 
  theme_bw(base_family = "sans") + 
  theme(legend.position = "none", 
        axis.text = element_text(size = 7, color = "#000000"), 
        axis.title = element_text(size = 8), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2), 
        plot.background = element_rect(fill = adjustcolor("#fafdf6"), color = NA), 
        panel.background = element_rect(fill = adjustcolor("#fafdf6"), color = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    ); p1

# __2.3 Fig. 2C ------------------------------------------------------------------------------------
data <- read.xlsx("data_share_Typhlomys.xlsx", sheet = "figure_2CD")
p2 <- ggplot(data, aes(x = adviser_gender, y = back_dur_percent)) + 
  geom_violin(aes(fill = adviser_gender, color = adviser_gender), width = 1.1, alpha = 0.2, lwd = 0.4) + 
  geom_boxplot(aes(fill = adviser_gender, color = adviser_gender), width = 0.42, outlier.shape = NA, alpha = 0.2, lwd = 0.2) + 
  geom_jitter(aes(color = adviser_gender), width = 0.18, height = 0, size = 1.2, alpha = 0.7, shape = 16) + 
  geom_segment(aes(x = 1, y = ggpp::as_npc(0.84), xend = 2, yend = ggpp::as_npc(0.84)), color = "#000000", linewidth = 0.4) + 
  annotate(geom = "text", x = 1.5, y = ggpp::as_npc(0.85), label = "**", size = (10*0.35), family = "sans") + 
  scale_fill_manual(values = c("#ED7D31", "#4472C4")) + 
  scale_color_manual(values = c("#ED7D31", "#4472C4")) + 
  labs(x = NULL, y = "Backward duration (%)    ") + 
  scale_x_discrete(labels = c("S-Female", "S-Male")) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) + 
  theme_bw(base_family = "sans") + 
  theme(legend.position = "none", 
        axis.text = element_text(size = 7, color = "#000000"), 
        axis.title = element_text(size = 8), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2), 
        plot.background = element_rect(fill = adjustcolor("#fafdf6"), color = NA), 
        panel.background = element_rect(fill = adjustcolor("#fafdf6"), color = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    ); p2


# __2.4 Fig. 2D ------------------------------------------------------------------------------------
p3 <- ggplot(data, aes(x = adviser_gender, y = back_times_percent)) + 
  geom_violin(aes(fill = adviser_gender, color = adviser_gender), width = 1.1, alpha = 0.2, lwd = 0.4) + 
  geom_boxplot(aes(fill = adviser_gender, color = adviser_gender), width = 0.42, outlier.shape = NA, alpha = 0.2, lwd = 0.2) + 
  geom_jitter(aes(color = adviser_gender), width = 0.18, height = 0, size = 1.2, alpha = 0.7, shape = 16) + 
  geom_segment(aes(x = 1, y = ggpp::as_npc(0.84), xend = 2, yend = ggpp::as_npc(0.84)), color = "#000000", linewidth = 0.45) + 
  annotate(geom = "text", x = 1.5, y = ggpp::as_npc(0.85), label = "**", size = (10*0.35), family = "sans") + 
  scale_fill_manual(values = c("#ED7D31", "#4472C4")) + 
  scale_color_manual(values = c("#ED7D31", "#4472C4")) + 
  labs(x = NULL, y = "Backward frequency (%)      ") + 
  scale_x_discrete(labels = c("S-Female", "S-Male")) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) + 
  theme_bw(base_family = "sans") + 
  theme(legend.position = "none", 
        axis.text = element_text(size = 7, color = "#000000"), 
        axis.title = element_text(size = 8), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2), 
        plot.background = element_rect(fill = adjustcolor("#fafdf6"), color = NA), 
        panel.background = element_rect(fill = adjustcolor("#fafdf6"), color = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    ); p3

# __2.5 Fig. 2E ------------------------------------------------------------------------------------
data <- read.xlsx("data_share_Typhlomys.xlsx", sheet = "figure_2EF")
p5 <- ggplot(data, aes(x = treat, y = times)) + 
  geom_rect(aes(xmin = 0.4, xmax = 2.5, ymin = ggpp::as_npc(0), ymax = ggpp::as_npc(1)), fill = "#fef3f4") + 
  geom_rect(aes(xmin = 2.5, xmax = 4.6, ymin = ggpp::as_npc(0), ymax = ggpp::as_npc(1)), fill = "#f2fbfc") + 
  geom_boxplot(aes(fill = treat, color = treat), width = 0.6, outlier.shape = NA, alpha = 0.2, lwd = 0.2) + 
  geom_jitter(aes(color = treat), width = 0.24, height = 0, size = 1.5, alpha = 0.7, shape = 16) + 
  annotate(geom = "text", x = 1.5, y = ggpp::as_npc(0.94), label = "S-Female", size = (7*0.35), family = "sans") + 
  annotate(geom = "text", x = 3.5, y = ggpp::as_npc(0.94), label = "S-Male", size = (7*0.35), family = "sans") + 
  geom_segment(aes(x = 2.5, y = ggpp::as_npc(0), xend = 2.5, yend = ggpp::as_npc(1)), linewidth = 0.34, linetype = 2, color = "#7f7f7f") + 
  geom_segment(aes(x = 1, y = ggpp::as_npc(0.84), xend = 2, yend = ggpp::as_npc(0.84)), color = "#000000", linewidth = 0.4) + 
  annotate(geom = "text", x = 1.5, y = ggpp::as_npc(0.85), label = "***", size = (10*0.35), family = "sans") + 
  geom_segment(aes(x = 3, y = ggpp::as_npc(0.84), xend = 4, yend = ggpp::as_npc(0.84)), color = "#000000", linewidth = 0.4) + 
  annotate(geom = "text", x = 3.5, y = ggpp::as_npc(0.85), label = "**", size = (10*0.35), family = "sans") + 
  scale_x_discrete(labels = c("Alone", "Paired", "Alone", "Paired")) + 
  scale_y_continuous(limits = c(0, 40), breaks = c(0, 20, 40)) + 
  scale_fill_manual(values = c("#ED7D31", "#4472C4", "#ED7D31", "#4472C4")) + 
  scale_color_manual(values = c("#ED7D31", "#4472C4", "#ED7D31", "#4472C4")) + 
  labs(x = NULL, y = "Gnawing frequency") + 
  theme_bw(base_family = "sans") + 
  theme(legend.position = "none", 
        axis.text = element_text(size = 7, color = "#000000"), 
        axis.title = element_text(size = 8), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    ); p5

# __2.6 Fig. 2F ------------------------------------------------------------------------------------
data <- read.xlsx("data_share_Typhlomys.xlsx", sheet = "figure_2EF")
p6 <- ggplot(data, aes(x = treat, y = sum)) + 
  geom_rect(aes(xmin = 0.4, xmax = 2.5, ymin = ggpp::as_npc(0), ymax = ggpp::as_npc(1)), fill = "#fef3f4") + 
  geom_rect(aes(xmin = 2.5, xmax = 4.6, ymin = ggpp::as_npc(0), ymax = ggpp::as_npc(1)), fill = "#f2fbfc") + 
  geom_boxplot(aes(fill = treat, color = treat), width = 0.6, outlier.shape = NA, alpha = 0.2, lwd = 0.2) + 
  geom_jitter(aes(color = treat), width = 0.24, height = 0, size = 1.5, alpha = 0.7, shape = 16) + 
  annotate(geom = "text", x = 1.5, y = ggpp::as_npc(0.94), label = "S-Female", size = (7*0.35), family = "sans") + 
  annotate(geom = "text", x = 3.5, y = ggpp::as_npc(0.94), label = "S-Male", size = (7*0.35), family = "sans") + 
  geom_segment(aes(x = 2.5, y = ggpp::as_npc(0), xend = 2.5, yend = ggpp::as_npc(1)), linewidth = 0.3, linetype = 2, color = "#7f7f7f") + 
  geom_segment(aes(x = 1, y = ggpp::as_npc(0.84), xend = 2, yend = ggpp::as_npc(0.84)), color = "#000000", linewidth = 0.4) + 
  annotate(geom = "text", x = 1.5, y = ggpp::as_npc(0.85), label = "*", size = (10*0.35), family = "sans") + 
  geom_segment(aes(x = 3, y = ggpp::as_npc(0.84), xend = 4, yend = ggpp::as_npc(0.84)), color = "#000000", linewidth = 0.4) + 
  annotate(geom = "text", x = 3.5, y = ggpp::as_npc(0.85), label = "*", size = (10*0.35), family = "sans") + 
  scale_x_discrete(labels = c("Alone", "Paired", "Alone", "Paired")) + 
  scale_y_continuous(limits = c(0, 6000), breaks = c(0, 3000, 6000)) + 
  scale_fill_manual(values = c("#ED7D31", "#4472C4", "#ED7D31", "#4472C4")) + 
  scale_color_manual(values = c("#ED7D31", "#4472C4", "#ED7D31", "#4472C4")) + 
  labs(x = NULL, y = "Gnawing duration (s)") + 
  theme_bw(base_family = "sans") + 
  theme(legend.position = "none", 
        axis.text = element_text(size = 7, color = "#000000"), 
        axis.text.y = element_text(angle = 90, hjust = 0.7), 
        axis.title = element_text(size = 8), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    ); p6

# __2.7 Fig. 2G ------------------------------------------------------------------------------------
data <- read.xlsx("data_share_Typhlomys.xlsx", sheet = "figure_2GH")
data <- data |> mutate(group_gender = case_when(group_gender == 1 ~ "F", group_gender == 2 ~ "M", TRUE ~ as.character(group_gender))) |> 
  mutate(treat = str_c(group_gender, treat, sep = "_"))
p7 <- ggplot(data, aes(x = treat, y = C_percent_actor)) + 
  geom_rect(aes(xmin = 0.4, xmax = 2.5, ymin = ggpp::as_npc(0), ymax = ggpp::as_npc(1)), fill = "#fef3f4") + 
  geom_rect(aes(xmin = 2.5, xmax = 4.6, ymin = ggpp::as_npc(0), ymax = ggpp::as_npc(1)), fill = "#f2fbfc") + 
  geom_jitter(aes(color = treat), width = 0.24, height = 0, size = 1.5, alpha = 0.7, shape = 16) + 
  annotate(geom = "text", x = 1.5, y = ggpp::as_npc(0.94), label = "S-Female", size = (7*0.35), family = "sans") + 
  annotate(geom = "text", x = 3.5, y = ggpp::as_npc(0.94), label = "S-Male", size = (7*0.35), family = "sans") + 
  geom_segment(aes(x = 2.5, y = ggpp::as_npc(0), xend = 2.5, yend = ggpp::as_npc(1)), linewidth = 0.34, linetype = 2, color = "#7f7f7f") + 
  # geom_segment(aes(x = 1, y = ggpp::as_npc(0.84), xend = 2, yend = ggpp::as_npc(0.84)), color = "#000000", linewidth = 0.35) + 
  # annotate(geom = "text", x = 1.5, y = ggpp::as_npc(0.88), label = "n.s.", size = (6*0.35), family = "sans") + 
  geom_segment(aes(x = 3, y = ggpp::as_npc(0.84), xend = 4, yend = ggpp::as_npc(0.84)), color = "#000000", linewidth = 0.35) + 
  annotate(geom = "text", x = 3.5, y = ggpp::as_npc(0.85), label = "**", size = (10*0.35), family = "sans") + 
  scale_x_discrete(labels = c("Alone", "Paired", "Alone", "Paired")) + 
  scale_y_continuous(limits = c(0, 1.2), breaks = c(0, 0.5, 1)) + 
  scale_fill_manual(values = c("#ED7D31", "#4472C4", "#ED7D31", "#4472C4")) + 
  scale_color_manual(values = c("#ED7D31", "#4472C4", "#ED7D31", "#4472C4")) + 
  labs(x = NULL, y = "Thick hilum region (%)    ") + 
  theme_bw(base_family = "sans") + 
  theme(legend.position = "none", 
        axis.text = element_text(size = 7, color = "#000000"), 
        axis.title = element_text(size = 8), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    ); p7

# __2.8 Fig. 2H ------------------------------------------------------------------------------------
data <- read.xlsx("data_share_Typhlomys.xlsx", sheet = "figure_2GH")
data <- data |> mutate(group_gender = case_when(group_gender == 1 ~ "F", group_gender == 2 ~ "M", TRUE ~ as.character(group_gender))) |> 
  mutate(treat = str_c(group_gender, treat, sep = "_"))
p8 <- ggplot(data, aes(x = treat, y = max_point_actor)) + 
  geom_rect(aes(xmin = 0.4, xmax = 2.5, ymin = ggpp::as_npc(0), ymax = ggpp::as_npc(1)), fill = "#fef3f4") + 
  geom_rect(aes(xmin = 2.5, xmax = 4.6, ymin = ggpp::as_npc(0), ymax = ggpp::as_npc(1)), fill = "#f2fbfc") + 
  geom_jitter(aes(color = treat), width = 0.24, height = 0, size = 1.5, alpha = 0.7, shape = 16) + 
  annotate(geom = "text", x = 1.5, y = ggpp::as_npc(0.94), label = "S-Female", size = (7*0.35), family = "sans") + 
  annotate(geom = "text", x = 3.5, y = ggpp::as_npc(0.94), label = "S-Male", size = (7*0.35), family = "sans") + 
  geom_segment(aes(x = 2.5, y = ggpp::as_npc(0), xend = 2.5, yend = ggpp::as_npc(1)), linewidth = 0.3, linetype = 2, color = "#7f7f7f") + 
  # geom_segment(aes(x = 1, y = ggpp::as_npc(0.84), xend = 2, yend = ggpp::as_npc(0.84)), color = "#000000", linewidth = 0.35) + 
  # annotate(geom = "text", x = 1.5, y = ggpp::as_npc(0.88), label = "n.s.", size = (6*0.35), family = "sans") + 
  geom_segment(aes(x = 3, y = ggpp::as_npc(0.84), xend = 4, yend = ggpp::as_npc(0.84)), color = "#000000", linewidth = 0.35) + 
  annotate(geom = "text", x = 3.5, y = ggpp::as_npc(0.85), label = "*", size = (10*0.35), family = "sans") + 
  scale_x_discrete(labels = c("Alone", "Paired", "Alone", "Paired")) + 
  scale_y_continuous(limits = c(0, 1.2), breaks = c(0, 0.5, 1)) + 
  scale_fill_manual(values = c("#ED7D31", "#4472C4", "#ED7D31", "#4472C4")) + 
  scale_color_manual(values = c("#ED7D31", "#4472C4", "#ED7D31", "#4472C4")) + 
  labs(x = NULL, y = "Specialization index (%)      ") + 
  theme_bw(base_family = "sans") + 
  theme(legend.position = "none", 
        axis.text = element_text(size = 7, color = "#000000"), 
        axis.title = element_text(size = 8), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    ); p8

cowplot::plot_grid(
  cowplot::plot_grid(
    cowplot::plot_grid(NULL, p1, ncol = 2, rel_widths = c(4, 6)), 
    ggarrange(p2, p3, ncol = 2, align = "hv"), 
    ncol = 1
  ), 
  ggarrange(p5, p6, p7, p8, ncol = 2, nrow = 2, align = "hv", vjust = 1.2), 
  rel_widths = c(8, 10)
)

ggsave(file = "save/fig_2_20260418.tiff", width = 18, height = 8, units = "cm", dpi = 300, limitsize = FALSE, bg = "#ffffff")
rm(data, p1, p2, p3, p4, p5, p6, p7, p8)

# 03. figure 3 -------------------------------------------------------------------------------------
# __3.1 Fig. 3A ------------------------------------------------------------------------------------
data <- read.xlsx("data_share_Typhlomys.xlsx", sheet = "figure_3AB") |> arrange(desc(point_color))
p1 <- ggplot(data, aes(x = group_gender, y = times)) + 
  geom_boxplot(data = data |> filter(point_color != "zero"), aes(fill = group_gender, color = group_gender), width = 0.5, outlier.shape = NA, alpha = 0.2, lwd = 0.2) + 
  geom_jitter(aes(color = point_color), width = 0.2, height = 1, size = 1.5, alpha = 0.7, shape = 16) + 
  geom_segment(aes(x = 1, y = ggpp::as_npc(0.92), xend = 3, yend = ggpp::as_npc(0.92)), color = "#000000", linewidth = 0.45) + 
  annotate(geom = "text", x = 2, y = ggpp::as_npc(0.93), label = "**", size = (10*0.35), family = "sans") + 
  geom_segment(aes(x = 2, y = ggpp::as_npc(0.84), xend = 3, yend = ggpp::as_npc(0.84)), color = "#000000", linewidth = 0.4) + 
  annotate(geom = "text", x = 2.5, y = ggpp::as_npc(0.88), label = "ns", size = (7*0.35), family = "sans") + 
  labs(x = NULL, y = "Gnawing frequency") + 
  scale_x_discrete(labels = c("B-Female", "B-Male", "Control")) + 
  scale_y_continuous(limits = c(-1, 20), breaks = c(0, 10, 20)) + 
  scale_fill_manual(values = c("#ED7D31", "#4472C4", "#2E7D32")) + 
  scale_color_manual(values = c("#ED7D31", "#4472C4", "#2E7D32", "#808080")) + 
  theme_bw(base_family = "sans") + 
  theme(legend.position = "none", 
        axis.text = element_text(size = 7, color = "#000000"), 
        axis.title = element_text(size = 8), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2), 
        plot.background = element_rect(fill = adjustcolor("#f9f7e7"), color = NA), 
        panel.background = element_rect(fill = adjustcolor("#f9f7e7"), color = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    ); p1

# __3.2 Fig. 3B ------------------------------------------------------------------------------------
p2 <- ggplot(data, aes(x = group_gender, y = sum)) + 
  geom_boxplot(data = data |> filter(point_color != "zero"), aes(fill = group_gender, color = group_gender), width = 0.5, outlier.shape = NA, alpha = 0.2, lwd = 0.2) + 
  geom_jitter(aes(color = point_color), width = 0.2, height = 150, size = 1.5, alpha = 0.7, shape = 16) + 
  geom_segment(aes(x = 1, y = ggpp::as_npc(0.92), xend = 3, yend = ggpp::as_npc(0.92)), color = "#000000", linewidth = 0.45) + 
  annotate(geom = "text", x = 2, y = ggpp::as_npc(0.93), label = "*", size = (10*0.35), family = "sans") + 
  geom_segment(aes(x = 2, y = ggpp::as_npc(0.84), xend = 3, yend = ggpp::as_npc(0.84)), color = "#000000", linewidth = 0.4) + 
  annotate(geom = "text", x = 2.5, y = ggpp::as_npc(0.88), label = "ns", size = (7*0.35), family = "sans") + 
  labs(x = NULL, y = "Gnawing duraiton (s)") + 
  scale_x_discrete(labels = c("B-Female", "B-Male", "Control")) + 
  scale_y_continuous(limits = c(-150, 3000), breaks = c(0, 1500, 3000)) + 
  scale_fill_manual(values = c("#ED7D31", "#4472C4", "#2E7D32")) + 
  scale_color_manual(values = c("#ED7D31", "#4472C4", "#2E7D32", "#808080")) + 
  theme_bw(base_family = "sans") + 
  theme(legend.position = "none", 
        axis.text = element_text(size = 7, color = "#000000"), 
        axis.text.y = element_text(angle = 90, hjust = 0.7), 
        axis.title = element_text(size = 8), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2), 
        plot.background = element_rect(fill = adjustcolor("#f9f7e7"), color = NA), 
        panel.background = element_rect(fill = adjustcolor("#f9f7e7"), color = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    ); p2


# __3.3 Fig. 3C ------------------------------------------------------------------------------------
data <- read.xlsx("data_share_Typhlomys.xlsx", sheet = "figure_3CD") |> arrange(desc(point_color))
p3 <- ggplot(data, aes(x = group_gender, y = C_percent)) + 
  geom_jitter(aes(color = point_color), width = 0.2, height = 0.05, size = 1.5, alpha = 0.7, shape = 16) + 
  geom_segment(aes(x = 1, y = ggpp::as_npc(0.92), xend = 3, yend = ggpp::as_npc(0.92)), color = "#000000", linewidth = 0.45) + 
  annotate(geom = "text", x = 2, y = ggpp::as_npc(0.93), label = "*", size = (10*0.35), family = "sans") + 
  geom_segment(aes(x = 2, y = ggpp::as_npc(0.85), xend = 3, yend = ggpp::as_npc(0.85)), color = "#000000", linewidth = 0.4) + 
  annotate(geom = "text", x = 2.5, y = ggpp::as_npc(0.86), label = "*", size = (10*0.35), family = "sans") + 
  labs(x = NULL, y = "Thick hilum region (%)    ") + 
  scale_x_discrete(labels = c("B-Female", "B-Male", "Control")) + 
  scale_y_continuous(limits = c(-0.05, 1.2), breaks = c(0, 0.5, 1)) + 
  scale_fill_manual(values = c("#ED7D31", "#4472C4", "#2E7D32")) + 
  scale_color_manual(values = c("#ED7D31", "#4472C4", "#2E7D32", "#808080")) + 
  theme_bw(base_family = "sans") + 
  theme(legend.position = "none", 
        axis.text = element_text(size = 7, color = "#000000"), 
        axis.title = element_text(size = 8), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2), 
        plot.background = element_rect(fill = adjustcolor("#f9f7e7"), color = NA), 
        panel.background = element_rect(fill = adjustcolor("#f9f7e7"), color = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    ); p3

# __3.4 Fig. 3D ------------------------------------------------------------------------------------
p4 <- ggplot(data, aes(x = group_gender, y = max_point)) + 
  geom_jitter(aes(color = point_color), width = 0.2, height = 0.05, size = 1.5, alpha = 0.7, shape = 16) + 
  geom_segment(aes(x = 1, y = ggpp::as_npc(0.92), xend = 3, yend = ggpp::as_npc(0.92)), color = "#000000", linewidth = 0.45) + 
  annotate(geom = "text", x = 2, y = ggpp::as_npc(0.93), label = "*", size = (10*0.35), family = "sans") + 
  geom_segment(aes(x = 2, y = ggpp::as_npc(0.84), xend = 3, yend = ggpp::as_npc(0.84)), color = "#000000", linewidth = 0.4) + 
  annotate(geom = "text", x = 2.5, y = ggpp::as_npc(0.88), label = "ns", size = (7*0.35), family = "sans") + 
  labs(x = NULL, y = "Specialization index (%)      ") + 
  scale_x_discrete(labels = c("B-Female", "B-Male", "Control")) + 
  scale_y_continuous(limits = c(-0.05, 1.2), breaks = c(0, 0.5, 1)) + 
  scale_fill_manual(values = c("#ED7D31", "#4472C4", "#2E7D32")) + 
  scale_color_manual(values = c("#ED7D31", "#4472C4", "#2E7D32", "#808080")) + 
  theme_bw(base_family = "sans") + 
  theme(legend.position = "none", 
        axis.text = element_text(size = 7, color = "#000000"), 
        axis.title = element_text(size = 8), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2), 
        plot.background = element_rect(fill = adjustcolor("#f9f7e7"), color = NA), 
        panel.background = element_rect(fill = adjustcolor("#f9f7e7"), color = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    ); p4

# __3.5 Fig. 3E ------------------------------------------------------------------------------------
data <- read.xlsx("data_share_Typhlomys.xlsx", sheet = "figure_3EF")
my_color <- colorRampPalette(c("#ED7D31", "#4472C4"), space = "Lab", interpolate = "linear")
set.seed(2003)
p5 <- data |> filter(variable == "C_percent") |> mutate(condition = str_c(group_gender, condition, sep = "_")) |> # mutate(value = value + runif(38, 0, 1)/10) |> 
  ggplot(aes(x = condition, y = value, group = id)) + 
  geom_line(alpha = 0.5, size = 0.4, lineend = "round", linejoin = "round") + 
  geom_point(color = "#ffffff", size = 2.5, shape = 16) + 
  geom_point(aes(color = id), size = 2.5, alpha = 0.5, shape = 16) + 
  annotate(geom = "text", x = 1.5, y = ggpp::as_npc(0.94), label = "B-Female", size = (7*0.35), family = "sans") + 
  annotate(geom = "text", x = 3.5, y = ggpp::as_npc(0.94), label = "B-Male", size = (7*0.35), family = "sans") + 
  geom_segment(aes(x = 2.5, y = ggpp::as_npc(0), xend = 2.5, yend = ggpp::as_npc(1)), linewidth = 0.3, linetype = 2, color = "#7f7f7f") + 
  scale_x_discrete(expand = c(0.2, 0.2), labels = c("Before", "After", "Before", "After")) + 
  scale_y_continuous(limits = c(0, 1.2), breaks = c(0, 0.5, 1)) + 
  scale_color_manual(values = c(my_color(6), my_color(13))) + 
  labs(x = NULL, y = "Thick hilum region (%)    ") +
  theme_bw(base_family = "sans") + 
  theme(legend.position = "none", 
        axis.text = element_text(size = 7, color = "#000000"), 
        axis.title = element_text(size = 8), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2), 
        plot.background = element_rect(fill = adjustcolor("#f9f7e7"), color = NA), 
        panel.background = element_rect(fill = adjustcolor("#f9f7e7"), color = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    ); p5

# __3.6 Fig. 3F ------------------------------------------------------------------------------------
set.seed(1999)
p6 <- data |> filter(variable == "Max_point") |> mutate(condition = str_c(group_gender, condition, sep = "_")) |> # mutate(value = value + runif(38, 0, 1)/10) |> 
  ggplot(aes(x = condition, y = value, group = id)) + 
  geom_line(alpha = 0.5, size = 0.4, lineend = "round", linejoin = "round") + 
  geom_point(color = "#ffffff", size = 2.5, shape = 16) + 
  geom_point(aes(color = id), size = 2.5, alpha = 0.5, shape = 16) + 
  annotate(geom = "text", x = 1.5, y = ggpp::as_npc(0.94), label = "B-Female", size = (7*0.35), family = "sans") + 
  annotate(geom = "text", x = 3.5, y = ggpp::as_npc(0.94), label = "B-Male", size = (7*0.35), family = "sans") + 
  geom_segment(aes(x = 2.5, y = ggpp::as_npc(0), xend = 2.5, yend = ggpp::as_npc(1)), linewidth = 0.3, linetype = 2, color = "#7f7f7f") + 
  scale_x_discrete(expand = c(0.2, 0.2), labels = c("Before", "After", "Before", "After")) + 
  scale_y_continuous(limits = c(0, 1.2), breaks = c(0, 0.5, 1)) + 
  scale_color_manual(values = c(my_color(6), my_color(13))) + 
  labs(x = NULL, y = "Specialization index (%)      ") +
  theme_bw(base_family = "sans") + 
  theme(legend.position = "none", 
        axis.text = element_text(size = 7, color = "#000000"), 
        axis.title = element_text(size = 8), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2), 
        plot.background = element_rect(fill = adjustcolor("#f9f7e7"), color = NA), 
        panel.background = element_rect(fill = adjustcolor("#f9f7e7"), color = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    ); p6

cowplot::plot_grid(
  ggarrange(p1, p2, nrow = 2, labels = c("A", "B"), font.label = list(size = 8, face = "bold", family = "sans"), align = "hv", vjust = 1.2), 
  cowplot::plot_grid(NULL, NULL, nrow = 2), 
  ggarrange(p3, p4, nrow = 2, labels = c("C", "D"), font.label = list(size = 8, face = "bold", family = "sans"), align = "hv", vjust = 1.2), 
  cowplot::plot_grid(NULL, NULL, nrow = 2), 
  ggarrange(p5, p6, nrow = 2, labels = c("E", "F"), font.label = list(size = 8, face = "bold", family = "sans"), align = "hv", vjust = 1.2), 
  ncol = 5, rel_widths = c(4.9, 0.15, 4.9, 0.15, 4.9)
)

ggsave(file = "save/fig_3_20260418.tiff", width = 15, height = 8, units = "cm", dpi = 300, limitsize = FALSE, bg = "#ffffff")
rm(data, my_color, p1, p2, p3, p4, p5, p6)
