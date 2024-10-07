pacman::p_load(tidyverse)

# Loading csv files
correct_final_vote_yes <- read.csv("correct_no_multicollinearity.csv")
correct_final_vote_no <- read.csv("correct_no_mean_no_finalvote.csv")
correct_yes_mean_final_vote_no <- read.csv("correct_yes_mean_no_finalvote.csv")
correct_yes_mean_final_vote_yes <- read.csv("correct_yes_mean_yes_finalvote.csv")


# Create the plot for final_vote_yes
ggplot(correct_final_vote_yes, aes(x = parliament_session, y = predicted_prob, color = environment)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~Indepentent_vari) +
  labs(
    title = "Predicted Probabilities No Mean Yes Final Vote",
    x = "Parliament Session",
    y = "Predicted Probability"
  ) +
  ylim(0.25, 1.00) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("correct_final_vote_yes.png", width = 28, height = 20, units = "cm", dpi = 300)


# Create the plot for final_vote_no
ggplot(correct_final_vote_no, aes(x = parliament_session, y = predicted_prob, color = environment)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~Indepentent_vari) +
  labs(
    title = "Predicted Probabilities No Mean No Final Vote",
    x = "Parliament Session",
    y = "Predicted Probability"
  ) +
  ylim(0.1, 1.05) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("correct_final_vote_no.png", width = 28, height = 20, units = "cm", dpi = 300)


# Create the plot for yes_mean_final_vote_yes
ggplot(correct_yes_mean_final_vote_yes, aes(x = parliament_session, y = predicted_prob, color = environment)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~Indepentent_vari) +
  labs(
    title = "Predicted Probabilities Yes Mean Yes Final Vote",
    x = "Parliament Session",
    y = "Predicted Probability"
  ) +
  ylim(0.1, 1.05) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("correct_yes_mean_final_vote_yes.png", width = 28, height = 20, units = "cm", dpi = 300)


# Create the plot for yes_mean_final_vote_no
ggplot(correct_yes_mean_final_vote_no, aes(x = parliament_session, y = predicted_prob, color = environment)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~Indepentent_vari) +
  labs(
    title = "Predicted Probabilities Yes Mean No Final Vote",
    x = "Parliament Session",
    y = "Predicted Probability"
  ) +
  ylim(0.1, 1.05) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("correct_yes_mean_final_vote_no.png", width = 28, height = 20, units = "cm", dpi = 300)