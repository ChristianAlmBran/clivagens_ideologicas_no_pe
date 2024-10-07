pacman::p_load(tidyverse)

# Loading csv files
final_vote_yes <- read.csv("final_vote_yes.csv")
final_vote_no <- read.csv("final_vote_no.csv")
final_vote_no_mean <- read.csv("final_vote_no_mean.csv")
final_vote_yes_mean <- read.csv("final_vote_yes_mean.csv")


# Create the plot for final_vote_yes
ggplot(final_vote_yes, aes(x = parliament_session, y = predicted_prob, color = environment)) +
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

ggsave("final_vote_yes.png", width = 28, height = 20, units = "cm", dpi = 300)


# Create the plot for final_vote_no
ggplot(final_vote_no, aes(x = parliament_session, y = predicted_prob, color = environment)) +
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

ggsave("final_vote_no.png", width = 28, height = 20, units = "cm", dpi = 300)


# Create the plot for final_vote_yes_mean
ggplot(final_vote_yes_mean, aes(x = parliament_session, y = predicted_prob, color = environment)) +
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

ggsave("final_vote_yes_mean.png", width = 28, height = 20, units = "cm", dpi = 300)


# Create the plot for final_vote_no_mean
ggplot(final_vote_no_mean, aes(x = parliament_session, y = predicted_prob, color = environment)) +
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

ggsave("final_vote_no_mean.png", width = 28, height = 20, units = "cm", dpi = 300)
