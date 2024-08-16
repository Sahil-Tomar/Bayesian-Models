# Load necessary library
library(ggplot2)

# Define the Poisson PMF function
poisson_pmf <- function(k, lambda) {
  (lambda^k * exp(-lambda)) / factorial(k)
}

# Define parameters
lambda <- 10
k_values <- 1:50

# Compute PMF values for each k
pmf_values <- sapply(k_values, poisson_pmf, lambda = lambda)

# Create a data frame for plotting
data <- data.frame(k = k_values, PMF = pmf_values)

# Plot the PMF using ggplot2 with vertical lines and detailed axis labels
ggplot(data, aes(x = k, y = PMF)) +
  geom_point() +
  geom_segment(aes(x = k, xend = k, y = 0, yend = PMF)) +
  labs(title = expression(paste("                               ", f(k, lambda))),
       x = "Number of Events (k)",
       y = "Probability of Occurrence") +
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.01)) +
  theme_minimal()
