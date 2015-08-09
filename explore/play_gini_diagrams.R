library(dplyr)
library(ggplot2)
library(scales)
library(ineq)
# generate vector (of incomes)
x <- c(541, 1463, 2445, 3438, 4437, 5401, 6392, 8304, 11904, 22261)
# compute Gini coefficient
ineq(x)
# compute Atkinson coefficient with parameter=0.5
ineq(x, parameter=0.5, type="Atkinson")


plot(Lc(x))

data(Ilocos)
head(Ilocos)


phil_lc <- Lc(Ilocos$income)
str(phil_lc)

phil_lc_df <- data_frame(p = phil_lc$p, L = phil_lc$L, L_general = phil_lc$L.general)
head(phil_lc_df)

ggplot(phil_lc_df, aes(x = p)) +
   geom_line(aes(y = L)) +
   geom_ribbon(aes(ymin = L, ymax = p), fill = "yellow", alpha = 0.5) +
   theme_minimal() +
   scale_x_continuous("Cumulative proportion of population", label = percent) +
   scale_y_continuous("Cumulative proportion of income", label = percent) +
   coord_equal() +
   annotate("text", x= 0.5, y = 0.53, label = "Perfect equality line", angle = 45) +
   annotate("text", x= 0.45, y = 0.1, label = "Lorenz curve") +
   annotate("text", x = 0.5, y = 0.35, label = "Gini area") +
   ggtitle("Income inequality in Ilocos, Philippines")
   

ggplot(phil_lc_df, aes(x = L, y =L_general)) + geom_point()
plot()
ineq(Ilocos$income)


incomes <- c(rnorm(1000, 1000, 100), rnorm(100, 10 ^ 6, 10 ^ 5), rnorm(30, 10^8, 10 ^ 7))
sim <- Lc(incomes)
sim <- data_frame(p = sim$p, L = sim$L)
ggplot(sim, aes(x = p)) +
   geom_line(aes(y = L)) +
   geom_ribbon(aes(ymin = L, ymax = p), fill = "yellow", alpha = 0.5) +
   theme_minimal() +
   scale_x_continuous("Cumulative proportion of population", label = percent) +
   scale_y_continuous("Cumulative proportion of income", label = percent) +
   coord_equal() +
   annotate("text", x= 0.5, y = 0.53, label = "Perfect equality line", angle = 45) +
   annotate("text", x= 0.45, y = 0.1, label = "Lorenz curve") +
   annotate("text", x = 0.5, y = 0.35, label = "Gini area") +
   ggtitle("Income inequality in simulated highly trimodal economy")


data_frame(incomes) %>%
   ggplot(aes(x = incomes)) +
   geom_density() +
   geom_rug() +
   scale_x_log10()
