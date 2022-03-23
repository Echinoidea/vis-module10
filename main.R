# Read Climate data csv
df <- read.csv("DailyDelhiClimateTrain.csv")


# ---- Using dataframes and ggplot2 ----

library(ggplot2)

# Convert date column (character) to date format
df$date <- as.Date(df$date, format="%Y-%m-%d")

# Plot meantemp with ggplot
ggplot(data=df, aes(x = date, y = meantemp, color = meantemp)) +
  geom_line() +
  geom_smooth(method = "lm", color = "black", size = 0.5, level = 1, se = FALSE) +
  scale_color_gradient(low = "deepskyblue2", high = "firebrick2") +
  labs(title = "Mean Temp over Time from 2013 to 2017 in Delhi",
       x = "Time",
       y = "Mean Temp") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "None"
  )

# Create boxplot showing meantemp averaged by month

library(dplyr)

df_grouped <- df %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(month, year) %>%
  group_by(month) %>%
  mutate(monthly_meantemp = aggregate(meantemp, list(month), FUN=mean)[,2]) %>%
  group_by(month, year)

df_grouped <- df_grouped[-1462,]

# meantemp seasonal effect
ggplot(data=df_grouped, aes(x = month, y = meantemp, fill = monthly_meantemp)) +
  geom_boxplot() +
  scale_fill_gradient(low = "lightblue2", high = "firebrick2") +
  labs(title = "Mean Temp by Month",
       subtitle = "Average monthly temperature from 2013 to 2017",
       x = "Month",
       y = "Mean Temp") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "None"
  )

