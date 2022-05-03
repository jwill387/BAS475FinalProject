Credit <- read.csv("credit.csv")

Credit <- Credit %>%
  mutate(Time = rev(row_number()))%>%
  as_tsibble(index = Time)
  
gg_tsdisplay(Credit, plot_type = "partial")

# -----------------------------------------------------------------------------

Training <- Credit[1:(nrow(Credit)-12),]
Holdout <- Credit[481:492,]

# -----------------------------------------------------------------------------

lambda <- Training %>%
  features(ï..credit_in_millions, features=guerrero)%>%
  pull(lambda_guerrero)

Training <- Training %>%
  mutate(bc_training=box_cox(ï..credit_in_millions, lambda))

Training <- Training %>%
  mutate(bcd_training=difference(bc_training))

Training %>%
  autoplot(bcd_training)

# -----------------------------------------------------------------------------

fit <- Training %>%
  model(
    arima = ARIMA(bcd_training)
  )

fit %>%
  glance()

fit %>%
  select(arima) %>%
  forecast(h = 12) %>%
  autoplot(Training)

# -----------------------------------------------------------------------------


