# setup
knitr::opts_chunk$set(echo = TRUE)
library(data.table)
library(car)
library(purrr)
source('../_load_packages.R')
library(ggtime)
Sys.setenv(lang="en-US")
Sys.setlocale("LC_TIME", "en_US.UTF-8")



# Load data
origin_data <- read.csv("../data.csv")
origin_data <- origin_data %>% 
  select(Unique.ID, Order.Creation.Time, lunar_date, Branch.Key) %>%
  mutate(Order.Creation.Time = as.Date(Order.Creation.Time,  format = "%m/%d/%Y"),
         lunar_date = as.Date(lunar_date,  format = "%m/%d/%Y")
         ) %>% 
  filter(Order.Creation.Time >= "2021-11-01",
         Branch.Key %in% c("A01", "A02", "A04", "A05", "A07", "A08", "A09"))


# One month before Lunar New Year: 11/29 ~ 12/29
# Lunar New Year period: Lunar New Year's Eve (12/30) ~ Day 4 (1/4)
# One month after Lunar New Year: 1/5 ~ 2/4

# Lunar dates cannot be inferred directly from missing records (stores are closed during some days)
# Lunar New Year in 2022 was 1/30~2/4 (Jan & Feb)
# Lunar New Year in 2023 was 1/20~1/25 (Jan)
# Lunar New Year in 2024 was 2/8~2/13 (Feb)

# March 2023 belongs to the leap second lunar month (not within one month after Lunar New Year), so AfterNewYear is set to 0

data <- origin_data %>% 
  mutate(
    Month = floor_date(Order.Creation.Time, unit = "month"),
    Month = yearmonth(Month)
  ) %>%
  group_by(Branch.Key, Month) %>%
  summarise(
    Customer.Traffic = n(),
    lunar_date_start = min(lunar_date, na.rm = TRUE),
    lunar_date_end = max(lunar_date, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  as_tsibble(index = Month, key = Branch.Key) %>% 
  fill_gaps() %>% 
  group_by_key() %>%
  mutate(
    Customer.Traffic = if_else(
      is.na(Customer.Traffic),
      mean(Customer.Traffic, na.rm = TRUE),
      Customer.Traffic
    )
  ) %>%
  ungroup() %>%  
  mutate(
    lunar_start_md = format(lunar_date_start, "%m-%d"),
    lunar_end_md   = format(lunar_date_end, "%m-%d"),
    BeforeNewYear = ifelse(lunar_end_md >= "11-30" & lunar_start_md <= "12-30", 1, 0),
    AfterNewYear = ifelse(lunar_end_md >= "01-05" & lunar_start_md <= "02-04",1, 0),
    NewYear = as.integer(
      Month %in% yearmonth(c("2022 Jan", "2022 Feb", "2023 Jan", "2024 Feb"))
    )
  ) %>% 
  mutate(
    BeforeNewYear = replace_na(BeforeNewYear, 0),
    AfterNewYear = replace_na(AfterNewYear, 0),
    NewYear = replace_na(NewYear, 0),
    AfterNewYear = ifelse(Month == yearmonth("2023 Mar"),0,AfterNewYear)
  )  %>% 
  select(Branch.Key, Month, Customer.Traffic, BeforeNewYear, NewYear, AfterNewYear)
  

data




# # Run A01 first

data_A01 <- data %>% 
  filter(Branch.Key == "A01") 

# fixed
train_end <-  "2023 Dec"
valid_start <- "2024 Jan"
train.data.A01 <- data_A01 %>% filter_index(~ train_end)
valid.data.A01 <- data_A01 %>% filter_index(valid_start ~ .)
max_traffic.A01 <- max(data_A01$Customer.Traffic) 

fit.naive.A01 <- train.data.A01 %>%
  model(NAIVE(Customer.Traffic))

fc.naive.A01.fixed <- fit.naive.A01%>%
  forecast(valid.data.A01)

# roll-forward
lengthTrainPeriod.A01 <- nrow(train.data.A01)
rollingWindowSize <- 1

data_tr.A01 <- data_A01 |> 
  slice(1:(n()-rollingWindowSize)) |>
  stretch_tsibble(.init=lengthTrainPeriod.A01, .step= 1)

# Naive
fc.naive.A01 <- data_tr.A01|>
  model(naive_model = NAIVE(Customer.Traffic)) |>
  forecast(h=rollingWindowSize) %>% 
  group_by(.id) %>%
  slice(1) %>% # 只取第3個
  ungroup()


# # Forecast plot

p.naive.A01 <-  data_A01 %>% 
  autoplot(Customer.Traffic) +
  autolayer(fitted(fit.naive.A01) %>% filter(!is.na(.fitted)), .fitted, color="coral1", linewidth = 0.8) +
  geom_line(aes(y = .mean, color="Roll-forward"), data = fc.naive.A01, linetype = "solid", linewidth = 0.8) +
  geom_line(aes(y = .mean, color = "Fixed"), data = fc.naive.A01.fixed,linetype = "solid",linewidth = 0.8) + 
  geom_vline(xintercept = as.numeric(as.Date(yearmonth(train_end))),linetype = "solid",color = "grey55",linewidth = 0.6) +
  geom_vline(xintercept = as.numeric(as.Date(yearmonth(valid_start))),linetype = "solid",color = "grey55",linewidth = 0.6) +
  annotate("segment", x = yearmonth(valid_start), y = max_traffic.A01 * 1.01, 
           xend = yearmonth("2024 Dec"), yend = max_traffic.A01 * 1.01,
           arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "grey55") +
  annotate(geom = "text",x = yearmonth("2024 Jul"),y = max_traffic.A01 * 1.05,label = "Validation",color = "grey37") +
  annotate("segment", x = yearmonth(min(data_A01$Month)), y = max_traffic.A01 * 1.01, 
           xend = yearmonth(train_end), yend = max_traffic.A01 * 1.01,
           arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "grey55") + 
  annotate(geom = "text",x = yearmonth("2023 Jan"),y = max_traffic.A01 *1.05,label = "Training") +
  scale_color_manual(name = "Forecast Type", values = c("Fixed" = "blue","Roll-forward" = "coral1")) +
  scale_x_yearmonth(date_breaks = "6 months", date_labels = "%Y %m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 90, hjust = 1)) +
  labs(title = "A01 Forecast (Naive)", x = "Time", y = "Customer Traffic")

p.naive.A01


# # error plot

fc.naive.res.A01 <- fc.naive.A01 |>
  left_join(valid.data.A01, by = "Month") |>
  mutate(rolled_errors = valid.data.A01$Customer.Traffic - fc.naive.A01$.mean)

df.errors.train <- train.data.A01 %>% 
  mutate(three.month.ahead.error = Customer.Traffic - lag(Customer.Traffic, rollingWindowSize)) %>% 
  filter(!is.na(three.month.ahead.error))

max_traffic_err.A01 <- max(abs(fc.naive.res.A01$rolled_errors), na.rm = TRUE)

p.errors <- df.errors.train %>%
  autoplot(three.month.ahead.error, linewidth = 0.8, aes(color = "Train")) +
  geom_line(aes(Month, rolled_errors, color="Validation"),data = fc.naive.res.A01,linetype = "dashed",linewidth = 0.8) +
  labs(title = "Errors", x = "Time", y = "Error") +
  geom_vline(xintercept = as.numeric(as.Date(yearmonth(train_end))),linetype = "solid",color = "grey55",linewidth = 0.6) +
  geom_vline(xintercept = as.numeric(as.Date(yearmonth(valid_start))),linetype = "solid",color = "grey55",linewidth = 0.6) +
  annotate("segment", x = yearmonth(valid_start), y = max_traffic_err.A01 * 0.95, 
           xend = yearmonth("2024 Dec"), yend = max_traffic_err.A01 * 0.95,
           arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "grey55") +
  annotate(geom = "text", x = yearmonth("2024 Jul"), y = max_traffic_err.A01 * 1.1, label = "Validation") +
  theme(axis.text.y = element_text(angle = 90, hjust = 1)) +
#  theme(legend.position = "none") +
  scale_x_yearmonth(date_breaks = "6 months", date_labels = "%Y %m") +
  scale_color_manual(name = "type", values = c("Train" = "#f28482","Validation" = "#45adad"))

p.errors


# run 7 branches
# roll-forward
run_naive_model <- function(data, branch_id, train_end = "2023 Oct", valid_start = "2024 Jan",
                             rollingWindowSize = 3) {
  
  message("[", branch_id, "] Training model...")
  
  data_branch <- data %>% 
    filter(Branch.Key == branch_id)
  
  train.data <- data_branch %>% filter_index(~ train_end)
  valid.data <- data_branch %>% filter_index(valid_start ~ .)
  
  max_traffic <- max(data_branch$Customer.Traffic, na.rm = TRUE)
  
  fit <- train.data %>%
    model(naive_model = NAIVE(Customer.Traffic))
  
  # roll-forward naive
  message("[", branch_id, "] Roll-forward forecasting...")
  lengthTrainPeriod <- nrow(train.data)
  
  data_tr <- data_branch |> 
    slice(1:(n() - rollingWindowSize)) |>
    stretch_tsibble(.init = lengthTrainPeriod, .step = 1)
  
  fc.roll <- data_tr |>
    model(naive_model = NAIVE(Customer.Traffic)) |>
    forecast(h = rollingWindowSize) %>% 
    group_by(.id) %>%
    slice(rollingWindowSize) %>%
    ungroup()
  
  # forecast plot
   message("[", branch_id, "] Drawing forecast plot...")
  
   p.forecast <- data_branch %>% 
    autoplot(Customer.Traffic) +
    autolayer(fitted(fit) %>% filter(!is.na(.fitted)), .fitted, color="coral1", linewidth = 0.8) +
    geom_line(aes(y = .mean),data = fc.roll,linetype = "dashed", color="coral1", linewidth = 0.8) +
    labs(title = paste0(branch_id, " Forecast (Naive)"),x = "Time",y = "Customer Traffic") +
    geom_vline(xintercept = as.numeric(as.Date(yearmonth(train_end))),linetype = "dashed",color = "grey55",linewidth = 0.6) +
    geom_vline(xintercept = as.numeric(as.Date(yearmonth(valid_start))),linetype = "dashed",color = "grey55",linewidth = 0.6) +
    annotate("segment", x = yearmonth(valid_start), y = max_traffic * 1.01, xend = max(valid.data$Month), yend = max_traffic * 1.01,
      arrow = arrow(length = unit(0.25, "cm"), ends = "both"),color = "grey55") +
    annotate(geom = "text",x = yearmonth("2024 Jul"),y = max_traffic * 1.05,label = "Validation",color = "grey37") +
    annotate("segment", x = min(data_branch$Month), y = max_traffic * 1.01, xend = yearmonth(train_end), yend = max_traffic * 1.01,
      arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "grey55") +
    annotate(geom = "text", x = yearmonth("2023 Jan"), y = max_traffic * 1.05, label = "Training") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 90, hjust = 1)) +
    scale_x_yearmonth(date_breaks = "6 months", date_labels = "%Y %m")
  
  # error plot
   message("[", branch_id, "] Drawing error plot...")
  
   fc.res <- fc.roll |>
    left_join(valid.data |> select(Month, Actual = Customer.Traffic),by = "Month") |>
    mutate(rolled_errors = Actual - .mean)
  
   df.errors.train <- train.data %>% 
    mutate(three.month.ahead.error = Customer.Traffic - lag(Customer.Traffic, rollingWindowSize)) %>% 
    filter(!is.na(three.month.ahead.error))
  
   max_traffic_err <- max(abs(fc.res$rolled_errors), na.rm = TRUE)
  
   p.error <- df.errors.train %>%
    autoplot(three.month.ahead.error, linewidth = 0.8, aes(color = "Train")) +
    geom_line(aes(Month, rolled_errors, color="Validation"),data = fc.res,linetype = "dashed",linewidth = 0.8) +
    labs(title = paste0(branch_id, " Errors (Naive)"), x = "Time", y = "Error") +
    geom_vline(xintercept = as.numeric(as.Date(yearmonth(train_end))), linetype = "solid", color = "grey55", linewidth = 0.6) +
    geom_vline( xintercept = as.numeric(as.Date(yearmonth(valid_start))), linetype = "solid", color = "grey55", linewidth = 0.6) +
    annotate("segment", x = yearmonth(valid_start), y = max_traffic_err * 0.95, xend = max(valid.data$Month), yend = max_traffic_err * 0.95,
      arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "grey55") +
    annotate(geom = "text", x = yearmonth("2024 Jul"), y = max_traffic_err * 1.1, label = "Validation") +
    theme(axis.text.y = element_text(angle = 90, hjust = 1)) +
    scale_x_yearmonth(date_breaks = "6 months", date_labels = "%Y %m") +
    scale_color_manual(name = "type", values = c("Train" = "#f28482","Validation" = "#45adad"))
  
   message("[", branch_id, "] Finished.")
  
  return(list(
    branch = branch_id,
    train_fit = fit,
    roll_forecast = fc.roll,
    roll_result = fc.res,
    forecast_plot = p.forecast,
    error_plot = p.error
  ))
}


# run function for all branches
branch_list <- c("A01", "A02", "A04", "A05", "A07", "A08", "A09")
library(progress)

# progress bar setup
pb <- progress_bar$new(
  total = length(branch_list),
  format = "[:bar] :current/:total :percent | Running :branch"
)

result_list <- list()

for (b in branch_list) {
  cat("\014")  # clear console
  pb$tick(tokens = list(branch = b))
  message("")
  result_list[[b]] <- run_naive_model(data, b)
}

message("Done.")



# plots
# forecast plot
for (b in names(result_list)) {
  print(result_list[[b]]$forecast_plot)
}


# error plot
for (b in names(result_list)) {
  print(result_list[[b]]$error_plot)
}

valid_errors_all <- bind_rows(
  lapply(result_list, function(x) {
    x$roll_result %>%
      mutate(Branch.Key = x$branch)
  })
)

p.valid.errors.all <- valid_errors_all %>%
  ggplot(aes(x = Month, y = rolled_errors, color = Branch.Key)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Validation Errors Across Branches (Naive)",
    x = "Time",
    y = "Rolling 3-Step Ahead Error",
    color = "Branch"
  ) +
  scale_x_yearmonth(date_breaks = "1 month", date_labels = "%Y %m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p.valid.errors.all


# accuracy

accuracy_table <- map_dfr(result_list, function(x){

  train_acc <- accuracy(x$train_fit) %>%
    mutate(
      Branch.Key = x$branch,
      Period = "Train"
    ) %>%
    select(Branch.Key, Period, .model, RMSE, MAPE)

  test_acc <- accuracy(
    x$roll_forecast,
    data %>%
      filter(
        Branch.Key == x$branch,
        Month >= yearmonth("2024 Jan")
      )
  ) %>%
    mutate(
      Branch.Key = x$branch,
      Period = "Test"
    ) %>%
    select(Branch.Key, Period, .model, RMSE, MAPE)

  bind_rows(train_acc, test_acc)
})

accuracy_table
