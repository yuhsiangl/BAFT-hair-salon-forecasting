knitr::opts_chunk$set(echo = TRUE)
library(data.table)
library(car)
library(purrr)
source('../_load_packages.R')
library(ggtime)
Sys.setenv(lang="en-US")
Sys.setlocale("LC_TIME", "en_US.UTF-8")


origin_data <- read.csv("../data.csv")
origin_data <- origin_data %>% 
  select(Unique.ID, Order.Creation.Time, lunar_date, Branch.Key) %>%
  mutate(Order.Creation.Time = as.Date(Order.Creation.Time,  format = "%m/%d/%Y"),
         lunar_date = as.Date(lunar_date,  format = "%m/%d/%Y")
         ) %>% 
  filter(Order.Creation.Time >= "2021-11-01",
         Branch.Key %in% c("A01", "A02", "A04", "A05", "A07", "A08", "A09"))


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


data_A01 <- data %>% 
  filter(Branch.Key == "A01") 

# fixed
train_end <-  "2023 Oct"
valid_start <- "2024 Jan"
train.data.A01 <- data_A01 %>% filter_index(~ train_end)
valid.data.A01 <- data_A01 %>% filter_index(valid_start ~ .)
max_traffic.A01 <- max(data_A01$Customer.Traffic) 

fit.naive.A01 <- train.data.A01 %>%
  model(
    ETS_raw = ETS(Customer.Traffic),
    ETS_log = ETS(log(Customer.Traffic + 1))
  )

fc.naive.A01.fixed <- fit.naive.A01 %>%
  forecast(valid.data.A01)

# roll-forward
lengthTrainPeriod.A01 <- nrow(train.data.A01)
rollingWindowSize <- 3

data_tr.A01 <- data_A01 |> 
  slice(1:(n()-rollingWindowSize)) |>
  stretch_tsibble(.init=lengthTrainPeriod.A01, .step= 1)

fc.naive.A01 <- data_tr.A01 |>
  model(
    ETS_raw = ETS(Customer.Traffic),
    ETS_log = ETS(log(Customer.Traffic + 1))
  ) |>
  forecast(h=rollingWindowSize) %>% 
  group_by(.id, .model) %>%
  slice(rollingWindowSize) %>% 
  ungroup()


p.naive.A01 <-  data_A01 %>% 
  autoplot(Customer.Traffic) +

  autolayer(
    fitted(fit.naive.A01) %>% filter(.model == "ETS_raw", !is.na(.fitted)),
    .fitted,
    color = "lightcoral",
    linewidth = 0.8
  ) +

  autolayer(
    fitted(fit.naive.A01) %>% filter(.model == "ETS_log", !is.na(.fitted)),
    .fitted,
    color = "lightskyblue3",
    linewidth = 0.8
  ) +

  geom_line(
    aes(y = .mean, color = "Roll-forward ETS Raw"),
    data = fc.naive.A01 %>% filter(.model == "ETS_raw"),
    linetype = "dashed",
    linewidth = 0.8
  ) +

  geom_line(
    aes(y = .mean, color = "Roll-forward ETS Log"),
    data = fc.naive.A01 %>% filter(.model == "ETS_log"),
    linetype = "dashed",
    linewidth = 0.8
  ) +

  geom_line(
    aes(y = .mean, color = "Fixed ETS Raw"),
    data = fc.naive.A01.fixed %>% filter(.model == "ETS_raw"),
    linetype = "solid",
    linewidth = 0.8
  ) +

  geom_line(
    aes(y = .mean, color = "Fixed ETS Log"),
    data = fc.naive.A01.fixed %>% filter(.model == "ETS_log"),
    linetype = "solid",
    linewidth = 0.8
  ) +

  geom_vline(
    xintercept = as.numeric(as.Date(yearmonth(train_end))),
    linetype = "solid",
    color = "grey55",
    linewidth = 0.6
  ) +

  geom_vline(
    xintercept = as.numeric(as.Date(yearmonth(valid_start))),
    linetype = "solid",
    color = "grey55",
    linewidth = 0.6
  ) +

  annotate(
    "segment",
    x = yearmonth(valid_start),
    y = max_traffic.A01 * 1.01,
    xend = yearmonth("2024 Dec"),
    yend = max_traffic.A01 * 1.01,
    arrow = arrow(length = unit(0.25, "cm"), ends = "both"),
    color = "grey55"
  ) +

  annotate(
    geom = "text",
    x = yearmonth("2024 Jul"),
    y = max_traffic.A01 * 1.05,
    label = "Validation",
    color = "grey37"
  ) +

  annotate(
    "segment",
    x = yearmonth(min(data_A01$Month)),
    y = max_traffic.A01 * 1.01,
    xend = yearmonth(train_end),
    yend = max_traffic.A01 * 1.01,
    arrow = arrow(length = unit(0.25, "cm"), ends = "both"),
    color = "grey55"
  ) +

  annotate(
    geom = "text",
    x = yearmonth("2023 Jan"),
    y = max_traffic.A01 * 1.05,
    label = "Training"
  ) +

  scale_color_manual(
    name = "Forecast Type",
    values = c(
      "Fixed ETS Raw" = "coral1",
      "Fixed ETS Log" = "deepskyblue3",
      "Roll-forward ETS Raw" = "coral1",
      "Roll-forward ETS Log" = "deepskyblue3"
    )
  ) +

  scale_x_yearmonth(date_breaks = "6 months", date_labels = "%Y %m") +

  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 90, hjust = 1),
    legend.position = "bottom"
  ) +

  labs(title = "A01 Forecast (ETS Auto)", x = "Time", y = "Customer Traffic")

p.naive.A01


fc.naive.res.A01 <- fc.naive.A01 |>
  left_join(
    valid.data.A01 |> select(Month, Actual = Customer.Traffic),
    by = "Month"
  ) |>
  mutate(rolled_errors = Actual - .mean)

df.errors.train <- augment(fit.naive.A01) %>%
  mutate(train_errors = Customer.Traffic - .fitted)

max_traffic_err.A01 <- max(
  abs(c(
    fc.naive.res.A01$rolled_errors,
    df.errors.train$train_errors
  )),
  na.rm = TRUE
)

p.errors <- df.errors.train %>%
  ggplot(aes(x = Month, y = train_errors, color = .model)) +

  geom_line(linewidth = 0.8) +

  geom_line(
    aes(Month, rolled_errors, color = .model),
    data = fc.naive.res.A01,
    linetype = "dashed",
    linewidth = 0.8
  ) +

  labs(title = "Errors", x = "Time", y = "Error", color = "Model") +

  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey40"
  ) +

  geom_vline(
    xintercept = as.numeric(as.Date(yearmonth(train_end))),
    linetype = "solid",
    color = "grey55",
    linewidth = 0.6
  ) +

  geom_vline(
    xintercept = as.numeric(as.Date(yearmonth(valid_start))),
    linetype = "solid",
    color = "grey55",
    linewidth = 0.6
  ) +

  annotate(
    "segment",
    x = yearmonth(valid_start),
    y = max_traffic_err.A01 * 0.95,
    xend = yearmonth("2024 Dec"),
    yend = max_traffic_err.A01 * 0.95,
    arrow = arrow(length = unit(0.25, "cm"), ends = "both"),
    color = "grey55"
  ) +

  annotate(
    geom = "text",
    x = yearmonth("2024 Jul"),
    y = max_traffic_err.A01 * 1.1,
    label = "Validation"
  ) +

  theme(
    axis.text.y = element_text(angle = 90, hjust = 1),
    legend.position = "bottom"
  ) +

  scale_x_yearmonth(date_breaks = "6 months", date_labels = "%Y %m") +

  scale_color_manual(
    values = c(
      "ETS_raw" = "#f28482",
      "ETS_log" = "#45adad"
    )
  )

p.errors


run_naive_model <- function(data, branch_id, train_end = "2023 Oct", valid_start = "2024 Jan",
                             rollingWindowSize = 3) {
  
  message("[", branch_id, "] Training model...")
  
  data_branch <- data %>% 
    filter(Branch.Key == branch_id)
  
  train.data <- data_branch %>% filter_index(~ train_end)
  valid.data <- data_branch %>% filter_index(valid_start ~ .)
  
  max_traffic <- max(data_branch$Customer.Traffic, na.rm = TRUE)
  
  fit <- train.data %>%
    model(
      ETS_raw = ETS(Customer.Traffic),
      ETS_log = ETS(log(Customer.Traffic + 1))
    )
  
  # roll-forward ETS
  message("[", branch_id, "] Roll-forward forecasting...")
  lengthTrainPeriod <- nrow(train.data)
  
  data_tr <- data_branch |> 
    slice(1:(n() - rollingWindowSize)) |>
    stretch_tsibble(.init = lengthTrainPeriod, .step = 1)
  
  fc.roll <- data_tr |>
    model(
      ETS_raw = ETS(Customer.Traffic),
      ETS_log = ETS(log(Customer.Traffic + 1))
    ) |>
    forecast(h = rollingWindowSize) %>% 
    group_by(.id, .model) %>%
    slice(rollingWindowSize) %>%
    ungroup()
  
  # forecast plot
  message("[", branch_id, "] Drawing forecast plot...")
  
  p.forecast <- data_branch %>% 
    autoplot(Customer.Traffic) +

    autolayer(
      fitted(fit) %>% filter(.model == "ETS_raw", !is.na(.fitted)),
      .fitted,
      color = "lightcoral",
      linewidth = 0.8
    ) +

    autolayer(
      fitted(fit) %>% filter(.model == "ETS_log", !is.na(.fitted)),
      .fitted,
      color = "lightskyblue3",
      linewidth = 0.8
    ) +

    geom_line(
      aes(y = .mean, color = "ETS Raw"),
      data = fc.roll %>% filter(.model == "ETS_raw"),
      linetype = "dashed",
      linewidth = 0.8
    ) +

    geom_line(
      aes(y = .mean, color = "ETS Log"),
      data = fc.roll %>% filter(.model == "ETS_log"),
      linetype = "dashed",
      linewidth = 0.8
    ) +

    labs(
      title = paste0(branch_id, " Forecast (ETS Auto)"),
      x = "Time",
      y = "Customer Traffic",
      color = "Model"
    ) +

    geom_vline(
      xintercept = as.numeric(as.Date(yearmonth(train_end))),
      linetype = "dashed",
      color = "grey55",
      linewidth = 0.6
    ) +

    geom_vline(
      xintercept = as.numeric(as.Date(yearmonth(valid_start))),
      linetype = "dashed",
      color = "grey55",
      linewidth = 0.6
    ) +

    annotate(
      "segment",
      x = yearmonth(valid_start),
      y = max_traffic * 1.01,
      xend = max(valid.data$Month),
      yend = max_traffic * 1.01,
      arrow = arrow(length = unit(0.25, "cm"), ends = "both"),
      color = "grey55"
    ) +

    annotate(
      geom = "text",
      x = yearmonth("2024 Jul"),
      y = max_traffic * 1.05,
      label = "Validation",
      color = "grey37"
    ) +

    annotate(
      "segment",
      x = min(data_branch$Month),
      y = max_traffic * 1.01,
      xend = yearmonth(train_end),
      yend = max_traffic * 1.01,
      arrow = arrow(length = unit(0.25, "cm"), ends = "both"),
      color = "grey55"
    ) +

    annotate(
      geom = "text",
      x = yearmonth("2023 Jan"),
      y = max_traffic * 1.05,
      label = "Training"
    ) +

    scale_color_manual(
      values = c(
        "ETS Raw" = "coral1",
        "ETS Log" = "deepskyblue3"
      )
    ) +

    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(angle = 90, hjust = 1),
      legend.position = "bottom"
    ) +

    scale_x_yearmonth(date_breaks = "6 months", date_labels = "%Y %m")
  
  # error plot
  message("[", branch_id, "] Drawing error plot...")
  
  fc.res <- fc.roll |>
    left_join(
      valid.data |> select(Month, Actual = Customer.Traffic),
      by = "Month"
    ) |>
    mutate(rolled_errors = Actual - .mean)
  
  df.errors.train <- augment(fit) %>%
    mutate(train_errors = Customer.Traffic - .fitted)
  
  max_traffic_err <- max(
    abs(c(
      fc.res$rolled_errors,
      df.errors.train$train_errors
    )),
    na.rm = TRUE
  )
  
  p.error <- df.errors.train %>%
    ggplot(aes(x = Month, y = train_errors, color = .model)) +

    geom_line(linewidth = 0.8) +

    geom_line(
      aes(Month, rolled_errors, color = .model),
      data = fc.res,
      linetype = "dashed",
      linewidth = 0.8
    ) +

    labs(
      title = paste0(branch_id, " Errors"),
      x = "Time",
      y = "Error",
      color = "Model"
    ) +

    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "grey40"
    ) +

    geom_vline(
      xintercept = as.numeric(as.Date(yearmonth(train_end))),
      linetype = "solid",
      color = "grey55",
      linewidth = 0.6
    ) +

    geom_vline(
      xintercept = as.numeric(as.Date(yearmonth(valid_start))),
      linetype = "solid",
      color = "grey55",
      linewidth = 0.6
    ) +

    annotate(
      "segment",
      x = yearmonth(valid_start),
      y = max_traffic_err * 0.95,
      xend = max(valid.data$Month),
      yend = max_traffic_err * 0.95,
      arrow = arrow(length = unit(0.25, "cm"), ends = "both"),
      color = "grey55"
    ) +

    annotate(
      geom = "text",
      x = yearmonth("2024 Jul"),
      y = max_traffic_err * 1.1,
      label = "Validation"
    ) +

    theme(
      axis.text.y = element_text(angle = 90, hjust = 1),
      legend.position = "bottom"
    ) +

    scale_x_yearmonth(date_breaks = "6 months", date_labels = "%Y %m") +

    scale_color_manual(
      values = c(
        "ETS_raw" = "#f28482",
        "ETS_log" = "#45adad"
      )
    )
  
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


branch_list <- c("A01", "A02", "A04", "A05", "A07", "A08", "A09")
library(progress)

# Progress bar
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

p.valid.errors.raw <- valid_errors_all %>%
  filter(.model == "ETS_raw") %>%
  ggplot(aes(x = Month, y = rolled_errors, color = Branch.Key)) +
  geom_line(linewidth = 0.8, linetype = "dashed") +
  labs(
    title = "Validation Errors Across Branches: ETS Raw",
    x = "Time",
    y = "Rolling 3-Step Ahead Error",
    color = "Branch"
  ) +
  scale_x_yearmonth(date_breaks = "1 month", date_labels = "%Y %m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p.valid.errors.raw


p.valid.errors.log <- valid_errors_all %>%
  filter(.model == "ETS_log") %>%
  ggplot(aes(x = Month, y = rolled_errors, color = Branch.Key)) +
  geom_line(linewidth = 0.8, linetype = "dashed") +
  labs(
    title = "Validation Errors Across Branches: ETS Log",
    x = "Time",
    y = "Rolling 3-Step Ahead Error",
    color = "Branch"
  ) +
  scale_x_yearmonth(date_breaks = "1 month", date_labels = "%Y %m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p.valid.errors.log


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


fabletools::report(result_list$A01$train_fit)
