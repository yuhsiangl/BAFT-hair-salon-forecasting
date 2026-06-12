# setup
knitr::opts_chunk$set(echo = TRUE)
library(data.table)
library(car)
library(purrr)
source('./_load_packages.R')
library(ggtime)
Sys.setenv(lang="en-US")
Sys.setlocale("LC_TIME", "en_US.UTF-8")


origin_data <- read.csv("data.csv")
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
  fill_gaps(Customer.Traffic = 0) %>% 
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


impute_value <- mean(
  data$Customer.Traffic[data$Branch.Key == "A07" & data$Customer.Traffic != 0],
  na.rm = TRUE
)

data <- data %>%
  mutate(
    Customer.Traffic = ifelse(
      Branch.Key == "A07" & Customer.Traffic == 0,
      impute_value,
      Customer.Traffic
    )
  )

data


# choose the branch you want to forecast, or comment out to forecast all branches
# data <- data %>% filter(Branch.Key %in% c("A07")) 

stepAhead <- 3
augment_list <- list()
roll_fc <- data %>%
  group_by_key() %>%
  group_modify(~ {
    branch_name <- .y$Branch.Key
    branch_data <- .x %>%
      mutate(Branch.Key = branch_name) %>%
      as_tsibble(index = Month, key = Branch.Key)
    valid_part <- branch_data %>% 
      filter(Month > yearmonth("2023 Oct"))
    pred_raw       <- rep(NA, 12)
    pred_raw_lower <- rep(NA, 12)
    pred_raw_upper <- rep(NA, 12)
    pred_log       <- rep(NA, 12)
    pred_log_lower <- rep(NA, 12)
    pred_log_upper <- rep(NA, 12)
    pred_naive <- rep(NA, 12)
    
    for(i in 1:12){
      train_end <- yearmonth("2023 Oct") + (i - 1)
      train_roll <- branch_data %>%
        filter(Month <= train_end)
      fit_roll <- train_roll %>%
        model(
          arima_raw = ARIMA(Customer.Traffic ~ BeforeNewYear + NewYear + AfterNewYear),
          arima_log = ARIMA(log1p(Customer.Traffic) ~ BeforeNewYear + NewYear + AfterNewYear),
          naive = NAIVE(Customer.Traffic)
        )
      future_part <- branch_data %>% 
        filter(Month >= train_end + 1, Month <= train_end + stepAhead) 
      print(future_part)
      
      fc_roll <- fit_roll %>% forecast(new_data = future_part) 
      print(fc_roll)
      
      fc_with_pi <- fc_roll %>%
        hilo(level = 90) %>%
        unpack_hilo(`90%`)   
      
      raw_row <- fc_with_pi %>%
        filter(.model == "arima_raw") %>%
        slice(stepAhead)
      pred_raw[i]       <- raw_row %>% pull(.mean)
      pred_raw_lower[i] <- raw_row %>% pull(`90%_lower`)
      pred_raw_upper[i] <- raw_row %>% pull(`90%_upper`)
      
      log_row <- fc_with_pi %>%
        filter(.model == "arima_log") %>%
        slice(stepAhead)
      pred_log[i]       <- log_row %>% pull(.mean)
      pred_log_lower[i] <- log_row %>% pull(`90%_lower`)
      pred_log_upper[i] <- log_row %>% pull(`90%_upper`)
      
      pred_naive[i] <- fc_roll %>% 
        filter(.model == "naive") %>% 
        slice(stepAhead) %>% 
        pull(.mean)
    }
    
    augment_list[[branch_name]] <<- augment(fit_roll)  
    valid_part <- branch_data %>% 
      filter(Month >= yearmonth("2024 Jan"))
    valid_part %>%
      as_tibble() %>%
      select(-Branch.Key) %>%
      mutate(
        predict_raw       = pred_raw,
        predict_raw_lower = pred_raw_lower,
        predict_raw_upper = pred_raw_upper,
        predict_log       = pred_log,
        predict_log_lower = pred_log_lower,
        predict_log_upper = pred_log_upper,
        predict_naive = pred_naive
      )
  }) %>%
  ungroup()
augment_all <- bind_rows(augment_list, .id = "Branch.Key") 


roll_fc

augment_all <- augment_all %>%filter(Month <= yearmonth("2023 Oct"))  #training結尾
augment_all

df.errors.roll <- roll_fc %>%
  mutate(
    raw_errors = Customer.Traffic - predict_raw,
    log_errors = Customer.Traffic - predict_log,
    nai_errors = Customer.Traffic - predict_naive
  )
df.errors.roll


branches <- unique(data$Branch.Key)

for (branch in branches) {
  branch_data <- data[data$Branch.Key == branch, ]
  branch_train <- augment_all %>%filter(Branch.Key == branch) 
  branch_fc <- roll_fc[roll_fc$Branch.Key == branch, ]
  
  p <- autoplot(branch_data, Customer.Traffic, linewidth = 0.8) +
    geom_line(
      aes(Month, .fitted, color = "ARIMA Raw"),
      data = branch_train %>% filter(.model == "arima_raw"),
      linewidth = 1
    )+
    geom_line(
      aes(Month, .fitted, color = "ARIMA Log"),
      data = branch_train %>% filter(.model == "arima_log"),
      linewidth = 1
    )+
    geom_line(
      aes(Month, predict_raw, color = "ARIMA Raw"),
      data = branch_fc,
      linetype = "dashed", 
      linewidth = 1
    ) +
    geom_line(
      aes(Month, predict_log, color = "ARIMA Log"),
      data = branch_fc,
      linetype = "dashed", 
      linewidth = 1
    ) +
    scale_color_manual(
      name = "Model Type", 
      values = c("ARIMA Raw" = "#FC5B47", "ARIMA Log" = "#1187C2") 
    ) +
    xlab("Time") +
    ylab("Customer Traffic") +
    ggtitle(paste("Branch:", branch)) +
    geom_vline(
      xintercept = as.numeric(as.Date(yearmonth("2023 Oct"))),
      linetype = "dashed",
      color = "grey55", 
      linewidth = 0.6
    ) +
    geom_vline(
      xintercept = as.numeric(as.Date(yearmonth("2024 Jan"))),
      linetype = "dashed",
      color = "grey55", 
      linewidth = 0.6
    ) +
    scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
    theme(legend.position = "bottom")
  
  print(p)
}


branches <- unique(data$Branch.Key)

for (branch in branches) {
  branch_data <- data[data$Branch.Key == branch, ]
  branch_train <- augment_all %>%filter(Branch.Key == branch) 
  branch_fc <- roll_fc[roll_fc$Branch.Key == branch, ]
  
  p <- autoplot(branch_data, Customer.Traffic, linewidth = 1, color="black") +
    geom_ribbon(
      aes(x = Month, ymin = predict_raw_lower, ymax = predict_raw_upper),
      data = branch_fc,
      alpha = 0.5,
      fill = '#FEECA4'
    ) +
    geom_line(
      aes(Month, .fitted, color = "ARIMA Raw"),
      data = branch_train %>% filter(.model == "arima_raw"),
      alpha = 0.7,
      linewidth = 1.5
    )+
    geom_line(
      aes(Month, predict_raw, color = "ARIMA Raw"),
      alpha = 0.7,
      data = branch_fc,
      linetype = "solid", 
      linewidth = 1.5,
    ) +
    geom_line(
      aes(Month, predict_naive, color = "NAIVE"),
      data = branch_fc,
      linetype = "11",
      linewidth = 1.5
    ) +
    geom_line(
      aes(Month, .fitted, color = "NAIVE"),
      data = branch_train %>% filter(.model == "naive"),
      linetype = "11",
      linewidth = 1.5
    ) +
    scale_color_manual(
      name = "Model", 
      values = c("ARIMA Raw" = "#FFBD3D", "NAIVE" = "#6DC4A7") 
    ) +
    xlab("") +
    ylab("Customer Traffic") +
    ggtitle(paste("Actual vs. Forecast:", branch)) +
    geom_vline(
      xintercept = as.numeric(as.Date(yearmonth("2023 Oct"))),
      color = "grey80", 
      linewidth = 0.4
    ) +
    geom_vline(
      xintercept = as.numeric(as.Date(yearmonth("2024 Jan"))),
      color = "grey80", 
      linewidth = 0.4
    ) +
    scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  
  print(p)
}



branches <- unique(data$Branch.Key)

for (branch in branches) {
  branch_data <- data[data$Branch.Key == branch, ]
  branch_train <- augment_all %>%filter(Branch.Key == branch) 
  branch_fc <- roll_fc[roll_fc$Branch.Key == branch, ]
  
  p <- autoplot(branch_data, Customer.Traffic, linewidth = 1, color="black") +

    geom_line(
      aes(Month, .fitted, color = "NAIVE"),
      data = branch_train %>% filter(.model == "naive"),
      linetype = "11",
      linewidth = 1.5
    )+
    geom_line(
      aes(Month, predict_naive, color = "NAIVE"),
      data = branch_fc,
      linetype = "11",
      linewidth = 1.5
    ) +
    scale_color_manual(
      name = "Model", 
      values = c("NAIVE" = "#6DC4A7")
    ) +
    xlab("") +
    ylab("Customer Traffic") +
    ggtitle(paste("Actual vs. Forecast:", branch)) +
    geom_vline(
      xintercept = as.numeric(as.Date(yearmonth("2023 Oct"))),
      color = "grey80", 
      linewidth = 0.4
    ) +
    geom_vline(
      xintercept = as.numeric(as.Date(yearmonth("2024 Jan"))),
      color = "grey80", 
      linewidth = 0.4
    ) +
    scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  
  print(p)
}




branches <- unique(data$Branch.Key)

for (branch in branches) {
  branch_data <- data[data$Branch.Key == branch, ]
  branch_train <- augment_all %>%
                  filter(Branch.Key == branch, Month <= yearmonth("2023 Oct"))  #training結尾
  branch_fc <- df.errors.roll[df.errors.roll$Branch.Key == branch, ]
  
  p <- ggplot() +
    geom_line(
      aes(Month, .resid, color = "ARIMA Raw"),
      data = branch_train %>% filter(.model == "arima_raw"),
      linewidth = 0.8
    )+
    geom_line(
      aes(Month, .resid, color = "ARIMA Log"),
      data = branch_train %>% filter(.model == "arima_log"),
      linewidth = 0.8
    )+
    geom_line(
      aes(Month, raw_errors, color = "ARIMA Raw"),
      data = branch_fc,
      linewidth = 0.8
    ) +
    geom_line(
      aes(Month, log_errors, color = "ARIMA Log"),
      data = branch_fc,
      linewidth = 0.8
    ) +
    scale_color_manual(
      name = "Model Type", 
      values = c("ARIMA Raw" = "#FC5B47", "ARIMA Log" = "#1187C2") 
    ) +
    xlab("Time") +
    ylab("Customer Traffic") +
    ggtitle(paste("Complete Error Timeline:", branch)) +
    geom_vline(
      xintercept = as.numeric(as.Date(yearmonth("2023 Oct"))),
      linetype = "dashed",
      color = "grey55", 
      linewidth = 0.6
    ) +
    geom_vline(
      xintercept = as.numeric(as.Date(yearmonth("2024 Jan"))),
      linetype = "dashed",
      color = "grey55", 
      linewidth = 0.6
    ) +
    geom_hline(yintercept = 0, color = "grey55", linetype = "solid",  linewidth = 0.6) +
    scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
    theme(legend.position = "bottom")
  
  print(p)
}


#Training -----------------------------------------------------
augment_all |>
  as_tibble() |> 
  group_by(Branch.Key, .model) |>
  summarise(
    TRA_RMSE = sqrt(mean((.resid)^2, na.rm = TRUE)),
    TRA_MAPE = mean(abs((.resid) / Customer.Traffic), na.rm = TRUE) * 100,
    .groups = "drop"
  )


#Validation -----------------------------------------------------
df.errors.roll |>
  as_tibble() |> 
  group_by(Branch.Key) |>
  summarise(
    VAL_RAW_RMSE = sqrt(mean((raw_errors)^2, na.rm = TRUE)),
    VAL_RAW_MAPE = mean(abs((raw_errors) / Customer.Traffic), na.rm = TRUE) * 100
  )


df.errors.roll |>
  as_tibble() |> 
  group_by(Branch.Key) |>
  summarise(
      VAL_LOG_RMSE = sqrt(mean((log_errors)^2, na.rm = TRUE)),
      VAL_LOG_MAPE = mean(abs((log_errors) / Customer.Traffic), na.rm = TRUE) * 100
  )

df.errors.roll |>
  as_tibble() |> 
  group_by(Branch.Key) |>
  summarise(
      VAL_nai_RMSE = sqrt(mean((nai_errors)^2, na.rm = TRUE)),
      VAL_nai_MAPE = mean(abs((nai_errors) / Customer.Traffic), na.rm = TRUE) * 100
  )
