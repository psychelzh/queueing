#' Simulate a M/M/c/K queue
#'
#' @param rate_a,rate_s The exponential distribution rates. `rate_a` is the
#'   arrival rate, `rate_s` is the service rate.
#' @param c The number of servers.
#' @param k The system capacity. When reaching system capacity, all new arrivals
#'   are balked (do not join).
#' @param n The number of customers to simulate.
simulate_mmck <- function(rate_a, rate_s, c, k, n) {
  customers <- data.frame(
    id = seq_len(n),
    arr_time = cumsum(rexp(n, rate_a)),
    serv_time = rexp(n, rate_s),
    depart_time = 0,
    balked = FALSE
  )
  services <- data.frame(
    id = seq_len(c),
    idle_at = 0
  )
  for (i in seq_len(n)) {
    cur_arr_time <- customers$arr_time[[i]]
    system_k <- nrow(subset(customers, depart_time > cur_arr_time)) + 1
    if (system_k > k) {
      customers$balked[i] <- TRUE
      customers$serv_time[i] <- NA
      next
    }
    serv_is_idle <- services$idle_at <= cur_arr_time
    if (any(serv_is_idle)) {
      serv_id <- sample(services$id[serv_is_idle], 1)
      cur_depart_time <- with(customers[i, ], arr_time + serv_time)
    } else {
      serv_id <- services$id[which.min(services$idle_at)]
      cur_depart_time <- customers$serv_time[i] +
        services$idle_at[services$id == serv_id]
    }
    services$idle_at[services$id == serv_id] <- cur_depart_time
    customers$depart_time[i] <- cur_depart_time
  }
  customers$wait_time <- with(
    customers,
    ifelse(
      is.na(serv_time),
      NA, depart_time - (arr_time + serv_time)
    )
  )
  customers
}
