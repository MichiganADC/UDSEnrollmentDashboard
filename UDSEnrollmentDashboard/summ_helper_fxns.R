################################################################################
## Helper functions for building UDS report summary table
################################################################################

# Fxn for outputting tables with one group variable
single_grp_table <- function(x, group_var) {
  distinct_grp_vals <- distinct(x, !!group_var)
  x %>% 
    group_by(!!group_var) %>% 
    summarize(Count = n()) %>%
    right_join(distinct_grp_vals, by = rlang::quo_text(group_var)) %>%
    arrange(!!group_var)
}

# Fxn for outputting tables with one group variable and one filter variable
single_grp_filter_table <- function(x, group_var, filter_var, filter_var_string) {
  distinct_grp_vals <- distinct(x, !!group_var)
  x %>% 
    group_by(!!group_var) %>% 
    filter(!!filter_var == filter_var_string) %>% 
    summarize(Count = n()) %>% 
    right_join(distinct_grp_vals, by = rlang::quot_text(group_var)) %>% 
    arrange(!!group_var)
}

# Fxn for outputting tables with two group variables
double_grp_table <- function(x, group_var_1, group_var_2) {
  distinct_grp_vals <- distinct(x, !!group_var_1)
  x %>% 
    group_by(!!group_var_1, !!group_var_2) %>% 
    summarize(Count = n()) %>% 
    spread(!!group_var_2, Count) %>% 
    right_join(distinct_grp_vals, by = rlang::quo_text(group_var_1)) %>%
    arrange(!!group_var_1)
}

# Fxn for outputting tables with three group variables
triple_grp_table <- function(x, group_var_1, group_var_2, group_var_3) {
  distinct_grp_vals <- distinct(x, !!group_var_1)
  x %>% 
    group_by(!!group_var_1, !!group_var_2, !!group_var_3) %>% 
    summarize(Count = n()) %>% 
    unite(col = United, !!group_var_2, !!group_var_3, sep = "_") %>% 
    spread(United, Count) %>% 
    right_join(distinct_grp_vals, by = rlang::quo_text(group_var_1)) %>%
    arrange(!!group_var_1)
}