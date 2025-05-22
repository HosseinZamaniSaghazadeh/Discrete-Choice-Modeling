require(tidyr)
require(plyr)
require(dplyr)
require(mlogit)

##### Loading Dataset #####
observed_data<-read.csv("Observed_Activity_Dataset.csv",header = T)
time_cost_data<-read.csv("Travel_Time_and_Cost_Matrix.csv",header = T)



##### General Setup #####
# Number of Observations
N <- 2


# Car Availability
delta_car_n <- setNames(
  as.integer(tapply(observed_data$MODE == "car", observed_data$PersonID, any)),
  unique(observed_data$PersonID)
) # delta_car_n['2'] = car availability for person 2

# General Case
delta_car_value = 0 # The general case should be solved two times (assuming car availability and not availability)

# Activity Duration Set (tau)
tau_set <- c(0, 1)


# Mandatory Activity Set (h)
h_set <- c(0, 1)


# Decision stage index (k)
k_set <- 0:36



##### Shop Setting #####
# Number of Size Attributes for Shopping
S_shop <- 2



##### Time Setting #####
# Time Grid
t_set <- seq(0, 360, by = 10)


# Time Step Size
delta_t <- 10


# Time After which Starting Work is Allowed
t_work_start_after <- 60


# Time Before which Starting Work Must Happen
t_work_start_before <- 180



##### Location Setting #####
# Set of All Possible Locations
l_set <- 1:6


# Set of All Possible Destination Locations
d_tilde_set <- 1:6



##### Shop Attributes #####
# Define the Shop Attributes
x_pls <- data.frame(
  purpose = rep("shop", 4),
  location = c(5, 5, 6, 6),
  attribute = c(1, 2, 1, 2),
  value = c(3000, 120, 2000, 130)
)



##### Activity Purpose #####
# Activity Purpose Set
p_set <- c("home", "work", "shop")


# Next Activity Purpose Set
p_tilde_set <- c("home", "work", "shop")



##### Transportation Mode #####
# Current Mode Set
m_set <- c("stay", "car", "walk")


# Next Mode Set
m_tilde_set <- c("stay", "car", "walk")



##### States #####
state_template <- list(
  t = NA, 
  l = NA, 
  p = NA, 
  tau = NA, 
  m = NA, 
  h = NA
)



##### Actions #####
action_template <- list(
  d_tilde = NA, 
  m_tilde = NA, 
  p_tilde = NA
)



##### Conditional Choice Set #####
# Location
L_act_n_list <- list()

for (person_id in unique(observed_data$PersonID)) {
  
  person_data <- observed_data[observed_data$PersonID == person_id, ]
  
  L_act_n_list[[as.character(person_id)]] <- list(
    shop = unique(person_data$DEST[person_data$ACT == "shop"]),
    work = unique(person_data$DEST[person_data$ACT == "work"]),
    home = unique(person_data$DEST[person_data$ACT == "home"])
  )
} # L_act_n_list[["1"]]$shop = L_act_1(shop)


# Mode
get_M_n <- function(delta_car_value, m) {
  if (delta_car_value == 0) {
    return(c("walk"))
  } else {
    if (m == "stay") {
      return(c("car", "walk"))
    } else if (m == "car") {
      return(c("car"))
    } else if (m == "walk") {
      return(c("walk"))
    } else {
      stop("Invalid mode!")
    }
  }
} # get_M_n(delta_car_n["1"], "stay") = the output is 'car' 'walk' because the person 1 has car and currently at home



# Set of Available Activity Purpose
get_P_n <- function(h) {
  if (h == 0) {
    return(c("home", "work", "shop"))
  } else if (h == 1) {
    return(c("home", "shop"))
  } else {
    stop("Invalid h value!")
  }
}


# Stay Choice Set (General and Individual)
get_C_stay <- function(l, p) {
  return(list(d_tilde = l, m_tilde = "stay", p_tilde = p))
} #C_stay_n <- get_C_stay(l, p)


# Travel Choice Set (General)
get_C_travel_general <- function(m, h, l) {
  
  # Available p_tilde
  available_p <- get_P_n(h)
  
  # Available m_tilde depending on mode and delta_car_n
  available_m <- get_M_n(delta_car_value, m = m) 
  
  choice_set_general <- list()
  
  for (p_tilde in available_p) {
    
    # Possible destinations
    if (p_tilde == "shop") {
      destinations <- c(5, 6)  # General shop destinations
    } else if (p_tilde == "work") {
      destinations <- c(3, 4)  # General work destinations
    } else if (p_tilde == "home") {
      destinations <- c(1, 2)  # General home locations
    }
    
    for (d_tilde in destinations) {
      for (m_tilde in available_m) {
        
        # Prevent moving to same zone by car
        if (m_tilde == "car" && l == d_tilde) {
          next
        }
        
        # ADD a special walk same-zone move
        if (m_tilde == "walk" && l == d_tilde) {
          # Same-zone walk allowed
          choice_set_general[[length(choice_set_general) + 1]] <- list(
            d_tilde = d_tilde,
            m_tilde = "walk",
            p_tilde = p_tilde
          )
          
          next
        }
        
        choice_set_general[[length(choice_set_general) + 1]] <- list(
          d_tilde = d_tilde,
          m_tilde = m_tilde,
          p_tilde = p_tilde
        )
      }
    }
  }
  
  return(choice_set_general)
}

# Travel Choice Set (Individuals)
get_C_travel_individual <- function(person_id, m, h, l) {
  
  # Available p_tilde
  available_p <- get_P_n(h)
  
  # Available m_tilde depending on mode and delta_car_n
  available_m <- get_M_n(delta_car_n[as.character(person_id)], m = m)
  
  choice_set_individual <- list()
  
  for (p_tilde in available_p) {
    
    # Possible destinations for this person
    destinations <- L_act_n_list[[as.character(person_id)]][[p_tilde]]
    
    for (d_tilde in destinations) {
      for (m_tilde in available_m) {
        
        # Prevent moving to same zone by car
        if (m_tilde == "car" && l == d_tilde) {
          next  # Skip this action
        }
        
        # ADD a special walk same-zone move
        if (m_tilde == "walk" && l == d_tilde) {
          # Same-zone walk allowed
          choice_set_individual[[length(choice_set_individual) + 1]] <- list(
            d_tilde = d_tilde,
            m_tilde = 'walk',
            p_tilde = p_tilde
          )
          
          next
        }
          
        choice_set_individual[[length(choice_set_individual) + 1]] <- list(
          d_tilde = d_tilde,
          m_tilde = m_tilde,
          p_tilde = p_tilde
        )
      }
    }
  }
  
  return(choice_set_individual)
}


# Full Choice Set (General)
get_C_general <- function(m, h, tau, l, p) {
  
  if (tau == 0) {
    # Stay: Only stay is allowed at tau = 0
    choice_set_general <- list(get_C_stay(l, p))
    
  } else if (tau == 1) {
    # Travel: Call the general travel choice set
    choice_set_general <- get_C_travel_general(m, h, l)
    
  } else {
    stop("Invalid tau value provided!")
  }
  
  return(choice_set_general)
}

# Full Choice Set (Individual)
get_C_individual <- function(person_id, m, h, tau, l, p) {
  
  if (tau == 0) {
    # Stay: Only stay is allowed at tau = 0
    choice_set_individual <- list(get_C_stay(l, p))
    
  } else if (tau == 1) {
    # Travel: Call the individual travel choice set
    choice_set_individual <- get_C_travel_individual(person_id, m, h, l)
    
  } else {
    stop("Invalid tau value provided!")
  }
  
  return(choice_set_individual)
}


##### Initialization #####
#Mode Constant
theta_mode_constant <- list('car' = 0, 'walk' = 0)# theta_car is base case, should not be estimated, and theta_walk is a free parameter (initial guess, will be updated later)


# Mode Travel Time
theta_TT_mode <- list('car' = 0, 'walk' = 0) # Both are free parameter (initial guess, will be updated later)


# Mode Travel Cost
theta_cost <- 0 # Free parameter (initial guess, will be updated later)


# Same Zone Walking
theta_sz_walk <- 0 # Free parameter (initial guess, will be updated later)


# Shopping
theta_C_shop <- 0 # Free parameter (initial guess, will be updated later)
theta_size_shop <- 0 # Free parameter (initial guess, will be updated later)
theta_shop_s <- list('s1' = 0, 's2' = 0) # Both are free parameter (initial guess, will be updated later)
theta_t_shop <- 0 # Free parameter (initial guess, will be updated later)


# Working (Defined every 60 minutes and for in-between times, interpolation is used)
theta_work_t <- list('s60' = 0, 's120' = 0, 's180' = 0) # 60 and 180 are free parameters (initial guess, will be updated later), but 120 is base case, should not be estimated


# Home (Defined every 60 minutes and for in-between times, interpolation is used) 
theta_home_t <- list('s0' = 0, 's60' = 0, 's120' = 0, 's180' = 0, 's240' = 0,
                     's300' = 0, 's360' = 0) # Free parameter (initial guess, will be updated later)




##### Travel Time and Travel Cost Look up Functions #####
# Travel Time
TT_lookup <- function(t, l, d_tilde, m_tilde) {
  row <- time_cost_data[
    time_cost_data$FROM == l &
      time_cost_data$TO == d_tilde &
      time_cost_data$MODE == m_tilde, 
  ]
  
  if (nrow(row) == 0) {
    return(Inf)  # If no travel possible, assume very large travel time
  } else {
    return(row$TT)
  }
} # TT_value <- TT_lookup(t_k, l_k, d_tilde, m_tilde)


# Travel Cost
TC_lookup <- function(t, l, d_tilde, m_tilde) {
  row <- time_cost_data[
    time_cost_data$FROM == l &
      time_cost_data$TO == d_tilde &
      time_cost_data$MODE == m_tilde, 
  ]
  
  if (nrow(row) == 0) {
    return(Inf)  # Assume high cost if not available
  } else {
    return(row$TC)
  }
} # TC_value <- TC_lookup(t_k, l_k, d_tilde, m_tilde)


# Define delta_szwalk
delta_szwalk <- function(l, d_tilde, m_tilde) {
  if (m_tilde == "walk" && l == d_tilde) {
    return(1)
  } else {
    return(0)
  }
}



##### Utilities #####
# Travel Utility Function
u_travel <- function(t, l, m, m_tilde, d_tilde) {
  
  # Mode Constants
  mode_constant <- if (m_tilde == "car" | m_tilde == "walk") {
    theta_mode_constant[[m_tilde]]
  }
    else if (m_tilde == "stay") {
    0  # No travel utility for "stay" mode
  } else {
    stop("Invalid mode in u_travel()!")
  }
  
  # Travel Time Coefficients
  mode_TT_coef <- if (m_tilde == "car" | m_tilde == "walk") {
    theta_TT_mode[[m_tilde]]
  }
    else {
    0
  }
  
  # Get Travel Time and Cost
  TT_value <- TT_lookup(t, l, d_tilde, m_tilde)
  TC_value <- TC_lookup(t, l, d_tilde, m_tilde)
  
  # Same Zone Walk Bonus
  delta_sz <- delta_szwalk(l, d_tilde, m_tilde)
  
  # Full Travel Utility
  utility_travel <- mode_constant +
    mode_TT_coef * TT_value +
    theta_cost * TC_value +
    theta_sz_walk * delta_sz
  
  return(utility_travel)
}


# Shop Utility Function
u_p_size <- function(l, p_tilde) {
  if (p_tilde != "shop") {
    return(0)  # Only shopping locations have size utility
  }
  
  # Filter the x_pls data for the given location and purpose
  relevant_x <- x_pls[x_pls$location == l & x_pls$purpose == p_tilde, ]
  
  if (nrow(relevant_x) == 0) {
    return(0)  # No size effect if no relevant attributes
  }
  
  sum_x <- 0
  for (i in 1:nrow(relevant_x)) {
    attribute_id <- as.character(relevant_x$attribute[i])
    x_value <- relevant_x$value[i]
    theta_s <- theta_shop_s[[attribute_id]]
    
    sum_x <- sum_x + x_value * exp(theta_s)
  }
  
  utility_p_size <- theta_C_shop + theta_size_shop * log(sum_x)
  return(utility_p_size)
}


# Work Constraint Utility Function
u_work_constraint <- function(t) {
  if (t >= t_work_start_after && t <= t_work_start_before) {
    return(0)  # Work start is allowed
  } else {
    return(-Inf)  # Work start is not allowed
  }
}

# Start Working Utility Function
u_start_work <- function(t, theta_work_t) {
  
  # Finding the time interval [t_j, t_j+1] where t falls
  t_grid <- as.numeric(names(theta_work_t))
  
  if (t <= min(t_grid)) {
    t_j <- min(t_grid)
    t_j1 <- t_grid[which(t_grid == t_j) + 1]
  } else if (t >= max(t_grid)) {
    t_j <- t_grid[length(t_grid) - 1]
    t_j1 <- max(t_grid)
  } else {
    t_j <- max(t_grid[t_grid <= t])
    t_j1 <- min(t_grid[t_grid >= t & t_grid != t_j])
  }
  
  
  # If t exactly matches a grid point
  if (t %in% t_grid) {
    utility_start_work <- theta_work_t[[as.character(t)]]
    return(utility_start_work)
  }
  
  
  # Otherwise, interpolate
  alpha1 <- (t_j1 - t) / (t_j1 - t_j)
  alpha2 <- (t - t_j) / (t_j1 - t_j)
  
  utility_start_work <- theta_work_t[[as.character(t_j)]] * alpha1 +
    theta_work_t[[as.character(t_j1)]] * alpha2
  
  return(utility_start_work)
}

# Starting Activity Utility Function
u_start <- function(l, t, h, p_tilde) {
  
  if (p_tilde == "shop") {
    return(u_p_size(l, p_tilde))
    
  } else if (p_tilde == "work") {
    work_constraint <- u_work_constraint(t)
    
    start_work_utility <- u_start_work(t, theta_work_t)
    return(work_constraint + start_work_utility)
    
  } else if (p_tilde == "home") {
    return(0)  # No start utility for home
  } else {
    stop("Invalid p_tilde provided in u_start() function")
  }
}


# Stay Home Utility Function
u_stay_home <- function(t, theta_home_t, delta_t) {
  
  t_grid <- as.numeric(names(theta_home_t))
  
  t_j <- max(t_grid[t_grid <= t])
  t_j1 <- min(t_grid[t_grid >= t & t_grid != t_j])
  
  # If t exactly matches a grid point
  if (t %in% t_grid) {
    stay_home_utility <- theta_home_t[[as.character(t)]] * delta_t
    return(stay_home_utility)
  }
  
  # Otherwise, interpolation
  alpha1 <- (t_j1 - t - 0.5 * delta_t) / (t_j1 - t_j)
  alpha2 <- (t + 0.5 * delta_t - t_j) / (t_j1 - t_j)
  
  stay_home_utility <- theta_home_t[[as.character(t_j)]] * delta_t * alpha1 +
    theta_home_t[[as.character(t_j1)]] * delta_t * alpha2
  
  return(stay_home_utility)
}

  # Activity Utility Function
u_act <- function(t, p_tilde, tau, theta_t_shop, theta_home_t, delta_t) {
  
  if (p_tilde == "shop") {
    return(theta_t_shop * delta_t)
    
  } else if (p_tilde == "home") {
    return(u_stay_home(t, theta_home_t, delta_t))
    
  } else if (p_tilde == "work") {
    return(0)
    
  } else {
    stop("Invalid activity purpose p_tilde!")
  }
}

# Full One-Stage Utility Function
u_xa <- function(t, l, p, tau, m, h, d_tilde, m_tilde, p_tilde) {
  
  if (m_tilde == "stay" && tau == 0) {
    # If staying and just started activity
    one_stage_utility <- u_act(t, p_tilde, tau, theta_t_shop, theta_home_t, delta_t) +
      u_start(l, t, h, p_tilde)
    
  } else {
    # Otherwise, traveling utility
    one_stage_utility <- u_travel(t, l, m, m_tilde, d_tilde)
  }
  
  return(one_stage_utility)
}