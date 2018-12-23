#'@title Utility function
#'@description
#'Returns the reward for a player given a payoff matrix and choices
#'
#'@param c_self Player's own choice
#'@param c_op Opponent's choice
#'@param player Which side of the payof matrix is used. 0: first player, 1: second player
#'@param p_matrix A given 2-by-2 payoff matrix

#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return The reward for the specified player
#'
#'
#'@references
#'Devaine et al. (2014a, 2014b, 2017)
#'
#'@export

U <- function(c_self, c_op, player, p_matrix){
  # get values from payoff matrix for player 1
  a_1 <- p_matrix$p1_reward[p_matrix$p1_choice == 1 & p_matrix$p2_choice == 1] #value if both choose 1
  b_1 <- p_matrix$p1_reward[p_matrix$p1_choice == 1 & p_matrix$p2_choice == 0] #value if self chooses 1 and opponent chooses 0
  c_1 <- p_matrix$p1_reward[p_matrix$p1_choice == 0 & p_matrix$p2_choice == 1] #value if self chooses 0 and opponent chooses 1
  d_1 <- p_matrix$p1_reward[p_matrix$p1_choice == 0 & p_matrix$p2_choice == 0] #value if both choose 0

  # get values from payoff matrix for player 2
  a_2 <- p_matrix$p2_reward[p_matrix$p1_choice == 1 & p_matrix$p2_choice == 1] #value if both choose 1
  b_2 <- p_matrix$p2_reward[p_matrix$p1_choice == 0 & p_matrix$p2_choice == 1] #value if self chooses 1 and opponent chooses 0
  c_2 <- p_matrix$p2_reward[p_matrix$p1_choice == 1 & p_matrix$p2_choice == 0] #value if self chooses 0 and opponent chooses 1
  d_2 <- p_matrix$p2_reward[p_matrix$p1_choice == 0 & p_matrix$p2_choice == 0] #value if both choose 0

  # calculate reward
  reward <-
    (1-player) * #for player 1
    (c_self * c_op * a_1 +           #if both choose 1
       c_self * (1 - c_op) * b_1 +     #if you choose 1 and opponent chooses 0
       (1 - c_self) * c_op * c_1 +     #if you choose 0 and the opponent chooses 1
       (1 - c_self) * (1 - c_op) * d_1 #if both choose 0
    ) +
    player * #for player 2
    (c_self * c_op * a_2 +           #if both choose 1
       c_self * (1 - c_op) * b_2 +     #if you choose 1 and opponent chooses 0
       (1 - c_self) * c_op * c_2 +     #if you choose 0 and the opponent chooses 1
       (1 - c_self) * (1 - c_op) * d_2 #if both choose 0
    )

  return(reward)
}

#'@title Expected payoff function
#'@description
#'Returns the value difference between choosing 1 and 0 over opponent chocie scenarios, each scenario weighted by the probability of the opponent's choice
#'
#'@param p_op_1 estimated choice probability of the opponent
#'@param player the current player
#'@param p_matrix a given payoff matrix
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return An expected payoff difference for 1
#'
#'
#'@references
#'Devaine et al. (2014a, 2014b, 2017)
#'
#'@export

#Expected payoff
expected_payoff_difference <- function(p_op_1, player, p_matrix) {
  #Returns the value difference between choosing 1 and 0 over opponent chocie scenarios, each scenario weighted by the probability of the opponent's choice
  #INPUT
  #p_op_1: estimated choice probability of the opponent
  #player: the current player
  #p_matrix: a given payoff matrix
  #OUTPUT
  #An expected payoff difference for 1

  e_payoff_dif <-
    p_op_1 * (U(1, 1, player, p_matrix) - U(0, 1, player, p_matrix)) +
    (1 - p_op_1) * (U(1, 0, player, p_matrix) - U(0, 0, player, p_matrix))

  return(e_payoff_dif)
}


#'@title Softmax function
#'@description
#'Returns a choice probability for 1 given an expected payoff
#'
#'@param e_payoff_diff An expected payoff difference for 1
#'@param behavioural_temperature A randomizing temperature parameter
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return A choice probability for 1
#'
#'
#'@references
#'Devaine et al. (2014a, 2014b, 2017)
#'
#'@export



#Softmax function
softmax <- function(e_payoff_diff, behavioural_temperature){
  #Returns a choice probability for 1 given an expected payoff
  #INPUT
  #e_payoff_diff: an expected payoff difference for 1
  #behavioural_temperature: a randomizing temperature parameter
  #OUTPUT
  #A choice probability for 1

  #prepare behavioural temperature
  behavioural_temperature <- exp(behavioural_temperature)

  #Calculate probability of choosing 1
  p_self_1 <- 1 / (1 + exp(-(e_payoff_diff / behavioural_temperature)))

  #Make an upper bound on softmax's output to avoid getting infinite values when transforming to logodds
  if (p_self_1 > 0.999) {
    p_self_1 = 0.999
  }
  #Similarily, make a lower bound
  if (p_self_1 < 0.001) {
    p_self_1 = 0.001
  }

  return(p_self_1)
}


#'@title basic_variance_update
#'@description
#'#0-ToM updates the variance of its parameter estimate
#'
#'@param prev_hidden_states a list structure containing the states from last round
#'@param params a list structure containing 0-ToM's volatility parameter
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return An updated estimate variance
#'
#'
#'@references
#'Devaine et al. (2014a, 2014b, 2017)
#'
#'@export
basic_variance_update <- function(prev_hidden_states, params) {
  #0-ToM updates the variance of its parameter estimate
  #INPUT
  #prev_hidden_states: a list structure containing the states from last round
  #params: a list structure containing 0-ToM's volatility parameter
  #OUTPUT
  #An updated estimate variance

  volatility <- params$volatility #the volatility parameter reduces learning, assuming that there is noise in the opponents decisions
  prev_variance_basic <- prev_hidden_states$own_hidden_states$variance_basic #the uncertainty of opponent probability
  prev_mean_basic <- prev_hidden_states$own_hidden_states$mean_basic #the mean estimate of opponent probability

  #prepare volatility
  volatility <- exp(volatility)
  #prepare variance
  prev_variance_basic <- exp(prev_variance_basic)

  #calculate the new variance
  variance_basic <-
    1 / (
      (1 / (volatility + prev_variance_basic)) +
        inv.logit(prev_mean_basic) * (1 - inv.logit(prev_mean_basic)))

  #logistic transform
  variance_basic <- log(variance_basic)

  return(variance_basic)
}


#'@title basic_mean_update
#'@description
#' 0-ToM updates the mean of its parameter estimate
#'
#'@param prev_hidden_states a list structure containing the states from last round
#'@param choices a vector of [1] own choice and [2] opponent's choice from last round
#'@param variance_basic the updated estimate variance
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return An updated mean estimate
#'
#'
#'@references
#'Devaine et al. (2014a, 2014b, 2017)
#'
#'@export
basic_mean_update <- function(prev_hidden_states, choices, variance_basic) {
  #0-ToM updates the mean of its parameter estimate
  #INPUT
  #prev_hidden_states: a list structure containing the states from last round
  #choices: a vector of [1] own choice and [2] opponent's choice from last round
  #variance_basic: the updated estimate variance
  #OUTPUT
  #An updated mean estimate

  prev_c_op <- choices[2] #opponent's choice
  prev_mean_basic <- prev_hidden_states$own_hidden_states$mean_basic #the uncertainty of opponent probability
  variance_basic #the uncertainty of opponent probability

  #prepare variance
  variance_basic <- exp(variance_basic)

  #calculate the new mean
  mean_basic <- prev_mean_basic + variance_basic * (prev_c_op - inv.logit(prev_mean_basic))

  return(mean_basic)
}


#'@title p_op_1_k_approx_fun
#'@description
#'Approximates the estimated choice probability of the opponent on the previous round. A semi-analytical approximation derived in Daunizeau, J. (2017)
#'
#'@param prev_hidden_states a list structure containing the states from last round
#'@param level k-ToM's sophistication level
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return An approximation of the estimated choice probability of last round.
#'
#'
#'@references
#'Daunizeau, J. (2017)
#'
#'@export

p_op_1_k_approx_fun <- function(prev_hidden_states, level){
  #Approximates the estimated choice probability of the opponent on the previous round. A semi-analytical approximation derived in Daunizeau, J. (2017)
  #INPUT
  #prev_hidden_states: a list structure containing the states from last round
  #level: k-ToM's sophistication level
  #OUTPUT
  #An approximation of the estimated choice probability of last round.

  #input
  #for each sophistication level
  prev_mean <- prev_hidden_states$own_hidden_states$mean # mean of opponent probability estimation VECTOR
  prev_variance <- prev_hidden_states$own_hidden_states$variance # the variance for each estimated parameter MATRIX
  prev_gradient <- prev_hidden_states$own_hidden_states$gradient # gradients for each parameter MATRIX

  #constants
  a <- 0.205
  b <- -0.319
  c <- 0.781
  d <- 0.870

  #prepare variance
  prev_variance_prepared <- NULL #make empty list
  #variance is exponated, gradient is squared
  for (level_index in 1:level) { #for each level
    #matrix-mutliply the transposed variances on the gradient. This gives a single value
    prev_variance_prepared[level_index] <- t(exp(prev_variance[level,])) %*% prev_gradient[level,]^2
  }

  #calculate estimated probability of opponent choice
  p_op_1_k_approx <-
    inv.logit((prev_mean + b * prev_variance_prepared^c) / sqrt(1 + a * prev_variance_prepared^d))

  #log-transform
  p_op_1_k_approx <- log(p_op_1_k_approx)

  return(p_op_1_k_approx)
}



#'@title update_pk
#'@description
#'Updates k-ToM's estimated probability of the opponent having each possible sophistication level.
#'
#'@param prev_hidden_states A list structure containing the states of last trial
#'@param choices a vector of 1 own choice and 2 opponent's choice from last round
#'@param p_op_1_k_approx an approximated opponent choice probability form last round
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return The updated level probabilities
#'
#'
#'@references
#'Devaine et al. (2014a, 2014b, 2017)
#'
#'@export
update_pk <- function(prev_hidden_states, choices, p_op_1_k_approx){
  #Updates k-ToM's estimated probability of the opponent having each possible sophistication level.
  #INPUT:
  #prev_hidden_states: A list structure containing the states of last trial
  #choices: a vector of [1] own choice and [2] opponent's choice from last round
  #p_op_1_k_approx: an approximated opponent choice probability form last round
  #OUTPUT
  #The updated level probabilities

  #input
  prev_c_op <- choices[2] #opponent's choice
  #for each sophistication level
  prev_p_k <- prev_hidden_states$own_hidden_states$p_k  # probability of sophistication level k VECTOR
  p_op_1_k_approx # probability of opponent choosing 1, approximated semi-analytically VECTOR

  #prepare probability
  p_op_1_k_approx <- exp(p_op_1_k_approx)

  #calculate probability of each possible sophistication level
  p_k <-
    prev_c_op* #if opponent chose 1
    ((prev_p_k*p_op_1_k_approx)/sum(prev_p_k*p_op_1_k_approx)) +
    (1-prev_c_op)* #if opponent chose 0
    (prev_p_k*(1-p_op_1_k_approx)/sum(prev_p_k*(1-p_op_1_k_approx)))

  if (length(p_k) > 1) { #if higher than a 1-ToM
    #Force 0-ToM probability so that the probabilities sum to 1
    p_k[1] <-
      1 - sum(p_k[2:length(p_k)])
  }

  return(p_k)
}


#'@title parameter_variance_update
#'@description
#'k-ToM updates the variance of its parameter estimates
#'
#'@param prev_hidden_states A list strucutre containing the states from the last round
#'@param params A list structure containing k-ToM's volatility parameter and a dummy parameter which decides which parmaeter estimates are affected by volatility
#'@param p_k A vector of level probabilities
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return
#'Updated variances of k-ToM's parameter estimates
#'
#'@references
#'Devaine et al. (2014a, 2014b, 2017)
#'
#'@export
parameter_variance_update <- function(prev_hidden_states, params, p_k) {
  #k-ToM updates the variance of its parameter estimates
  #INPUT
  #prev_hidden_states: a list strucutre containing the states from the last round
  #params: a list structure containing k-ToM's volatility parameter and a dummy parameter which decides which parmaeter estimates are affected by volatility
  #p_k: a vector of level probabilities
  #OUTPUT
  #Updated variances of k-ToM's parameter estimates

  #input
  volatility <- params$volatility
  volatility_dummy <- params$volatility_dummy #dummy parable flags which parameters are affected by volatility
  #for each k:
  prev_param_mean <- prev_hidden_states$own_hidden_states$param_mean #the mean for each estimated parameter MATRIX
  prev_variance <- prev_hidden_states$own_hidden_states$variance #the uncertainty for each estimated parameter MATRIX
  prev_gradient <- prev_hidden_states$own_hidden_states$gradient #the gradient for each estimated parameter MATRIX
  p_k #the probability of sopistication level k VECTOR

  #prepare volatility
  volatility <- exp(volatility)*volatility_dummy

  #prepare variance
  prev_variance <- exp(prev_variance)

  #calculate new variance
  variance <-
    1 /
    (1 / (prev_variance + volatility) +
       p_k *
       inv.logit(prev_param_mean) * (1 - inv.logit(prev_param_mean)) *
       prev_gradient^2)

  #logistic transform
  variance <- log(variance)

  return(variance)
}



#'@title parameter_mean_update
#'@description
#'k-ToM updates the mean of its parameter estimates
#'
#'@param prev_hidden_states A list strucutre containing the states from the last round
#'@param choices a vector of 1 own choice and 2 opponent's choice from last round
#'@param p_k a vector of level probabilities
#'@param variance k-ToM's variance on parameter estimates
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return Updated means of k-ToM's parameter estimates
#'
#'
#'@references
#'Devaine et al. (2014a, 2014b, 2017)
#'
#'@export
parameter_mean_update <- function(prev_hidden_states, choices, p_k, variance){
  #k-ToM updates the mean of its parameter estimates
  #INPUT
  #prev_hidden_states: a list strucutre containing the states from the last round
  #choices: a vector of [1] own choice and [2] opponent's choice from last round
  #p_k: a vector of level probabilities
  #variance: k-ToM's variance on parameter estimates
  #OUTPUT
  #Updated means of k-ToM's parameter estimates

  #input
  prev_c_op <- choices[2] #opponent's choice
  #for each sophistication level k:
  prev_mean <- prev_hidden_states$own_hidden_states$mean #the mean of opponent probability estimation VECTOR
  prev_param_mean <- prev_hidden_states$own_hidden_states$param_mean #the mean for each estimated parameter MATRIX
  prev_gradient <- prev_hidden_states$own_hidden_states$gradient #the gradient for each estimated parameter MATRIX
  p_k #the probability of sophistication level k VECTOR
  variance #the variance of each estimated parameter MATRIX

  #prepare variance
  variance <- exp(variance)*prev_gradient

  #calculate new mean estimates
  param_mean <-
    prev_param_mean + p_k * variance * (prev_c_op - inv.logit(prev_mean))

  #?# "for numerical purposes" - unsure if necessary
  #param_mean <- inv.logit(logit(param_mean))

  return(param_mean)
}




#'@title gradient_update
#'@description
#'k-ToM calculates the gradient between parameter estimates and choice probability estimates
#'
#'@param opponent_prev_hidden_states opponents previous hidden states
#'@param mean the mean of k-ToM's choice probability estimate of its opponent
#'@param param_mean the means of k-ToM's parameter estimates
#'@param reverse_choices a vector of k-ToM's 1 opponent's choice and 2 own choice. Inserted as choices when simulating the opponent
#'@param opponent_level the level of the opponent for which the gradient is calculated
#'@param opponent_player the reverse player role of k-ToM's own. Inserted as player role when simulaitng the opponent
#'@param p_matrix a given 2-by-2 payoff matrix
#'
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return The gradient between each parameter estimate and the choice probability estimate
#'
#'
#'@references
#'Devaine et al. (2014a, 2014b, 2017)
#'
#'@export

gradient_update <- function(opponent_prev_hidden_states, params, mean, param_mean, reverse_choices, opponent_level, opponent_player, p_matrix.) {
  #k-ToM calculates the gradient between parameter estimates and choice probability estimates
  #INPUT
  #opponent_prev_hidden_states:
  #mean: the mean of k-ToM's choice probability estimate of its opponent
  #param_mean: the means of k-ToM's parameter estimates
  #reverse_choices: a vector of k-ToM's [1] opponent's choice and [2] own choice. Inserted as choices when simulating the opponent
  #opponent_level: the level of the opponent for which the gradient is calculated
  #opponent_player: the reverse player role of k-ToM's own. Inserted as player role when simulaitng the opponent
  #p_matrix: a given 2-by-2 payoff matrix
  #OUTPUT
  #The gradient between each parameter estimate and the choice probability estimate

  #input
  opponent_prev_hidden_states #opponent's hidden states, for running the learning function
  reverse_choices #opponent's perspective
  opponent_level #
  opponent_player #
  mean #the mean of opponent probability estimation VECTOR
  param_mean #the mean for each estimated parameter MATRIX

  #Make empty list for filling in gradients
  gradient <- NULL

  for (param in 1:length(param_mean)) {

    #calculate increment
    increment <- max(abs(1e-4*param_mean[param]), 1e-4)

    #use the parameter estimates
    param_mean_incremented <- param_mean
    #but use one of the incremented instead
    param_mean_incremented[param] <- param_mean[param] + increment

    #Make a list for parameters to be inserted
    opponent_params <- list(
      behavioural_temperature = param_mean_incremented[2], #temperature is the second column in the matrix
      volatility = param_mean_incremented[1], #volatility is the first column in the matrix
      volatility_dummy = params$volatility_dummy
    )

    #run the learning function of opponent using param_mean_temp as parameter values
    opponent_hidden_states_incremented <- rec_learning_function(prev_hidden_states = opponent_prev_hidden_states,
                                                                params = opponent_params,
                                                                choices = reverse_choices,
                                                                level = opponent_level,
                                                                player = opponent_player,
                                                                p_matrix = p_matrix.)

    #run the decision function of opponent using the temporary hidden states
    mean_incremented <- decision_function(hidden_states = opponent_hidden_states_incremented,
                                          params = opponent_params,
                                          player = opponent_player,
                                          level = opponent_level,
                                          p_matrix = p_matrix.)

    #calculate the gradient between parameter increment and probability estimate
    gradient[param] <- (mean_incremented - mean)/increment
  }

  return(gradient)
}


#'@title rec_learning_function
#'@description
#'k-ToM's learning function, where it updates its level probability, parameter, choice probability and gradient etimates. This is called recursively.
#'
#'@param prev_hidden_states a list structure containing the states from last round
#'@param params a list structure containing k-ToM's volatility parameter
#'@param choices a vector of 1 own choice and 2 opponent's choice from last round
#'@param level k-ToM's sophistication level
#'@param player k-ToM's player role, i.e. which side of the payoff matrix is used
#'@param p_matrix a given 2-by-2 payoff matrix
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return A list structure containing the updates estimates by k-ToM and all the recursively simulated opponents
#'
#'
#'@references
#'Devaine et al. (2014a, 2014b, 2017)
#'
#'@export

rec_learning_function <- function(
  prev_hidden_states,
  params,
  choices,
  level,
  player,
  p_matrix
) {
  #k-ToM's learning function, where it updates its level probability, parameter, choice probability and gradient etimates. This is called recursively.
  #INPUT
  #prev_hidden_states: a list structure containing the states from last round
  #params: a list structure containing k-ToM's volatility parameter
  #choices: a vector of [1] own choice and [2] opponent's choice from last round
  #level: k-ToM's sophistication level
  #player: k-ToM's player role, i.e. which side of the payoff matrix is used
  #p_matrix: a given 2-by-2 payoff matrix
  #OUTPUT:
  #A list structure containing the updates estimates by k-ToM and all the recursively simulated opponents

  #p_matrix is stored under another name, to avoid recursion-related errors
  p_matrix. <- p_matrix

  #Make empty list for filling with updated values
  new_hidden_states <- list()

  if (level == 0) { #If the (simulated) agent is a 0-ToM

    #Update 0-ToM's uncertainty of opponent choice probability
    variance_basic <- basic_variance_update(prev_hidden_states, params)

    #Update 0-ToM's mean estimate of opponent choice probability
    mean_basic <- basic_mean_update(prev_hidden_states, choices, variance_basic)

    #Gather own hidden states into one list
    own_hidden_states <- list(mean_basic = mean_basic, variance_basic = variance_basic)

  } else { #If the (simulated) agent is a K-ToM

    #Update p_k
    p_op_1_k_approx <- p_op_1_k_approx_fun(prev_hidden_states, level)
    p_k <- update_pk(prev_hidden_states, choices, p_op_1_k_approx)

    variance <- parameter_variance_update(prev_hidden_states, params, p_k)
    param_mean <- parameter_mean_update(prev_hidden_states, choices, p_k, variance)

    #Make empty structures for filling in new means
    mean <- NULL
    gradient <- matrix(NA, ncol = ncol(param_mean), nrow = level) #An empty matrix with a column for each parameter and a row for each level

    #Prepare opponent's perspective
    reverse_choices <- choices[2:1]
    opponent_player <- 1-player

    #Now we need to go through each possible opponent's level one at a time. Highest opponent level is 1 lower than own level
    for (level_index in 1:level) {

      #Set the simulated opponents level. "level_index" is one higher than the actual level because it isn't possible to index 0
      opponent_level <- level_index-1
      #Extract the currently simulated opponent's hidden states
      opponent_hidden_states <- prev_hidden_states[[level_index]]
      #Extract the estimated parameters of the current opponent
      opponent_params <- list(
        behavioural_temperature = param_mean[level_index, 2], #temperature is the second column in the matrix
        volatility = param_mean[level_index, 1], #volatility is the first column in the matrix
        volatility_dummy = params$volatility_dummy
      )

      #Simulate opponent learning
      new_opponent_hidden_states <- rec_learning_function(prev_hidden_states = opponent_hidden_states,
                                                          params = opponent_params,
                                                          choices = reverse_choices,
                                                          level = opponent_level,
                                                          player = opponent_player,
                                                          p_matrix = p_matrix.)

      #Simulate opponent deciding
      mean[level_index] <- decision_function(hidden_states = new_opponent_hidden_states,
                                             params = opponent_params,
                                             player = opponent_player,
                                             level = opponent_level,
                                             p_matrix = p_matrix.)

      #Update gradient
      gradient[level_index,] <- gradient_update(opponent_prev_hidden_states = opponent_hidden_states,
                                                params = params,
                                                mean = mean[level_index],
                                                param_mean = param_mean[level_index,], #only input the param_mean for the current level
                                                reverse_choices = reverse_choices,
                                                opponent_level = opponent_level,
                                                opponent_player = opponent_player,
                                                p_matrix. = p_matrix.)

      #Save opponent's hidden states in the list structure. Name it k-ToM
      eval(parse(text = paste(
        "new_hidden_states$ToM_",
        opponent_level,
        " = new_opponent_hidden_states",
        sep = "")))
    }

    #Gather own hidden states into one list
    own_hidden_states <- list(p_k = p_k, mean = mean, param_mean = param_mean, variance = variance, gradient = gradient)
  }

  #Save own updated hidden states to new hidden states
  new_hidden_states$own_hidden_states <- own_hidden_states

  return(new_hidden_states)
}



#'@title basic_p_op_1_fun
#'@description
#'0-ToM combines the mean and variance of its parameter estimate into a final choice probability estimate.
#'NB: this is the function taken from the VBA package (Daunizeau 2014), which does not use 0-ToM's volatility parameter
#'
#'@param hidden_states 0-ToM's updated estimates for this round
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return The estimated choice probability of the opponent
#'
#'
#'@references
#'(Daunizeau 2014)
#'
#'@export
basic_p_op_1_fun <- function(hidden_states, params){
  #0-ToM combines the mean and variance of its parameter estimate into a final choice probability estimate.
  #NB: this is the function taken from the VBA package (Daunizeau 2014), which does not use 0-ToM's volatility parameter
  #INPUT
  #hidden_states: 0-ToM's updated estimates for this round
  #OUTPUT
  #The estimated choice probability of the opponent

  #for each sophistication level k:
  mean_basic <- hidden_states$own_hidden_states$mean_basic #mean opponent probability estimate VECTOR
  variance_basic <- hidden_states$own_hidden_states$variance_basic #variance of parameter estimates MATRIX
  a <- 0.36 #this number is taken from the matlab code in ObsRecGen

  #Prepare variance
  variance_basic <- exp(variance_basic)

  #calculate opponent's probability of choosing 1
  p_op_1_basic <- inv.logit(mean_basic / sqrt(1 + a * variance_basic))

  return(p_op_1_basic)
}




#'@title p_op_1_k_fun
#'@description
#'k-ToM combines the mean choice probability estimate and the variances of its parameter estimates into a final choice probability estimate.
#'NB: this is the function taken from the VBA package (Daunizeau 2014), which does not use k-ToM's volatility parameter
#'
#'@param hidden_states k-ToM's updated estimates for this round
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return The estimated choice probabilities of the opponent, for each possible opponent level
#'
#'
#'@references
#'Daunizeau (2014)
#'
#'@export
p_op_1_k_fun <- function(hidden_states, params){
  #k-ToM combines the mean choice probability estimate and the variances of its parameter estimates into a final choice probability estimate.
  #NB: this is the function taken from the VBA package (Daunizeau 2014), which does not use k-ToM's volatility parameter
  #INPUT
  #hidden_states: k-ToM's updated estimates for this round
  #OUTPUT
  #The estimated choice probabilities of the opponent, for each possible opponent level

  #for each sophistication level k:
  mean <- hidden_states$own_hidden_states$mean #mean opponent probability estimate VECTOR
  variance <- hidden_states$own_hidden_states$variance #variance of parameter estimates MATRIX
  gradient <- hidden_states$own_hidden_states$gradient
  a <- 0.36 #this number is taken from the VBA package function ObsRecGen

  #Prepare variance
  variance <- rowSums(exp(variance) * gradient^2) #summing the variances of each parameter (after weighting by gradient). One sum per sophistication level

  #calculate opponent's probability of choosing 1
  p_op_1_k <- inv.logit(mean / sqrt(1 + a * variance))

  return(p_op_1_k)
}





#'@title decision_function
#'@description
#'k-ToM's decision function, where it calculates its own choice probability based on the updated estimates
#'
#'@param hidden_states k-ToM's updated estimates
#'@param params a list structure containing k-ToM's volatility and behavioural temperature parameters
#'@param player k-ToM's player role, i.e. which side of the payoff matrix is used
#'@param level k-ToM's sophistication level k
#'@param p_matrix a given 2-by-2 payoff matrix
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return k-ToM's own choice probability
#'
#'
#'@references
#'Devaine et al. (2014a, 2014b, 2017)
#'
#'@export
decision_function <- function(
  hidden_states,
  params,
  player,
  level,
  p_matrix
) {
  #k-ToM's decision function, where it calculates its own choice probability based on the updated estimates
  #INPUT
  #hidden_states: k-ToM's updated estimates
  #params: a list structure containing k-ToM's volatility and behavioural temperature parameters
  #player: k-ToM's player role, i.e. which side of the payoff matrix is used
  #level: k-ToM's sophistication level k
  #p_matrix: a given 2-by-2 payoff matrix
  #OUTPUT
  #k-ToM's own choice probability

  if (level == 0) { #If the (simulated) agent is a 0-ToM

    #Calculate opponent probability of choosing 1
    p_op_1 <- basic_p_op_1_fun(hidden_states, params)

  } else { #If the (simulated) agent is a K-ToM

    #Calculate opponent probability of choosing 1, for each k
    p_op_1_k <- p_op_1_k_fun(hidden_states, params)

    #extract probabilities for each opponent level
    p_k <- hidden_states$own_hidden_states$p_k

    #Weigh probabilities by corresponding level probabilities, to calculate an aggregate probability of opponent choosing 1
    p_op_1 <- sum(p_op_1_k * p_k)
  }

  #Calculate the expected payoff difference
  e_payoff_dif <- expected_payoff_difference(p_op_1, player, p_matrix)

  #Put into the softmax function to calculate the probability of choosing 1
  p_self_1 <- softmax(e_payoff_dif, params$behavioural_temperature)

  #Make into logodds
  p_self_1 <- logit(p_self_1)

  return(p_self_1)
}


#'@title The k-ToM function
#'@description
#'The full k-ToM function. First it first updates level probability, choice probability, parameter and gradient estimates. Then it calculates the estimated choice proability of the opponent, and calculates its own choice probability in response. The function also contains the simpler 0-ToM strategy, which only updates opponent's choice probability, and reacts.
#'
#'@param params a list structure containing k-ToM's volatility parameter, the dummy variable which decides which parameter estimates are affected by volatility, and the behavioural temperature. If a string is inputted, default values are used.
#'@param hidden_states the estimates from last round
#'@param player k-ToM's player role, i.e. which side of the payoff matrix is used
#'@param level k-ToM's sophistication level k
#'@param p_matrix a given 2-by-2 payoff matrix
#'@param choice_self k-ToM's choice from last round
#'@param choice_op opponent's choice from last round
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return A list structure containing k-ToM's choice and updated estimates
#'
#'
#'@references
#'Devaine et al. (2014a, 2014b, 2017)
#'
#'@export
k_ToM <- function(params = "default", hidden_states, player, level = NULL, p_matrix, choice_self = NULL, choice_op = NULL, return_hidden_states = T) {
  #The full k-ToM function. First it first updates level probability, choice probability, parameter and gradient estimates. Then it calculates the estimated choice proability of the opponent, and calculates its own choice probability in response. The function also contains the simpler 0-ToM strategy, which only updates opponent's choice probability, and reacts.
  #INPUT:
  #params: a list structure containing k-ToM's volatility parameter, the dummy variable which decides which parameter estimates are affected by volatility, and the behavioural temperature. If a string is inputted, default values are used.
  #hidden_states: the estimates from last round
  #player: k-ToM's player role, i.e. which side of the payoff matrix is used
  #level: k-ToM's sophistication level k
  #p_matrix: a given 2-by-2 payoff matrix
  #choice_self: k-ToM's choice from last round
  #choice_op: opponent's choice from last round
  #OUTPUT:
  #a list structure containing k-ToM's choice and updated estimates

  if (class(params) != "list"){ #Use default parameter values if nothing else is specified
    message("No parameter values specified, using default values")

    params <- list(behavioural_temperature = -1, # these are the values used in the Matlab script
                   volatility = -2,
                   volatility_dummy = c(1,0),  #dummy parable flags which parameters are affected by volatility
                   level = level
    )
  }

  #if no level where specified use the one specified in the loop
  if (is.null(level)){
    level <- params$level
  }

  #the input comes from last round
  prev_hidden_states <- hidden_states

  #bind choices together for easy reorganising
  choices <- c(choice_self, choice_op)

  if (is.null(choice_self)){ #If first round or missed trial

    new_hidden_states <- prev_hidden_states #No update

  } else {

    #Update hidden states
    new_hidden_states <-
      rec_learning_function(
        prev_hidden_states,
        params,
        choices,
        level,
        player,
        p_matrix)
  }

  #Calculate decision probability
  p_self_1 <-
    decision_function(
      hidden_states = new_hidden_states,
      params,
      player,
      level,
      p_matrix)

  #Make logodds into probability
  p_self_1 <- inv.logit(p_self_1)

  #Choose
  choice <- rbinom(1, 1, p_self_1)

  return(list(choice = choice, hidden_states = new_hidden_states))
}


#'@title rec_prepare_k_ToM
#'@description
#'A recursive function for preparing the list structure containing estimates which is used in the k-ToM function.
#'
#'@param level k-ToM's sophistication level k
#'@param priors a list structure containing all the prior values for k-ToM. If a string is given, default values are used.
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return A list structure which is ued as the previous estimates in k-ToM's first round.
#'
#'
#'@references
#'Devaine et al. (2014a, 2014b, 2017)
#'
#'@export
rec_prepare_k_ToM <- function(level, priors = "default") {
  #A recursive function for preparing the list structure containing estimates which is used in the k-ToM function.
  #INPUT
  #level: k-ToM's sophistication level k
  #priors: a list structure containing all the prior values for k-ToM. If a string is given, default values are used.
  #OUTPUT
  #A list structure which is ued as the previous estimates in k-ToM's first round.


  if (class(priors) != "list"){ #Use default priors if nothing else is specified

    message("No priors specified, using default priors")

    priors <- list(mean_basic = 0, #agnostic
                   variance_basic = 0, #will be exponated, so exp(0) = 1
                   mean = 0, #agnostic
                   variance = c(0,0), #will be exponated, so exp(0) = 1
                   param_mean = c(0,0), #not agnostic. These will be exponated efore being used as parameters. So this is a prior of exp(0) = 1
                   gradient = c(0,0) #is zero. in the Matlab script, one parameter was set to 1 if "flag for noisy" was set. [1] is volatility, [2] is temperature
    )
  }

  #Make empty list for filling with updated values
  new_hidden_states <- NULL

  if (level == 0) { #If the (simulated) agent is a 0-ToM

    #Set prior variance on 0-ToM's estimate of opponent choice probability
    variance_basic <- priors$variance_basic
    #Set prior mean on 0-ToM's estimate of opponent choice probability
    mean_basic <- priors$mean_basic

    #Gather own hidden states into one list
    own_hidden_states <- list(mean_basic = mean_basic, variance_basic = variance_basic)

  } else { #If the (simulated) agent is a K-ToM

    #Set priors, one for each of opponent's possible sophistication level
    #Probability is agnostic
    p_k <- rep(1, level)/level
    #Mean of opponent choice probability estimate
    mean <- rep(priors$mean, level)
    #Variance on parameter estimates
    variance <- t(matrix(rep(priors$variance, level),
                         nrow = length(priors$variance)))
    #Mean of parameter estimates
    param_mean <- t(matrix(rep(priors$param_mean, level),
                           nrow = length(priors$param_mean)))
    #Gradient
    gradient <- t(matrix(rep(priors$gradient, level),
                         nrow = length(priors$gradient)))

    #Gather own hidden states into one list
    own_hidden_states <- list(p_k = p_k, mean = mean, param_mean = param_mean, variance = variance, gradient = gradient)

    #Now we need to go through each possible opponent's level one at a time. Highest opponent level is 1 lower than own level
    for (level_index in 1:level) {

      #Set the simulated opponents level. "level_index" is one higher than the actual level because it isn't possible to index 0
      opponent_level <- level_index-1

      #Get hidden states from simulated opponents
      new_opponent_hidden_states <- rec_prepare_k_ToM(level = opponent_level, priors)

      #Save opponent's hidden states in the list structure. Name it k-ToM
      eval(parse(text = paste(
        "new_hidden_states$ToM_",
        opponent_level,
        " = new_opponent_hidden_states",
        sep = "")))
    }
  }

  #Save own updated hidden states to new hidden states
  new_hidden_states$own_hidden_states <- own_hidden_states

  return(new_hidden_states)
}






