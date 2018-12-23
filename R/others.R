#'@title Fetch payoff matrix
#'@description
#'Makes a payoff matrix for use in the simulation.
#'The following string commands are for getting pre-made matrices:
#'staghunt" - the stag hunt game
#'penny_cooperative" - a cooperative penny matching game
#'penny_competitive" - a competitive penny matching game
#'prisoners_dilemma" - the prisoner's dilemma game
#'party" - the party dilemma game
#'sexes" - the battle of the sexes game
#'chicken" - the chicken game
#'deadlock" - a deadlock game
#'custom - for specifying a custom game
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return
#'A payoff matrix
#'
#'@references
#'
#'
#'@export
fetch_p_matrix <-  function(game, custom = c(0,0,0,0,0,0,0,0)) {

  if (game == "staghunt") {
    p_matrix <-
      data_frame(
        p1_choice = c(1, 0, 1, 0),
        p2_choice = c(1, 1, 0, 0),
        p1_reward = c(5, 3, 0, 3),
        p2_reward = c(5, 0, 3, 3),
        game_title = c(game,
                       NA,NA,NA))
    return(p_matrix)

  } else if (game == "penny_cooperative") {
    p_matrix <-
      data_frame(
        p1_choice = c(1,  0,  1, 0),
        p2_choice = c(1,  1,  0, 0),
        p1_reward = c(1, -1, -1, 1),
        p2_reward = c(1, -1, -1, 1),
        game_title = c(game,
                       NA,NA,NA))
    return(p_matrix)

  } else if (game == "penny_competitive") {
    p_matrix <-
      data_frame(
        p1_choice = c( 1,  0,  1,  0),
        p2_choice = c( 1,  1,  0,  0),
        p1_reward = c(-1,  1,  1, -1),
        p2_reward = c( 1, -1, -1,  1),
        game_title = c(game,
                       NA,NA,NA))
    return(p_matrix)

  } else if (game == "prisoners_dilemma") {
    p_matrix <-
      data_frame(
        p1_choice = c(1, 0, 1, 0),
        p2_choice = c(1, 1, 0, 0),
        p1_reward = c(2, 3,-1, 0),
        p2_reward = c(2,-1, 3, 0),
        game_title = c(game,
                       NA,NA,NA))
    return(p_matrix)

  } else if (game == "party") {
    p_matrix <-
      data_frame(
        p1_choice = c(1,  0, 1, 0),
        p2_choice = c(1,  1, 0, 0),
        p1_reward = c(10, 0, 0, 5),
        p2_reward = c(10, 0, 0, 5),
        game_title = c(game,
                       NA,NA,NA))
    return(p_matrix)

  } else if (game == "sexes") {
    p_matrix <-
      data_frame(
        p1_choice = c(1,  0, 1, 0),
        p2_choice = c(1,  1, 0, 0),
        p1_reward = c(5,  0, 0, 10),
        p2_reward = c(10, 0, 0, 5),
        game_title = c(game,
                       NA,NA,NA))
    return(p_matrix)

  } else if (game == "chicken") {
    p_matrix <-
      data_frame(
        p1_choice = c(1,  0,  1,  0),
        p2_choice = c(1,  1,  0,  0),
        p1_reward = c(0, -1,  1, -1000),
        p2_reward = c(0,  1, -1, -1000),
        game_title = c(game,
                       NA,NA,NA))
    return(p_matrix)

  } else if (game == "deadlock") {
    p_matrix <-
      data_frame(
        p1_choice = c(1,  0,  1,  0),
        p2_choice = c(1,  1,  0,  0),
        p1_reward = c(2,  0,  3,  1),
        p2_reward = c(2,  3,  0,  1),
        game_title = c(game,
                       NA,NA,NA))
    return(p_matrix)

  } else if (game == "custom") {
    p_matrix <-
      data_frame(
        p1_choice = c(1,          0,         1,         0),
        p2_choice = c(1,          1,         0,         0),
        p1_reward = c(custom[1], custom[3],  custom[5], custom[7]),
        p2_reward = c(custom[2], custom[4],  custom[6], custom[8]),
        game_title = c(game,
                       NA,NA,NA))
    return(p_matrix)
  }
}



#'@title prepare
#'@description
#'Function for generating parameter values and priors for an agent. Parameter values are specified as vectors, where the first number is the mean and the second number is the standard deviation of the normal distribution they are sampled from. If no values are specified, defaults are used. Only the values for the strategy specified are returned.
#'Implemented agent strategies:
#'RB: Random Bias
#'TFT: Tit for Tat
#'WSLS: Win-stay Loose-switch
#'k-ToM: a k-level theory of mind agent. Note that you input the desired k-level in place of k, e.g. 1-ToM
#'
#'@param strategy_string A string of the strategy used by the agent.
#'@param RB A vector of mean and standard deviation of the choice probability parameter (used for RB)
#'@param TFT A vector of mean and standard deviation of the choice probability parameter (used for TFT)
#'@param WSLS A vector of mean and standard deviation of the win stay probability and the loose shift parameters  (used for WSLS)
#'@param k_ToM A vector of mean and standard deviation of the volatility and behavioural temperature parameters (used for k-ToM)
#'@param k_ToM_priors A list structure containing priors for all of k-ToM's estimates. This is fed to the red_prepare_k_tom function. If a string is specified, defaults will be used
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return A list structure containing the sampled parameter values of the agent and the states for the first round (if any).
#'
#'
#'@references
#'
#'
#'@export
prepare <- function(strategy_string,
                    RB = c(prop_mean = 0.8,
                           prop_sd = 0.1),
                    TFT = c(prob_mean = 0.9,
                            prob_sd = 0.1),
                    WSLS = c(stay_mean = 0.9,
                             stay_sd = 0.1,
                             switch_mean = 0.9,
                             switch_sd = 0.1),
                    k_ToM = c(volatility_mean = -2,
                              volatility_sd = 0,
                              temperature_mean = -10,
                              temperature_sd = 0),
                    k_ToM_priors = "default"){
  #Function for generating parameter values and priors for an agent. Parameter values are specified as vectors, where the first number is the mean and the second number is the standard deviation of the normal distribution they are sampled from. If no values are specified, defaults are used. Only the values for the strategy specified are returned.
  #INPUT
  #strategy_string: A string of the strategy used by the agent.
  #Implemented agent strategies:
  #RB: Random Bias
  #TFT: Tit for Tat
  #WSLS: Win-stay Loose-switch
  #k-ToM: a k-level theory of mind agent. Note that you input the desired k-level in place of k, e.g. 1-ToM
  #RB: A vector of mean and standard deviation of the choice probability parameter
  #TFT: A vector of mean and standard deviation of the choice probability parameter
  #WSLS: A vector of mean and standard deviation of the win stay probability and the loose shift parameters
  #k_ToM: A vector of mean and standard deviation of the volatility and behavioural temperature parameters
  #k_ToM_priors: A list structure containing priors for all of k-ToM's estimates. This is fed to the red_prepare_k_tom function. If a string is specified, defaults will be used
  #OUTPUT
  #A list structure containing the sampled parameter values of the agent and the states for the first round (if any).

  if (strategy_string == "RB") { #For Random Bias
    params <- list(
      "prop" = inv.logit(rnorm(1, mean = logit(RB[1]), sd = RB[2])))
    hidden_states <- "None"

  } else if (strategy_string == "WSLS") { #For Win-Stay-Loose-Switch
    params <- list(
      "stay_prob" = inv.logit(rnorm(1, mean = logit(WSLS[1]), sd = WSLS[2])),
      "switch_prob" = inv.logit(rnorm(1, mean = logit(WSLS[3]), sd = WSLS[4])))
    hidden_states <- "None"

  } else if (strategy_string == "TFT") { #For Tit for Tat
    params <- list(
      "copy_prob" = inv.logit(rnorm(1, mean = logit(TFT[1]), sd = TFT[2])))
    hidden_states <- "None"

  } else if (grepl("-ToM", strategy_string)) { #if the strategy is a k-ToM
    level <- as.numeric(str_extract(strategy_string, "[0-9]")) #save the level
    params <- list(
      volatility = rnorm(1, mean = k_ToM[1], sd = k_ToM[2]),
      volatility_dummy = c(1,0),
      behavioural_temperature = rnorm(1, mean = k_ToM[3], sd = k_ToM[4]),
      level = level)
    hidden_states <- rec_prepare_k_ToM(level = level, priors = k_ToM_priors)

  } else if (strategy_string == "PlaySelf") {
    params <- list(
      "player" = "A human player is playing")
    hidden_states <- "None"
  } else { #if no strategy is recognized
    stop("Could not find strategy, try to check for spelling errors")
  }

  result <- list(params = params, hidden_states = hidden_states)
  return(result)
}

#'@title create_agents
#'@description
#'Creates a dataframe with agents and their opponents.
#'If prepare_with_defaults is set to TRUE, this also prepares their parameter values and priors, which can fed to the compete function.
#'If prepare_with_defaults is set to FALSE, the output needs to be inserted into the prepare_df function.
#'
#'#implemented agent strategies
#'RB: Random Bias
#'TFT: Tit for Tat
#'WSLS: Win-stay Loose-switch
#'SoftmaxTitTat: A tit for tat using a softmax function as well as an utility function
#'k-ToM: a k-level theory of mind agent. Note that you input the desired k-level in place of k, e.g. 1-ToM
#'
#'implemented matchup types include
#'RR: Round robin, every strategy in the strategies input matched up against every other strategy
#'random: The strategies in the strategies input is randomly matched up
#'half_half: the first half is matched up with the second half
#'
#'@param strategies A list of strategies given as a vector
#'@param match_up a string argument deciding which tournament structure is used
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return
#'
#'
#'@references
#'
#'
#'@export
create_agents <- function(strategies, match_up = "RR", prepare_with_defaults = T){
  #Creates a dataframe with agents and their opponents.
  #If prepare_with_defaults is set to TRUE, this also prepares their parameter values and priors, which can fed to the compete function.
  #If prepare_with_defaults is set to FALSE, the output needs to be inserted into the prepare_df function.
  #INPUT
  #strategies: A list of strategies given as a vector
  #implemented agent strategies
  #RB: Random Bias
  #TFT: Tit for Tat
  #WSLS: Win-stay Loose-switch
  #SoftmaxTitTat: A tit for tat using a softmax function as well as an utility function
  #k-ToM: a k-level theory of mind agent. Note that you input the desired k-level in place of k, e.g. 1-ToM
  #match_up: a string argument deciding which tournament structure is used
  #implemented matchup types include
  #RR: Round robin, every strategy in the strategies input matched up against every other strategy
  #random: The strategies in the strategies input is randomly matched up
  #half_half: the first half is matched up with the second half
  #OUTPUT
  #A dataframe with the created agents matched up. If prepare_with_defaults is set to TRUE, parameter values and priors have also been set.

  if (match_up == "RR"){
    output <- as_data_frame(t(combn(strategies, 2))) %>% dplyr::select(player1 = V1, player2 = V2)
  } else if (match_up == "half_half" & length(strategies) %% 2 == 0){
    output <- as_data_frame(matrix(strategies, ncol = 2)) %>% dplyr::select(player1 = V1, player2 = V2)
  } else if (match_up == "random" & length(strategies) %% 2 == 0){
    output <- as_data_frame(matrix(sample(strategies, replace=F), ncol = 2)) %>% dplyr::select(player1 = V1, player2 = V2)
  } else {
    stop("Please input valid match_up type
         or if using 'half_half' or 'random' make sure that your strategies input has an even length")
  }

  if (prepare_with_defaults){

    #calling the prepare function
    tmp1 <- sapply(output$player1, prepare)
    tmp2 <- sapply(output$player2, prepare)
    #save params
    output$params_p1 <- t(tmp1)[,1]
    output$params_p2 <- t(tmp2)[,1]
    #Save hidden states
    output$hidden_states_p1 <- t(tmp1)[,2]
    output$hidden_states_p2 <- t(tmp2)[,2]
  }

  return(output)
  }



#'@title prepare_df
#'@description
#'Sets parameter values and priors for agents in a matched-up dataframe, to be fed to the compete function.
#'
#'@param agent_df_unprep a tibble with agent strategies matched up, but without set parameter values and prios. Created by the prepare_agents function
#'@param args a string vector of arguments to pass to the prepare function
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return A dataframe with agents matched up and with parameter values and priors, to be fed to the compete function
#'
#'
#'@references
#'
#'
#'@export
prepare_df <- function(agent_df_unprep, args = NULL){
  #Sets parameter values and priors for agents in a matched-up dataframe, to be fed to the compete function.
  #INPUT
  #agent_df_unprep: a tibble with agent strategies matched up, but without set parameter values and prios. Created by the prepare_agents function
  #args, a string vector of arguments to pass to the prepare function
  #Two string per competing pair, the first for player 1 and the second for player 2, in the order they appear in the dataframe
  #OUTPUT
  #A dataframe with agents matched up and with parameter values and priors, to be fed to the compete function

  p1_args <- args[1:(nrow(agent_df_unprep))]

  p2_args <- args[-(1:(nrow(agent_df_unprep)))]

  #adding commas for the eval expression, should only be there is there is aditional arguments
  if_else(nchar(p1_args) != 0 ,paste(",", p1_args), p1_args)
  if_else(nchar(p2_args) != 0 ,paste(",", p2_args), p2_args)

  #Make variables for saving - this prevents an error regarding unitialized columns
  agent_df_unprep$params_p1 <- NA
  agent_df_unprep$params_p2 <- NA

  agent_df_unprep$hidden_states_p1 <- NA
  agent_df_unprep$hidden_states_p2 <- NA

  for (i in 1:length(p1_args)){
    eval(parse(text =
                 paste("tmp <- prepare(strategy_string = ", "'", agent_df_unprep$player1[i], "'", ", ", p1_args[i],")", sep = "")))
    agent_df_unprep$params_p1[i] <- list(tmp$params)
    agent_df_unprep$hidden_states_p1[i] <- list(tmp$hidden_states)
    eval(parse(text =
                 paste("tmp <- prepare(strategy_string = ", "'", agent_df_unprep$player2[i], "'", ", ", p2_args[i],")", sep = "")))
    agent_df_unprep$params_p2[i] <- list(tmp$params)
    agent_df_unprep$hidden_states_p2[i] <- list(tmp$hidden_states)
  }

  return(agent_df_unprep)
}



#'@title compete
#'@description
#'Makes two agents play against each other a set number of rounds
#'
#'@param player1 player 1's strategy function
#'@param player2 player 2's strategy function
#'@param p1_params a list structure containing player 1's parameter values
#'@param p2_params a list structure containing player 2's parameter values
#'@param p1_hidden_states player 1's starting states, i.e. priors
#'@param p2_hidden_states player 2's starting states, i.e. priors
#'@param p_matrix a given 2-by-2 payoff matrix
#'@param n_rounds number of rounds to compete
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return A dataframe containing the two agents' choices, rewards and states, for each round
#'
#'
#'@references
#'
#'
#'@export
compete <- function(player1, player2, p1_params, p2_params, p1_hidden_states = NA, p2_hidden_states = NA, p_matrix, n_rounds = 10){
  #Makes two agents play against each other a set number of rounds
  #INPUT
  #player1: player 1's strategy function
  #player2: player 2's strategy function
  #p1_params: a list structure containing player 1's parameter values
  #p2_params: a list structure containing player 2's parameter values
  #p1_hidden_states: player 1's starting states, i.e. priors
  #p2_hidden_states: player 2's starting states, i.e. priors
  #p_matrix: a given 2-by-2 payoff matrix
  #n_rounds: number of rounds to compete
  #OUTPUT
  #A dataframe containing the two agents' choices, rewards and states, for each round

  #this is for when the compete_all function is used - it simply unpacks the apply function
  if (class(player1) == "list"){
    p2_hidden_states <- list(player1[[6]])
    p1_hidden_states <- list(player1[[5]])
    p2_params <- list(player1[[4]])
    p1_params <- list(player1[[3]])
    player2 <- player1[[2]]
    player1 <- player1[[1]]
  }

  if(is.na(p1_hidden_states) | is.na(p2_hidden_states)){
    message("One of both of the hidden states were not specified, hopefully your agents don't need them.
            If they do you should specifiy the hidden states")
  }

  #setting to NULL to signify it is the first round
  p1_choice <- NULL
  p2_choice <- NULL

  #fetch strategy function - just the player name unless it is a k-ToM
  strat_p1 <- if_else(grepl("-ToM", player1), "k_ToM", player1)
  strat_p2 <- if_else(grepl("-ToM", player2), "k_ToM", player2)

  #Progressbar only used if neither player is a human
  progressbar = strat_p1 != "PlaySelf" & strat_p2 != "PlaySelf"

  if (progressbar) {
    #Print current pair
    message(paste("Current players: ", player1, " vs ", player2, sep = ""))
    #Create progress bar
    pb <- txtProgressBar(min = 0, max = n_rounds, style = 3)}

  for (round in 1:n_rounds){

    if (progressbar) {setTxtProgressBar(pb, round)}

    #calls the strategy function for each agent and gives it the player as a argument
    p1 <- do.call(strat_p1,
                  args = list(params = p1_params[[1]],
                              player = 0,
                              hidden_states = p1_hidden_states[[1]],
                              p_matrix = p_matrix,
                              choice_self = p1_choice,
                              choice_op = p2_choice,
                              return_hidden_states = T
                  ))

    p2 <- do.call(strat_p2,
                  args = list(params = p2_params[[1]],
                              hidden_states = p2_hidden_states[[1]],
                              player = 1,
                              p_matrix = p_matrix,
                              choice_self = p2_choice,
                              choice_op = p1_choice,
                              return_hidden_states = T
                  ))
    #params = "default", hidden_states, player, level, p_matrix, choice_self, choice_op, return_hidden_states = T


    #results for next round
    p1_hidden_states <- list(p1$hidden_states)
    p2_hidden_states <- list(p2$hidden_states)
    p1_choice <- p1$choice
    p2_choice <- p2$choice

    #generate result
    result <- p_matrix[p_matrix$p1_choice == p1_choice & p_matrix$p2_choice == p2_choice,]





    #save results
    round_result <- data_frame(player = c(player1, player2),
                               choice = c(p1$choice, p2$choice),
                               points = c(result$p1_reward, result$p2_reward),
                               round_nr = round,
                               pair = paste(player1, player2, sep = " / "),
                               hidden_states = c(p1_hidden_states, p2_hidden_states)
    )

    if (round == 1){
      result_df <- round_result
    } else {
      result_df <- rbind(result_df, round_result)
    }
  }
  if (progressbar) {close(pb)}
  return(result_df)
  }




#'@title compete_all
#'@description A function for making all agents in a dataframe compete
#'
#'@param agent_df A tibble (dataframe) with matched agents their parameter values and priors. Created with create_agents and/or prepare_df
#'@param n_rounds number of rounds the agents compete
#'@param p_matrix A given 2-by-2 payoff matrix
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return A tibble (dataframe) containing the choices, rewards and states for every round, and for every competing pair
#'
#'
#'@references
#'
#'
#'@export
compete_all <- function(agent_df, p_matrix, n_rounds = 10){
  #A function for making all agents in a dataframe compete
  #INPUT
  #agent_df: A tibble (dataframe) with matched agents their parameter values and priors. Created with create_agents and/or prepare_df
  #n_rounds: number of rounds the agents compete
  #p_matrix: A given 2-by-2 payoff matrix
  #OUTPUT
  #A tibble (dataframe) containing the choices, rewards and states for every round, and for every competing pair

  result_list <- apply(agent_df, 1, compete, n_rounds = n_rounds, p_matrix = p_matrix)
  result_df <- result_list %>% bind_rows()

  return(result_df)
}



