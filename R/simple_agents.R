
#'@title Play self
#'@description
#'Lets a human player make a choice
#'
#'@param player the current player role
#'@param p_matrix the current payoff matric
#'@param choice_self player's own choice on last round
#'@param choice_op opponent's choice last round
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return player's choice
#'
#'
#'@references
#'
#'
#'@export

PlaySelf <- function(params = NULL, hidden_states = NULL, player = NULL, p_matrix = NULL, choice_self = NULL, choice_op = NULL, return_hidden_states = F, messages = T){
  #Lets a human player make a choice
  #INPUT
  #player: the current player role
  #p_matrix: the current payoff matric
  #choice_self: player's own choice on last round
  #choice_op: opponent's choice last round
  #OUTPUT
  #player's choice

  if (messages){
    #Responds from last round
    if (is.null(choice_op) == F){ #if there was a previous round

      #Print the choices and rewards form last round
      message(paste("Last round you chose: ", choice_self,
                    ". Your opponent chose: ", choice_op,
                    ". Your score was: ", U(choice_self, choice_op, player, p_matrix),
                    ".", sep = ""))

      #Remind how to see the payoff matrix
      message("If you want to print the payoff matrix again, respond 'print'")

    } else {
      #Say which player you are
      message(paste("You are player", player + 1))

      #Fetch the name if the game and say it
      game_string <-p_matrix$game_title[1]
      game_string <- str_replace(game_string, "_matrix", "")
      message(paste("You are currently playing the ", game_string, " game.",sep = ""))

      #Show the payoff matrix
      message("The following is the payoff matrix for this game:")
      print(p_matrix[1:4])
    }
  }

  choice = NA
  while(choice %in% c(1, 0) == F){ #waiting for player to make a choice

    if (choice %in%  c("print", "Print", "'print'")){ #If they write print

      #Fetch the name if the game and say it
      game_string <-p_matrix$game_title[1]
      game_string <- str_replace(game_string, "_matrix", "")
      message(paste("You are currently playing the ", game_string, " game.",sep = ""))

      #Say which player the human is
      message(paste("You are player", player + 1))

      #Print the payoff matrix
      message("The following is the payoff matrix for this game:")
      print(p_matrix[1:4])

    } else if (is.na(choice) == F) { #If they write something that doesn't make sense
      message("invalid response, please try again.")
    }

    #Get choice from player
    choice <- readline(prompt = "What do you choose this round? Respond 1 or 0: ")

  }


  if (return_hidden_states == T){
    return(list(choice = as.numeric(choice), hidden_states = hidden_states))
  } else {
    return(as.numeric(choice))
  }
}


#'@title Random Bias (RB)
#'@description
#'RB: A Random Bias strategy choice. Selects randomly with a given probability
#'
#'@param params a list of 1 element, RB's choice probability parameter
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return RB's choice
#'
#'
#'@references
#'
#'
#'@export

RB <- function(params, hidden_states = NULL, player = NULL, p_matrix = NULL, choice_self = NULL, choice_op = NULL, return_hidden_states = F){
  #RB: A Random Bias strategy choice. Selects randomly with a given probability
  #INPUT
  #params: a list of 1 element, RB's choice probability parameter
  #OUTPUT
  #RB's choice

  #randomly select rabbit or stag (with a slight bias)
  choice <- rbinom(1, 1, prob = params$prop)

  if (return_hidden_states == T){
    return(list(choice = choice, hidden_states = hidden_states))
  } else {
    return(choice)
  }
}


#'@title Tit For Tat (TFT)
#'@description
#'A probabilistic Tit for Tat strategy. Copies the opponent's last choice with a given probability.
#'
#'@param params list of 1 element: TFT's choice probability parameter
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return TFT's choice
#'
#'
#'@references
#'Shelling (1981)
#'
#'@export

TFT <- function(params, hidden_states = NULL, player = NULL, p_matrix = NULL, choice_self, choice_op, return_hidden_states = F) {
  #A probabilistic Tit for Tat strategy. Copies the opponent's last choice with a given probability.
  #INPUT
  #params: list of 1 element: TFT's choice probability parameter
  #OUTPUT
  #TFT's choice

  #The probability of TFT copying opponent's choice
  copy_prob = params$copy_prob

  if (is.null(choice_op)) { #initial round or missed trial
    choice <- rbinom(1, 1, 0.5) #make random choice
  } else {
    #Decide whether TFT copies opponent
    copy = rbinom(1, 1, copy_prob)
    #Calculate resulting choice
    choice = copy*choice_op + (1-copy)*(1-choice_op)
  }

  if (return_hidden_states == T){
    return(list(choice = choice, hidden_states = hidden_states))
  } else {
    return(choice)
  }
}


#'@title Win-stay loose-switch (WSLS)
#'@description
#'A probabilistic Win-stay Loose-shift strategy. Copies itself if it won last round, does the opposite if it lost, both with given probabilities
#'
#'@param params A list of two elements, WSLS's win stay probability parameter and loose shift probability parameter
#'
#'@author
#'K. Enevoldsen & P. Waade
#'
#'@return WSLS's choice
#'
#'
#'@references
#'Nowak, M., & Sigmund, K. (1993). A strategy of win-stay, lose-shift that outperforms tit-for-tat in the Prisoner's Dilemma game. Nature, 364(6432), 56.
#'
#'@export

WSLS <- function(params, hidden_states = NULL, player, p_matrix, choice_self, choice_op, return_hidden_states = F){
  #A probabilistic Win-stay Loose-shift strategy. Copies itself if it won last round, does the opposite if it lost, both with given probabilities
  #INPUT
  #params: a list of two elements, WSLS's win stay probability parameter and loose shift probability parameter
  #OUTPUT
  #WSLS's choice

  if (is.null(choice_op)) { #initial round or missed trial
    choice <- rbinom(1, 1, 0.5) #make random choice
  } else {

    #Read in parameters
    stay_prob <- params$stay_prob
    switch_prob <- params$switch_prob
    #Read in score from last round
    prev_reward <- U(choice_self, choice_op, player, p_matrix)

    #Calculate the mean of all possible rewards for current player
    mean_reward <- (1 - player) * mean(p_matrix$p1_reward) + player * mean(p_matrix$p2_reward)

    #Calculate choice
    if (prev_reward > mean_reward) { #if the agent won, i.e. got more than mean amount of points
      stay <- rbinom(1, 1, stay_prob) #decide whether agent stays
      choice <- stay * choice_self + (1-stay) * (1-choice_self) #staying -> same choice as last

    } else if (prev_reward < mean_reward) { #if the agent lost, i.e. got less than mean score
      switch <- rbinom(1, 1, switch_prob) #decide whether agent switches
      choice <- switch * (1-choice_self) + (1-switch) * choice_self #switching -> opposite choice of last

    } else if (prev_reward == mean_reward) { #if the agent got mean score
      choice <- rbinom(1, 1, 0.5) #make random choice
    }
  }

  if (return_hidden_states == T){
    return(list(choice = choice, hidden_states = hidden_states))
  } else {
    return(choice)
  }
}





