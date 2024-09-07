#' @title
#'   Step 1: Monty Hall Game Initialization.
#'
#' @description
#'   `create_game()` produces a 3x1 vector with the elements
#'   "goat", "goat", and "car" in random order
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal". There are three closed doors. Behind one of the doors
#'   there is a car, and behind the other two there are goats.
#'   The contestant wins the car if they select the corresponding door.
#'
#' @param ... `create_game()` uses no arguments.
#'
#' @return
#'   `create_game()` returns a 3x1 vector with the elements
#'   "goat", "goat", and "car" in random order
#'
#' @examples
#'   game <-create_game()
#'   game
#'   [1] "car"  "goat" "goat"
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}



#' @title
#'   Step 2: The Contestant Selects a Door
#'
#' @description
#'   `select_door( )` produces a random integer between 1 and 3
#'
#' @details
#'   The contestant selection points to one of the elements
#'   in the vector `game`. The door remains closed (what is behind
#'   it remains unknown to the contestant).
#'
#' @param ... no arguments are used by the function.
#'
#' @return ... number between 1 and 3
#'   `select_door( )` returns a random integer between 1 and 3
#'
#' @examples
#'   game
#'   [1] "car"  "goat" "goat"
#'   a.pick <- select_door()
#'   a.pick
#'   [1] 2
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Step 3: The Host Opens a Goat Door

#' @description
#'   `open_goat_door(game, a.pick)` always reveals a goat behind
#'    one of the doors that the contestant has not picked.
#'
#' @details
#'   If the contestant picked a door with the car,
#'   `open_goat_door(game, a.pick)` (the host) selects
#'   any of the two remaining doors;
#'   otherwise, the host selects the only remaining door with a goat.
#'
#' @param
#'   game: a vector with the elements "goat", "goat", "car" in random order;
#'         the return value of `select_door`
#'   a.pick: an integer between 1 and 3; the return value of `select_door`
#'
#' @return ... an integer between 1 and 3
#'
#' @examples
#'   game
#'   [1] "car"  "goat" "goat"
#'   a.pick <- select_door()
#'   a.pick
#'   [1] 2
#'   opened.door <- open_goat_door( game, a.pick )
#'   opened.door
#'   [1] 3
#'
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats
  if( game[ a.pick ] == "car" )
  {
    goat.doors <- doors[ game != "car" ]
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  {
    opened.door <- doors[ game != "car" & doors != a.pick ]
  }
  return( opened.door ) # number between 1 and 3
}



#' @title
#'   Step 4: The Contestant can Switch or Maintain their Initial Choice
#'
#' @description
#'   `change_door(stay, opened.door, a.pick)`
#'    The contestant strategy is represented by the stay logical parameter,
#'    given the content of one of the doors has been revealed by the host.
#'
#' @details
#'    There are two possible strategies: after the host opens a door with a goat,
#'    the contestant either maintains their original selection or changes their
#'    door pick.
#'
#' @param
#'   stay: logical; TRUE (initial pick is maintained), FALSE: switch door choice
#'   opened.door: integer between 1 and 3, the index to a goat
#'   a.pick: integer between 1 and 3, the initial contestant pick
#'
#' @return ... an integer between 1 and 3
#'
#' @examples
#'   game
#'   [1] "car"  "goat" "goat"
#'   a.pick <- select_door()
#'   a.pick
#'   [1] 2
#'   opened.door <- open_goat_door( game, a.pick )
#'   opened.door
#'   [1] 3
#'   final.pick <- change_door(TRUE, opened.door, a.pick )
#'   final.pick
#'   [1] 2
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3)

  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ]
  }

  return( final.pick )  # number between 1 and 3
}


#' @title
#'   Game Result
#'
#' @description
#'   determine_winer( final.pick, game ) reveals whether the contestant final
#'   pick is a car or a goat.
#'
#' @details
#'  The element of the vector "game" corresponding to the final contestant pick
#'  is revealed; if this element is "car", the contestant wins; otherwise, they
#'  loose.
#'
#' @param
#'  final.pick: an integer between 1 and 3, the final contestant's choice
#'  game: a 3x1 vector with two "goat" and one "car" elements, in random order
#'
#' @return ... a string of characters
#'  "WIN" if the contestant pick corresponds to "car"
#'  "LOSE" if the contestant pick corresponds to "goat"
#'
#' @examples
#'   game
#'   [1] "car"  "goat" "goat"
#'   a.pick <- select_door()
#'   a.pick
#'   [1] 2
#'   opened.door <- open_goat_door( game, a.pick )
#'   opened.door
#'   [1] 3
#'   final.pick <- change_door(TRUE, opened.door, a.pick )
#'   final.pick
#'   [1] 2
#'   result <- determine_winner( final.pick, game )
#'   result
#'   [1] "LOSE"
#'
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}



#' @title
#'  Full Set Monty Hall Game Functions
#'
#' @description
#'  `play_game()` contains all the functions for the Monty Hall Game
#'  nested under a single function
#'
#' @details
#'  This function calls the Monty Hall game functions and returns the result of the game
#'  for both possible contestant strategies.
#'
#' @param ... no parameters for this function
#'
#' @return ... a data frame with two columns: "strategy" (Stay or Switch)
#'  and "outcome" ("win" or "loose")
#'
#' @examples
#'  P <- play_game()
#'     strategy outcome
#'   1     stay     WIN
#'   2   switch    LOSE
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'  Monty Hall Game Simulation
#'
#' @description
#' play_n_games(n) will repeat the game n times
#'
#' @details
#' It allows to evaluate the probability of winning depending on the chosen
#' strategy
#' @param
#' n: number of times the game is played in the simulation
#'
#' @return
#'  'play_n_games(n)' returns a data frame with 2*n observations
#'   of two chr variables: strategy ("stay" or "switch" ); and
#'   outcome ("LOSE" or "WIN")
#'
#' @examples
#'   P <- play_n_games(10000)
#'               outcome
#'   strategy  LOSE   WIN
#'       stay  0.66  0.34
#'     switch  0.34  0.66
#'   str(P)
#'  'data.frame':	20000 obs. of  2 variables:
#'   $ strategy: chr  "stay" "switch" "stay" "switch" ...
#'   $ outcome : chr  "LOSE" "WIN" "LOSE" "WIN" ...
#'
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>%
    print()

  return( results.df )

}
