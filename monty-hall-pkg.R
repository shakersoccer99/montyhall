#' @title
#'   Create a new Monty Hall Problem game.
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door.
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#' @examples
#'   create_game()
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}



#' @title
#'   Select an initial door in the Monty Hall game.
#' @description
#'   `select_door()` randomly selects one door out of three possible doors.
#' @details
#'   Simulates the contestant making their initial selection from three doors (numbered 1, 2, and 3).
#' @return
#'   The function returns a single integer between 1 and 3 representing the chosen door.
#' @examples
#'   select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Host opens a goat door.
#' @description
#'   `open_goat_door()` selects a door with a goat behind it that was not chosen by the contestant.
#' @details
#'   If the contestant initially chose a car, the host randomly picks one of the two remaining goat doors.
#'   If the contestant chose a goat door, the host opens the single remaining goat door.
#' @param game A character vector of length 3 representing the game setup (e.g., "goat", "goat", "car").
#' @param a.pick An integer between 1 and 3 indicating the door selected by the contestant.
#' @return The function returns a single integer between 1 and 3 representing the door opened by the host.
#' @examples
#'   game <- create_game()
#'   pick <- select_door()
#'   open_goat_door( game, pick )
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
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
#'   Change or keep the contestant's door selection.
#' @description
#'   `change_door()` determines the contestant's final door selection based on whether they stay or switch.
#' @details
#'   If `stay = TRUE`, the final choice remains the initial pick (`a.pick`).
#'   If `stay = FALSE`, the contestant switches to the remaining unopened door.
#' @param stay Logical scalar (`TRUE` or `FALSE`). Default is `TRUE`. Indicates if the contestant stays with their original pick.
#' @param opened.door An integer between 1 and 3 representing the door opened by the host.
#' @param a.pick An integer between 1 and 3 representing the contestant's initial door selection.
#' @return The function returns a single integer between 1 and 3 representing the final door selection.
#' @examples
#'   game <- create_game()
#'   pick <- select_door()
#'   opened <- open_goat_door( game, pick )
#'   change_door( stay = FALSE, opened.door = opened, a.pick = pick )
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
#'   Determine if the contestant wins or loses.
#' @description
#'   `determine_winner()` checks if the contestant's final door choice contains a car or a goat.
#' @details
#'   Evaluates the element at `game[ final.pick ]` and returns `"WIN"` if it equals `"car"`, or `"LOSE"` if it equals `"goat"`.
#' @param final.pick An integer between 1 and 3 representing the contestant's final door selection.
#' @param game A character vector of length 3 representing the game setup.
#' @return The function returns a single character string: `"WIN"` or `"LOSE"`.
#' @examples
#'   game <- create_game()
#'   pick <- select_door()
#'   opened <- open_goat_door( game, pick )
#'   final <- change_door( stay = TRUE, opened.door = opened, a.pick = pick )
#'   determine_winner( final, game )
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
