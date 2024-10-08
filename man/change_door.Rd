% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg.R
\name{change_door}
\alias{change_door}
\title{Step 4: The Contestant can Switch or Maintain their Initial Choice}
\usage{
change_door(stay = T, opened.door, a.pick)
}
\arguments{
\item{stay:}{logical; TRUE (initial pick is maintained), FALSE: switch door choice
opened.door: integer between 1 and 3, the index to a goat
a.pick: integer between 1 and 3, the initial contestant pick}
}
\value{
... an integer between 1 and 3
}
\description{
\code{change_door(stay, opened.door, a.pick)}
The contestant strategy is represented by the stay logical parameter,
given the content of one of the doors has been revealed by the host.
}
\details{
There are two possible strategies: after the host opens a door with a goat,
the contestant either maintains their original selection or changes their
door pick.
}
\examples{
  game
  [1] "car"  "goat" "goat"
  a.pick <- select_door()
  a.pick
  [1] 2
  opened.door <- open_goat_door( game, a.pick )
  opened.door
  [1] 3
  final.pick <- change_door(TRUE, opened.door, a.pick )
  final.pick
  [1] 2

}
