% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_poke_types.R
\name{poke_types_ui}
\alias{poke_types_ui}
\alias{poke_types_server}
\title{UI module for generating the pokemon type}
\usage{
poke_types_ui(id)

poke_types_server(id, selected)
}
\arguments{
\item{id, }{character used to specify namespace, see \code{shiny::\link[shiny]{NS}}}

\item{selected}{Input containing the selected pokemon index.}
}
\value{
a \code{shiny::\link[shiny]{tagList}} containing UI elements
}
\description{
UI module for generating the pokemon type

Server module generating the pokemon types info boxes
}
