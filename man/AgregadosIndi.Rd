% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Indelec.R
\name{AgregadosIndi}
\alias{AgregadosIndi}
\title{Utilities. Obtaining electoral indicators from aggregated data}
\usage{
AgregadosIndi(Ano = 0, Mes = "", RutaDescarga = "", Auto = TRUE, datos = "")
}
\arguments{
\item{Ano}{Required only for automatic procedure. It is the year of the electoral process that
the user wants to study. It must be a four-digit numerical value.}

\item{Mes}{Required only for automatic procedure.is the month of the electoral process, it must
be a string composed of two characters, associated with the month in which the elections were held.}

\item{RutaDescarga}{Required only for automatic procedure. must be a string indicating the local path
of the user's computer where to download the file from the Spanish Ministry of Home Office (MIR).
This file is then automatically deleted once the process is completed. The user has to bear in mind that this path must indicate a
place where R has read and write permission, otherwise the process will not be completed.}

\item{Auto}{It can take the logical values TRUE or FALSE. By default it has the value TRUE to indicate
that the user wants the automatic procedure. In the case of wanting a manual procedure, this parameter will take the FALSE value.}

\item{datos}{Required only for manual procedure. It must be a data.frame with three columns:
The first column containing the name of the parties, the second column the number of votes and
the third column the number of seats that the political party in question obtains.}
}
\value{
Returns a data.frame with four elements:

1.- Its name is 'dat' and it is a data.frame containing the votes and seats of each candidate party,
in absolute terms and in percentages, also the accumulated distribution of these values
and a last column containing an indicator of electoral disproportionality that is calculated
as the difference between the percentage of votes and the percentage of seats obtained.

2.- Its name is 'grafico' and it is a ggpplot object where the difference between the percentage of
votes and seats is represented in a bar chart.

3.- Its name is 'In_despro' and it is a data.fame containing the values of the disproportionality
indices indicated above.

4.- Its name is 'In_dimen' and it is a dataframe with the values of the voting dimension indicators
indicated above. Both the electoral and parliamentary versions are provided.
}
\description{
With this function, the following indicators of electoral disproportionality can be
obtained for aggregated data:
\itemize{
\item Sainte Lagüe (SL)
\item RAE (R)
\item RAE Corrected (Rco)
\item Loosemore & Hanby (LH)
\item Gallagher's Minimum Squares index (Gcm)
\item Maximum deviation index (Lmax)
\item Cox & Shugart index (Cs)
\item Corrected Cox & Shugart index (CS_correg)
\item Tukey's line (LT)
}

All the values of these indicators are in an output data.frame where the names of the columns
coincide with the identification of these indicators which appear between brackets above.

Likewise the following voting size indicators are also obtained:
\itemize{
\item Electoral and Parliamentary fragmentation (F)
\item Effective number of political parties (N)
\item Hyperfractionality index (Hiper)
\item Molinar's effective number of political parties (NP)
\item Concentration of vote (Con)
\item Competitiveness between parties (Comp)
}

The electoral and parliamentary versions are calculated for these voting size indicators.
Their values are stored in the output of this function within a data.frame with the
name of the columns indicated with the initials shown above in parentheses and the suffix '_electoral' is added
to indicate that it is the electoral version or the suffix '_parliamentary' to indicate
that it is the parliamentary version.

\strong{Important note}. With this function you can get the results automatically or manually.
With the automatic form, the data are downloaded from the Spanish Ministry of Home Office. With
the manual form, you have to provide the data using a data.frame with the structure that will
be indicated later.
}
\examples{
d<-AgregadosIndi(2019,"04",RutaDescarga = "F:/")


}
