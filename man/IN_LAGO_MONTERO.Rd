% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Electoral_index.R
\name{IN_LAGO_MONTERO}
\alias{IN_LAGO_MONTERO}
\title{Nationalisation Index of Montero and Lago (2010)}
\usage{
IN_LAGO_MONTERO(
  Ano,
  Mes,
  Ruta,
  n_escanos = 350,
  Auto = TRUE,
  d_votos,
  d_escanos
)
}
\arguments{
\item{Ano}{(Only for automatic case) It represents the year of the data you want to download.
This data can be numeric or character (preferably numeric).}

\item{Mes}{(For automatic case only) This is the month in which the election was made. It must
be of the character type and two digits, i.e. "04" is valid but "4" is not.}

\item{Ruta}{(Only for automatic case) A path of the local hard disk where the file is downloaded
from the Ministry of Home Office must be indicated. Once the download is finished
the downloaded file is deleted automatically.}

\item{n_escanos}{(Only for automatic case) This is the total number of seats to be covered. By
default, it has a value of 350 which are the deputies who are elected to the Congress of Deputies in Spain.}

\item{Auto}{It contains a logical value that by default is TRUE, indicating that the automatic procedure
is done. If you pass to the function the FALSE value then the procedure would be manual.}

\item{d_votos}{(Sólo para procedimiento manual). Es un data.frame, conteniendo en la primera
columna la denominación de las regiones o circunscripciones. El resto de las columnas se
debe haber una por cada partido político que contenga los votos que ese partido ha tenido
en cada una de las circunscripciones, y la denominación de esa columna, serán las
siglas de ese partido.}

\item{d_escanos}{(For manual procedure only). It is a data.frame, containing in the first
column the name of the regions or districts. The rest of the columns must be one for each
political party containing the votes that each party has obtained in each of the districts, and
the name of that column will be the acronym of that party.}
}
\value{
The returned value is a list object, with three positions. The first position
contains the value of the index (it is called 'V_indice'), the second one the vector containing
the votes with respect to the national total expressed as part per unit (it is called 'Porcentaje_votos'),
and the third component are the disputed seats (expressed as part per unit) of the districts where the
political party under study is presented. Its name is 'Porcentaje_escanos'.
}
\description{
This nationalisation index, whose value ranges between 0 and 1,
was proposed by Lago and Montero (2010) and is based on the decision of the
parties to take part in the electoral process in all or only some districts. It
is calculated by taking into account the electoral results of the parties, as
well as the number of seats in the districts where they are elegible. The
formula that is used is as follows:

\deqn{E=\sum_{j=1}^{J}p_{j}^{e}*q_{j}}{E=sum(p_j*q_j;j=1,2,..,J)}

Where:
p_j is the number of votes obtained in all the territories over the total expressed as parts per unit.
q_j is the number of seats (out of the total number of seats) in the districts where political
formation j is presented, expressed as parts per unit.

Two procedures have been enabled for the calculation of this index, one will be called 'automatic' and
the other one 'manual'. With the automatic procedure it will use the data of the Spanish
Ministry of Home Office and, without further intervention, the indicator for the elections to the
General Courts that has been indicated with the parameters of the function will be calculated.
For the manual procedure, the data to be processed must be passed as an input of the function, as
indicated below in the section concerning the parameters.
}
\examples{
#Procedimiento automÃ¡tico
s<-IN_LAGO_MONTERO(2019,"04","F:/",n_escanos = 350)
if(!(is.null(s) || is.na(s))){
print(s$V_indice)
print(s$Porcentaje_votos)
print(s$Porcentaje_escanos)
}

#Procedimiento manual
da1 <- data.frame( # Contains the votes
 Reg=c("Alava","Albacete","Madrid","Barcelona","Valladolid"),
 PSOE=c(400,300,0,50,25),
 PP=c(300,200,10,150,2),
 Cs=c(400,0,3,300,45),
 Uno=c(465,23,341,263,0))

da2 <- data.frame( # Contains the total seats of each province
 Reg=c("Alava","Albacete","Madrid","Barcelona","Valladolid"),
 escanos=c(2,3,6,5,4))

s2<-IN_LAGO_MONTERO(Auto = FALSE,d_votos = da1,d_escanos = da2)
}
