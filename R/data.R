

#' Properties of 8124 mushrooms.
#'
#' A dataset containing 23 categorical properties of 23 different species of gilled
#' mushrooms including a categorization if it is edible or not.
#'
#' @format A data frame with 8124 rows and 23 columns:
#' \describe{
#'   \item{bruises}{\code{bruises} \code{no}}
#'   \item{cap-color}{\code{brown} \code{yellow} \code{white} \code{gray} \code{red} \code{pink} \code{buff} \code{purple} \code{cinnamon} \code{green}}
#'   \item{cap-shape}{\code{convex} \code{bell} \code{sunken} \code{flat} \code{knobbed} \code{conical}}
#'   \item{cap-surface}{\code{smooth} \code{scaly} \code{fibrous} \code{grooves}}
#'   \item{edible}{\code{poisonous} \code{edible}}
#'   \item{gill-attachment}{\code{free} \code{attached}}
#'   \item{gill-color}{\code{black} \code{brown} \code{gray} \code{pink} \code{white} \code{chocolate} \code{purple} \code{red} \code{buff} \code{green} \code{yellow} \code{orange}}
#'   \item{gill-size}{\code{narrow} \code{broad}}
#'   \item{gill-spacing}{\code{close} \code{crowded}}
#'   \item{habitat}{\code{urban} \code{grasses} \code{meadows} \code{woods} \code{paths} \code{waste} \code{leaves}}
#'   \item{odor}{\code{pungent} \code{almond} \code{anise} \code{none} \code{foul} \code{creosote} \code{fishy} \code{spicy} \code{musty}}
#'   \item{population}{\code{scattered} \code{numerous} \code{abundant} \code{several} \code{solitary} \code{clustered}}
#'   \item{ring-number}{\code{one} \code{two} \code{none}}
#'   \item{ring-type}{\code{pendant} \code{evanescent} \code{large} \code{flaring} \code{none}}
#'   \item{spore-print-color}{\code{black} \code{brown} \code{purple} \code{chocolate} \code{white} \code{green} \code{orange} \code{yellow} \code{buff}}
#'   \item{stalk-color-above-ring}{\code{white} \code{gray} \code{pink} \code{brown} \code{buff} \code{red} \code{orange} \code{cinnamon} \code{yellow}}
#'   \item{stalk-color-below-ring}{\code{white} \code{pink} \code{gray} \code{buff} \code{brown} \code{red} \code{yellow} \code{orange} \code{cinnamon}}
#'   \item{stalk-root}{\code{equal} \code{club} \code{bulbous} \code{rooted} \code{NA}}
#'   \item{stalk-shape}{\code{enlarging} \code{tapering}}
#'   \item{stalk-surface-above-ring}{\code{smooth} \code{fibrous} \code{silky} \code{scaly}}
#'   \item{stalk-surface-below-ring}{\code{smooth} \code{fibrous} \code{scaly} \code{silky}}
#'   \item{veil-color}{\code{white} \code{brown} \code{orange} \code{yellow}}
#'   \item{veil-type}{\code{partial}}
#' }
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Mushroom}
#'
#'
#'
#' @details
#'   The records are drawn from
#'   G. H. Lincoff (1981) (Pres.),
#'   \emph{The Audubon Society Field Guide to North American Mushrooms}.
#'   New York: Alfred A. Knopf.
#'   (See pages 500--525 for the Agaricus and Lepiota Family.)
#'
#'   The Guide clearly states that there is no simple rule for determining
#'   the edibility of a mushroom; no rule like \dQuote{leaflets three, let
#'   it be} for Poisonous Oak and Ivy.
#'
#'  The actual dataset from the UCI repository has been cleaned up to properly
#'  label the missing values and have the full category names instead of their
#'  abbreviations.
#'
#' @references
#'   Blake, C.L. & Merz, C.J. (1998).
#'   UCI Repository of Machine Learning Databases.
#'   Irvine, CA: University of California, Department of Information and
#'   Computer Science.
#'
#'
#' @examples
#'   data("mushroom")
#'   summary(mushroom)
#'
#' @keywords datasets
#'
"mushroom"
