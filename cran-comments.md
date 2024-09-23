## Resubmission 1.0.1
Resubmission to address CRAN comments.

> The Title field should be in title case. Current version is: 'Assemble `SSN` Objects'. This is OK, but please use straight rather than directed single quotes.

Fixed.


> Is there some reference about the method you can add in the Description field in the form Authors (year) <doi:10.....>?

Added (Peterson, E.E. and Ver Hoef, J.M., (2014) <doi:10.18637/jss.v056.i02>) to Description field.

## Resubmission 1.0.0
Resubmission to address CRAN comments.

## R CMD check results SSNbler 1.0.0
0 errors | 0 warnings | 0 notes

win-builder results checking the development version of R (accessed by
running `devtools::check_win_devel()`).

There were no ERRORS or WARNINGS, but 1 NOTE:
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Erin Peterson <erin@peterson-consulting.com>'

The maintainer's name and email address are correct.

## Changes since last submission

* Updated package version number to adhere to <major><minor><patch> numbering

> Please always explain all acronyms in the description text. -> SSN
  * Defined SSN acronym in the description text.

> Please always write package names, software names and API
(application programming interface) names in single quotes in title
and description. e.g: --> 'SSN2'

Wrote package and software names with single quotes throughout package documentation, including the DESCRIPTION file. Wrote object class names with backticks.

> Please note that package names are case sensitive.

Noted. We did not see any examples where the case of the package name was incorrect.

> Please omit the redundant "in R" at the end of your title.

Removed.

> If there are references describing the methods in your package,
please add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'https:' and angle brackets for
auto-linking. (If you want to add a title as well please put it in
quotes: "Title")

Reference included.

> You write information messages to the console that cannot be easily
suppressed. It is more R like to generate objects that can be used to
extract the information a user is interested in, and then print() that
object. Instead of cat() rather use message()/warning() or
if(verbose)cat(..) (or maybe stop()) if you really have to write text
to the console. (except for print, summary, interactive functions) ->
R/ssn_assemble.R line 747

Modified ssn_assemble() to ensure that all calls to message() and cat() are wrapped in if(verbose == TRUE) statements, which allows users to easily suppress output messages printed to the
console. When arguments check and verbose are both set to TRUE,
details from ssn_check() are printed to the console. The SSN object is not checked if check = TRUE and verbose = FALSE. This warning is returned:

Warning message: In ssn_assemble(edges = edges, lsn_path = out.path, obs_sites = site.list[["obs"]],  :
check == TRUE and verbose == FALSE. SSN object will not be
checked. Set verbose = TRUE to check SSN object or run ssn_check() separately.

The user also has the option to run ssn_check() outside of
ssn_assemble(), which returns a simple TRUE/FALSE when verbose = FALSE.

> Please make sure that you do not change the user's options. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited.

In createBinaryID(), added an on.exit(setwd(old.wd)) call before
changing the working directory using local_dir() (Lines 3-6).


## New Release 0.1.0

This is a new release

## R CMD check results

Here is the output from a submission to win-builder checking the development version of R (accessed by running `devtools::check_win_devel()`).

0 errors | 0 warnings | 0 notes
