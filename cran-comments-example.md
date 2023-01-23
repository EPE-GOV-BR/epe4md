Check results: I always state that there were no errors or warnings. Any NOTEs 
go in a bulleted list. For each NOTE, I include the message from R CMD check and
a brief description of why I think it’s OK. If there were no NOTEs, I’d say 
“There were no ERRORs, WARNINGs or NOTEs”

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: 'R6'

  R6 is a build-time dependency.
 
-------------------------------------------------------------------------------- 

Downstream dependencies: If there are downstream dependencies, I run R CMD check
on each package and summarise the results. If there are no downstream 
dependencies, keep this section, but say: “There are currently no downstream 
dependencies for this package”.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of httr 
(https://github.com/wch/checkresults/blob/master/httr/r-release). 
All packages that I could install passed except:

* Ecoengine: this appears to be a failure related to config on 
  that machine. I couldn't reproduce it locally, and it doesn't 
  seem to be related to changes in httr (the same problem exists 
  with httr 0.4).
