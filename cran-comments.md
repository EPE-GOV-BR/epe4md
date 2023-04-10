## R CMD check results

  This is the first submission.

    There was 1 NOTE:
    
* checking package dependencies ... NOTE
  Imports includes 21 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.
  
  All the packages listed in the import field are used.
  
* checking examples ... [199s] NOTE
  Examples with CPU (user + system) or elapsed time > 5s
                   user system elapsed
  epe4md_calcula 186.08   2.47  188.61
  
  The tests take so much time because they are using databases as variables and 
  they need more time to process than regular variables.

## Downstream dependencies

  There are currently no downstream dependencies for this package.
