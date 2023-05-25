## R CMD check results

    There was 5 NOTES:
    
❯ checking CRAN incoming feasibility ... [12s] NOTE
  
  This is the first submission.

❯ checking package dependencies ... NOTE
  Imports includes 21 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.
  
  All the packages listed in the import field are used.
  
❯ checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''
    
  As noted in [R-hub issue #560](https://github.com/r-hub/rhub/issues/560), 
  this could be due to something in R-hub and not on this package. According to 
  R-hub maintainer, this should be ignored.
  
  I am unable to find this NULL file/directory anywhere, even with the ls() 
  command in RStudio. I even deleted my local repo and re-cloned it, but this 
  note persists.

❯ checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
    
  As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), 
  this could be due to a bug/crash in MiKTeX and can likely be ignored.

## Downstream dependencies

  There are currently no downstream dependencies for this package.
