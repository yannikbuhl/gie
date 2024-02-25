## Resubmission
This is a resubmission. In this version I have:

* Made sure the function to get API results fails gracefully (i.e., only gives a message instead of an error or a warning), including all information possible, if the online resource is giving back a status other than 200.
* Significantly broadened the possible use of the package by adding support for another essential database
* Simplified function names
* Added two new functions to the package
* Added a helper dataset

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.
