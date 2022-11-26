## Resubmission
This is a resubmission. In this version I have:

* Adjusted two unit tests so they will not be able to fail on CRAN tests anymore
* Made sure the function to get API results fails gracefully, including all information possible, if the online resource is giving back a status other than 200

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.
