# This script is solely there to print the version of R and of the installed libraries
# of the running container.

print('SESSION INFO:')
print(sessionInfo())

print('Importing...')
library(sf)
library(curl)
library(rgbif)
library(rvertnet)
library(rinat)
library(rfishbase)

print('SESSION INFO NOW:')
print(sessionInfo())

print('DONE!')

