# Getter for S3 objects
mcGet <- function(x, attr, ...) UseMethod("mcGet")

mcSet <- function(object, attr, new.attr, ...) UseMethod("mcSet")
