# GettingDataCourseProject

This repository contains R code that downloads and does some preprocessing on Human
Activity Recognition data set. The full description and the data can be found
[here](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones).

See `CodeBook.md` for details.

## Notes

*  Only variables containing mean() & std() are used.
*  Requires the plyr & reshape packages.
*  Downloads the dataset if it is not in the current directory.
