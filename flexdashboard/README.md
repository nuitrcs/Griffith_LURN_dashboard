# To compile the .Rmd files and create images

This directory contains the dashboard code: `SI-29_dashboard.Rmd`.  To compile the code on the command line please see the commands below in the codeblock:  

```
$ conda activate r4-griffith-wsl
$ Rscript -e "rmarkdown::render('SI-29_dashboard.Rmd', params = list(patient_id = 100, patient_week = 0))"
$ Rscript -e "webshot::rmdshot(doc = 'SI-29_dashboard.Rmd', rmd_args = list(params = list(patient_id = 100, patient_week = 0)), file = 'SI-29_plots.png', vwidth = 1275, vheight = 1650)"
$ Rscript -e "webshot::rmdshot(doc = 'SI-29_dashboard.Rmd', rmd_args = list(params = list(patient_id = 100, patient_week = 0, show_table = TRUE)), file = 'SI-29_tables.png', vwidth = 1275, vheight = 2000, delay = 1)"
```
- The first line activates my `conda` environment, which contains all the libraries listed in the `README.md` file in the root directory.  (If you haven't created a conda environment, you don't need to execute that line).  
- The second line renders the .Rmd file into an .html file, which can be opened in a browser.  This file has two tabs, the first for the figures and the second for the table.  In this command there are two params: `patient_id` selects the desired patient to analyze, and `patient_week` selects the week of the doctor visit (can be any number in `[0, 4, 8, 12, 16, 20, 24]`).
- The third line renders the tab with the figures into a .png image.  This takes similar arguments to the previous command, and also has a file name and a width and height.  I selected these width and height values to be the same aspect ratio as an 8.5/11 inch paper.
- The fourth line is similar to the previous one, but renders the tab with the table to a .png image.  Note that there is a delay built in because the .Rmd code needs some time to selecte the correct tab. 

Please note that this code requires a `../data` directory with the synthetic data files (as described in the `README.md` file within the root directory of this repo).

In the future, I could probably create a double-clickable executable file that runs the Rscript command and opens the images.

The `old` directory here contains dashboards with different figures that are no longer used. 
