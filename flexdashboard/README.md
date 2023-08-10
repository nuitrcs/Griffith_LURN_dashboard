# To compile the .Rmd files and create images

(more instructions coming soon)

```
$ conda activate r4-griffith-wsl
$ Rscript -e "rmarkdown::render('SI-29_dashboard.Rmd', params = list(patient_id = 100, patient_week = 0))"
$ Rscript -e "webshot::rmdshot(doc = 'SI-29_dashboard.Rmd', rmd_args = list(params = list(patient_id = 100, patient_week = 0)), file = 'SI-29_plots.png', vwidth = 1275, vheight = 1650)"
$ Rscript -e "webshot::rmdshot(doc = 'SI-29_dashboard.Rmd', rmd_args = list(params = list(patient_id = 100, patient_week = 0, show_table = TRUE)), file = 'SI-29_tables.png', vwidth = 1275, vheight = 2000, delay = 1)"

```

In the future, I could probably create a double-clickable executable file that runs the Rscript command and opens the images.

