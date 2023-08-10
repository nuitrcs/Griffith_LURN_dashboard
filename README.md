# Griffith LURN dashboard
A dashboard to show data from Prof. James Griffith's urology project

To compile the .Rmd file and create images, use commands like (more instructions coming soon):
```
$ conda activate r4-griffith-wsl
$ Rscript -e "rmarkdown::render('AMGtest.Rmd', params = list(patient_id = 100, patient_week = 0))"
$ Rscript -e "webshot::rmdshot(doc = 'AMGtest.Rmd', rmd_args = list(params = list(patient_id = 100, patient_week = 0)), file = 'AMGtest_plots.png', vwidth = 1275, vheight = 1650)"
$ Rscript -e "webshot::rmdshot(doc = 'AMGtest.Rmd', rmd_args = list(params = list(patient_id = 100,  patient_week = 0, show_table = TRUE)), file = 'AMGtest_tables.png', vwidth = 1275, vheight = 1650, delay = 1)"

```

