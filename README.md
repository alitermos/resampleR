# resampleR
A short script that lays out a complete resampling approach for unbiased data analysis. Data in R's ratdat library is used for demonstration. This R script is designed to process and analyze rodent dataset provided by the `ratdat` package. It focuses on exploring the dataset, cleaning the data with respect to missing values and unbalanced samples, and visualizing the proportions of male and female rodents across various species. The script ensures that the data used in analysis are not biased towards one sex by resampling the data where necessary.

## Dependencies
To run this script, you will need R installed on your machine along with the following R packages:
- `ratdat`: For accessing the rodent dataset.
- `ggplot2`: For data visualization.
- `scales`: For formatting plot scales.

These packages can be installed from CRAN using the following commands:
```r
install.packages("ratdat")
install.packages("ggplot2")
install.packages("scales")
```
## Note
The script comes as is with no generalizations or further automations for the sake of direct ease of use. Feel free to fork and modify for generalizations, automations, or change of sampling criteria. For questions related to resampleR, please contact me through *ali.adnan.termos@gmail.com* or *atermos@asu.edu*. 
