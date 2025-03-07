# data

Place data file(s) in this folder.

```{r
FWSdata <- read_csv(file = "data-raw/nobel.csv")}

Then, include codebooks (variables, and their descriptions) for your data file(s)
using the following format.

## name of data file

- `variable1`: Description of variable 1
- `variable2`: Description of variable 2
- `variable3`: Description of variable 3
- ...
