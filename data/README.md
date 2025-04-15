# data

```{r}
HS_2018_19_data <- read_csv(file = "data/HS_2018_19_data.csv")
```

## HS_2018_19_data

- `category`: Developmental area being assessed (e.g., Social-Emotional, Cognitive, etc.).
- `bottom`: Lower bound of the assessment scale range for expectations.
- `top`: Upper bound of the assessment scale range for expectations.
- `number_children`: Number of children in a group or observation.
- `average`: Average assessment score for the group.
- `number_below`: Number of children scoring below expectations.
- `percent_below`: Percentage of children scoring below expectations.
- `number_meeting_exceeding`: Number of children meeting or exceeding expectations.
- `percent_meeting_exceeding`: Percentage of children meeting or exceeding expectations.
- `season`: Season in which the data was collected (Fall, Winter, or Spring).
- `year`: Academic year (e.g., "18-19").
- `age_group`: Age group of the children (e.g., "3-4" or "4-5").
