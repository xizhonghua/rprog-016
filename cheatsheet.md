## R Cheat Sheet

format a file name with **fixed** id length with **leading zeros**
```R
> sprintf("%s/file_%03d.csv", "path/to/folder", 2)
[1] "path/to/folder/file_002.csv"
```

create a **data frame** with **column names**
```R
> id <- c(1,2,3)
> age <- c(20,18,22)
> gender <- c("m", "f", "f")
> data.frame(id, age, gender)
  id age gender
1  1  20      m
2  2  18      f
3  3  22      f
```
