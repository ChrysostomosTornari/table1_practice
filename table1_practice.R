library(boot)

# All based on https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html

data <- melanoma

#factoring
data$status <- factor(data$status,
                          levels = c(2,1,3),
                          labels = c("Alive", "Melanoma death", "Non-melanoma death"))
data$sex <- factor(data$sex,
                   levels = c(0, 1),
                   labels = c("Female", "Male"))

data$ulcer <- factor(data$ulcer,
                     levels = c(0,1),
                     labels = c("Absent", "Present"))
  
library(table1)  

label(data$sex) <- "Sex"
label(data$age) <- "Age"
units(data$age) <- "years"

label(data$ulcer) <- "Ulceration"
label(data$thickness) <- "Thickness"
units(data$thickness) <- "mm"
table1(~ sex + age + ulcer + thickness | status, data = data, caption = "Basic Stats", footnote = "Also known as Breslow thickness.")

# Improving the spans of the table

# The labels need to go into a list if you want to have spans

table_labels <- list(variables = list(sex = "Sex",
                                      age = "Age (years)", # Notice how it doesn't appear possible to use the units function here
                                      ulcer = "Ulceration",
                                      thickness = "Thickness (mm)"),
                     groups = list("", "Death", ""))

# The word 'death' is removed from the table headings now that it is in the spanning header
levels(data$status) <- c("Alive", "Melanoma", "Non-melanoma")

# The "Strata" go in the order we want them displayed

strata <- c(split(data, data$status), list(Total = data)) # DOESN't MAKE SENSE YET

# Here are the renders that I have used for years and had to fiddle with a lot

my.render.cont <- function(x){
  with(stats.apply.rounding(stats.default(x), digits = 2), c("test",
    "MEDIAN [IQR]" = sprintf("%s [%s-%s]", MEDIAN, Q1, Q3),
    "test2 mean (SD)" = sprintf("%s (&plusmn %s)", MEAN, SD)))
}

my.render.cat <- function(x){
  c("", sapply(stats.default(x), function(y) with(y, sprintf("%d (%0.0f %%)", FREQ, PCT))))
}

caption <- "Basic Stats2"
footnote <- "Also known as Breslow thickness.2"
table1(strata, table_labels, groupspan = c(1,2, 1), caption = caption,
       footnote = footnote, render.continuous = my.render.cont,
       render.categorical = my.render.cat)
# This explanation far down the exampled (not really in the help file) helps a lot:
# The list of recognized keywords comes from the output of the stats.default function and includes: 
# N, NMISS, MEAN, SD, CV, GMEAN, GCV, MEDIAN, MIN, MAX, IQR, Q1, Q2, Q3, T1, T2, FREQ, PCT. 
# Keyword matching is case insensitive, and any text other than the keywords is left untouched. 
# We can specify a vector of character strings, in which case each result will be displayed in its
# own row in the table. We can use a named vector to specify labels for each row; a dot (‘.’) 
# can be used to indicate that the abbreviated code string itself be used as the row label. 
# Significant digits can be controlled using the digits argument (default: 3). 
# Here is a continuation of the example from the previous section that produces the desired result:

# table1(strata, labels, groupspan=c(1, 3, 1),
#        render.continuous=c(.="Mean (CV%)", .="Median [Min, Max]",
#                            "Geo. mean (Geo. CV%)"="GMEAN (GCV%)"))

# There are neater ways to create nesting in the columns if you do not need to manually define a span WITHIN the data
# contained in a column i.e. if you are nesting the contents of one column against another.
# This is ver non-'tidy' as it seems to be designed to work with wide datasets but is probably very useful 
# for real-world datasets that I encounter

table1(~ age + ulcer + thickness | status*sex, data = data)
