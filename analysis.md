# Setup

The source for all the repositories is located at
<https://carpentries.github.io/curriculum-feed/carpentries_lessons.json>.
To avoid having to pull from it multiple times I’m going to set up a
folder:

``` r
library("fs")       # Filesystem navigation
library("jsonlite") # parsing JSON files
library("purrr")    # handling lists (JSON files)
```

    ## 
    ## Attaching package: 'purrr'

    ## The following object is masked from 'package:jsonlite':
    ## 
    ##     flatten

``` r
library("dplyr")    # handling data frames and magic
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library("tidyr")    # separating values
library("ggplot2")  # visualization
library("forcats")  # ordering factors
library("magrittr") # for the %T>% pipe I love so well
```

    ## 
    ## Attaching package: 'magrittr'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

    ## The following object is masked from 'package:purrr':
    ## 
    ##     set_names

``` r
library("gh")       # accessing GitHub's API
library("polite")   # being respectful when downloading files
library("here")     # so I can always remember where I started
```

    ## here() starts at /home/zhian/Documents/Carpentries/Git/zkamvar--postmaul

``` r
library("pegboard") # parsing and analysis of carpentries episodes
library("git2r")    # downloading github repositories
```

    ## 
    ## Attaching package: 'git2r'

    ## The following object is masked from 'package:magrittr':
    ## 
    ##     add

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     pull

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     is_empty, when

``` r
library("waldo")    # comparing objects
```

``` r
raw_data <- here::here("data", "raw")
raw_lessons <- fs::path(raw_data, "carpentries_lessons.json")
if (fs::file_exists(raw_lessons)) {
  
} else {
  fs::dir_create(raw_data)
  res <- download.file(
    url = "https://carpentries.github.io/curriculum-feed/carpentries_lessons.json",
    destfile = raw_lessons
  )
  if (res == 0L) {
    fs::file_chmod(raw_lessons, "a-wx")
  }
}
```

    ## NULL

``` r
lessons <- read_json(raw_lessons, simplifyVector = FALSE) %>%
  purrr::map_dfr(.f = dplyr::as_tibble) %>%
  dplyr::mutate_if(is.character, 
    ~dplyr::if_else(. == "NA", as.character(NA), .)) %>%
  dplyr::mutate(life_cycle = forcats::fct_relevel(life_cycle,
      "pre-alpha", "alpha", "beta", "stable", "on-hold", "deprecated"
    )) %>%
  dplyr::mutate(life_cycle = forcats::fct_explicit_na(life_cycle)) %>%
  print()
```

    ## # A tibble: 88 x 6
    ##    repository        curriculum   life_cycle language `_row` URL                
    ##    <chr>             <chr>        <fct>      <chr>    <chr>  <chr>              
    ##  1 capstone-novice-… <NA>         pre-alpha  en       1      <NA>               
    ##  2 lc-spreadsheets   lc           beta       en       2      https://github.com…
    ##  3 rr-automation     reproducibl… on-hold    en       3      https://github.com…
    ##  4 lesson-example    <NA>         stable     en       4      https://github.com…
    ##  5 git-novice-es     swc-es       stable     es       5      https://github.com…
    ##  6 shell-economics   dc-economics pre-alpha  en       6      https://github.com…
    ##  7 openrefine-ecolo… dc-ecology   stable     en       7      https://github.com…
    ##  8 python-humanitie… <NA>         pre-alpha  en       8      <NA>               
    ##  9 hg-novice         swc          stable     en       9      https://github.com…
    ## 10 spreadsheet-huma… <NA>         pre-alpha  en       10     <NA>               
    ## # … with 78 more rows

# First look at available lessons

I’m going to take a look at all of the software carpentry lessons first.
There are currently 88 available lessons, but they are in different
stages of completion:

``` r
ggplot(lessons, aes(y = life_cycle, fill = curriculum)) +
  geom_bar(orientation = "y")
```

![](analysis_files/figure-gfm/lesson%20vis-1.png)<!-- -->

Let’s inspect the ones that are stable and then look at the ones that
are lower down, first we should filter for those that are stable and
actually have GitHub URLs:

``` r
stable <- lessons %>% filter(
  life_cycle == "stable", 
  curriculum != "instructor-training",
  !is.na(curriculum), # these are template repositories
  !is.na(URL)         # no GitHub URL is not very useful for me
)
stable
```

    ## # A tibble: 36 x 6
    ##    repository       curriculum life_cycle language `_row` URL                   
    ##    <chr>            <chr>      <fct>      <chr>    <chr>  <chr>                 
    ##  1 git-novice-es    swc-es     stable     es       5      https://github.com/sw…
    ##  2 openrefine-ecol… dc-ecology stable     en       7      https://github.com/da…
    ##  3 hg-novice        swc        stable     en       9      https://github.com/sw…
    ##  4 matlab-novice-i… swc        stable     en       11     https://github.com/sw…
    ##  5 r-novice-inflam… swc        stable     en       12     https://github.com/sw…
    ##  6 spreadsheet-eco… dc-ecology stable     en       13     https://github.com/da…
    ##  7 python-novice-g… swc        stable     en       16     https://github.com/sw…
    ##  8 lc-data-intro    lc         stable     en       19     https://github.com/Li…
    ##  9 R-ecology-lesson dc-ecology stable     en       20     https://github.com/da…
    ## 10 git-novice       swc        stable     en       24     https://github.com/sw…
    ## # … with 26 more rows

I end up with 36 repositories to play with:

``` r
ggplot(stable, aes(y = curriculum, fill = language)) +
  geom_bar(orientation = "y")
```

![](analysis_files/figure-gfm/curriculum-language-1.png)<!-- -->

# Inspection of repository features

I will use the {gh} package to inspect the features of each repository:

  - tags
  - directory structure
  - dependencies

I can use the GitHub API to get the contents of the repositories:
<https://developer.github.com/v3/repos/contents/#get-contents>. The {gh}
package allows me to write the responses to disk so that I don’t have to
query every time I want to re-run the analysis. If I wanted to do this
with fresh data, all I would need to do is to clear the data folder.

One of the tripping points here is that not all of the repositories will
have `_episodes_rmd/` directories, so I will need to walk over these
with `purrr::possibly()`, a nice little failsafe function.

``` r
safegh <- purrr::possibly(gh::gh, otherwise = list(NA))

rmd_episodes <- function(owner, repo) {
  safegh <- purrr::slowly(
    f    = purrr::possibly(gh::gh, otherwise = list(NA)),
    rate = rate_delay(pause = 2)
  )
  OR <- glue::glue("{owner}--{repo}")
  safegh("/repos/:owner/:repo/contents/_episodes_rmd", 
    owner = owner, 
    repo = repo, 
    .destfile = here::here(fs::path("data", "rmd_JSON", OR, ext = "json"))
  )
}

vorhees <- purrr::possibly(jsonlite::read_json, otherwise = list())

lesson_has_rmd <- . %>%
  tidyr::separate(URL, into = c(NA, NA, NA, "user", NA), sep = "/", remove = FALSE) %>%
  mutate(user = purrr::walk2(.x = user, .y = repository, .f = rmd_episodes)) %>%
  mutate(JSON = purrr::map2(.x = user, .y = repository,
    .f = ~vorhees(
      here::here("data", "rmd_JSON", glue::glue("{.x}--{.y}.json"))
    )
  )) %>%
  filter(lengths(JSON) > 2)
has_rmd <- stable %>% lesson_has_rmd
has_rmd_all <- lessons %>% 
  lesson_has_rmd %>% 
  filter(repository != "lesson-example") %>%
  arrange(life_cycle)
```

    ## Warning: Expected 5 pieces. Additional pieces discarded in 1 rows [6].

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--capstone-novice-spreadsheet-
    ## biblio.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--python-humanities-
    ## lesson.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--spreadsheet-humanities-
    ## lesson.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--R-genomics.json': No such
    ## file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--geospatial-python.json': No
    ## such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--deep-learning_intro.json':
    ## No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--docker-introduction.json':
    ## No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--sql-humanities-lesson.json':
    ## No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--NA.json': No such file or
    ## directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--Data-Science-for-Docs.json':
    ## No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--gap-lesson.json': No such
    ## file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--git-Rstudio-course.json': No
    ## such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--jupyter-notebooks-
    ## intro.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--shell-extras.json': No such
    ## file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--NA.json': No such file or
    ## directory
    
    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--NA.json': No such file or
    ## directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/
    ## Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--python-packaging-
    ## publishing.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--NA.json': No such file or
    ## directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--jupyter_maps.json': No such
    ## file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--IoT_arduino_nano.json': No
    ## such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/
    ## Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--OpenRefine-humanities-
    ## lesson.json': No such file or directory

After parsing, we find that we have RMarkdown files for a grand total of
15 lessons and 7 stable lessons. From here, we can grab these lessons to
see if we can build them with our docker container.

The lessons we have are:

``` r
knitr::kable(has_rmd_all %>% select(URL, curriculum, life_cycle, language))
```

| URL                                                           | curriculum           | life\_cycle | language |
| :------------------------------------------------------------ | :------------------- | :---------- | :------- |
| <https://github.com/LibraryCarpentry/lc-r>                    | lc                   | pre-alpha   | en       |
| <https://github.com/datacarpentry/genomics-r-intro>           | dc-genomics          | alpha       | en       |
| <https://github.com/swcarpentry/r-novice-inflammation>        | swc                  | stable      | en       |
| <https://github.com/swcarpentry/r-novice-gapminder>           | swc                  | stable      | en       |
| <https://github.com/datacarpentry/organization-geospatial>    | dc-geospatial        | stable      | en       |
| <https://github.com/swcarpentry/r-novice-gapminder-es>        | swc-es               | stable      | es       |
| <https://github.com/datacarpentry/r-intro-geospatial>         | dc-geospatial        | stable      | en       |
| <https://github.com/datacarpentry/r-raster-vector-geospatial> | dc-geospatial        | stable      | en       |
| <https://github.com/datacarpentry/r-socialsci>                | dc-socsci            | stable      | en       |
| <https://github.com/datacarpentry/rr-automation>              | reproducible-science | on-hold     | en       |
| <https://github.com/datacarpentry/rr-publication>             | reproducible-science | on-hold     | en       |
| <https://github.com/datacarpentry/rr-intro>                   | reproducible-science | on-hold     | en       |
| <https://github.com/datacarpentry/rr-version-control>         | reproducible-science | on-hold     | en       |
| <https://github.com/datacarpentry/rr-organization1>           | reproducible-science | on-hold     | en       |
| <https://github.com/datacarpentry/rr-literate-programming>    | reproducible-science | on-hold     | en       |

The command to use is:

``` bash
docker run --rm -it -e USER=$(id -u) -e GROUP=$(id -g) -v ${PWD}:/home/rstudio rocker/verse:4.0.0 make -C /home/rstudio lesson-md
```

Let’s download these repos:

``` r
rmdpath <- path("data", "rmd-repos")
if (!dir_exists(rmdpath)) {
  dir_create(path = rmdpath)
}
g <- function(u, r, path = rmdpath) {
  URL <- glue::glue("https://github.com/{u}/{r}.git")
  path <- glue::glue("{path}/{u}--{r}R{c(3, 4)}")
  if (fs::dir_exists(path[[1]])) {
    git2r::pull(repo = path[[1]])
    git2r::pull(repo = path[[2]])
  } else {
    git2r::clone(URL, local_path = path[[1]])
    git2r::clone(URL, local_path = path[[2]])
  }
}
purrr::walk2(has_rmd_all$user, has_rmd_all$repository, g, rmdpath) 
```

Now all the repos have been downloaded, we can render the episodes under
different versions of R and see how they go. Note that I have to use the
geospatial R container in order to get things to work properly.

``` r
get_path <- function(u, r) {
  fs::path_abs(fs::path("data", "rmd-repos", glue::glue("{u}--{r}")))
}

run_docker <- function(the_path, R_VERSION) {
  make____it <- "install2.r checkpoint && make -B -j 4 -C /home/rstudio lesson-md"
  docker_run <- "docker run --rm -it -v {the_path}:/home/rstudio"
  contai_ner <- "rocker/geospatial:{R_VERSION} /bin/bash -c '{make____it}'"
  system(glue::glue("{glue::glue(docker_run)} {glue::glue(contai_ner)}"))
}

dockin <- function(u, r) {

  the_path <- get_path(u, r)

  # Run with R 3.6.3 ---------------------------
  r3_path <- glue::glue("{the_path}R3")
  if (params$build_md) run_docker(r3_path, "3.6.3")
  r3 <- try(Lesson$new(r3_path, rmd = FALSE))

  # Run with R 4.0.0 ---------------------------
  r4_path <- glue::glue("{the_path}R4")
  if (params$build_md) run_docker(r4_path, "4.0.0")
  r4 <- try(Lesson$new(r4_path, rmd = FALSE))

  list(r3, r4)

}

res <- purrr::map2(has_rmd_all$user, has_rmd_all$repository, dockin)
```

    ## Error: PCDATA invalid Char value 27 [9]
    ## Error: PCDATA invalid Char value 27 [9]

``` r
names(res) <- has_rmd_all$repository
```

We can iterate and compare:

```` r
cmpr <- function(lesson, name) {

  cat(glue::glue("\n## Lesson: {name}\n\n"))
  if (!inherits(lesson[[1]], "Lesson")) {
    cat("\n\n----ERRORED----\n\n")
    return(invisible(NULL))
  }

  o1 <- map(lesson[[1]]$episodes, ~.x$output %>% xml2::xml_text())
  o2 <- map(lesson[[2]]$episodes, ~.x$output %>% xml2::xml_text())

  for (i in seq_along(lesson[[1]]$episodes)) {

    cat(glue::glue("\n#### Episode: {name}/{basename(lesson[[1]]$files[i])}\n"))
    cat("\n```diff\n")
    print(waldo::compare(o1[[i]], o2[[i]]))
    cat("\n```\n")
  }
}

purrr::walk2(res, names(res), cmpr)
````

## Lesson: lc-r

#### Episode: lc-r/00-before-we-start.md

``` diff
✔ No differences
```

#### Episode: lc-r/01-intro-to-r.md

``` diff
✔ No differences
```

#### Episode: lc-r/02-starting-with-data.md

``` diff
lines(x[[2]]) vs lines(y[[2]])
+ "✔ ggplot2 3.3.0     ✔ dplyr   0.8.5"
- "✔ ggplot2 3.3.1     ✔ dplyr   1.0.0"
  "✔ tibble  3.0.1     ✔ stringr 1.4.0"
+ "✔ tidyr   1.0.2     ✔ forcats 0.5.0"
- "✔ tidyr   1.1.0     ✔ forcats 0.5.0"
  "✔ purrr   0.3.4     "
  ""
```

#### Episode: lc-r/03-data-cleaning-and-transformation.md

``` diff
lines(x[[2]]) vs lines(y[[2]])
+ "✔ ggplot2 3.3.0     ✔ dplyr   0.8.5"
- "✔ ggplot2 3.3.1     ✔ dplyr   1.0.0"
  "✔ tibble  3.0.1     ✔ stringr 1.4.0"
+ "✔ tidyr   1.0.2     ✔ forcats 0.5.0"
- "✔ tidyr   1.1.0     ✔ forcats 0.5.0"
  "✔ purrr   0.3.4     "
  ""

lines(x[[14]]) vs lines(y[[14]])
+ "# A tibble: 10 x 2"
- "`summarise()` ungrouping output (override with `.groups` argument)"
+ "   format       mean_checkouts"
+ "   <chr>                 <dbl>"
+ " 1 book                3.23   "
+ " 2 cd-rom              0.333  "
+ " 3 database            0      "
+ " 4 e-gov doc           0.0402 "
+ " 5 image               0.0275 "
+ " 6 kit/object          1.33   "
+ " 7 map                10.6    "
+ " 8 microform           0.00122"
+ " 9 online video        0      "
+ "10 serial              0      "
  ""

lines(x[[15]]) vs lines(y[[15]])
+ "# A tibble: 34 x 3"
- "# A tibble: 10 x 2"
+ "   call_class count sum_tot_chkout"
- "   format       mean_checkouts"
+ "   <chr>      <int>          <dbl>"
- "   <chr>                 <dbl>"
+ " 1 E            487           3114"
- " 1 book                3.23   "
+ " 2 <NA>         459           3024"
- " 2 cd-rom              0.333  "
+ " 3 H           1142           2902"
- " 3 database            0      "
+ " 4 P            800           2645"
- " 4 e-gov doc           0.0402 "
+ " 5 F            240           1306"
- " 5 image               0.0275 "
+ " 6 Q            333           1305"
- " 6 kit/object          1.33   "
+ " 7 B            426           1233"
- " 7 map                10.6    "
and 8 more ...

`lines(x[[16]])` is absent
`lines(y[[16]])` is a character vector ('`summarise()` ungrouping output (override with `.groups` argument)', '')

`lines(x[[17]])` is absent
`lines(y[[17]])` is a character vector ('# A tibble: 34 x 3', '   call_class count sum_tot_chkout', '   <chr>      <int>          <dbl>', ' 1 E            487           3114', ' 2 <NA>         459           3024', ...)
```

#### Episode: lc-r/04-data-viz-ggplot.md

``` diff
lines(x[[2]]) vs lines(y[[2]])
+ "✔ ggplot2 3.3.0     ✔ dplyr   0.8.5"
- "✔ ggplot2 3.3.1     ✔ dplyr   1.0.0"
  "✔ tibble  3.0.1     ✔ stringr 1.4.0"
+ "✔ tidyr   1.0.2     ✔ forcats 0.5.0"
- "✔ tidyr   1.1.0     ✔ forcats 0.5.0"
  "✔ purrr   0.3.4     "
  ""

lines(x[[5]]) vs lines(y[[5]])
+ "The following objects are masked from 'package:dplyr':"
- "The following objects are masked from 'package:base':"
  ""
+ "    intersect, setdiff, union"
- "    date, intersect, setdiff, union"
  ""

lines(x[[6]]) vs lines(y[[6]])
+ "The following objects are masked from 'package:base':"
- "`stat_bin()` using `bins = 30`. Pick better value with `binwidth`."
+ ""
+ "    date, intersect, setdiff, union"
  ""

lines(x[[7]]) vs lines(y[[7]])
+ "`stat_bin()` using `bins = 30`. Pick better value with `binwidth`."
- ""
- "   0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 "
- "2348  875  638  464  362  282  199  146  118   97   84   50   41   46   40   33 "
- "  16   17   18   19   20   21   22   23   24   25   26   27   28   29   30   31 "
- "  17   20   26   17   14   12    7   15    7    8    6    6    3    3    2    4 "
- "  32   33   34   35   36   38   39   40   41   43   47   61   63   69   79  106 "
- "   1    5    4    3    2    2    3    1    1    1    1    1    2    1    1    1 "
- " 113 "
- "   1 "
  ""

lines(x[[8]]) vs lines(y[[8]])
+ ""
- "[1] \"numeric\""
+ "   0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 "
+ "2348  875  638  464  362  282  199  146  118   97   84   50   41   46   40   33 "
+ "  16   17   18   19   20   21   22   23   24   25   26   27   28   29   30   31 "
+ "  17   20   26   17   14   12    7   15    7    8    6    6    3    3    2    4 "
+ "  32   33   34   35   36   38   39   40   41   43   47   61   63   69   79  106 "
+ "   1    5    4    3    2    2    3    1    1    1    1    1    2    1    1    1 "
+ " 113 "
+ "   1 "
  ""

`lines(x[[9]])`: "[1] \"numeric\"" ""
`lines(y[[9]])`: "[1] \"Date\""    ""

lines(x[[10]]) vs lines(y[[10]])
+ "[1] \"Date\""
- "`summarise()` ungrouping output (override with `.groups` argument)"
  ""
```

## Lesson: genomics-r-intro

#### Episode: genomics-r-intro/01-introduction.md

``` diff
✔ No differences
```

#### Episode: genomics-r-intro/02-r-basics.md

``` diff
✔ No differences
```

#### Episode: genomics-r-intro/03-basics-factors-dataframes.md

``` diff
```

## Warning in diff\_myers(args\[\[“a”\]\], args\[\[“b”\]\], max.diffs =

## args\[\[“max.diffs”\]\], : Exceeded `max.diffs`: 13 vs 100 allowed. Diff is probably

## suboptimal.

## Warning in diff\_myers(args\[\[“a”\]\], args\[\[“b”\]\], max.diffs =

## args\[\[“max.diffs”\]\], : Exceeded `max.diffs`: 13 vs 100 allowed. Diff is probably

## suboptimal.

``` 

lines(x[[1]]) vs lines(y[[1]])
+ "      sample_id          CHROM          POS             ID         "
- "  sample_id            CHROM                POS             ID         "
+ " SRR2584863: 25   CP000819.1:801   Min.   :   1521   Mode:logical  "
- " Length:801         Length:801         Min.   :   1521   Mode:logical  "
+ " SRR2584866:766                    1st Qu.:1115970   NA's:801      "
- " Class :character   Class :character   1st Qu.:1115970   NA's:801      "
+ " SRR2589044: 10                    Median :2290361                 "
- " Mode  :character   Mode  :character   Median :2290361                 "
+ "                                   Mean   :2243682                 "
- "                                       Mean   :2243682                 "
+ "                                   3rd Qu.:3317082                 "
- "                                       3rd Qu.:3317082                 "
+ "                                   Max.   :4629225                 "
- "                                       Max.   :4629225                 "
+ "                                                                   "
- "                                                                       "
+ "      REF            ALT           QUAL          FILTER          INDEL        "
- "     REF                ALT                 QUAL          FILTER       "
+ " G      :214   A       :211   Min.   :  4.385   Mode:logical   Mode :logical  "
- " Length:801         Length:801         Min.   :  4.385   Mode:logical  "
and 101 more ...

lines(x[[2]])[1:10] vs lines(y[[2]])[1:10]
  "'data.frame':\t801 obs. of  29 variables:"
+ " $ sample_id    : Factor w/ 3 levels \"SRR2584863\",\"SRR2584866\",..: 1 1 1 1 1 1 1 1 1 1 ..."
- " $ sample_id    : chr  \"SRR2584863\" \"SRR2584863\" \"SRR2584863\" \"SRR2584863\" ..."
+ " $ CHROM        : Factor w/ 1 level \"CP000819.1\": 1 1 1 1 1 1 1 1 1 1 ..."
- " $ CHROM        : chr  \"CP000819.1\" \"CP000819.1\" \"CP000819.1\" \"CP000819.1\" ..."
  " $ POS          : int  9972 263235 281923 433359 473901 648692 1331794 1733343 2103887 2333538 ..."
  " $ ID           : logi  NA NA NA NA NA NA ..."
+ " $ REF          : Factor w/ 59 levels \"A\",\"ACAGCCAGCCAGCCAGCCAGCCAGCCAGCCAG\",..: 49 33 33 30 24 16 16 33 2 12 ..."
- " $ REF          : chr  \"T\" \"G\" \"G\" \"CTTTTTTT\" ..."
+ " $ ALT          : Factor w/ 57 levels \"A\",\"AC\",\"ACAGCCAGCCAGCCAGCCAGCCAGCCAGCCAGCCAG\",..: 31 46 46 29 25 46 1 1 4 15 ..."
- " $ ALT          : chr  \"G\" \"T\" \"T\" \"CTTTTTTTT\" ..."
  " $ QUAL         : num  91 85 217 64 228 210 178 225 56 167 ..."
  " $ FILTER       : logi  NA NA NA NA NA NA ..."
  " $ INDEL        : logi  FALSE FALSE FALSE TRUE TRUE FALSE ..."

lines(x[[2]])[22:31] vs lines(y[[2]])[22:31]
  " $ HOB          : logi  NA NA NA NA NA NA ..."
  " $ AC           : int  1 1 1 1 1 1 1 1 1 1 ..."
  " $ AN           : int  1 1 1 1 1 1 1 1 1 1 ..."
+ " $ DP4          : Factor w/ 217 levels \"0,0,0,2\",\"0,0,0,3\",..: 3 132 73 141 176 104 61 74 133 137 ..."
- " $ DP4          : chr  \"0,0,0,4\" \"0,1,0,5\" \"0,0,4,5\" \"0,1,3,8\" ..."
  " $ MQ           : int  60 33 60 60 60 60 60 60 60 60 ..."
+ " $ Indiv        : Factor w/ 3 levels \"/home/dcuser/dc_workshop/results/bam/SRR2584863.aligned.sorted.bam\",..: 1 1 1 1 1 1 1 1 1 1 ..."
- " $ Indiv        : chr  \"/home/dcuser/dc_workshop/results/bam/SRR2584863.aligned.sorted.bam\" \"/home/dcuser/dc_workshop/results/bam/SRR2584863.aligned.sorted.bam\" \"/home/dcuser/dc_workshop/results/bam/SRR2584863.aligned.sorted.bam\" \"/home/dcuser/dc_workshop/results/bam/SRR2584863.aligned.sorted.bam\" ..."
+ " $ gt_PL        : Factor w/ 206 levels \"100,0\",\"103,0\",..: 16 10 134 198 142 127 93 142 9 80 ..."
- " $ gt_PL        : chr  \"121,0\" \"112,0\" \"247,0\" \"91,0\" ..."
  " $ gt_GT        : int  1 1 1 1 1 1 1 1 1 1 ..."
+ " $ gt_GT_alleles: Factor w/ 57 levels \"A\",\"AC\",\"ACAGCCAGCCAGCCAGCCAGCCAGCCAGCCAGCCAG\",..: 31 46 46 29 25 46 1 1 4 15 ..."
- " $ gt_GT_alleles: chr  \"G\" \"T\" \"T\" \"CTTTTTTTT\" ..."
  ""

lines(x[[3]]) vs lines(y[[3]])
+ "[1] T        G        G        CTTTTTTT CCGC     C       "
- "[1] \"T\"        \"G\"        \"G\"        \"CTTTTTTT\" \"CCGC\"     \"C\"       "
+ "59 Levels: A ACAGCCAGCCAGCCAGCCAGCCAGCCAGCCAG ACCCC ACCCCCCC ... TGGGGGGG"
  ""

lines(x[[4]]) vs lines(y[[4]])
+ " Factor w/ 59 levels \"A\",\"ACAGCCAGCCAGCCAGCCAGCCAGCCAGCCAG\",..: 49 33 33 30 24 16 16 33 2 12 ..."
- " chr [1:801] \"T\" \"G\" \"G\" \"CTTTTTTT\" \"CCGC\" \"C\" \"C\" \"G\" ..."
  ""

    lines(x[[5]])                              | lines(y[[5]])           
[1] "[1] SRR2584863"                           - "[1] \"SRR2584863\"" [1]
[2] "Levels: SRR2584863 SRR2584866 SRR2589044" -                         
[3] ""                                         | ""                   [2]

`lines(x[[7]])`: "[1] T"     "57 Levels: A AC ... TGGGGGGGGG" ""
`lines(y[[7]])`: "[1] \"T\""                                  ""

lines(x[[10]]) vs lines(y[[10]])
+ "[1] SRR2584863 SRR2584863 SRR2584863 SRR2584863"
- "[1] \"SRR2584863\" \"SRR2584863\" \"SRR2584863\" \"SRR2584863\""
+ "Levels: SRR2584863 SRR2584866 SRR2589044"
  ""

lines(x[[12]]) vs lines(y[[12]])
+ "[1] SRR2584863 SRR2584863 SRR2584863 SRR2584863 SRR2584863 SRR2584863"
- "[1] \"SRR2584863\" \"SRR2584863\" \"SRR2584863\" \"SRR2584863\" \"SRR2584863\""
+ "Levels: SRR2584863 SRR2584866 SRR2589044"
- "[6] \"SRR2584863\""
  ""

lines(x[[15]]) vs lines(y[[15]])
+ "[1] SRR2584863 SRR2584863 SRR2584863 SRR2584863 SRR2584863 SRR2584863"
- "[1] \"SRR2584863\" \"SRR2584863\" \"SRR2584863\" \"SRR2584863\" \"SRR2584863\""
+ "Levels: SRR2584863 SRR2584866 SRR2589044"
- "[6] \"SRR2584863\""
  ""

And 3 more differences ...
```

#### Episode: genomics-r-intro/04-dplyr.md

``` diff
lines(x[[4]]) vs lines(y[[4]])
  "Rows: 25"
  "Columns: 3"
+ "$ REF <fct> T, G, G, CTTTTTTT, CCGC, C, C, G, ACAGCCAGCCAGCCAGCCAGCCAGCCAGCCA…"
- "$ REF <chr> \"T\", \"G\", \"G\", \"CTTTTTTT\", \"CCGC\", \"C\", \"C\", \"G\", \"ACAGCCAGCCAGCC…"
+ "$ ALT <fct> G, T, T, CTTTTTTTT, CCGCGC, T, A, A, ACAGCCAGCCAGCCAGCCAGCCAGCCAG…"
- "$ ALT <chr> \"G\", \"T\", \"T\", \"CTTTTTTTT\", \"CCGCGC\", \"T\", \"A\", \"A\", \"ACAGCCAGCCA…"
  "$ DP  <int> 4, 6, 10, 12, 10, 10, 8, 11, 3, 7, 9, 20, 12, 19, 15, 10, 14, 9, …"
  ""

lines(x[[7]])[1:11] vs lines(y[[7]])[1:11]
  "Rows: 801"
  "Columns: 30"
+ "$ sample_id     <fct> SRR2584863, SRR2584863, SRR2584863, SRR2584863, SRR2584…"
- "$ sample_id     <chr> \"SRR2584863\", \"SRR2584863\", \"SRR2584863\", \"SRR2584863\",…"
+ "$ CHROM         <fct> CP000819.1, CP000819.1, CP000819.1, CP000819.1, CP00081…"
- "$ CHROM         <chr> \"CP000819.1\", \"CP000819.1\", \"CP000819.1\", \"CP000819.1\",…"
  "$ POS           <int> 9972, 263235, 281923, 433359, 473901, 648692, 1331794, …"
  "$ ID            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…"
+ "$ REF           <fct> T, G, G, CTTTTTTT, CCGC, C, C, G, ACAGCCAGCCAGCCAGCCAGC…"
- "$ REF           <chr> \"T\", \"G\", \"G\", \"CTTTTTTT\", \"CCGC\", \"C\", \"C\", \"G\", \"ACAG…"
+ "$ ALT           <fct> G, T, T, CTTTTTTTT, CCGCGC, T, A, A, ACAGCCAGCCAGCCAGCC…"
- "$ ALT           <chr> \"G\", \"T\", \"T\", \"CTTTTTTTT\", \"CCGCGC\", \"T\", \"A\", \"A\", \"A…"
  "$ QUAL          <dbl> 91.0000, 85.0000, 217.0000, 64.0000, 228.0000, 210.0000…"
  "$ FILTER        <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…"
  "$ INDEL         <lgl> FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, T…"

lines(x[[7]])[23:33] vs lines(y[[7]])[23:33]
  "$ HOB           <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…"
  "$ AC            <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…"
  "$ AN            <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…"
+ "$ DP4           <fct> \"0,0,0,4\", \"0,1,0,5\", \"0,0,4,5\", \"0,1,3,8\", \"1,0,2,7\", …"
- "$ DP4           <chr> \"0,0,0,4\", \"0,1,0,5\", \"0,0,4,5\", \"0,1,3,8\", \"1,0,2,7\", …"
  "$ MQ            <int> 60, 33, 60, 60, 60, 60, 60, 60, 60, 60, 25, 60, 10, 60,…"
+ "$ Indiv         <fct> /home/dcuser/dc_workshop/results/bam/SRR2584863.aligned…"
- "$ Indiv         <chr> \"/home/dcuser/dc_workshop/results/bam/SRR2584863.aligne…"
+ "$ gt_PL         <fct> \"121,0\", \"112,0\", \"247,0\", \"91,0\", \"255,0\", \"240,0\", \"2…"
- "$ gt_PL         <chr> \"121,0\", \"112,0\", \"247,0\", \"91,0\", \"255,0\", \"240,0\", \"2…"
  "$ gt_GT         <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…"
+ "$ gt_GT_alleles <fct> G, T, T, CTTTTTTTT, CCGCGC, T, A, A, ACAGCCAGCCAGCCAGCC…"
- "$ gt_GT_alleles <chr> \"G\", \"T\", \"T\", \"CTTTTTTTT\", \"CCGCGC\", \"T\", \"A\", \"A\", \"A…"
  "$ POLPROB       <dbl> 1.0000000, 1.0000000, 1.0000000, 0.9999996, 1.0000000, …"
  ""

lines(x[[8]])[1:11] vs lines(y[[8]])[1:11]
  "Rows: 801"
  "Columns: 30"
+ "$ sample_id     <fct> SRR2584863, SRR2584863, SRR2584863, SRR2584863, SRR2584…"
- "$ sample_id     <chr> \"SRR2584863\", \"SRR2584863\", \"SRR2584863\", \"SRR2584863\",…"
+ "$ CHROM         <fct> CP000819.1, CP000819.1, CP000819.1, CP000819.1, CP00081…"
- "$ CHROM         <chr> \"CP000819.1\", \"CP000819.1\", \"CP000819.1\", \"CP000819.1\",…"
  "$ POS           <int> 9972, 263235, 281923, 433359, 473901, 648692, 1331794, …"
  "$ ID            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…"
+ "$ REF           <fct> T, G, G, CTTTTTTT, CCGC, C, C, G, ACAGCCAGCCAGCCAGCCAGC…"
- "$ REF           <chr> \"T\", \"G\", \"G\", \"CTTTTTTT\", \"CCGC\", \"C\", \"C\", \"G\", \"ACAG…"
+ "$ ALT           <fct> G, T, T, CTTTTTTTT, CCGCGC, T, A, A, ACAGCCAGCCAGCCAGCC…"
- "$ ALT           <chr> \"G\", \"T\", \"T\", \"CTTTTTTTT\", \"CCGCGC\", \"T\", \"A\", \"A\", \"A…"
  "$ QUAL          <dbl> 91.0000, 85.0000, 217.0000, 64.0000, 228.0000, 210.0000…"
  "$ FILTER        <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…"
  "$ INDEL         <lgl> FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, T…"

lines(x[[8]])[23:33] vs lines(y[[8]])[23:33]
  "$ HOB           <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…"
  "$ AC            <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…"
  "$ AN            <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…"
+ "$ DP4           <fct> \"0,0,0,4\", \"0,1,0,5\", \"0,0,4,5\", \"0,1,3,8\", \"1,0,2,7\", …"
- "$ DP4           <chr> \"0,0,0,4\", \"0,1,0,5\", \"0,0,4,5\", \"0,1,3,8\", \"1,0,2,7\", …"
  "$ MQ            <int> 60, 33, 60, 60, 60, 60, 60, 60, 60, 60, 25, 60, 10, 60,…"
+ "$ Indiv         <fct> /home/dcuser/dc_workshop/results/bam/SRR2584863.aligned…"
- "$ Indiv         <chr> \"/home/dcuser/dc_workshop/results/bam/SRR2584863.aligne…"
+ "$ gt_PL         <fct> \"121,0\", \"112,0\", \"247,0\", \"91,0\", \"255,0\", \"240,0\", \"2…"
- "$ gt_PL         <chr> \"121,0\", \"112,0\", \"247,0\", \"91,0\", \"255,0\", \"240,0\", \"2…"
  "$ gt_GT         <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…"
+ "$ gt_GT_alleles <fct> G, T, T, CTTTTTTTT, CCGCGC, T, A, A, ACAGCCAGCCAGCCAGCC…"
- "$ gt_GT_alleles <chr> \"G\", \"T\", \"T\", \"CTTTTTTTT\", \"CCGCGC\", \"T\", \"A\", \"A\", \"A…"
  "$ POLPROB       <dbl> 1.0000000, 1.0000000, 1.0000000, 0.9999996, 1.0000000, …"
  ""

lines(x[[10]]) vs lines(y[[10]])
+ "# A tibble: 3 x 2"
- "`summarise()` ungrouping output (override with `.groups` argument)"
+ "  sample_id  `n()`"
+ "  <fct>      <int>"
+ "1 SRR2584863    25"
+ "2 SRR2584866   766"
+ "3 SRR2589044    10"
  ""

    lines(x[[11]])           | lines(y[[11]])          
[1] "# A tibble: 3 x 2"      | "# A tibble: 3 x 2"  [1]
[2] "  sample_id  `max(DP)`" - "  sample_id  `n()`" [2]
[3] "  <fct>          <int>" - "  <chr>      <int>" [3]
[4] "1 SRR2584863        20" - "1 SRR2584863    25" [4]
[5] "2 SRR2584866        79" - "2 SRR2584866   766" [5]
[6] "3 SRR2589044        16" - "3 SRR2589044    10" [6]
[7] ""                       | ""                   [7]

`lines(x[[12]])` is absent
`lines(y[[12]])` is a character vector ('`summarise()` ungrouping output (override with `.groups` argument)', '')

`lines(x[[13]])` is absent
`lines(y[[13]])` is a character vector ('# A tibble: 3 x 2', '  sample_id  `max(DP)`', '  <chr>          <int>', '1 SRR2584863        20', '2 SRR2584866        79', ...)
```

#### Episode: genomics-r-intro/05-data-visualization.md

``` diff
✔ No differences
```

#### Episode: genomics-r-intro/XX-knitr-markdown.md

``` diff
✔ No differences
```

## Lesson: r-novice-inflammation

#### Episode: r-novice-inflammation/01-starting-with-data.md

``` diff
✔ No differences
```

#### Episode: r-novice-inflammation/02-func-R.md

``` diff
✔ No differences
```

#### Episode: r-novice-inflammation/03-loops-R.md

``` diff
✔ No differences
```

#### Episode: r-novice-inflammation/04-cond.md

``` diff
✔ No differences
```

#### Episode: r-novice-inflammation/05-cmdline.md

``` diff
lines(x[[2]])[1:9] vs lines(y[[2]])[1:9]
+ "R version 3.6.3 (2020-02-29)"
- "R version 4.0.0 (2020-04-24)"
  "Platform: x86_64-pc-linux-gnu (64-bit)"
+ "Running under: Debian GNU/Linux 10 (buster)"
- "Running under: Ubuntu 20.04 LTS"
  ""
  "Matrix products: default"
+ "BLAS/LAPACK: /usr/lib/x86_64-linux-gnu/libopenblasp-r0.3.5.so"
- "BLAS/LAPACK: /usr/lib/x86_64-linux-gnu/openblas-openmp/libopenblasp-r0.3.8.so"
  ""
  "locale:"
  " [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              "

lines(x[[2]])[17:21] vs lines(y[[2]])[17:21]
  "[1] stats     graphics  grDevices utils     datasets  methods   base     "
  ""
  "loaded via a namespace (and not attached):"
+ "[1] compiler_3.6.3"
- "[1] compiler_4.0.0"
  ""

    lines(x[[4]])                 | lines(y[[4]])                    
[1] "/usr/local/lib/R/bin/exec/R" | "/usr/local/lib/R/bin/exec/R" [1]
[2] "--no-save"                   | "--no-save"                   [2]
[3] "--no-restore"                | "--no-restore"                [3]
[4] "--slave"                     - "--no-echo"                   [4]
[5] "--no-restore"                | "--no-restore"                [5]
[6] "--file=print-args.R"         | "--file=print-args.R"         [6]
[7] ""                            | ""                            [7]

    lines(x[[6]])                 | lines(y[[6]])                    
[1] "/usr/local/lib/R/bin/exec/R" | "/usr/local/lib/R/bin/exec/R" [1]
[2] "--no-save"                   | "--no-save"                   [2]
[3] "--no-restore"                | "--no-restore"                [3]
[4] "--slave"                     - "--no-echo"                   [4]
[5] "--no-restore"                | "--no-restore"                [5]
[6] "--file=print-args.R"         | "--file=print-args.R"         [6]
[7] "--args"                      | "--args"                      [7]
```

#### Episode: r-novice-inflammation/06-best-practices-R.md

``` diff
✔ No differences
```

#### Episode: r-novice-inflammation/07-knitr-R.md

``` diff
✔ No differences
```

#### Episode: r-novice-inflammation/08-making-packages-R.md

``` diff
✔ No differences
```

#### Episode: r-novice-inflammation/09-supp-intro-rstudio.md

``` diff
✔ No differences
```

#### Episode: r-novice-inflammation/10-supp-addressing-data.md

``` diff
✔ No differences
```

#### Episode: r-novice-inflammation/11-supp-read-write-csv.md

``` diff
lines(x[[4]]) vs lines(y[[4]])
+ "  [1] \"Green\" \"1\"     \"Green\" \"5\"     \"4\"     \"Green\" \"Green\" \"2\"     \"5\"    "
- "  [1] \"Green\" \" Red\"  \"Green\" \"White\" \"Red\"   \"Green\" \"Green\" \"Black\" \"White\""
+ " [10] \"4\"     \"4\"     \"5\"     \"Green\" \"Green\" \"2\"     \"4\"     \"Green\" \"Green\""
- " [10] \"Red\"   \"Red\"   \"White\" \"Green\" \"Green\" \"Black\" \"Red\"   \"Green\" \"Green\""
+ " [19] \"5\"     \"Green\" \"Green\" \"Green\" \"4\"     \"Green\" \"4\"     \"4\"     \"4\"    "
- " [19] \"White\" \"Green\" \"Green\" \"Green\" \"Red\"   \"Green\" \"Red\"   \"Red\"   \"Red\"  "
+ " [28] \"4\"     \"5\"     \"Green\" \"4\"     \"5\"     \"2\"     \"4\"     \"2\"     \"2\"    "
- " [28] \"Red\"   \"White\" \"Green\" \"Red\"   \"White\" \"Black\" \"Red\"   \"Black\" \"Black\""
+ " [37] \"Green\" \"4\"     \"2\"     \"4\"     \"2\"     \"2\"     \"4\"     \"4\"     \"5\"    "
- " [37] \"Green\" \"Red\"   \"Black\" \"Red\"   \"Black\" \"Black\" \"Red\"   \"Red\"   \"White\""
+ " [46] \"2\"     \"Green\" \"4\"     \"4\"     \"2\"     \"2\"     \"4\"     \"5\"     \"4\"    "
- " [46] \"Black\" \"Green\" \"Red\"   \"Red\"   \"Black\" \"Black\" \"Red\"   \"White\" \"Red\"  "
+ " [55] \"Green\" \"Green\" \"2\"     \"Green\" \"5\"     \"2\"     \"4\"     \"Green\" \"Green\""
- " [55] \"Green\" \"Green\" \"Black\" \"Green\" \"White\" \"Black\" \"Red\"   \"Green\" \"Green\""
+ " [64] \"5\"     \"2\"     \"4\"     \"4\"     \"2\"     \"Green\" \"5\"     \"Green\" \"4\"    "
- " [64] \"White\" \"Black\" \"Red\"   \"Red\"   \"Black\" \"Green\" \"White\" \"Green\" \"Red\"  "
+ " [73] \"5\"     \"5\"     \"Green\" \"Green\" \"Green\" \"Green\" \"Green\" \"5\"     \"2\"    "
- " [73] \"White\" \"White\" \"Green\" \"Green\" \"Green\" \"Green\" \"Green\" \"White\" \"Black\""
+ " [82] \"Green\" \"5\"     \"2\"     \"2\"     \"4\"     \"4\"     \"5\"     \"5\"     \"5\"    "
- " [82] \"Green\" \"White\" \"Black\" \"Black\" \"Red\"   \"Red\"   \"White\" \"White\" \"White\""
and 5 more ...

lines(x[[5]]) vs lines(y[[5]])
  "'data.frame':\t100 obs. of  3 variables:"
+ " $ Color: chr  \"Green\" \"1\" \"Green\" \"5\" ..."
- " $ Color: chr  \"Green\" \" Red\" \"Green\" \"White\" ..."
  " $ Speed: int  32 45 35 34 25 41 34 29 31 26 ..."
+ " $ State: Factor w/ 4 levels \"Arizona\",\"Colorado\",..: 3 1 2 1 1 1 3 2 1 2 ..."
- " $ State: chr  \"NewMexico\" \"Arizona\" \"Colorado\" \"Arizona\" ..."
  ""
```

#### Episode: r-novice-inflammation/12-supp-factors.md

``` diff
✔ No differences
```

#### Episode: r-novice-inflammation/13-supp-data-structures.md

``` diff
`lines(x[[33]])`: "[1] \"matrix\""            ""
`lines(y[[33]])`: "[1] \"matrix\" \"array\" " ""
```

#### Episode: r-novice-inflammation/14-supp-call-stack.md

``` diff
✔ No differences
```

#### Episode: r-novice-inflammation/15-supp-loops-in-depth.md

``` diff
`lines(x[[6]])`: "   user  system elapsed " "  0.017   0.001   0.017 " ""
`lines(y[[6]])`: "   user  system elapsed " "  0.019   0.000   0.019 " ""

`lines(x[[7]])`: "   user  system elapsed " "  0.018   0.000   0.018 " ""
`lines(y[[7]])`: "   user  system elapsed " "  0.016   0.000   0.016 " ""
```

## Lesson: r-novice-gapminder

#### Episode: r-novice-gapminder/01-rstudio-intro.md

``` diff
     lines(x[[23]])                  | lines(y[[23]])                      
[29] "    }"                         | "    }"                         [29]
[30] "    else all.names"            | "    else all.names"            [30]
[31] "}"                             | "}"                             [31]
[32] "<bytecode: 0x55dc14efaf70>"    - "<bytecode: 0x55aefc8ac4f8>"    [32]
[33] "<environment: namespace:base>" | "<environment: namespace:base>" [33]
[34] ""                              | ""                              [34]
```

#### Episode: r-novice-gapminder/02-project-intro.md

``` diff
✔ No differences
```

#### Episode: r-novice-gapminder/03-seeking-help.md

``` diff
lines(x[[1]])[1:9] vs lines(y[[1]])[1:9]
+ "R version 3.6.3 (2020-02-29)"
- "R version 4.0.0 (2020-04-24)"
  "Platform: x86_64-pc-linux-gnu (64-bit)"
+ "Running under: Debian GNU/Linux 10 (buster)"
- "Running under: Ubuntu 20.04 LTS"
  ""
  "Matrix products: default"
+ "BLAS/LAPACK: /usr/lib/x86_64-linux-gnu/libopenblasp-r0.3.5.so"
- "BLAS/LAPACK: /usr/lib/x86_64-linux-gnu/openblas-openmp/libopenblasp-r0.3.8.so"
  ""
  "locale:"
  " [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              "

lines(x[[1]])[20:27] vs lines(y[[1]])[20:27]
  "[1] knitr_1.28              requirements_0.0.0.9000 remotes_2.1.1          "
  ""
  "loaded via a namespace (and not attached):"
+ " [1] compiler_3.6.3  magrittr_1.5    htmltools_0.4.0 tools_3.6.3    "
- " [1] compiler_4.0.0  magrittr_1.5    htmltools_0.4.0 tools_4.0.0    "
+ " [5] yaml_2.2.1      Rcpp_1.0.4.6    stringi_1.4.6   rmarkdown_2.1  "
- " [5] yaml_2.2.1      Rcpp_1.0.4.6    stringi_1.4.6   rmarkdown_2.2  "
+ " [9] stringr_1.4.0   xfun_0.13       digest_0.6.25   rlang_0.4.5    "
- " [9] stringr_1.4.0   xfun_0.14       digest_0.6.25   rlang_0.4.6    "
  "[13] evaluate_0.14  "
  ""
```

#### Episode: r-novice-gapminder/04-data-structures-part1.md

``` diff
    lines(x[[3]])                | lines(y[[3]])                             
[1] "[1] calico black  tabby "   - "[1] \"calico\" \"black\"  \"tabby\" " [1]
[2] "Levels: black calico tabby" -                                           
[3] ""                           | ""                                     [2]

`lines(x[[6]])`: "[1] NA NA NA"   ""
`lines(y[[6]])`: "[1] \"double\"" ""

`lines(x[[8]])`: "[1] \"double\""  ""
`lines(y[[8]])`: "[1] \"integer\"" ""

`lines(x[[9]])`: "[1] \"integer\"" ""
`lines(y[[9]])`: "[1] \"complex\"" ""

`lines(x[[10]])`: "[1] \"complex\"" ""
`lines(y[[10]])`: "[1] \"logical\"" ""

`lines(x[[11]])`: "[1] \"logical\""   ""
`lines(y[[11]])`: "[1] \"character\"" ""

`lines(x[[13]])`: "[1] \"integer\""    ""
`lines(y[[13]])`: "[1] \"data.frame\"" ""

`lines(x[[14]])`: "[1] NA NA NA NA"       ""
`lines(y[[14]])`: "[1] FALSE FALSE FALSE" ""

`lines(x[[15]])`: "[1] \"data.frame\"" ""
`lines(y[[15]])`: "[1] \"\" \"\" \"\"" ""

`lines(x[[16]])`: "[1] FALSE FALSE FALSE"     ""
`lines(y[[16]])`: " chr [1:3] \"\" \"\" \"\"" ""

And 60 more differences ...
```

#### Episode: r-novice-gapminder/05-data-structures-part2.md

``` diff
lines(x[[5]]) vs lines(y[[5]])
+ "    coat weight likes_string age"
- "           coat weight likes_string age"
+ "1 calico    2.1            1   2"
- "1        calico    2.1            1   2"
+ "2  black    5.0            0   3"
- "2         black    5.0            0   3"
+ "3  tabby    3.2            1   5"
- "3         tabby    3.2            1   5"
+ "4   <NA>    3.3            1   9"
- "4 tortoiseshell    3.3            1   9"
  ""

`lines(x[[6]])`: "[1] \"black\"  \"calico\" \"tabby\" " ""
`lines(y[[6]])`: "NULL"                                 ""

lines(x[[7]])[1:5] vs lines(y[[7]])[1:5]
  "'data.frame':\t5 obs. of  4 variables:"
+ " $ coat        : Factor w/ 4 levels \"black\",\"calico\",..: 2 1 3 NA 4"
- " $ coat        : Factor w/ 1 level \"tortoiseshell\": NA NA NA 1 1"
  " $ weight      : num  2.1 5 3.2 3.3 3.3"
  " $ likes_string: int  1 0 1 1 1"
  " $ age         : num  2 3 5 9 9"

lines(x[[8]])[1:5] vs lines(y[[8]])[1:5]
  "'data.frame':\t5 obs. of  4 variables:"
+ " $ coat        : chr  \"calico\" \"black\" \"tabby\" NA ..."
- " $ coat        : chr  NA NA NA \"tortoiseshell\" ..."
  " $ weight      : num  2.1 5 3.2 3.3 3.3"
  " $ likes_string: int  1 0 1 1 1"
  " $ age         : num  2 3 5 9 9"

lines(x[[9]]) vs lines(y[[9]])
  "           coat weight likes_string age"
+ "1        calico    2.1            1   2"
- "1          <NA>    2.1            1   2"
+ "2         black    5.0            0   3"
- "2          <NA>    5.0            0   3"
+ "3         tabby    3.2            1   5"
- "3          <NA>    3.2            1   5"
+ "4          <NA>    3.3            1   9"
- "4 tortoiseshell    3.3            1   9"
  "5 tortoiseshell    3.3            1   9"
  ""

lines(x[[10]]) vs lines(y[[10]])
  "           coat weight likes_string age"
+ "1        calico    2.1            1   2"
- "1          <NA>    2.1            1   2"
+ "2         black    5.0            0   3"
- "2          <NA>    5.0            0   3"
+ "3         tabby    3.2            1   5"
- "3          <NA>    3.2            1   5"
  "5 tortoiseshell    3.3            1   9"
  ""

lines(x[[11]]) vs lines(y[[11]])
  "           coat weight likes_string age"
+ "1        calico    2.1            1   2"
- "4 tortoiseshell    3.3            1   9"
+ "2         black    5.0            0   3"
+ "3         tabby    3.2            1   5"
  "5 tortoiseshell    3.3            1   9"
  ""

lines(x[[12]]) vs lines(y[[12]])
  "           coat weight likes_string"
+ "1        calico    2.1            1"
- "4 tortoiseshell    3.3            1"
+ "2         black    5.0            0"
+ "3         tabby    3.2            1"
  "5 tortoiseshell    3.3            1"
  ""

lines(x[[13]]) vs lines(y[[13]])
  "           coat weight likes_string"
+ "1        calico    2.1            1"
- "4 tortoiseshell    3.3            1"
+ "2         black    5.0            0"
+ "3         tabby    3.2            1"
  "5 tortoiseshell    3.3            1"
  ""

lines(x[[14]]) vs lines(y[[14]])
  "            coat weight likes_string age"
+ "1         calico    2.1            1   2"
- "4  tortoiseshell    3.3            1   9"
+ "2          black    5.0            0   3"
+ "3          tabby    3.2            1   5"
  "5  tortoiseshell    3.3            1   9"
+ "11        calico    2.1            1   2"
- "41 tortoiseshell    3.3            1   9"
+ "21         black    5.0            0   3"
+ "31         tabby    3.2            1   5"
  "51 tortoiseshell    3.3            1   9"
  ""

And 5 more differences ...
```

#### Episode: r-novice-gapminder/06-data-subsetting.md

``` diff
✔ No differences
```

#### Episode: r-novice-gapminder/07-control-flow.md

``` diff
✔ No differences
```

#### Episode: r-novice-gapminder/08-plot-ggplot2.md

``` diff
✔ No differences
```

#### Episode: r-novice-gapminder/09-vectorization.md

``` diff
✔ No differences
```

#### Episode: r-novice-gapminder/10-functions.md

``` diff
✔ No differences
```

#### Episode: r-novice-gapminder/11-writing-data.md

``` diff
✔ No differences
```

#### Episode: r-novice-gapminder/12-plyr.md

``` diff
✔ No differences
```

#### Episode: r-novice-gapminder/13-dplyr.md

``` diff
lines(x[[4]]) vs lines(y[[4]])
  "'data.frame':\t1704 obs. of  6 variables:"
+ " $ country  : Factor w/ 142 levels \"Afghanistan\",..: 1 1 1 1 1 1 1 1 1 1 ..."
- " $ country  : chr  \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" ..."
  " $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ..."
  " $ pop      : num  8425333 9240934 10267083 11537966 13079460 ..."
+ " $ continent: Factor w/ 5 levels \"Africa\",\"Americas\",..: 3 3 3 3 3 3 3 3 3 3 ..."
- " $ continent: chr  \"Asia\" \"Asia\" \"Asia\" \"Asia\" ..."
  " $ lifeExp  : num  28.8 30.3 32 34 36.1 ..."
  " $ gdpPercap: num  779 821 853 836 740 ..."
  ""

lines(x[[5]]) vs lines(y[[5]])
  "tibble [1,704 × 6] (S3: grouped_df/tbl_df/tbl/data.frame)"
+ " $ country  : Factor w/ 142 levels \"Afghanistan\",..: 1 1 1 1 1 1 1 1 1 1 ..."
- " $ country  : chr [1:1704] \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" ..."
  " $ year     : int [1:1704] 1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ..."
  " $ pop      : num [1:1704] 8425333 9240934 10267083 11537966 13079460 ..."
+ " $ continent: Factor w/ 5 levels \"Africa\",\"Americas\",..: 3 3 3 3 3 3 3 3 3 3 ..."
- " $ continent: chr [1:1704] \"Asia\" \"Asia\" \"Asia\" \"Asia\" ..."
  " $ lifeExp  : num [1:1704] 28.8 30.3 32 34 36.1 ..."
  " $ gdpPercap: num [1:1704] 779 821 853 836 740 ..."
  " - attr(*, \"groups\")= tibble [5 × 2] (S3: tbl_df/tbl/data.frame)"
+ "  ..$ continent: Factor w/ 5 levels \"Africa\",\"Americas\",..: 1 2 3 4 5"
- "  ..$ continent: chr [1:5] \"Africa\" \"Americas\" \"Asia\" \"Europe\" ..."
+ "  ..$ .rows    :List of 5"
- "  ..$ .rows    : list<int> [1:5] "
  "  .. ..$ : int [1:624] 25 26 27 28 29 30 31 32 33 34 ..."
  "  .. ..$ : int [1:300] 49 50 51 52 53 54 55 56 57 58 ..."
  "  .. ..$ : int [1:396] 1 2 3 4 5 6 7 8 9 10 ..."
  "  .. ..$ : int [1:360] 13 14 15 16 17 18 19 20 21 22 ..."
  "  .. ..$ : int [1:24] 61 62 63 64 65 66 67 68 69 70 ..."
- "  .. ..@ ptype: int(0) "
and 2 more ...

lines(x[[6]]) vs lines(y[[6]])
+ "# A tibble: 2 x 2"
- "`summarise()` ungrouping output (override with `.groups` argument)"
+ " country      mean_lifeExp"
+ " <fct>               <dbl>"
+ "1 Iceland              76.5"
+ "2 Sierra Leone         36.8"
  ""

lines(x[[7]]) vs lines(y[[7]])
+ "# A tibble: 1 x 2"
- "`summarise()` ungrouping output (override with `.groups` argument)"
+ " country      mean_lifeExp"
+ " <fct>               <dbl>"
+ "1 Sierra Leone         36.8"
  ""

    lines(x[[8]])            | lines(y[[8]])                    
[1] "# A tibble: 1 x 2"      - "# A tibble: 2 x 2"           [1]
[2] " country mean_lifeExp"  - " country      mean_lifeExp"  [2]
[3] " <fct>          <dbl>"  - " <chr>               <dbl>"  [3]
[4] "1 Iceland         76.5" - "1 Iceland              76.5" [4]
                             - "2 Sierra Leone         36.8" [5]
[5] ""                       | ""                            [6]

    lines(x[[9]])       | lines(y[[9]])                    
[1] "# A tibble: 5 x 2" - "# A tibble: 1 x 2"           [1]
[2] "  continent     n" - " country      mean_lifeExp"  [2]
[3] "  <fct>     <int>" - " <chr>               <dbl>"  [3]
[4] "1 Africa       52" - "1 Sierra Leone         36.8" [4]
[5] "2 Asia         33" -                                  
[6] "3 Europe       30" -                                  
[7] "4 Americas     25" -                                  
[8] "5 Oceania       2" -                                  
[9] ""                  | ""                            [5]

    lines(x[[10]])      | lines(y[[10]])              
[1] "# A tibble: 5 x 2" - "# A tibble: 1 x 2"      [1]
[2] "  continent se_le" - " country mean_lifeExp"  [2]
[3] "  <fct>     <dbl>" - " <chr>          <dbl>"  [3]
[4] "1 Africa    0.366" - "1 Iceland         76.5" [4]
[5] "2 Americas  0.540" -                             
[6] "3 Asia      0.596" -                             
[7] "4 Europe    0.286" -                             
[8] "5 Oceania   0.775" -                             
[9] ""                  | ""                       [5]

lines(x[[11]]) vs lines(y[[11]])
+ "# A tibble: 5 x 5"
- "`summarise()` regrouping output by 'continent' (override with `.groups` argument)"
+ "  continent mean_le min_le max_le se_le"
+ "  <fct>       <dbl>  <dbl>  <dbl> <dbl>"
+ "1 Africa       48.9   23.6   76.4 0.366"
+ "2 Americas     64.7   37.6   80.7 0.540"
+ "3 Asia         60.1   28.8   82.6 0.596"
+ "4 Europe       71.9   43.6   81.8 0.286"
+ "5 Oceania      74.3   69.1   81.2 0.775"
  ""

`lines(x[[12]])` is absent
`lines(y[[12]])` is a character vector ('`summarise()` regrouping output by \'continent\' (override with `.groups` argument)', '')

`lines(x[[13]])` is absent
`lines(y[[13]])` is a character vector ('  continent  n', '1    Africa 52', '2      Asia 33', '3    Europe 30', '4  Americas 25', ...)

And 8 more differences ...
```

#### Episode: r-novice-gapminder/14-tidyr.md

``` diff
lines(x[[5]]) vs lines(y[[5]])
+ "# A tibble: 15 x 3"
- "`summarise()` regrouping output by 'continent' (override with `.groups` argument)"
+ "# Groups:   continent [5]"
+ "  continent obs_type       means"
+ "  <chr>     <chr>          <dbl>"
+ "1 Africa    gdpPercap     2194. "
+ "2 Africa    lifeExp         48.9"
+ "3 Africa    pop        9916003. "
+ "4 Americas  gdpPercap     7136. "
+ "5 Americas  lifeExp         64.7"
+ "6 Americas  pop       24504795. "
+ "7 Asia      gdpPercap     7902. "
+ "8 Asia      lifeExp         60.1"
+ "9 Asia      pop       77038722. "
+ "10 Europe    gdpPercap    14469. "
+ "11 Europe    lifeExp         71.9"
+ "12 Europe    pop       17169765. "
+ "13 Oceania   gdpPercap    18622. "
+ "14 Oceania   lifeExp         74.3"
+ "15 Oceania   pop        8874672. "
and 1 more ...

    lines(x[[6]])   | lines(y[[6]])                                     
[1] "[1] 1704    6" - "# A tibble: 15 x 3"               [1]            
                    - "# Groups:   continent [5]"        [2]            
                    - "  continent obs_type       means" [3]            
                    - "  <chr>     <chr>          <dbl>" [4]            
                    - "1 Africa    gdpPercap     2194. " [5]            
                    - "2 Africa    lifeExp         48.9" [6]            
                    - "3 Africa    pop        9916003. " [7]            
                    - "4 Americas  gdpPercap     7136. " [8]            
                    - "5 Americas  lifeExp         64.7" [9]            
                    - "6 Americas  pop       24504795. " [10]           
... ...               ...                                and 10 more ...

lines(x[[8]]) vs lines(y[[8]])
+ "[1] \"continent\" \"country\"   \"year\"      \"gdpPercap\" \"lifeExp\"   \"pop\"      "
- "[1] 1704    6"
  ""

lines(x[[9]]) vs lines(y[[9]])
+ "[1] \"country\"   \"year\"      \"pop\"       \"continent\" \"lifeExp\"   \"gdpPercap\""
- "[1] \"continent\" \"country\"   \"year\"      \"gdpPercap\" \"lifeExp\"   \"pop\"      "
  ""

lines(x[[10]]) vs lines(y[[10]])
+ "[1] TRUE"
- "[1] \"country\"   \"year\"      \"pop\"       \"continent\" \"lifeExp\"   \"gdpPercap\""
  ""

lines(x[[11]]) vs lines(y[[11]])
+ "# A tibble: 6 x 6"
- "[1] \"Attributes: < Component \\\"class\\\": Lengths (3, 1) differ (string compare on first 1) >\""
+ "  country  year      pop continent lifeExp gdpPercap"
- "[2] \"Attributes: < Component \\\"class\\\": 1 string mismatch >\"                                "
+ "  <chr>   <int>    <dbl> <chr>       <dbl>     <dbl>"
- "[3] \"Component \\\"country\\\": 1704 string mismatches\"                                         "
+ "1 Algeria  1952  9279525 Africa       43.1     2449."
- "[4] \"Component \\\"pop\\\": Mean relative difference: 1.634504\"                                 "
+ "2 Algeria  1957 10270856 Africa       45.7     3014."
- "[5] \"Component \\\"continent\\\": 1212 string mismatches\"                                       "
+ "3 Algeria  1962 11000948 Africa       48.3     2551."
- "[6] \"Component \\\"lifeExp\\\": Mean relative difference: 0.203822\"                             "
+ "4 Algeria  1967 12760499 Africa       51.4     3247."
- "[7] \"Component \\\"gdpPercap\\\": Mean relative difference: 1.162302\"                           "
+ "5 Algeria  1972 14760787 Africa       54.5     4183."
+ "6 Algeria  1977 17152804 Africa       58.0     4910."
  ""

lines(x[[12]]) vs lines(y[[12]])
+ "      country year      pop continent lifeExp gdpPercap"
- "# A tibble: 6 x 6"
+ "1 Afghanistan 1952  8425333      Asia  28.801  779.4453"
- "  country  year      pop continent lifeExp gdpPercap"
+ "2 Afghanistan 1957  9240934      Asia  30.332  820.8530"
- "  <chr>   <int>    <dbl> <chr>       <dbl>     <dbl>"
+ "3 Afghanistan 1962 10267083      Asia  31.997  853.1007"
- "1 Algeria  1952  9279525 Africa       43.1     2449."
+ "4 Afghanistan 1967 11537966      Asia  34.020  836.1971"
- "2 Algeria  1957 10270856 Africa       45.7     3014."
+ "5 Afghanistan 1972 13079460      Asia  36.088  739.9811"
- "3 Algeria  1962 11000948 Africa       48.3     2551."
+ "6 Afghanistan 1977 14880372      Asia  38.438  786.1134"
- "4 Algeria  1967 12760499 Africa       51.4     3247."
- "5 Algeria  1972 14760787 Africa       54.5     4183."
- "6 Algeria  1977 17152804 Africa       58.0     4910."
  ""

lines(x[[13]]) vs lines(y[[13]])
+ "[1] TRUE"
- "      country year      pop continent lifeExp gdpPercap"
- "1 Afghanistan 1952  8425333      Asia  28.801  779.4453"
- "2 Afghanistan 1957  9240934      Asia  30.332  820.8530"
- "3 Afghanistan 1962 10267083      Asia  31.997  853.1007"
- "4 Afghanistan 1967 11537966      Asia  34.020  836.1971"
- "5 Afghanistan 1972 13079460      Asia  36.088  739.9811"
- "6 Afghanistan 1977 14880372      Asia  38.438  786.1134"
  ""

lines(x[[14]]) vs lines(y[[14]])
+ "tibble [5,112 × 4] (S3: tbl_df/tbl/data.frame)"
- "[1] \"Attributes: < Component \\\"class\\\": Lengths (3, 1) differ (string compare on first 1) >\""
+ " $ var_ID    : chr [1:5112] \"Africa_Algeria\" \"Africa_Algeria\" \"Africa_Algeria\" \"Africa_Algeria\" ..."
- "[2] \"Attributes: < Component \\\"class\\\": 1 string mismatch >\"                                "
+ " $ obs_type  : chr [1:5112] \"gdpPercap\" \"gdpPercap\" \"gdpPercap\" \"gdpPercap\" ..."
+ " $ year      : int [1:5112] 1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ..."
+ " $ obs_values: num [1:5112] 2449 3014 2551 3247 4183 ..."
  ""

lines(x[[15]]) vs lines(y[[15]])
+ "tibble [5,112 × 3] (S3: tbl_df/tbl/data.frame)"
- "tibble [5,112 × 4] (S3: tbl_df/tbl/data.frame)"
+ " $ ID_var    : chr [1:5112] \"Africa_Algeria\" \"Africa_Algeria\" \"Africa_Algeria\" \"Africa_Algeria\" ..."
- " $ var_ID    : chr [1:5112] \"Africa_Algeria\" \"Africa_Algeria\" \"Africa_Algeria\" \"Africa_Algeria\" ..."
+ " $ var_names : chr [1:5112] \"gdpPercap_1952\" \"gdpPercap_1957\" \"gdpPercap_1962\" \"gdpPercap_1967\" ..."
- " $ obs_type  : chr [1:5112] \"gdpPercap\" \"gdpPercap\" \"gdpPercap\" \"gdpPercap\" ..."
- " $ year      : int [1:5112] 1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ..."
  " $ obs_values: num [1:5112] 2449 3014 2551 3247 4183 ..."
  ""

And 4 more differences ...
```

#### Episode: r-novice-gapminder/15-knitr-markdown.md

``` diff
✔ No differences
```

#### Episode: r-novice-gapminder/16-wrap-up.md

``` diff
✔ No differences
```

## Lesson: organization-geospatial

#### Episode: organization-geospatial/01-intro-raster-data.md

``` diff
✔ No differences
```

#### Episode: organization-geospatial/02-intro-vector-data.md

``` diff
✔ No differences
```

#### Episode: organization-geospatial/03-crs.md

``` diff
✔ No differences
```

#### Episode: organization-geospatial/04-geo-landscape.md

``` diff
✔ No differences
```

## Lesson: r-novice-gapminder-es

#### Episode: r-novice-gapminder-es/01-rstudio-intro.md

``` diff
     lines(x[[23]])                  | lines(y[[23]])                      
[29] "    }"                         | "    }"                         [29]
[30] "    else all.names"            | "    else all.names"            [30]
[31] "}"                             | "}"                             [31]
[32] "<bytecode: 0x556359943ee0>"    - "<bytecode: 0x55afeabb5448>"    [32]
[33] "<environment: namespace:base>" | "<environment: namespace:base>" [33]
[34] ""                              | ""                              [34]
```

#### Episode: r-novice-gapminder-es/02-project-intro.md

``` diff
✔ No differences
```

#### Episode: r-novice-gapminder-es/03-seeking-help.md

``` diff
lines(x[[1]])[1:9] vs lines(y[[1]])[1:9]
+ "R version 3.6.3 (2020-02-29)"
- "R version 4.0.0 (2020-04-24)"
  "Platform: x86_64-pc-linux-gnu (64-bit)"
+ "Running under: Debian GNU/Linux 10 (buster)"
- "Running under: Ubuntu 20.04 LTS"
  ""
  "Matrix products: default"
+ "BLAS/LAPACK: /usr/lib/x86_64-linux-gnu/libopenblasp-r0.3.5.so"
- "BLAS/LAPACK: /usr/lib/x86_64-linux-gnu/openblas-openmp/libopenblasp-r0.3.8.so"
  ""
  "locale:"
  " [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              "

lines(x[[1]])[20:25] vs lines(y[[1]])[20:25]
  "[1] checkpoint_0.4.9 stringr_1.4.0    knitr_1.28      "
  ""
  "loaded via a namespace (and not attached):"
+ "[1] compiler_3.6.3 magrittr_1.5   tools_3.6.3    stringi_1.4.6  xfun_0.13     "
- "[1] compiler_4.0.0 magrittr_1.5   tools_4.0.0    stringi_1.4.6  xfun_0.14     "
  "[6] evaluate_0.14 "
  ""
```

#### Episode: r-novice-gapminder-es/04-data-structures-part1.md

``` diff
lines(x[[3]]) vs lines(y[[3]])
+ "[1] mixto    negro    atigrado"
- "[1] \"mixto\"    \"negro\"    \"atigrado\""
+ "Levels: atigrado mixto negro"
  ""

`lines(x[[6]])`: "[1] NA NA NA"      ""
`lines(y[[6]])`: "[1] \"character\"" ""

`lines(x[[7]])`: "[1] \"factor\""  ""
`lines(y[[7]])`: "[1] \"numeric\"" ""

`lines(x[[8]])`: "[1] \"numeric\""    ""
`lines(y[[8]])`: "[1] \"data.frame\"" ""

`lines(x[[9]])`: "[1] \"data.frame\""   ""
`lines(y[[9]])`: " num [1:3] 2.1 5 3.2" ""

`lines(x[[10]])`: " num [1:3] 2.1 5 3.2" ""
`lines(y[[10]])`: "[1] 2 6 3"            ""

`lines(x[[11]])`: "[1] 2 6 3"        ""
`lines(y[[11]])`: " num [1:3] 2 6 3" ""

`lines(x[[12]])`: " num [1:3] 2 6 3"          ""
`lines(y[[12]])`: " chr [1:2] \"a\" \"TRUE\"" ""

`lines(x[[13]])`: " chr [1:2] \"a\" \"TRUE\"" ""
`lines(y[[13]])`: " num [1:2] 0 1"            ""

`lines(x[[14]])`: " num [1:2] 0 1"        ""
`lines(y[[14]])`: "[1] \"0\" \"2\" \"4\"" ""

And 50 more differences ...
```

#### Episode: r-novice-gapminder-es/05-data-structures-part2.md

``` diff
`lines(x[[8]])`: "[1] \"atigrado\" \"mixto\"    \"negro\"   " ""
`lines(y[[8]])`: "NULL"                                       ""

lines(x[[9]])[1:5] vs lines(y[[9]])[1:5]
  "'data.frame':\t6 obs. of  4 variables:"
+ " $ color            : Factor w/ 4 levels \"atigrado\",\"mixto\",..: 2 3 1 NA NA 4"
- " $ color            : Factor w/ 1 level \"tortoiseshell\": NA NA NA 1 1 1"
  " $ peso             : num  2.1 5 3.2 3.3 3.3 3.3"
  " $ legusta_la_cuerda: num  1 0 1 1 1 1"
  " $ edad             : num  2 3 5 9 9 9"

lines(x[[10]])[1:5] vs lines(y[[10]])[1:5]
  "'data.frame':\t6 obs. of  4 variables:"
+ " $ color            : chr  \"mixto\" \"negro\" \"atigrado\" NA ..."
- " $ color            : chr  NA NA NA \"tortoiseshell\" ..."
  " $ peso             : num  2.1 5 3.2 3.3 3.3 3.3"
  " $ legusta_la_cuerda: num  1 0 1 1 1 1"
  " $ edad             : num  2 3 5 9 9 9"

lines(x[[11]]) vs lines(y[[11]])
  "          color peso legusta_la_cuerda edad"
+ "1         mixto  2.1                 1    2"
- "1          <NA>  2.1                 1    2"
+ "2         negro  5.0                 0    3"
- "2          <NA>  5.0                 0    3"
+ "3      atigrado  3.2                 1    5"
- "3          <NA>  3.2                 1    5"
+ "4          <NA>  3.3                 1    9"
- "4 tortoiseshell  3.3                 1    9"
+ "5          <NA>  3.3                 1    9"
- "5 tortoiseshell  3.3                 1    9"
  "6 tortoiseshell  3.3                 1    9"
  ""

lines(x[[12]]) vs lines(y[[12]])
  "          color peso legusta_la_cuerda edad"
+ "1         mixto  2.1                 1    2"
- "1          <NA>  2.1                 1    2"
+ "2         negro  5.0                 0    3"
- "2          <NA>  5.0                 0    3"
+ "3      atigrado  3.2                 1    5"
- "3          <NA>  3.2                 1    5"
+ "5          <NA>  3.3                 1    9"
- "5 tortoiseshell  3.3                 1    9"
  "6 tortoiseshell  3.3                 1    9"
  ""

lines(x[[13]]) vs lines(y[[13]])
  "          color peso legusta_la_cuerda edad"
+ "1         mixto  2.1                 1    2"
- "4 tortoiseshell  3.3                 1    9"
+ "2         negro  5.0                 0    3"
- "5 tortoiseshell  3.3                 1    9"
+ "3      atigrado  3.2                 1    5"
  "6 tortoiseshell  3.3                 1    9"
  ""

lines(x[[14]]) vs lines(y[[14]])
  "          color peso legusta_la_cuerda"
+ "1         mixto  2.1                 1"
- "4 tortoiseshell  3.3                 1"
+ "2         negro  5.0                 0"
- "5 tortoiseshell  3.3                 1"
+ "3      atigrado  3.2                 1"
  "6 tortoiseshell  3.3                 1"
  ""

lines(x[[15]]) vs lines(y[[15]])
  "          color peso legusta_la_cuerda"
+ "1         mixto  2.1                 1"
- "4 tortoiseshell  3.3                 1"
+ "2         negro  5.0                 0"
- "5 tortoiseshell  3.3                 1"
+ "3      atigrado  3.2                 1"
  "6 tortoiseshell  3.3                 1"
  ""

lines(x[[16]]) vs lines(y[[16]])
  "           color peso legusta_la_cuerda edad"
+ "1          mixto  2.1                 1    2"
- "4  tortoiseshell  3.3                 1    9"
+ "2          negro  5.0                 0    3"
- "5  tortoiseshell  3.3                 1    9"
+ "3       atigrado  3.2                 1    5"
  "6  tortoiseshell  3.3                 1    9"
+ "11         mixto  2.1                 1    2"
- "41 tortoiseshell  3.3                 1    9"
+ "21         negro  5.0                 0    3"
- "51 tortoiseshell  3.3                 1    9"
+ "31      atigrado  3.2                 1    5"
  "61 tortoiseshell  3.3                 1    9"
  ""

lines(x[[17]]) vs lines(y[[17]])
  "          color peso legusta_la_cuerda edad"
+ "1         mixto  2.1                 1    2"
- "1 tortoiseshell  3.3                 1    9"
+ "2         negro  5.0                 0    3"
- "2 tortoiseshell  3.3                 1    9"
+ "3      atigrado  3.2                 1    5"
- "3 tortoiseshell  3.3                 1    9"
  "4 tortoiseshell  3.3                 1    9"
+ "5         mixto  2.1                 1    2"
- "5 tortoiseshell  3.3                 1    9"
+ "6         negro  5.0                 0    3"
- "6 tortoiseshell  3.3                 1    9"
+ "7      atigrado  3.2                 1    5"
+ "8 tortoiseshell  3.3                 1    9"
  ""

And 4 more differences ...
```

#### Episode: r-novice-gapminder-es/06-data-subsetting.md

``` diff
✔ No differences
```

#### Episode: r-novice-gapminder-es/07-control-flow.md

``` diff
✔ No differences
```

#### Episode: r-novice-gapminder-es/08-plot-ggplot2.md

``` diff
✔ No differences
```

#### Episode: r-novice-gapminder-es/09-vectorization.md

``` diff
✔ No differences
```

#### Episode: r-novice-gapminder-es/10-functions.md

``` diff
✔ No differences
```

#### Episode: r-novice-gapminder-es/11-writing-data.md

``` diff
✔ No differences
```

#### Episode: r-novice-gapminder-es/12-plyr.md

``` diff
✔ No differences
```

#### Episode: r-novice-gapminder-es/13-dplyr.md

``` diff
lines(x[[4]]) vs lines(y[[4]])
  "'data.frame':\t1704 obs. of  6 variables:"
+ " $ country  : Factor w/ 142 levels \"Afghanistan\",..: 1 1 1 1 1 1 1 1 1 1 ..."
- " $ country  : chr  \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" ..."
  " $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ..."
  " $ pop      : num  8425333 9240934 10267083 11537966 13079460 ..."
+ " $ continent: Factor w/ 5 levels \"Africa\",\"Americas\",..: 3 3 3 3 3 3 3 3 3 3 ..."
- " $ continent: chr  \"Asia\" \"Asia\" \"Asia\" \"Asia\" ..."
  " $ lifeExp  : num  28.8 30.3 32 34 36.1 ..."
  " $ gdpPercap: num  779 821 853 836 740 ..."
  ""

lines(x[[5]]) vs lines(y[[5]])
  "tibble [1,704 × 6] (S3: grouped_df/tbl_df/tbl/data.frame)"
+ " $ country  : Factor w/ 142 levels \"Afghanistan\",..: 1 1 1 1 1 1 1 1 1 1 ..."
- " $ country  : chr [1:1704] \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" ..."
  " $ year     : int [1:1704] 1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ..."
  " $ pop      : num [1:1704] 8425333 9240934 10267083 11537966 13079460 ..."
+ " $ continent: Factor w/ 5 levels \"Africa\",\"Americas\",..: 3 3 3 3 3 3 3 3 3 3 ..."
- " $ continent: chr [1:1704] \"Asia\" \"Asia\" \"Asia\" \"Asia\" ..."
  " $ lifeExp  : num [1:1704] 28.8 30.3 32 34 36.1 ..."
  " $ gdpPercap: num [1:1704] 779 821 853 836 740 ..."
  " - attr(*, \"groups\")= tibble [5 × 2] (S3: tbl_df/tbl/data.frame)"
+ "  ..$ continent: Factor w/ 5 levels \"Africa\",\"Americas\",..: 1 2 3 4 5"
- "  ..$ continent: chr [1:5] \"Africa\" \"Americas\" \"Asia\" \"Europe\" ..."
+ "  ..$ .rows    :List of 5"
- "  ..$ .rows    : list<int> [1:5] "
  "  .. ..$ : int [1:624] 25 26 27 28 29 30 31 32 33 34 ..."
  "  .. ..$ : int [1:300] 49 50 51 52 53 54 55 56 57 58 ..."
  "  .. ..$ : int [1:396] 1 2 3 4 5 6 7 8 9 10 ..."
  "  .. ..$ : int [1:360] 13 14 15 16 17 18 19 20 21 22 ..."
  "  .. ..$ : int [1:24] 61 62 63 64 65 66 67 68 69 70 ..."
- "  .. ..@ ptype: int(0) "
and 2 more ...

lines(x[[6]]) vs lines(y[[6]])
+ "# A tibble: 2 x 2"
- "`summarise()` ungrouping output (override with `.groups` argument)"
+ " country      mean_lifeExp"
+ " <fct>               <dbl>"
+ "1 Iceland              76.5"
+ "2 Sierra Leone         36.8"
  ""

lines(x[[7]]) vs lines(y[[7]])
+ "# A tibble: 1 x 2"
- "`summarise()` ungrouping output (override with `.groups` argument)"
+ " country      mean_lifeExp"
+ " <fct>               <dbl>"
+ "1 Sierra Leone         36.8"
  ""

    lines(x[[8]])            | lines(y[[8]])                    
[1] "# A tibble: 1 x 2"      - "# A tibble: 2 x 2"           [1]
[2] " country mean_lifeExp"  - " country      mean_lifeExp"  [2]
[3] " <fct>          <dbl>"  - " <chr>               <dbl>"  [3]
[4] "1 Iceland         76.5" - "1 Iceland              76.5" [4]
                             - "2 Sierra Leone         36.8" [5]
[5] ""                       | ""                            [6]

    lines(x[[9]])       | lines(y[[9]])                    
[1] "# A tibble: 5 x 2" - "# A tibble: 1 x 2"           [1]
[2] "  continent     n" - " country      mean_lifeExp"  [2]
[3] "  <fct>     <int>" - " <chr>               <dbl>"  [3]
[4] "1 Africa       52" - "1 Sierra Leone         36.8" [4]
[5] "2 Asia         33" -                                  
[6] "3 Europe       30" -                                  
[7] "4 Americas     25" -                                  
[8] "5 Oceania       2" -                                  
[9] ""                  | ""                            [5]

    lines(x[[10]])       | lines(y[[10]])              
[1] "# A tibble: 5 x 2"  - "# A tibble: 1 x 2"      [1]
[2] "  continent se_pop" - " country mean_lifeExp"  [2]
[3] "  <fct>      <dbl>" - " <chr>          <dbl>"  [3]
[4] "1 Africa     0.366" - "1 Iceland         76.5" [4]
[5] "2 Americas   0.540" -                             
[6] "3 Asia       0.596" -                             
[7] "4 Europe     0.286" -                             
[8] "5 Oceania    0.775" -                             
[9] ""                   | ""                       [5]

lines(x[[11]]) vs lines(y[[11]])
+ "# A tibble: 5 x 5"
- "`summarise()` regrouping output by 'continent' (override with `.groups` argument)"
+ "  continent mean_le min_le max_le se_le"
+ "  <fct>       <dbl>  <dbl>  <dbl> <dbl>"
+ "1 Africa       48.9   23.6   76.4 0.366"
+ "2 Americas     64.7   37.6   80.7 0.540"
+ "3 Asia         60.1   28.8   82.6 0.596"
+ "4 Europe       71.9   43.6   81.8 0.286"
+ "5 Oceania      74.3   69.1   81.2 0.775"
  ""

`lines(x[[12]])` is absent
`lines(y[[12]])` is a character vector ('`summarise()` regrouping output by \'continent\' (override with `.groups` argument)', '')

`lines(x[[13]])` is absent
`lines(y[[13]])` is a character vector ('  continent  n', '1    Africa 52', '2      Asia 33', '3    Europe 30', '4  Americas 25', ...)

And 8 more differences ...
```

#### Episode: r-novice-gapminder-es/14-tidyr.md

``` diff
lines(x[[5]]) vs lines(y[[5]])
+ "# A tibble: 15 x 3"
- "`summarise()` regrouping output by 'continent' (override with `.groups` argument)"
+ "# Groups:   continent [5]"
+ "   continent obs_type       means"
+ "   <chr>     <chr>          <dbl>"
+ " 1 Africa    gdpPercap     2194. "
+ " 2 Africa    lifeExp         48.9"
+ " 3 Africa    pop        9916003. "
+ " 4 Americas  gdpPercap     7136. "
+ " 5 Americas  lifeExp         64.7"
+ " 6 Americas  pop       24504795. "
+ " 7 Asia      gdpPercap     7902. "
+ " 8 Asia      lifeExp         60.1"
+ " 9 Asia      pop       77038722. "
+ "10 Europe    gdpPercap    14469. "
+ "11 Europe    lifeExp         71.9"
+ "12 Europe    pop       17169765. "
+ "13 Oceania   gdpPercap    18622. "
+ "14 Oceania   lifeExp         74.3"
+ "15 Oceania   pop        8874672. "
and 1 more ...

    lines(x[[6]])   | lines(y[[6]])                                      
[1] "[1] 1704    6" - "# A tibble: 15 x 3"                [1]            
                    - "# Groups:   continent [5]"         [2]            
                    - "   continent obs_type       means" [3]            
                    - "   <chr>     <chr>          <dbl>" [4]            
                    - " 1 Africa    gdpPercap     2194. " [5]            
                    - " 2 Africa    lifeExp         48.9" [6]            
                    - " 3 Africa    pop        9916003. " [7]            
                    - " 4 Americas  gdpPercap     7136. " [8]            
                    - " 5 Americas  lifeExp         64.7" [9]            
                    - " 6 Americas  pop       24504795. " [10]           
... ...               ...                                 and 10 more ...

lines(x[[8]]) vs lines(y[[8]])
+ "[1] \"continent\" \"country\"   \"year\"      \"gdpPercap\" \"lifeExp\"   \"pop\"      "
- "[1] 1704    6"
  ""

lines(x[[9]]) vs lines(y[[9]])
+ "[1] \"country\"   \"year\"      \"pop\"       \"continent\" \"lifeExp\"   \"gdpPercap\""
- "[1] \"continent\" \"country\"   \"year\"      \"gdpPercap\" \"lifeExp\"   \"pop\"      "
  ""

lines(x[[10]]) vs lines(y[[10]])
+ "[1] \"Component \\\"country\\\": 1704 string mismatches\"              "
- "[1] \"country\"   \"year\"      \"pop\"       \"continent\" \"lifeExp\"   \"gdpPercap\""
+ "[2] \"Component \\\"pop\\\": Mean relative difference: 1.634504\"      "
+ "[3] \"Component \\\"continent\\\": 1212 string mismatches\"            "
+ "[4] \"Component \\\"lifeExp\\\": Mean relative difference: 0.203822\"  "
+ "[5] \"Component \\\"gdpPercap\\\": Mean relative difference: 1.162302\""
  ""

lines(x[[11]]) vs lines(y[[11]])
+ "  country year      pop continent lifeExp gdpPercap"
- "[1] \"Component \\\"country\\\": 1704 string mismatches\"              "
+ "1 Algeria 1952  9279525    Africa  43.077  2449.008"
- "[2] \"Component \\\"pop\\\": Mean relative difference: 1.634504\"      "
+ "2 Algeria 1957 10270856    Africa  45.685  3013.976"
- "[3] \"Component \\\"continent\\\": 1212 string mismatches\"            "
+ "3 Algeria 1962 11000948    Africa  48.303  2550.817"
- "[4] \"Component \\\"lifeExp\\\": Mean relative difference: 0.203822\"  "
+ "4 Algeria 1967 12760499    Africa  51.407  3246.992"
- "[5] \"Component \\\"gdpPercap\\\": Mean relative difference: 1.162302\""
+ "5 Algeria 1972 14760787    Africa  54.518  4182.664"
+ "6 Algeria 1977 17152804    Africa  58.014  4910.417"
  ""

lines(x[[12]]) vs lines(y[[12]])
+ "      country year      pop continent lifeExp gdpPercap"
- "  country year      pop continent lifeExp gdpPercap"
+ "1 Afghanistan 1952  8425333      Asia  28.801  779.4453"
- "1 Algeria 1952  9279525    Africa  43.077  2449.008"
+ "2 Afghanistan 1957  9240934      Asia  30.332  820.8530"
- "2 Algeria 1957 10270856    Africa  45.685  3013.976"
+ "3 Afghanistan 1962 10267083      Asia  31.997  853.1007"
- "3 Algeria 1962 11000948    Africa  48.303  2550.817"
+ "4 Afghanistan 1967 11537966      Asia  34.020  836.1971"
- "4 Algeria 1967 12760499    Africa  51.407  3246.992"
+ "5 Afghanistan 1972 13079460      Asia  36.088  739.9811"
- "5 Algeria 1972 14760787    Africa  54.518  4182.664"
+ "6 Afghanistan 1977 14880372      Asia  38.438  786.1134"
- "6 Algeria 1977 17152804    Africa  58.014  4910.417"
  ""

lines(x[[13]]) vs lines(y[[13]])
+ "[1] TRUE"
- "      country year      pop continent lifeExp gdpPercap"
- "1 Afghanistan 1952  8425333      Asia  28.801  779.4453"
- "2 Afghanistan 1957  9240934      Asia  30.332  820.8530"
- "3 Afghanistan 1962 10267083      Asia  31.997  853.1007"
- "4 Afghanistan 1967 11537966      Asia  34.020  836.1971"
- "5 Afghanistan 1972 13079460      Asia  36.088  739.9811"
- "6 Afghanistan 1977 14880372      Asia  38.438  786.1134"
  ""

lines(x[[14]]) vs lines(y[[14]])
+ "'data.frame':\t5112 obs. of  4 variables:"
- "[1] TRUE"
+ " $ var_ID    : chr  \"Africa_Algeria\" \"Africa_Angola\" \"Africa_Benin\" \"Africa_Botswana\" ..."
+ " $ obs_type  : chr  \"gdpPercap\" \"gdpPercap\" \"gdpPercap\" \"gdpPercap\" ..."
+ " $ year      : int  1952 1952 1952 1952 1952 1952 1952 1952 1952 1952 ..."
+ " $ obs_values: num  2449 3521 1063 851 543 ..."
  ""

lines(x[[15]]) vs lines(y[[15]])
+ "'data.frame':\t5112 obs. of  3 variables:"
- "'data.frame':\t5112 obs. of  4 variables:"
+ " $ ID_var    : chr  \"Africa_Algeria\" \"Africa_Angola\" \"Africa_Benin\" \"Africa_Botswana\" ..."
- " $ var_ID    : chr  \"Africa_Algeria\" \"Africa_Angola\" \"Africa_Benin\" \"Africa_Botswana\" ..."
+ " $ var_names : chr  \"gdpPercap_1952\" \"gdpPercap_1952\" \"gdpPercap_1952\" \"gdpPercap_1952\" ..."
- " $ obs_type  : chr  \"gdpPercap\" \"gdpPercap\" \"gdpPercap\" \"gdpPercap\" ..."
- " $ year      : int  1952 1952 1952 1952 1952 1952 1952 1952 1952 1952 ..."
  " $ obs_values: num  2449 3521 1063 851 543 ..."
  ""

And 3 more differences ...
```

#### Episode: r-novice-gapminder-es/15-knitr-markdown.md

``` diff
✔ No differences
```

#### Episode: r-novice-gapminder-es/16-wrap-up.md

``` diff
✔ No differences
```

## Lesson: r-intro-geospatial

#### Episode: r-intro-geospatial/01-rstudio-intro.md

``` diff
✔ No differences
```

#### Episode: r-intro-geospatial/02-project-intro.md

``` diff
✔ No differences
```

#### Episode: r-intro-geospatial/03-data-structures-part1.md

``` diff
lines(x[[1]]) vs lines(y[[1]])
+ "[1] Denmark Sweden  Norway "
- "[1] \"Denmark\" \"Sweden\"  \"Norway\" "
+ "Levels: Denmark Norway Sweden"
  ""

`lines(x[[4]])`: "[1] NA NA NA"    ""
`lines(y[[4]])`: "[1] \"numeric\"" ""

`lines(x[[6]])`: "[1] \"numeric\"" ""
`lines(y[[6]])`: "[1] \"integer\"" ""

`lines(x[[7]])`: "[1] \"integer\"" ""
`lines(y[[7]])`: "[1] \"complex\"" ""

`lines(x[[8]])`: "[1] \"complex\"" ""
`lines(y[[8]])`: "[1] \"logical\"" ""

`lines(x[[9]])`: "[1] \"logical\""   ""
`lines(y[[9]])`: "[1] \"character\"" ""

`lines(x[[10]])`: "[1] \"character\"" ""
`lines(y[[10]])`: "[1] \"factor\""    ""

`lines(x[[11]])`: "[1] \"factor\""    ""
`lines(y[[11]])`: "[1] \"character\"" ""

`lines(x[[12]])`: "[1] \"factor\""     ""
`lines(y[[12]])`: "[1] \"data.frame\"" ""

`lines(x[[13]])`: "[1] NA NA NA"          ""
`lines(y[[13]])`: "[1] FALSE FALSE FALSE" ""

And 52 more differences ...
```

#### Episode: r-intro-geospatial/04-data-structures-part2.md

``` diff
lines(x[[1]]) vs lines(y[[1]])
  "'data.frame':\t1704 obs. of  6 variables:"
+ " $ country  : Factor w/ 142 levels \"Afghanistan\",..: 1 1 1 1 1 1 1 1 1 1 ..."
- " $ country  : chr  \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" ..."
  " $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ..."
  " $ pop      : num  8425333 9240934 10267083 11537966 13079460 ..."
+ " $ continent: Factor w/ 5 levels \"Africa\",\"Americas\",..: 3 3 3 3 3 3 3 3 3 3 ..."
- " $ continent: chr  \"Asia\" \"Asia\" \"Asia\" \"Asia\" ..."
  " $ lifeExp  : num  28.8 30.3 32 34 36.1 ..."
  " $ gdpPercap: num  779 821 853 836 740 ..."
  ""

`lines(x[[3]])`: "[1] \"factor\""    ""
`lines(y[[3]])`: "[1] \"character\"" ""

lines(x[[4]]) vs lines(y[[4]])
+ " Factor w/ 142 levels \"Afghanistan\",..: 1 1 1 1 1 1 1 1 1 1 ..."
- " chr [1:1704] \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" ..."
  ""

lines(x[[17]])[4:8] vs lines(y[[17]])[4:8]
  "1702 Zimbabwe 1997 11404948    Africa  46.809   792.4500          TRUE"
  "1703 Zimbabwe 2002 11926563    Africa  39.989   672.0386          TRUE"
  "1704 Zimbabwe 2007 12311143    Africa  43.487   469.7093          TRUE"
+ "1705   Norway 2016  5000000      <NA>  80.300 49400.0000         FALSE"
- "1705   Norway 2016  5000000    Nordic  80.300 49400.0000         FALSE"
  ""

lines(x[[18]]) vs lines(y[[18]])
+ "[1] \"Africa\"   \"Americas\" \"Asia\"     \"Europe\"   \"Oceania\" "
- "NULL"
  ""

lines(x[[19]]) vs lines(y[[19]])
  "      country year      pop continent lifeExp  gdpPercap below_average"
+ "1700 Zimbabwe 1987  9216418    Africa  62.351   706.1573          TRUE"
- "1700 Zimbabwe 1987  9216418      <NA>  62.351   706.1573          TRUE"
+ "1701 Zimbabwe 1992 10704340    Africa  60.377   693.4208          TRUE"
- "1701 Zimbabwe 1992 10704340      <NA>  60.377   693.4208          TRUE"
+ "1702 Zimbabwe 1997 11404948    Africa  46.809   792.4500          TRUE"
- "1702 Zimbabwe 1997 11404948      <NA>  46.809   792.4500          TRUE"
+ "1703 Zimbabwe 2002 11926563    Africa  39.989   672.0386          TRUE"
- "1703 Zimbabwe 2002 11926563      <NA>  39.989   672.0386          TRUE"
+ "1704 Zimbabwe 2007 12311143    Africa  43.487   469.7093          TRUE"
- "1704 Zimbabwe 2007 12311143      <NA>  43.487   469.7093          TRUE"
  "1705   Norway 2016  5000000    Nordic  80.300 49400.0000         FALSE"
  ""

lines(x[[20]])[1:8] vs lines(y[[20]])[1:9]
  "'data.frame':\t1704 obs. of  7 variables:"
+ " $ country      : Factor w/ 142 levels \"Afghanistan\",..: 1 1 1 1 1 1 1 1 1 1 ..."
- " $ country      : chr  \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" ..."
  " $ year         : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ..."
  " $ pop          : num  8425333 9240934 10267083 11537966 13079460 ..."
+ " $ continent    : Factor w/ 6 levels \"Africa\",\"Americas\",..: 3 3 3 3 3 3 3 3 3 3 ..."
- " $ continent    : chr  \"Asia\" \"Asia\" \"Asia\" \"Asia\" ..."
- "  ..- attr(*, \"levels\")= chr \"Nordic\""
  " $ lifeExp      : num  28.8 30.3 32 34 36.1 ..."
  " $ gdpPercap    : num  779 821 853 836 740 ..."
  " $ below_average: logi  TRUE TRUE TRUE TRUE TRUE TRUE ..."

lines(x[[21]])[1:5] vs lines(y[[21]])[1:5]
  "'data.frame':\t1704 obs. of  7 variables:"
+ " $ country      : Factor w/ 142 levels \"Afghanistan\",..: 1 1 1 1 1 1 1 1 1 1 ..."
- " $ country      : chr  \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" ..."
  " $ year         : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ..."
  " $ pop          : num  8425333 9240934 10267083 11537966 13079460 ..."
  " $ continent    : chr  \"Asia\" \"Asia\" \"Asia\" \"Asia\" ..."
```

#### Episode: r-intro-geospatial/05-data-subsetting.md

``` diff
✔ No differences
```

#### Episode: r-intro-geospatial/06-dplyr.md

``` diff
lines(x[[4]]) vs lines(y[[4]])
  "'data.frame':\t1704 obs. of  6 variables:"
+ " $ country  : Factor w/ 142 levels \"Afghanistan\",..: 1 1 1 1 1 1 1 1 1 1 ..."
- " $ country  : chr  \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" ..."
  " $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ..."
  " $ pop      : num  8425333 9240934 10267083 11537966 13079460 ..."
+ " $ continent: Factor w/ 5 levels \"Africa\",\"Americas\",..: 3 3 3 3 3 3 3 3 3 3 ..."
- " $ continent: chr  \"Asia\" \"Asia\" \"Asia\" \"Asia\" ..."
  " $ lifeExp  : num  28.8 30.3 32 34 36.1 ..."
  " $ gdpPercap: num  779 821 853 836 740 ..."
  ""

lines(x[[5]]) vs lines(y[[5]])
  "tibble [1,704 × 6] (S3: grouped_df/tbl_df/tbl/data.frame)"
+ " $ country  : Factor w/ 142 levels \"Afghanistan\",..: 1 1 1 1 1 1 1 1 1 1 ..."
- " $ country  : chr [1:1704] \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" \"Afghanistan\" ..."
  " $ year     : int [1:1704] 1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ..."
  " $ pop      : num [1:1704] 8425333 9240934 10267083 11537966 13079460 ..."
+ " $ continent: Factor w/ 5 levels \"Africa\",\"Americas\",..: 3 3 3 3 3 3 3 3 3 3 ..."
- " $ continent: chr [1:1704] \"Asia\" \"Asia\" \"Asia\" \"Asia\" ..."
  " $ lifeExp  : num [1:1704] 28.8 30.3 32 34 36.1 ..."
  " $ gdpPercap: num [1:1704] 779 821 853 836 740 ..."
  " - attr(*, \"groups\")= tibble [5 × 2] (S3: tbl_df/tbl/data.frame)"
+ "  ..$ continent: Factor w/ 5 levels \"Africa\",\"Americas\",..: 1 2 3 4 5"
- "  ..$ continent: chr [1:5] \"Africa\" \"Americas\" \"Asia\" \"Europe\" ..."
+ "  ..$ .rows    :List of 5"
- "  ..$ .rows    : list<int> [1:5] "
  "  .. ..$ : int [1:624] 25 26 27 28 29 30 31 32 33 34 ..."
  "  .. ..$ : int [1:300] 49 50 51 52 53 54 55 56 57 58 ..."
  "  .. ..$ : int [1:396] 1 2 3 4 5 6 7 8 9 10 ..."
  "  .. ..$ : int [1:360] 13 14 15 16 17 18 19 20 21 22 ..."
  "  .. ..$ : int [1:24] 61 62 63 64 65 66 67 68 69 70 ..."
- "  .. ..@ ptype: int(0) "
and 2 more ...

lines(x[[6]]) vs lines(y[[6]])
+ "# A tibble: 5 x 2"
- "`summarise()` ungrouping output (override with `.groups` argument)"
+ "  continent mean_gdpPercap"
+ "  <fct>              <dbl>"
+ "1 Africa             2194."
+ "2 Americas           7136."
+ "3 Asia               7902."
+ "4 Europe            14469."
+ "5 Oceania           18622."
  ""

    lines(x[[7]])                 | lines(y[[7]])                   
[1] "# A tibble: 2 x 2"           - "# A tibble: 5 x 2"          [1]
[2] " country      mean_lifeExp"  - "  continent mean_gdpPercap" [2]
[3] " <fct>               <dbl>"  - "  <chr>              <dbl>" [3]
[4] "1 Iceland              76.5" - "1 Africa             2194." [4]
[5] "2 Sierra Leone         36.8" - "2 Americas           7136." [5]
                                  - "3 Asia               7902." [6]
                                  - "4 Europe            14469." [7]
                                  - "5 Oceania           18622." [8]
[6] ""                            | ""                           [9]

lines(x[[8]]) vs lines(y[[8]])
+ "# A tibble: 1 x 2"
- "`summarise()` ungrouping output (override with `.groups` argument)"
+ " country      mean_lifeExp"
+ " <fct>               <dbl>"
+ "1 Sierra Leone         36.8"
  ""

    lines(x[[9]])            | lines(y[[9]])                    
[1] "# A tibble: 1 x 2"      - "# A tibble: 2 x 2"           [1]
[2] " country mean_lifeExp"  - " country      mean_lifeExp"  [2]
[3] " <fct>          <dbl>"  - " <chr>               <dbl>"  [3]
[4] "1 Iceland         76.5" - "1 Iceland              76.5" [4]
                             - "2 Sierra Leone         36.8" [5]
[5] ""                       | ""                            [6]

    lines(x[[10]])      | lines(y[[10]])                   
[1] "# A tibble: 5 x 2" - "# A tibble: 1 x 2"           [1]
[2] "  continent     n" - " country      mean_lifeExp"  [2]
[3] "  <fct>     <int>" - " <chr>               <dbl>"  [3]
[4] "1 Africa       52" - "1 Sierra Leone         36.8" [4]
[5] "2 Asia         33" -                                  
[6] "3 Europe       30" -                                  
[7] "4 Americas     25" -                                  
[8] "5 Oceania       2" -                                  
[9] ""                  | ""                            [5]

    lines(x[[11]])      | lines(y[[11]])              
[1] "# A tibble: 5 x 2" - "# A tibble: 1 x 2"      [1]
[2] "  continent se_le" - " country mean_lifeExp"  [2]
[3] "  <fct>     <dbl>" - " <chr>          <dbl>"  [3]
[4] "1 Africa    0.366" - "1 Iceland         76.5" [4]
[5] "2 Americas  0.540" -                             
[6] "3 Asia      0.596" -                             
[7] "4 Europe    0.286" -                             
[8] "5 Oceania   0.775" -                             
[9] ""                  | ""                       [5]

lines(x[[12]]) vs lines(y[[12]])
+ "# A tibble: 5 x 5"
- "`summarise()` regrouping output by 'continent' (override with `.groups` argument)"
+ "  continent mean_le min_le max_le se_le"
+ "  <fct>       <dbl>  <dbl>  <dbl> <dbl>"
+ "1 Africa       48.9   23.6   76.4 0.366"
+ "2 Americas     64.7   37.6   80.7 0.540"
+ "3 Asia         60.1   28.8   82.6 0.596"
+ "4 Europe       71.9   43.6   81.8 0.286"
+ "5 Oceania      74.3   69.1   81.2 0.775"
  ""

`lines(x[[13]])` is absent
`lines(y[[13]])` is a character vector ('`summarise()` regrouping output by \'continent\' (override with `.groups` argument)', '')

And 6 more differences ...
```

#### Episode: r-intro-geospatial/07-plot-ggplot2.md

``` diff
✔ No differences
```

#### Episode: r-intro-geospatial/08-writing-data.md

``` diff
✔ No differences
```

## Lesson: r-raster-vector-geospatial

#### Episode: r-raster-vector-geospatial/01-raster-structure.md

``` diff
lines(x[[2]])[2:8] vs lines(y[[2]])[2:8]
  "dimensions : 1367, 1697, 2319799  (nrow, ncol, ncell)"
  "resolution : 1, 1  (x, y)"
  "extent     : 731453, 733150, 4712471, 4713838  (xmin, xmax, ymin, ymax)"
+ "crs        : +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "
- "crs        : +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs "
  "source     : /home/rstudio/_episodes_rmd/data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_dsmCrop.tif "
  "names      : HARV_dsmCrop "
  "values     : 305.07, 416.07  (min, max)"

    lines(x[[3]])          | lines(y[[3]])             
[1] "        HARV_dsmCrop" | "        HARV_dsmCrop" [1]
[2] "Min.          305.55" - "Min.          305.33" [2]
[3] "1st Qu.       345.66" - "1st Qu.       345.50" [3]
[4] "Median        359.76" - "Median        359.57" [4]
[5] "3rd Qu.       374.24" - "3rd Qu.       374.21" [5]
[6] "Max.          414.54" - "Max.          416.07" [6]
[7] "NA's            0.00" | "NA's            0.00" [7]
[8] ""                     | ""                     [8]

lines(x[[6]]) vs lines(y[[6]])
  "CRS arguments:"
+ " +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84"
- " +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs "
+ "+towgs84=0,0,0 "
  ""
```

#### Episode: r-raster-vector-geospatial/02-raster-plot.md

``` diff
`lines(x[[5]])`: "[1] \"#00A600FF\" \"#ECB176FF\" \"#F2F2F2FF\"" ""
`lines(y[[5]])`: "[1] \"#00A600\" \"#ECB176\" \"#F2F2F2\""       ""

lines(x[[6]])[2:8] vs lines(y[[6]])[2:8]
  "dimensions : 1367, 1697, 2319799  (nrow, ncol, ncell)"
  "resolution : 1, 1  (x, y)"
  "extent     : 731453, 733150, 4712471, 4713838  (xmin, xmax, ymin, ymax)"
+ "crs        : +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "
- "crs        : +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs "
  "source     : /home/rstudio/_episodes_rmd/data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_DSMhill.tif "
  "names      : HARV_DSMhill "
  "values     : -0.7136298, 0.9999997  (min, max)"
```

#### Episode: r-raster-vector-geospatial/03-raster-reproject-in-r.md

``` diff
lines(x[[1]]) vs lines(y[[1]])
  "CRS arguments:"
+ " +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84"
- " +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs "
+ "+towgs84=0,0,0 "
  ""

lines(x[[2]]) vs lines(y[[2]])
+ "CRS arguments:"
- "CRS arguments: +proj=longlat +datum=WGS84 +no_defs "
+ " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
  ""

lines(x[[3]]) vs lines(y[[3]])
  "CRS arguments:"
+ " +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84"
- " +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs "
+ "+towgs84=0,0,0 "
  ""

lines(x[[4]]) vs lines(y[[4]])
+ "CRS arguments:"
- "CRS arguments: +proj=longlat +datum=WGS84 +no_defs "
+ " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
  ""
```

#### Episode: r-raster-vector-geospatial/04-raster-calculations-in-r.md

``` diff
✔ No differences
```

#### Episode: r-raster-vector-geospatial/05-raster-multi-band-in-r.md

``` diff
lines(x[[1]])[3:9] vs lines(y[[1]])[3:9]
  "dimensions : 2317, 3073, 7120141  (nrow, ncol, ncell)"
  "resolution : 0.25, 0.25  (x, y)"
  "extent     : 731998.5, 732766.8, 4712956, 4713536  (xmin, xmax, ymin, ymax)"
+ "crs        : +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "
- "crs        : +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs "
  "source     : /home/rstudio/_episodes_rmd/data/NEON-DS-Airborne-Remote-Sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif "
  "names      : HARV_RGB_Ortho "
  "values     : 0, 255  (min, max)"

lines(x[[2]])[2:8] vs lines(y[[2]])[2:8]
  "dimensions : 2317, 3073, 7120141, 3  (nrow, ncol, ncell, nlayers)"
  "resolution : 0.25, 0.25  (x, y)"
  "extent     : 731998.5, 732766.8, 4712956, 4713536  (xmin, xmax, ymin, ymax)"
+ "crs        : +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "
- "crs        : +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs "
  "names      : HARV_RGB_Ortho.1, HARV_RGB_Ortho.2, HARV_RGB_Ortho.3 "
  "min values :                0,                0,                0 "
  "max values :              255,              255,              255 "

lines(x[[3]])[4:10] vs lines(y[[3]])[4:10]
  "dimensions : 2317, 3073, 7120141  (nrow, ncol, ncell)"
  "resolution : 0.25, 0.25  (x, y)"
  "extent     : 731998.5, 732766.8, 4712956, 4713536  (xmin, xmax, ymin, ymax)"
+ "crs        : +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "
- "crs        : +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs "
  "source     : /home/rstudio/_episodes_rmd/data/NEON-DS-Airborne-Remote-Sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif "
  "names      : HARV_RGB_Ortho.1 "
  "values     : 0, 255  (min, max)"

lines(x[[3]])[16:22] vs lines(y[[3]])[16:22]
  "dimensions : 2317, 3073, 7120141  (nrow, ncol, ncell)"
  "resolution : 0.25, 0.25  (x, y)"
  "extent     : 731998.5, 732766.8, 4712956, 4713536  (xmin, xmax, ymin, ymax)"
+ "crs        : +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "
- "crs        : +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs "
  "source     : /home/rstudio/_episodes_rmd/data/NEON-DS-Airborne-Remote-Sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif "
  "names      : HARV_RGB_Ortho.2 "
  "values     : 0, 255  (min, max)"

lines(x[[3]])[28:34] vs lines(y[[3]])[28:34]
  "dimensions : 2317, 3073, 7120141  (nrow, ncol, ncell)"
  "resolution : 0.25, 0.25  (x, y)"
  "extent     : 731998.5, 732766.8, 4712956, 4713536  (xmin, xmax, ymin, ymax)"
+ "crs        : +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "
- "crs        : +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs "
  "source     : /home/rstudio/_episodes_rmd/data/NEON-DS-Airborne-Remote-Sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif "
  "names      : HARV_RGB_Ortho.3 "
  "values     : 0, 255  (min, max)"

lines(x[[4]])[3:9] vs lines(y[[4]])[3:9]
  "dimensions : 2317, 3073, 7120141  (nrow, ncol, ncell)"
  "resolution : 0.25, 0.25  (x, y)"
  "extent     : 731998.5, 732766.8, 4712956, 4713536  (xmin, xmax, ymin, ymax)"
+ "crs        : +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "
- "crs        : +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs "
  "source     : /home/rstudio/_episodes_rmd/data/NEON-DS-Airborne-Remote-Sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif "
  "names      : HARV_RGB_Ortho.2 "
  "values     : 0, 255  (min, max)"

`lines(x[[9]])`: "44248 bytes" ""
`lines(y[[9]])`: "50104 bytes" ""

`lines(x[[10]])`: "170897168 bytes" ""
`lines(y[[10]])`: "15256 bytes"     ""

lines(x[[12]]) vs lines(y[[12]])
  " [1] [             [<-           anyDuplicated as_tibble     as.data.frame"
+ " [6] as.raster     as.tbl_cube   bind          boxplot       brick        "
- " [6] as.raster     bind          boxplot       brick         coerce       "
+ "[11] coerce        coordinates   determinant   duplicated    edit         "
- "[11] coordinates   determinant   duplicated    edit          extent       "
+ "[16] extent        extract       head          initialize    isSymmetric  "
- "[16] extract       head          initialize    isSymmetric   Math         "
+ "[21] Math          Math2         Ops           raster        rasterize    "
- "[21] Math2         Ops           raster        rasterize     relist       "
+ "[26] relist        subset        summary       surfaceArea   tail         "
- "[26] subset        summary       surfaceArea   tail          trim         "
+ "[31] trim          unique        weighted.mean writeValues  "
- "[31] unique        weighted.mean writeValues  "
  "see '?methods' for accessing help and source code"
  ""
```

#### Episode: r-raster-vector-geospatial/06-vector-open-shapefile-in-r.md

``` diff
lines(x[[1]])[3:7] vs lines(y[[1]])[3:7]
  "geometry type:  POLYGON"
  "dimension:      XY"
  "bbox:           xmin: 732128 ymin: 4713209 xmax: 732251.1 ymax: 4713359"
+ "CRS:            32618"
- "projected CRS:  WGS 84 / UTM zone 18N"
  ""

lines(x[[3]]) vs lines(y[[3]])
  "Coordinate Reference System:"
+ "  User input: 32618 "
- "  User input: WGS 84 / UTM zone 18N "
  "  wkt:"
+ "PROJCS[\"WGS 84 / UTM zone 18N\","
- "PROJCRS[\"WGS 84 / UTM zone 18N\","
+ "    GEOGCS[\"WGS 84\","
- "    BASEGEOGCRS[\"WGS 84\","
+ "        DATUM[\"WGS_1984\","
- "        DATUM[\"World Geodetic System 1984\","
+ "            SPHEROID[\"WGS 84\",6378137,298.257223563,"
- "            ELLIPSOID[\"WGS 84\",6378137,298.257223563,"
+ "                AUTHORITY[\"EPSG\",\"7030\"]],"
- "                LENGTHUNIT[\"metre\",1]]],"
+ "            AUTHORITY[\"EPSG\",\"6326\"]],"
  "        PRIMEM[\"Greenwich\",0,"
+ "            AUTHORITY[\"EPSG\",\"8901\"]],"
- "            ANGLEUNIT[\"degree\",0.0174532925199433]],"
+ "        UNIT[\"degree\",0.0174532925199433,"
- "        ID[\"EPSG\",4326]],"
and 40 more ...

lines(x[[5]])[2:8] vs lines(y[[5]])[2:8]
  "geometry type:  POLYGON"
  "dimension:      XY"
  "bbox:           xmin: 732128 ymin: 4713209 xmax: 732251.1 ymax: 4713359"
+ "CRS:            32618"
- "projected CRS:  WGS 84 / UTM zone 18N"
  "  id                       geometry"
  "1  1 POLYGON ((732128 4713359, 7..."
  ""

lines(x[[6]])[3:7] vs lines(y[[6]])[3:7]
  "geometry type:  MULTILINESTRING"
  "dimension:      XY"
  "bbox:           xmin: 730741.2 ymin: 4711942 xmax: 733295.5 ymax: 4714260"
+ "CRS:            32618"
- "projected CRS:  WGS 84 / UTM zone 18N"
  ""

lines(x[[7]])[3:7] vs lines(y[[7]])[3:7]
  "geometry type:  POINT"
  "dimension:      XY"
  "bbox:           xmin: 732183.2 ymin: 4713265 xmax: 732183.2 ymax: 4713265"
+ "CRS:            32618"
- "projected CRS:  WGS 84 / UTM zone 18N"
  ""

lines(x[[10]]) vs lines(y[[10]])
  "Coordinate Reference System:"
+ "  User input: 32618 "
- "  User input: WGS 84 / UTM zone 18N "
  "  wkt:"
+ "PROJCS[\"WGS 84 / UTM zone 18N\","
- "PROJCRS[\"WGS 84 / UTM zone 18N\","
+ "    GEOGCS[\"WGS 84\","
- "    BASEGEOGCRS[\"WGS 84\","
+ "        DATUM[\"WGS_1984\","
- "        DATUM[\"World Geodetic System 1984\","
+ "            SPHEROID[\"WGS 84\",6378137,298.257223563,"
- "            ELLIPSOID[\"WGS 84\",6378137,298.257223563,"
+ "                AUTHORITY[\"EPSG\",\"7030\"]],"
- "                LENGTHUNIT[\"metre\",1]]],"
+ "            AUTHORITY[\"EPSG\",\"6326\"]],"
  "        PRIMEM[\"Greenwich\",0,"
+ "            AUTHORITY[\"EPSG\",\"8901\"]],"
- "            ANGLEUNIT[\"degree\",0.0174532925199433]],"
+ "        UNIT[\"degree\",0.0174532925199433,"
- "        ID[\"EPSG\",4326]],"
and 40 more ...

lines(x[[12]]) vs lines(y[[12]])
  "Coordinate Reference System:"
+ "  User input: 32618 "
- "  User input: WGS 84 / UTM zone 18N "
  "  wkt:"
+ "PROJCS[\"WGS 84 / UTM zone 18N\","
- "PROJCRS[\"WGS 84 / UTM zone 18N\","
+ "    GEOGCS[\"WGS 84\","
- "    BASEGEOGCRS[\"WGS 84\","
+ "        DATUM[\"WGS_1984\","
- "        DATUM[\"World Geodetic System 1984\","
+ "            SPHEROID[\"WGS 84\",6378137,298.257223563,"
- "            ELLIPSOID[\"WGS 84\",6378137,298.257223563,"
+ "                AUTHORITY[\"EPSG\",\"7030\"]],"
- "                LENGTHUNIT[\"metre\",1]]],"
+ "            AUTHORITY[\"EPSG\",\"6326\"]],"
  "        PRIMEM[\"Greenwich\",0,"
+ "            AUTHORITY[\"EPSG\",\"8901\"]],"
- "            ANGLEUNIT[\"degree\",0.0174532925199433]],"
+ "        UNIT[\"degree\",0.0174532925199433,"
- "        ID[\"EPSG\",4326]],"
and 40 more ...
```

#### Episode: r-raster-vector-geospatial/07-vector-shapefile-attributes-in-r.md

``` diff
lines(x[[1]])[2:8] vs lines(y[[1]])[2:8]
  "geometry type:  POINT"
  "dimension:      XY"
  "bbox:           xmin: 732183.2 ymin: 4713265 xmax: 732183.2 ymax: 4713265"
+ "CRS:            32618"
- "projected CRS:  WGS 84 / UTM zone 18N"
  "  Un_ID Domain DomainName       SiteName Type       Sub_Type     Lat      Long"
  "1     A      1  Northeast Harvard Forest Core Advanced Tower 42.5369 -72.17266"
  "  Zone  Easting Northing                Ownership    County annotation"

lines(x[[4]])[2:8] vs lines(y[[4]])[2:8]
  "geometry type:  MULTILINESTRING"
  "dimension:      XY"
  "bbox:           xmin: 730741.2 ymin: 4712685 xmax: 732232.3 ymax: 4713726"
+ "CRS:            32618"
- "projected CRS:  WGS 84 / UTM zone 18N"
  "  OBJECTID_1 OBJECTID       TYPE             NOTES MISCNOTES RULEID"
  "1         14       48 woods road Locust Opening Rd      <NA>      5"
  "2         40       91   footpath              <NA>      <NA>      6"

    lines(x[[7]])                      | lines(y[[7]])                         
[1] "[1] Harvard University, LTER"     - "[1] \"Harvard University, LTER\"" [1]
[2] "Levels: Harvard University, LTER" -                                       
[3] ""                                 | ""                                 [2]

lines(x[[9]]) vs lines(y[[9]])
+ " [1] woods road footpath   footpath   stone wall stone wall stone wall"
- " [1] \"woods road\" \"footpath\"   \"footpath\"   \"stone wall\" \"stone wall\""
+ " [7] stone wall stone wall stone wall boardwalk  woods road woods road"
- " [6] \"stone wall\" \"stone wall\" \"stone wall\" \"stone wall\" \"boardwalk\" "
+ "[13] woods road"
- "[11] \"woods road\" \"woods road\" \"woods road\""
+ "Levels: boardwalk footpath stone wall woods road"
  ""

lines(x[[10]]) vs lines(y[[10]])
+ "[1] \"boardwalk\"  \"footpath\"   \"stone wall\" \"woods road\""
- "NULL"
  ""

lines(x[[14]]) vs lines(y[[14]])
+ "[1] \"boardwalk\"  \"footpath\"   \"stone wall\" \"woods road\""
- "NULL"
  ""

lines(x[[15]]) vs lines(y[[15]])
+ "[1] \"boardwalk\"  \"footpath\"   \"stone wall\" \"woods road\""
- "NULL"
  ""

`lines(x[[16]])`: "[1] \"factor\""    ""
`lines(y[[16]])`: "[1] \"character\"" ""

lines(x[[17]]) vs lines(y[[17]])
+ "[1] \"Bicycles and Horses Allowed\"     \"Bicycles and Horses NOT ALLOWED\""
- "NULL"
+ "[3] \"DO NOT SHOW ON REC MAP\"         "
  ""

lines(x[[18]])[4:8] vs lines(y[[18]])[4:8]
  "dimension:      XYZ"
  "bbox:           xmin: -124.7258 ymin: 24.49813 xmax: -66.9499 ymax: 49.38436"
  "z_range:        zmin: 0 zmax: 0"
+ "CRS:            4326"
- "geographic CRS: WGS 84"
  ""

And 1 more differences ...
```

#### Episode: r-raster-vector-geospatial/08-vector-plot-shapefiles-custom-legend.md

``` diff
lines(x[[1]])[3:7] vs lines(y[[1]])[3:7]
  "geometry type:  POINT"
  "dimension:      XY"
  "bbox:           xmin: 731405.3 ymin: 4712845 xmax: 732275.3 ymax: 4713846"
+ "CRS:            32618"
- "projected CRS:  WGS 84 / UTM zone 18N"
  ""

`lines(x[[2]])`: "[1] \"Histosols\"   \"Inceptisols\"" ""
`lines(y[[2]])`: "NULL"                                ""
```

#### Episode: r-raster-vector-geospatial/09-vector-when-data-dont-line-up-crs.md

``` diff
lines(x[[1]])[4:8] vs lines(y[[1]])[4:8]
  "dimension:      XYZ"
  "bbox:           xmin: -124.7258 ymin: 24.49813 xmax: -66.9499 ymax: 49.38436"
  "z_range:        zmin: 0 zmax: 0"
+ "CRS:            4326"
- "geographic CRS: WGS 84"
  ""

lines(x[[2]])[4:8] vs lines(y[[2]])[4:8]
  "dimension:      XYZ"
  "bbox:           xmin: -124.7258 ymin: 24.49813 xmax: -66.9499 ymax: 49.38436"
  "z_range:        zmin: 0 zmax: 0"
+ "CRS:            4326"
- "geographic CRS: WGS 84"
  ""

lines(x[[3]]) vs lines(y[[3]])
  "Coordinate Reference System:"
+ "  User input: 32618 "
- "  User input: WGS 84 / UTM zone 18N "
  "  wkt:"
+ "PROJCS[\"WGS 84 / UTM zone 18N\","
- "PROJCRS[\"WGS 84 / UTM zone 18N\","
+ "    GEOGCS[\"WGS 84\","
- "    BASEGEOGCRS[\"WGS 84\","
+ "        DATUM[\"WGS_1984\","
- "        DATUM[\"World Geodetic System 1984\","
+ "            SPHEROID[\"WGS 84\",6378137,298.257223563,"
- "            ELLIPSOID[\"WGS 84\",6378137,298.257223563,"
+ "                AUTHORITY[\"EPSG\",\"7030\"]],"
- "                LENGTHUNIT[\"metre\",1]]],"
+ "            AUTHORITY[\"EPSG\",\"6326\"]],"
  "        PRIMEM[\"Greenwich\",0,"
+ "            AUTHORITY[\"EPSG\",\"8901\"]],"
- "            ANGLEUNIT[\"degree\",0.0174532925199433]],"
+ "        UNIT[\"degree\",0.0174532925199433,"
- "        ID[\"EPSG\",4326]],"
and 40 more ...

lines(x[[4]]) vs lines(y[[4]])
  "Coordinate Reference System:"
+ "  User input: 4326 "
- "  User input: WGS 84 "
  "  wkt:"
+ "GEOGCS[\"WGS 84\","
- "GEOGCRS[\"WGS 84\","
+ "    DATUM[\"WGS_1984\","
- "    DATUM[\"World Geodetic System 1984\","
+ "        SPHEROID[\"WGS 84\",6378137,298.257223563,"
- "        ELLIPSOID[\"WGS 84\",6378137,298.257223563,"
+ "            AUTHORITY[\"EPSG\",\"7030\"]],"
- "            LENGTHUNIT[\"metre\",1]]],"
+ "        AUTHORITY[\"EPSG\",\"6326\"]],"
  "    PRIMEM[\"Greenwich\",0,"
+ "        AUTHORITY[\"EPSG\",\"8901\"]],"
- "        ANGLEUNIT[\"degree\",0.0174532925199433]],"
+ "    UNIT[\"degree\",0.0174532925199433,"
- "    CS[ellipsoidal,2],"
+ "        AUTHORITY[\"EPSG\",\"9122\"]],"
- "        AXIS[\"latitude\",north,"
and 8 more ...

lines(x[[5]]) vs lines(y[[5]])
  "Coordinate Reference System:"
+ "  User input: 4326 "
- "  User input: WGS 84 "
  "  wkt:"
+ "GEOGCS[\"WGS 84\","
- "GEOGCRS[\"WGS 84\","
+ "    DATUM[\"WGS_1984\","
- "    DATUM[\"World Geodetic System 1984\","
+ "        SPHEROID[\"WGS 84\",6378137,298.257223563,"
- "        ELLIPSOID[\"WGS 84\",6378137,298.257223563,"
+ "            AUTHORITY[\"EPSG\",\"7030\"]],"
- "            LENGTHUNIT[\"metre\",1]]],"
+ "        AUTHORITY[\"EPSG\",\"6326\"]],"
  "    PRIMEM[\"Greenwich\",0,"
+ "        AUTHORITY[\"EPSG\",\"8901\"]],"
- "        ANGLEUNIT[\"degree\",0.0174532925199433]],"
+ "    UNIT[\"degree\",0.0174532925199433,"
- "    CS[ellipsoidal,2],"
+ "        AUTHORITY[\"EPSG\",\"9122\"]],"
- "        AXIS[\"latitude\",north,"
and 8 more ...

lines(x[[8]])[4:8] vs lines(y[[8]])[4:8]
  "dimension:      XYZ"
  "bbox:           xmin: -80.51989 ymin: 37.91685 xmax: -66.9499 ymax: 47.45716"
  "z_range:        zmin: 0 zmax: 0"
+ "CRS:            4326"
- "geographic CRS: WGS 84"
  ""
```

#### Episode: r-raster-vector-geospatial/10-vector-csv-to-shapefile-in-r.md

``` diff
lines(x[[1]]) vs lines(y[[1]])
  "'data.frame':\t21 obs. of  16 variables:"
  " $ easting   : num  731405 731934 731754 731724 732125 ..."
  " $ northing  : num  4713456 4713415 4713115 4713595 4713846 ..."
+ " $ geodeticDa: Factor w/ 1 level \"WGS84\": 1 1 1 1 1 1 1 1 1 1 ..."
- " $ geodeticDa: chr  \"WGS84\" \"WGS84\" \"WGS84\" \"WGS84\" ..."
+ " $ utmZone   : Factor w/ 1 level \"18N\": 1 1 1 1 1 1 1 1 1 1 ..."
- " $ utmZone   : chr  \"18N\" \"18N\" \"18N\" \"18N\" ..."
+ " $ plotID    : Factor w/ 21 levels \"HARV_015\",\"HARV_033\",..: 1 2 3 4 5 6 7 8 9 10 ..."
- " $ plotID    : chr  \"HARV_015\" \"HARV_033\" \"HARV_034\" \"HARV_035\" ..."
+ " $ stateProvi: Factor w/ 1 level \"MA\": 1 1 1 1 1 1 1 1 1 1 ..."
- " $ stateProvi: chr  \"MA\" \"MA\" \"MA\" \"MA\" ..."
+ " $ county    : Factor w/ 1 level \"Worcester\": 1 1 1 1 1 1 1 1 1 1 ..."
- " $ county    : chr  \"Worcester\" \"Worcester\" \"Worcester\" \"Worcester\" ..."
+ " $ domainName: Factor w/ 1 level \"Northeast\": 1 1 1 1 1 1 1 1 1 1 ..."
- " $ domainName: chr  \"Northeast\" \"Northeast\" \"Northeast\" \"Northeast\" ..."
+ " $ domainID  : Factor w/ 1 level \"D01\": 1 1 1 1 1 1 1 1 1 1 ..."
- " $ domainID  : chr  \"D01\" \"D01\" \"D01\" \"D01\" ..."
+ " $ siteID    : Factor w/ 1 level \"HARV\": 1 1 1 1 1 1 1 1 1 1 ..."
- " $ siteID    : chr  \"HARV\" \"HARV\" \"HARV\" \"HARV\" ..."
+ " $ plotType  : Factor w/ 2 levels \"distributed\",..: 1 2 2 2 2 2 2 2 2 2 ..."
and 9 more ...

lines(x[[5]]) vs lines(y[[5]])
+ "[1] WGS84 WGS84 WGS84 WGS84 WGS84 WGS84"
- "[1] \"WGS84\" \"WGS84\" \"WGS84\" \"WGS84\" \"WGS84\" \"WGS84\""
+ "Levels: WGS84"
  ""

lines(x[[6]]) vs lines(y[[6]])
+ "[1] 18N 18N 18N 18N 18N 18N"
- "[1] \"18N\" \"18N\" \"18N\" \"18N\" \"18N\" \"18N\""
+ "Levels: 18N"
  ""

lines(x[[7]]) vs lines(y[[7]])
  "Coordinate Reference System:"
+ "  User input: 32618 "
- "  User input: WGS 84 / UTM zone 18N "
  "  wkt:"
+ "PROJCS[\"WGS 84 / UTM zone 18N\","
- "PROJCRS[\"WGS 84 / UTM zone 18N\","
+ "    GEOGCS[\"WGS 84\","
- "    BASEGEOGCRS[\"WGS 84\","
+ "        DATUM[\"WGS_1984\","
- "        DATUM[\"World Geodetic System 1984\","
+ "            SPHEROID[\"WGS 84\",6378137,298.257223563,"
- "            ELLIPSOID[\"WGS 84\",6378137,298.257223563,"
+ "                AUTHORITY[\"EPSG\",\"7030\"]],"
- "                LENGTHUNIT[\"metre\",1]]],"
+ "            AUTHORITY[\"EPSG\",\"6326\"]],"
  "        PRIMEM[\"Greenwich\",0,"
+ "            AUTHORITY[\"EPSG\",\"8901\"]],"
- "            ANGLEUNIT[\"degree\",0.0174532925199433]],"
+ "        UNIT[\"degree\",0.0174532925199433,"
- "        ID[\"EPSG\",4326]],"
and 40 more ...

lines(x[[8]]) vs lines(y[[8]])
  "Coordinate Reference System:"
+ "  User input: 32618 "
- "  User input: WGS 84 / UTM zone 18N "
  "  wkt:"
+ "PROJCS[\"WGS 84 / UTM zone 18N\","
- "PROJCRS[\"WGS 84 / UTM zone 18N\","
+ "    GEOGCS[\"WGS 84\","
- "    BASEGEOGCRS[\"WGS 84\","
+ "        DATUM[\"WGS_1984\","
- "        DATUM[\"World Geodetic System 1984\","
+ "            SPHEROID[\"WGS 84\",6378137,298.257223563,"
- "            ELLIPSOID[\"WGS 84\",6378137,298.257223563,"
+ "                AUTHORITY[\"EPSG\",\"7030\"]],"
- "                LENGTHUNIT[\"metre\",1]]],"
+ "            AUTHORITY[\"EPSG\",\"6326\"]],"
  "        PRIMEM[\"Greenwich\",0,"
+ "            AUTHORITY[\"EPSG\",\"8901\"]],"
- "            ANGLEUNIT[\"degree\",0.0174532925199433]],"
+ "        UNIT[\"degree\",0.0174532925199433,"
- "        ID[\"EPSG\",4326]],"
and 40 more ...

lines(x[[10]]) vs lines(y[[10]])
  "Coordinate Reference System:"
+ "  User input: 32618 "
- "  User input: WGS 84 / UTM zone 18N "
  "  wkt:"
+ "PROJCS[\"WGS 84 / UTM zone 18N\","
- "PROJCRS[\"WGS 84 / UTM zone 18N\","
+ "    GEOGCS[\"WGS 84\","
- "    BASEGEOGCRS[\"WGS 84\","
+ "        DATUM[\"WGS_1984\","
- "        DATUM[\"World Geodetic System 1984\","
+ "            SPHEROID[\"WGS 84\",6378137,298.257223563,"
- "            ELLIPSOID[\"WGS 84\",6378137,298.257223563,"
+ "                AUTHORITY[\"EPSG\",\"7030\"]],"
- "                LENGTHUNIT[\"metre\",1]]],"
+ "            AUTHORITY[\"EPSG\",\"6326\"]],"
  "        PRIMEM[\"Greenwich\",0,"
+ "            AUTHORITY[\"EPSG\",\"8901\"]],"
- "            ANGLEUNIT[\"degree\",0.0174532925199433]],"
+ "        UNIT[\"degree\",0.0174532925199433,"
- "        ID[\"EPSG\",4326]],"
and 40 more ...

lines(x[[11]]) vs lines(y[[11]])
  "'data.frame':\t2 obs. of  13 variables:"
  " $ decimalLat: num  42.5 42.5"
  " $ decimalLon: num  -72.2 -72.2"
+ " $ country   : Factor w/ 1 level \"unitedStates\": 1 1"
- " $ country   : chr  \"unitedStates\" \"unitedStates\""
+ " $ stateProvi: Factor w/ 1 level \"MA\": 1 1"
- " $ stateProvi: chr  \"MA\" \"MA\""
+ " $ county    : Factor w/ 1 level \"Worcester\": 1 1"
- " $ county    : chr  \"Worcester\" \"Worcester\""
+ " $ domainName: Factor w/ 1 level \"Northeast\": 1 1"
- " $ domainName: chr  \"Northeast\" \"Northeast\""
+ " $ domainID  : Factor w/ 1 level \"D01\": 1 1"
- " $ domainID  : chr  \"D01\" \"D01\""
+ " $ siteID    : Factor w/ 1 level \"HARV\": 1 1"
- " $ siteID    : chr  \"HARV\" \"HARV\""
+ " $ plotType  : Factor w/ 1 level \"tower\": 1 1"
- " $ plotType  : chr  \"tower\" \"tower\""
+ " $ subtype   : Factor w/ 1 level \"phenology\": 1 1"
- " $ subtype   : chr  \"phenology\" \"phenology\""
  " $ plotSize  : int  40000 40000"
and 4 more ...

lines(x[[12]]) vs lines(y[[12]])
  "Coordinate Reference System:"
+ "  User input: 4326 "
- "  User input: WGS 84 "
  "  wkt:"
+ "GEOGCS[\"WGS 84\","
- "GEOGCRS[\"WGS 84\","
+ "    DATUM[\"WGS_1984\","
- "    DATUM[\"World Geodetic System 1984\","
+ "        SPHEROID[\"WGS 84\",6378137,298.257223563,"
- "        ELLIPSOID[\"WGS 84\",6378137,298.257223563,"
+ "            AUTHORITY[\"EPSG\",\"7030\"]],"
- "            LENGTHUNIT[\"metre\",1]]],"
+ "        AUTHORITY[\"EPSG\",\"6326\"]],"
  "    PRIMEM[\"Greenwich\",0,"
+ "        AUTHORITY[\"EPSG\",\"8901\"]],"
- "        ANGLEUNIT[\"degree\",0.0174532925199433]],"
+ "    UNIT[\"degree\",0.0174532925199433,"
- "    CS[ellipsoidal,2],"
+ "        AUTHORITY[\"EPSG\",\"9122\"]],"
- "        AXIS[\"latitude\",north,"
and 8 more ...

lines(x[[13]]) vs lines(y[[13]])
  "Coordinate Reference System:"
+ "  User input: 4326 "
- "  User input: WGS 84 "
  "  wkt:"
+ "GEOGCS[\"WGS 84\","
- "GEOGCRS[\"WGS 84\","
+ "    DATUM[\"WGS_1984\","
- "    DATUM[\"World Geodetic System 1984\","
+ "        SPHEROID[\"WGS 84\",6378137,298.257223563,"
- "        ELLIPSOID[\"WGS 84\",6378137,298.257223563,"
+ "            AUTHORITY[\"EPSG\",\"7030\"]],"
- "            LENGTHUNIT[\"metre\",1]]],"
+ "        AUTHORITY[\"EPSG\",\"6326\"]],"
  "    PRIMEM[\"Greenwich\",0,"
+ "        AUTHORITY[\"EPSG\",\"8901\"]],"
- "        ANGLEUNIT[\"degree\",0.0174532925199433]],"
+ "    UNIT[\"degree\",0.0174532925199433,"
- "    CS[ellipsoidal,2],"
+ "        AUTHORITY[\"EPSG\",\"9122\"]],"
- "        AXIS[\"latitude\",north,"
and 8 more ...
```

#### Episode: r-raster-vector-geospatial/11-vector-raster-integration.md

``` diff
✔ No differences
```

#### Episode: r-raster-vector-geospatial/12-time-series-raster.md

``` diff
lines(x[[7]]) vs lines(y[[7]])
  "'data.frame':\t5345 obs. of  46 variables:"
+ " $ date     : Factor w/ 5345 levels \"2001-02-11\",\"2001-02-12\",..: 1 2 3 4 5 6 7 8 9 10 ..."
- " $ date     : chr  \"2001-02-11\" \"2001-02-12\" \"2001-02-13\" \"2001-02-14\" ..."
  " $ jd       : int  42 43 44 45 46 47 48 49 50 51 ..."
  " $ airt     : num  -10.7 -9.8 -2 -0.5 -0.4 -3 -4.5 -9.9 -4.5 3.2 ..."
+ " $ f.airt   : Factor w/ 2 levels \"\",\"E\": 1 1 1 1 1 1 1 1 1 1 ..."
- " $ f.airt   : chr  \"\" \"\" \"\" \"\" ..."
  " $ airtmax  : num  -6.9 -2.4 5.7 1.9 2.4 1.3 -0.7 -3.3 0.7 8.9 ..."
+ " $ f.airtmax: Factor w/ 2 levels \"\",\"E\": 1 1 1 1 1 1 1 1 1 1 ..."
- " $ f.airtmax: chr  \"\" \"\" \"\" \"\" ..."
  " $ airtmin  : num  -15.1 -17.4 -7.3 -5.7 -5.7 -9 -12.7 -17.1 -11.7 -1.3 ..."
+ " $ f.airtmin: Factor w/ 2 levels \"\",\"E\": 1 1 1 1 1 1 1 1 1 1 ..."
- " $ f.airtmin: chr  \"\" \"\" \"\" \"\" ..."
  " $ rh       : int  40 45 70 78 69 82 66 51 57 62 ..."
+ " $ f.rh     : Factor w/ 3 levels \"\",\"E\",\"M\": 1 1 1 1 1 1 1 1 1 1 ..."
- " $ f.rh     : chr  \"\" \"\" \"\" \"\" ..."
  " $ rhmax    : int  58 85 100 100 100 100 100 71 81 78 ..."
+ " $ f.rhmax  : Factor w/ 3 levels \"\",\"E\",\"M\": 1 1 1 1 1 1 1 1 1 1 ..."
- " $ f.rhmax  : chr  \"\" \"\" \"\" \"\" ..."
  " $ rhmin    : int  22 14 34 59 37 46 30 34 37 42 ..."
and 51 more ...

lines(x[[8]])[2:8] vs lines(y[[8]])[2:8]
  "dimensions : 652, 696, 453792, 3  (nrow, ncol, ncell, nlayers)"
  "resolution : 30, 30  (x, y)"
  "extent     : 230775, 251655, 4704825, 4724385  (xmin, xmax, ymin, ymax)"
+ "crs        : +proj=utm +zone=19 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "
- "crs        : +proj=utm +zone=19 +datum=WGS84 +units=m +no_defs "
  "names      : X277_HARV_landRGB.1, X277_HARV_landRGB.2, X277_HARV_landRGB.3 "
  "min values :                  26,                  29,                  79 "
  "max values :                 255,                 255,                 255 "
```

#### Episode: r-raster-vector-geospatial/13-plot-time-series-rasters-in-r.md

``` diff
✔ No differences
```

#### Episode: r-raster-vector-geospatial/14-extract-ndvi-from-rasters-in-r.md

``` diff
✔ No differences
```

## Lesson: r-socialsci

#### Episode: r-socialsci/00-intro.md

``` diff
✔ No differences
```

#### Episode: r-socialsci/01-intro-to-r.md

``` diff
✔ No differences
```

#### Episode: r-socialsci/02-starting-with-data.md

``` diff
✔ No differences
```

#### Episode: r-socialsci/03-dplyr-tidyr.md

``` diff
lines(x[[7]]) vs lines(y[[7]])
+ "# A tibble: 3 x 2"
- "`summarise()` ungrouping output (override with `.groups` argument)"
+ "  village  mean_no_membrs"
+ "  <chr>             <dbl>"
+ "1 Chirodzo           7.08"
+ "2 God                6.86"
+ "3 Ruaca              7.57"
  ""

lines(x[[8]]) vs lines(y[[8]])
+ "# A tibble: 9 x 3"
- "# A tibble: 3 x 2"
+ "# Groups:   village [3]"
- "  village  mean_no_membrs"
+ "  village  memb_assoc mean_no_membrs"
- "  <chr>             <dbl>"
+ "  <chr>    <chr>               <dbl>"
- "1 Chirodzo           7.08"
+ "1 Chirodzo no                   8.06"
- "2 God                6.86"
+ "2 Chirodzo yes                  7.82"
- "3 Ruaca              7.57"
+ "3 Chirodzo <NA>                 5.08"
+ "4 God      no                   7.13"
+ "5 God      yes                  8   "
+ "6 God      <NA>                 6   "
+ "7 Ruaca    no                   7.18"
+ "8 Ruaca    yes                  9.5 "
+ "9 Ruaca    <NA>                 6.22"
  ""

lines(x[[9]]) vs lines(y[[9]])
+ "# A tibble: 9 x 3"
- "`summarise()` regrouping output by 'village' (override with `.groups` argument)"
+ "  village  memb_assoc mean_no_membrs"
+ "  <chr>    <chr>               <dbl>"
+ "1 Chirodzo no                   8.06"
+ "2 Chirodzo yes                  7.82"
+ "3 Chirodzo <NA>                 5.08"
+ "4 God      no                   7.13"
+ "5 God      yes                  8   "
+ "6 God      <NA>                 6   "
+ "7 Ruaca    no                   7.18"
+ "8 Ruaca    yes                  9.5 "
+ "9 Ruaca    <NA>                 6.22"
  ""

lines(x[[10]]) vs lines(y[[10]])
+ "# A tibble: 6 x 3"
- "# A tibble: 9 x 3"
  "# Groups:   village [3]"
  "  village  memb_assoc mean_no_membrs"
  "  <chr>    <chr>               <dbl>"
  "1 Chirodzo no                   8.06"
  "2 Chirodzo yes                  7.82"
+ "3 God      no                   7.13"
- "3 Chirodzo <NA>                 5.08"
+ "4 God      yes                  8   "
- "4 God      no                   7.13"
+ "5 Ruaca    no                   7.18"
- "5 God      yes                  8   "
+ "6 Ruaca    yes                  9.5 "
- "6 God      <NA>                 6   "
- "7 Ruaca    no                   7.18"
- "8 Ruaca    yes                  9.5 "
- "9 Ruaca    <NA>                 6.22"
  ""

lines(x[[11]]) vs lines(y[[11]])
+ "# A tibble: 6 x 4"
- "`summarise()` regrouping output by 'village' (override with `.groups` argument)"
+ "# Groups:   village [3]"
+ "  village  memb_assoc mean_no_membrs min_membrs"
+ "  <chr>    <chr>               <dbl>      <dbl>"
+ "1 Chirodzo no                   8.06          4"
+ "2 Chirodzo yes                  7.82          2"
+ "3 God      no                   7.13          3"
+ "4 God      yes                  8             5"
+ "5 Ruaca    no                   7.18          2"
+ "6 Ruaca    yes                  9.5           5"
  ""

lines(x[[12]]) vs lines(y[[12]])
+ "# A tibble: 6 x 4"
- "# A tibble: 9 x 3"
+ "# Groups:   village [3]"
- "  village  memb_assoc mean_no_membrs"
+ "  village  memb_assoc mean_no_membrs min_membrs"
- "  <chr>    <chr>               <dbl>"
+ "  <chr>    <chr>               <dbl>      <dbl>"
- "1 Chirodzo no                   8.06"
+ "1 Chirodzo yes                  7.82          2"
- "2 Chirodzo yes                  7.82"
+ "2 Ruaca    no                   7.18          2"
- "3 Chirodzo <NA>                 5.08"
+ "3 God      no                   7.13          3"
- "4 God      no                   7.13"
+ "4 Chirodzo no                   8.06          4"
- "5 God      yes                  8   "
+ "5 God      yes                  8             5"
- "6 God      <NA>                 6   "
+ "6 Ruaca    yes                  9.5           5"
- "7 Ruaca    no                   7.18"
and 3 more ...

lines(x[[13]]) vs lines(y[[13]])
+ "# A tibble: 6 x 4"
- "`summarise()` regrouping output by 'village' (override with `.groups` argument)"
+ "# Groups:   village [3]"
+ "  village  memb_assoc mean_no_membrs min_membrs"
+ "  <chr>    <chr>               <dbl>      <dbl>"
+ "1 God      yes                  8             5"
+ "2 Ruaca    yes                  9.5           5"
+ "3 Chirodzo no                   8.06          4"
+ "4 God      no                   7.13          3"
+ "5 Chirodzo yes                  7.82          2"
+ "6 Ruaca    no                   7.18          2"
  ""

    lines(x[[14]])      | lines(y[[14]])                                       
[1] "# A tibble: 3 x 2" - "# A tibble: 6 x 3"                    [1]           
[2] "  village      n"  - "# Groups:   village [3]"              [2]           
[3] "  <chr>    <int>"  - "  village  memb_assoc mean_no_membrs" [3]           
[4] "1 Chirodzo    39"  - "  <chr>    <chr>               <dbl>" [4]           
[5] "2 God         43"  - "1 Chirodzo no                   8.06" [5]           
[6] "3 Ruaca       49"  - "2 Chirodzo yes                  7.82" [6]           
                        - "3 God      no                   7.13" [7]           
                        - "4 God      yes                  8   " [8]           
                        - "5 Ruaca    no                   7.18" [9]           
                        - "6 Ruaca    yes                  9.5 " [10]          
... ...                   ...                                    and 1 more ...

lines(x[[15]]) vs lines(y[[15]])
+ "# A tibble: 3 x 2"
- "`summarise()` regrouping output by 'village' (override with `.groups` argument)"
+ "  village      n"
+ "  <chr>    <int>"
+ "1 Ruaca       49"
+ "2 God         43"
+ "3 Chirodzo    39"
  ""

lines(x[[16]]) vs lines(y[[16]])
+ "# A tibble: 2 x 2"
- "# A tibble: 6 x 4"
+ "  no_meals     n"
- "# Groups:   village [3]"
+ "     <dbl> <int>"
- "  village  memb_assoc mean_no_membrs min_membrs"
+ "1        2    52"
- "  <chr>    <chr>               <dbl>      <dbl>"
+ "2        3    79"
- "1 Chirodzo no                   8.06          4"
- "2 Chirodzo yes                  7.82          2"
- "3 God      no                   7.13          3"
- "4 God      yes                  8             5"
- "5 Ruaca    no                   7.18          2"
- "6 Ruaca    yes                  9.5           5"
  ""

And 19 more differences ...
```

#### Episode: r-socialsci/04-ggplot2.md

``` diff
✔ No differences
```

#### Episode: r-socialsci/0x-json.md

``` diff
✔ No differences
```

## Lesson: rr-automation

#### Episode: rr-automation/01-automation.md

``` diff
✔ No differences
```

#### Episode: rr-automation/02-functions.md

``` diff
✔ No differences
```

#### Episode: rr-automation/03-functions-for-data.md

``` diff
✔ No differences
```

#### Episode: rr-automation/04-functions-for-figures.md

``` diff
✔ No differences
```

#### Episode: rr-automation/05-testing.md

``` diff
✔ No differences
```

#### Episode: rr-automation/06-automating.md

``` diff
✔ No differences
```

## Lesson: rr-publication

#### Episode: rr-publication/01-publication.md

``` diff
✔ No differences
```

## Lesson: rr-intro

—-ERRORED—-

## Lesson: rr-version-control

#### Episode: rr-version-control/01-git-github.md

``` diff
✔ No differences
```

#### Episode: rr-version-control/02-git-in-github.md

``` diff
✔ No differences
```

#### Episode: rr-version-control/03-git-in-rstudio.md

``` diff
✔ No differences
```

## Lesson: rr-organization1

#### Episode: rr-organization1/01-file-naming.md

``` diff
✔ No differences
```

#### Episode: rr-organization1/02-file-organization.md

``` diff
✔ No differences
```

## Lesson: rr-literate-programming

#### Episode: rr-literate-programming/01-data-manipulation.md

``` diff
✔ No differences
```

#### Episode: rr-literate-programming/02-literate-programming.md

``` diff
✔ No differences
```

#### Episode: rr-literate-programming/03-explore-knitr.md

``` diff
✔ No differences
```

#### Episode: rr-literate-programming/rmd\_example.md

``` diff
✔ No differences
```

# Session Information

``` r
sessioninfo::session_info()
```

    ## ─ Session info ───────────────────────────────────────────────────────────────
    ##  setting  value                       
    ##  version  R version 4.0.2 (2020-06-22)
    ##  os       Ubuntu 18.04.4 LTS          
    ##  system   x86_64, linux-gnu           
    ##  ui       X11                         
    ##  language en_US:en                    
    ##  collate  en_US.UTF-8                 
    ##  ctype    en_US.UTF-8                 
    ##  tz       America/Los_Angeles         
    ##  date     2020-07-21                  
    ## 
    ## ─ Packages ───────────────────────────────────────────────────────────────────
    ##  package     * version    date       lib source                               
    ##  assertthat    0.2.1      2019-03-21 [1] CRAN (R 4.0.0)                       
    ##  backports     1.1.8      2020-06-17 [1] CRAN (R 4.0.0)                       
    ##  cli           2.0.2      2020-02-28 [1] CRAN (R 4.0.0)                       
    ##  colorspace    1.4-1      2019-03-18 [1] CRAN (R 4.0.0)                       
    ##  commonmark    1.7        2018-12-01 [1] CRAN (R 4.0.0)                       
    ##  crayon        1.3.4.9000 2020-05-10 [1] Github (r-lib/crayon@dcf6d44)        
    ##  diffobj       0.3.0      2020-05-11 [1] CRAN (R 4.0.0)                       
    ##  digest        0.6.25     2020-02-23 [1] CRAN (R 4.0.0)                       
    ##  dplyr       * 1.0.0      2020-05-29 [1] CRAN (R 4.0.2)                       
    ##  ellipsis      0.3.1      2020-05-15 [1] CRAN (R 4.0.0)                       
    ##  evaluate      0.14       2019-05-28 [1] CRAN (R 4.0.0)                       
    ##  fansi         0.4.1      2020-01-08 [1] RSPM (R 4.0.0)                       
    ##  farver        2.0.3      2020-01-16 [1] CRAN (R 4.0.0)                       
    ##  forcats     * 0.5.0      2020-03-01 [1] CRAN (R 4.0.0)                       
    ##  fs          * 1.4.2      2020-06-30 [1] CRAN (R 4.0.2)                       
    ##  generics      0.0.2      2018-11-29 [1] CRAN (R 4.0.0)                       
    ##  ggplot2     * 3.3.2      2020-06-19 [1] CRAN (R 4.0.0)                       
    ##  gh          * 1.1.0      2020-01-24 [1] CRAN (R 4.0.0)                       
    ##  git2r       * 0.27.1     2020-05-03 [1] CRAN (R 4.0.0)                       
    ##  glue          1.4.1      2020-05-13 [1] CRAN (R 4.0.0)                       
    ##  gtable        0.3.0      2019-03-25 [1] CRAN (R 4.0.0)                       
    ##  here        * 0.1        2017-05-28 [1] CRAN (R 4.0.0)                       
    ##  highr         0.8        2019-03-20 [1] CRAN (R 4.0.0)                       
    ##  htmltools     0.5.0      2020-06-16 [1] CRAN (R 4.0.0)                       
    ##  httr          1.4.1      2019-08-05 [1] CRAN (R 4.0.0)                       
    ##  jsonlite    * 1.7.0      2020-06-25 [1] CRAN (R 4.0.1)                       
    ##  knitr         1.29       2020-06-23 [1] CRAN (R 4.0.0)                       
    ##  labeling      0.3        2014-08-23 [1] CRAN (R 4.0.0)                       
    ##  lifecycle     0.2.0      2020-03-06 [1] CRAN (R 4.0.0)                       
    ##  magrittr    * 1.5        2014-11-22 [1] CRAN (R 4.0.0)                       
    ##  memoise       1.1.0      2017-04-21 [1] CRAN (R 4.0.0)                       
    ##  munsell       0.5.0      2018-06-12 [1] CRAN (R 4.0.0)                       
    ##  pegboard    * 0.0.0.9000 2020-06-26 [1] Github (carpentries/pegboard@720c419)
    ##  pillar        1.4.6      2020-07-10 [1] CRAN (R 4.0.2)                       
    ##  pkgconfig     2.0.3      2019-09-22 [1] CRAN (R 4.0.0)                       
    ##  polite      * 0.1.1      2019-11-30 [1] CRAN (R 4.0.0)                       
    ##  purrr       * 0.3.4      2020-04-17 [1] CRAN (R 4.0.0)                       
    ##  R6            2.4.1      2019-11-12 [1] CRAN (R 4.0.0)                       
    ##  ratelimitr    0.4.1      2018-10-07 [1] CRAN (R 4.0.0)                       
    ##  Rcpp          1.0.5      2020-07-06 [1] CRAN (R 4.0.2)                       
    ##  rematch2      2.1.2      2020-05-01 [1] CRAN (R 4.0.0)                       
    ##  rlang         0.4.7      2020-07-09 [1] CRAN (R 4.0.2)                       
    ##  rmarkdown     2.3        2020-06-18 [1] CRAN (R 4.0.1)                       
    ##  robotstxt     0.7.7      2020-06-27 [1] CRAN (R 4.0.1)                       
    ##  rprojroot     1.3-2      2018-01-03 [1] CRAN (R 4.0.0)                       
    ##  rvest         0.3.5      2019-11-08 [1] CRAN (R 4.0.0)                       
    ##  scales        1.1.1      2020-05-11 [1] CRAN (R 4.0.0)                       
    ##  sessioninfo   1.1.1      2018-11-05 [1] CRAN (R 4.0.0)                       
    ##  stringi       1.4.6      2020-02-17 [1] CRAN (R 4.0.0)                       
    ##  stringr       1.4.0      2019-02-10 [1] CRAN (R 4.0.0)                       
    ##  tibble        3.0.3      2020-07-10 [1] CRAN (R 4.0.2)                       
    ##  tidyr       * 1.1.0      2020-05-20 [1] CRAN (R 4.0.0)                       
    ##  tidyselect    1.1.0      2020-05-11 [1] CRAN (R 4.0.0)                       
    ##  tinkr         0.0.0.9000 2020-06-19 [1] Github (ropenscilabs/tinkr@9fdad3b)  
    ##  usethis       1.6.1      2020-04-29 [1] CRAN (R 4.0.0)                       
    ##  utf8          1.1.4      2018-05-24 [1] CRAN (R 4.0.0)                       
    ##  vctrs         0.3.2      2020-07-15 [1] CRAN (R 4.0.2)                       
    ##  waldo       * 0.2.0      2020-07-13 [1] CRAN (R 4.0.2)                       
    ##  withr         2.2.0      2020-04-20 [1] CRAN (R 4.0.0)                       
    ##  xfun          0.15       2020-06-21 [1] CRAN (R 4.0.0)                       
    ##  xml2          1.3.2      2020-04-23 [1] CRAN (R 4.0.0)                       
    ##  yaml          2.2.1      2020-02-01 [1] CRAN (R 4.0.0)                       
    ## 
    ## [1] /home/zhian/R/library
    ## [2] /usr/local/lib/R/site-library
    ## [3] /usr/lib/R/site-library
    ## [4] /usr/lib/R/library
