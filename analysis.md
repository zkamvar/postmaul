Setup
=====

The source for all the repositories is located at
<a href="https://carpentries.github.io/curriculum-feed/carpentries_lessons.json" class="uri">https://carpentries.github.io/curriculum-feed/carpentries_lessons.json</a>.
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
    ##    repository                         curriculum           life_cycle language `_row` URL                                                       
    ##    <chr>                              <chr>                <fct>      <chr>    <chr>  <chr>                                                     
    ##  1 capstone-novice-spreadsheet-biblio <NA>                 pre-alpha  en       1      <NA>                                                      
    ##  2 lc-spreadsheets                    lc                   beta       en       2      https://github.com/LibraryCarpentry/lc-spreadsheets       
    ##  3 rr-automation                      reproducible-science on-hold    en       3      https://github.com/datacarpentry/rr-automation            
    ##  4 lesson-example                     <NA>                 stable     en       4      https://github.com/carpentries/lesson-example             
    ##  5 git-novice-es                      swc-es               stable     es       5      https://github.com/swcarpentry/git-novice-es              
    ##  6 shell-economics                    dc-economics         pre-alpha  en       6      https://github.com/datacarpentry/shell-economics/         
    ##  7 openrefine-ecology-lesson          dc-ecology           stable     en       7      https://github.com/datacarpentry/openrefine-ecology-lesson
    ##  8 python-humanities-lesson           <NA>                 pre-alpha  en       8      <NA>                                                      
    ##  9 hg-novice                          swc                  stable     en       9      https://github.com/swcarpentry/hg-novice                  
    ## 10 spreadsheet-humanities-lesson      <NA>                 pre-alpha  en       10     <NA>                                                      
    ## # … with 78 more rows

First look at available lessons
===============================

I’m going to take a look at all of the software carpentry lessons first.
There are currently 88 available lessons, but they are in different
stages of completion:

``` r
ggplot(lessons, aes(y = life_cycle, fill = curriculum)) +
  geom_bar(orientation = "y")
```

![](analysis_files/figure-markdown_github/lesson%20vis-1.png)

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
    ##    repository                 curriculum life_cycle language `_row` URL                                                        
    ##    <chr>                      <chr>      <fct>      <chr>    <chr>  <chr>                                                      
    ##  1 git-novice-es              swc-es     stable     es       5      https://github.com/swcarpentry/git-novice-es               
    ##  2 openrefine-ecology-lesson  dc-ecology stable     en       7      https://github.com/datacarpentry/openrefine-ecology-lesson 
    ##  3 hg-novice                  swc        stable     en       9      https://github.com/swcarpentry/hg-novice                   
    ##  4 matlab-novice-inflammation swc        stable     en       11     https://github.com/swcarpentry/matlab-novice-inflammation  
    ##  5 r-novice-inflammation      swc        stable     en       12     https://github.com/swcarpentry/r-novice-inflammation       
    ##  6 spreadsheet-ecology-lesson dc-ecology stable     en       13     https://github.com/datacarpentry/spreadsheet-ecology-lesson
    ##  7 python-novice-gapminder    swc        stable     en       16     https://github.com/swcarpentry/python-novice-gapminder     
    ##  8 lc-data-intro              lc         stable     en       19     https://github.com/LibraryCarpentry/lc-data-intro          
    ##  9 R-ecology-lesson           dc-ecology stable     en       20     https://github.com/datacarpentry/R-ecology-lesson          
    ## 10 git-novice                 swc        stable     en       24     https://github.com/swcarpentry/git-novice                  
    ## # … with 26 more rows

I end up with 36 repositories to play with:

``` r
ggplot(stable, aes(y = curriculum, fill = language)) +
  geom_bar(orientation = "y")
```

![](analysis_files/figure-markdown_github/curriculum-language-1.png)

Inspection of repository features
=================================

I will use the {gh} package to inspect the features of each repository:

-   tags
-   directory structure
-   dependencies

I can use the GitHub API to get the contents of the repositories:
<a href="https://developer.github.com/v3/repos/contents/#get-contents" class="uri">https://developer.github.com/v3/repos/contents/#get-contents</a>.
The {gh} package allows me to write the responses to disk so that I
don’t have to query every time I want to re-run the analysis. If I
wanted to do this with fresh data, all I would need to do is to clear
the data folder.

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

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--capstone-novice-spreadsheet-biblio.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--python-humanities-lesson.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--spreadsheet-humanities-lesson.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--R-genomics.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--geospatial-python.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--deep-learning_intro.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--docker-introduction.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--sql-humanities-lesson.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--NA.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--Data-Science-for-Docs.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--gap-lesson.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--git-Rstudio-course.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--jupyter-notebooks-intro.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--shell-extras.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--NA.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--NA.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--python-packaging-publishing.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--NA.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--jupyter_maps.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--IoT_arduino_nano.json': No such file or directory

    ## Warning in open.connection(con, "rb"): cannot open file '/home/zhian/Documents/Carpentries/Git/zkamvar--postmaul/data/rmd_JSON/NA--OpenRefine-humanities-lesson.json': No such file or directory

After parsing, we find that we have RMarkdown files for a grand total of
15 lessons and 7 stable lessons. From here, we can grab these lessons to
see if we can build them with our docker container.

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
  path <- glue::glue("{path}/{u}--{r}")
  if (fs::dir_exists(path)) {
    git2r::pull(repo = path)
  } else {
    git2r::clone(URL, local_path = path)
  }
}
purrr::walk2(has_rmd_all$user, has_rmd_all$repository, g, rmdpath) 
```

    ## [updated] 0de7e29a10..6bcf0ae23b refs/remotes/origin/gh-pages
    ## [new]     71ca575532ce202f5cd3 refs/remotes/origin/guide
    ## [updated] f5474fac9b..94b98bda6e refs/remotes/origin/master

Now all the repos have been downloaded, we can render the episodes under
different versions of R and see how they go. Note that I have to use the
geospatial R container in order to get things to work properly.

``` r
run_docker <- function(the_path, R_VERSION) {
  docker_run <- "docker run --rm -it -v {the_path}:/home/rstudio"
  contai_ner <- "rocker/geospatial:{R_VERSION} make -C /home/rstudio lesson-md"
  system(glue::glue("cd {the_path}; git clean -fd .; git checkout -- ."))
  system(glue::glue("rm -rf {the_path}/_episodes/*"))
  system(glue::glue("{glue::glue(docker_run)} {glue::glue(contai_ner)}"))
}

dockin <- function(u, r) {

  pth <- glue::glue("{u}--{r}")
  the_path <- fs::path_abs(fs::path("data", "rmd-repos", pth))

  # Run with R 3.6.3 ---------------------------
  run_docker(the_path, "3.6.3")
  r3 <- try(Lesson$new(the_path, rmd = FALSE))

  # Run with R 4.0.0 ---------------------------
  run_docker(the_path, "4.0.0")
  r4 <- try(Lesson$new(the_path, rmd = FALSE))

  system(glue::glue("cd {the_path}; git clean -fd .; git checkout -- ."))


  list(r3, r4)

}

res <- purrr::map2(has_rmd_all$user, has_rmd_all$repository, dockin)
```

    ## Error: PCDATA invalid Char value 27 [9]
    ## Error: PCDATA invalid Char value 27 [9]

We can iterate and compare:

``` r
cmpr <- function(lesson) {

  if (!inherits(lesson[[1]], "Lesson")) {
    return(NULL)
  }
  otag    <- ".//self::d1:code_block[@ktag='{: .output}']"
  o1 <- map(lesson[[1]]$episodes, ~.x$code %>% xml2::xml_find_all(otag) %>% xml2::xml_text())
  o2 <- map(lesson[[2]]$episodes, ~.x$code %>% xml2::xml_find_all(otag) %>% xml2::xml_text())
  for (i in length(o1)) {
    
    message(glue::glue("Lesson: {basename(lesson[[1]]$path)}  Episode: {basename(lesson[[1]]$files[i])}"))
    print(waldo::compare(o1[[i]], o2[[i]]))
  }
}

purrr::walk(res, cmpr)
```

    ## Lesson: LibraryCarpentry--lc-r  Episode: 04-data-viz-ggplot.md

    ## `lines(x[[2]])`: "✔ ggplot2 3.3.0     ✔ dplyr   0.8.5" "✔ tibble  3.0.1     ✔ stringr 1.4.0" "✔ tidyr   1.0.2     ✔ forcats 0.5.0" "✔ purrr   0.3.4     " ""
    ## `lines(y[[2]])`: "✔ ggplot2 3.3.1     ✔ dplyr   1.0.0" "✔ tibble  3.0.1     ✔ stringr 1.4.0" "✔ tidyr   1.1.0     ✔ forcats 0.5.0" "✔ purrr   0.3.4     " ""
    ## 
    ## `lines(x[[5]])`: "The following objects are masked from 'package:dplyr':" "" "    intersect, setdiff, union"       ""
    ## `lines(y[[5]])`: "The following objects are masked from 'package:base':"  "" "    date, intersect, setdiff, union" ""
    ## 
    ## `lines(x[[6]])`: "The following objects are masked from 'package:base':"              "" "    date, intersect, setdiff, union" ""
    ## `lines(y[[6]])`: "`stat_bin()` using `bins = 30`. Pick better value with `binwidth`."                                          ""
    ## 
    ##     lines(x[[7]])                                                        | lines(y[[7]])                                                                          
    ## [1] "`stat_bin()` using `bins = 30`. Pick better value with `binwidth`." - ""                                                                                 [1] 
    ##                                                                          - "   0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 " [2] 
    ##                                                                          - "2348  875  638  464  362  282  199  146  118   97   84   50   41   46   40   33 " [3] 
    ##                                                                          - "  16   17   18   19   20   21   22   23   24   25   26   27   28   29   30   31 " [4] 
    ##                                                                          - "  17   20   26   17   14   12    7   15    7    8    6    6    3    3    2    4 " [5] 
    ##                                                                          - "  32   33   34   35   36   38   39   40   41   43   47   61   63   69   79  106 " [6] 
    ##                                                                          - "   1    5    4    3    2    2    3    1    1    1    1    1    2    1    1    1 " [7] 
    ##                                                                          - " 113 "                                                                            [8] 
    ##                                                                          - "   1 "                                                                            [9] 
    ## [2] ""                                                                   | ""                                                                                 [10]
    ## 
    ##      lines(x[[8]])                                                                      | lines(y[[8]])        
    ##  [1] ""                                                                                 - "[1] \"numeric\"" [1]
    ##  [2] "   0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 " -                      
    ##  [3] "2348  875  638  464  362  282  199  146  118   97   84   50   41   46   40   33 " -                      
    ##  [4] "  16   17   18   19   20   21   22   23   24   25   26   27   28   29   30   31 " -                      
    ##  [5] "  17   20   26   17   14   12    7   15    7    8    6    6    3    3    2    4 " -                      
    ##  [6] "  32   33   34   35   36   38   39   40   41   43   47   61   63   69   79  106 " -                      
    ##  [7] "   1    5    4    3    2    2    3    1    1    1    1    1    2    1    1    1 " -                      
    ##  [8] " 113 "                                                                            -                      
    ##  [9] "   1 "                                                                            -                      
    ## [10] ""                                                                                 | ""                [2]
    ## 
    ## `lines(x[[9]])`: "[1] \"numeric\"" ""
    ## `lines(y[[9]])`: "[1] \"Date\""    ""
    ## 
    ## `lines(x[[10]])`: "[1] \"Date\""                                                       ""
    ## `lines(y[[10]])`: "`summarise()` ungrouping output (override with `.groups` argument)" ""

    ## Lesson: datacarpentry--genomics-r-intro  Episode: XX-knitr-markdown.md

    ## ✔ No differences

    ## Lesson: swcarpentry--r-novice-inflammation  Episode: 15-supp-loops-in-depth.md

    ## ✔ No differences

    ## Lesson: swcarpentry--r-novice-gapminder  Episode: 16-wrap-up.md

    ## ✔ No differences

    ## Lesson: datacarpentry--organization-geospatial  Episode: 04-geo-landscape.md

    ## ✔ No differences

    ## Lesson: datacarpentry--r-intro-geospatial  Episode: 08-writing-data.md

    ## ✔ No differences

    ## Lesson: datacarpentry--r-raster-vector-geospatial  Episode: 14-extract-ndvi-from-rasters-in-r.md

    ## ✔ No differences

    ## Lesson: datacarpentry--r-socialsci  Episode: 0x-json.md

    ## ✔ No differences

Session Information
===================

``` r
sessioninfo::session_info()
```

    ## ─ Session info ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    ##  setting  value                       
    ##  version  R version 4.0.2 (2020-06-22)
    ##  os       Ubuntu 18.04.4 LTS          
    ##  system   x86_64, linux-gnu           
    ##  ui       X11                         
    ##  language en_US:en                    
    ##  collate  en_US.UTF-8                 
    ##  ctype    en_US.UTF-8                 
    ##  tz       America/Los_Angeles         
    ##  date     2020-07-20                  
    ## 
    ## ─ Packages ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    ##  package     * version    date       lib source                               
    ##  assertthat    0.2.1      2019-03-21 [1] CRAN (R 4.0.0)                       
    ##  backports     1.1.8      2020-06-17 [1] CRAN (R 4.0.0)                       
    ##  cli           2.0.2      2020-02-28 [1] CRAN (R 4.0.0)                       
    ##  colorspace    1.4-1      2019-03-18 [1] CRAN (R 4.0.0)                       
    ##  commonmark    1.7        2018-12-01 [1] CRAN (R 4.0.0)                       
    ##  crayon        1.3.4.9000 2020-05-10 [1] Github (r-lib/crayon@dcf6d44)        
    ##  curl          4.3        2019-12-02 [1] CRAN (R 4.0.0)                       
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
