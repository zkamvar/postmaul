# postmaul

>  Post mauls are similar to sledgehammers in shape, but are meant to drive
>  wooden fence posts or tree stakes into the earth.
>  <https://en.wikipedia.org/wiki/Sledgehammer#Post_maul>

## What is this?

The `postmaul` repo is an analysis of the requirements of The Carpentries'
lesson websites. It may evolve into something more than an analysis, but at the
moment, it is merely a snapshot in time. As of 2020-03-30, all of The
Carpentries' websites require the following for building:

1. GNU Make
2. Python >= 3.4
3. Jekyll >= 3.8.5

Some of the websites---like [python-novice-gapminder]---are static markdown 
sites where all example code and expected output is provided at the time of 
writing. However, sites like [r-novice-gapminder] are a bit different in that
they require that the example code be run to produce output.

I am attempting to use the GitHub API to see if I can come up with a generally
broad list of requirements for different sites. Perhaps this can be adapted 
as a method of adding value to the current database of sites for continuous
deployment<a name="#CD"> (CD).

## Where am I starting from?

At the moment, I have the idea that we can use docker-compose to create [CD] for
the lessons. I have written up my notes in my walkthrough of the [docker-compose
tutorial], where I have set up a docker container, added a rule to the Makefile,
and added `docker-compose.yml` file that effectively replaces a complex docker
command. 

[python-novice-gapminder]: https://github.com/swcarpentry/python-novice-gapminder

[r-novice-gapminder]: https://github.com/swcarpentry/r-novice-gapminder

[docker-compose tutorial]: https://github.com/zkamvar/toot-docker-compose


