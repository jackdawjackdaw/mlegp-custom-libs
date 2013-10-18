mlegp-custom-libs
====

Libraries for Gaussian-Process based FANOVA in R > 3.0.

The versions of mlegpFULL and adapt provided by the authors were not
compatible with the latest R major version, which needs a `NAMESPACE` file.

The R packages here contain valid namespace files.

Building/Installing
----

Install adapt first

    $ cd ./adapt
    $ R CMD INSTALL .
    $ cd ..

Install mlegpFULL next

    $ cd ./mlegpFULL
    $ R CMD INSTALL .
    $ cd ..


Enjoy
