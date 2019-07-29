# transformer

[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis CI build status](https://travis-ci.com/acidgenomics/transformer.svg?branch=master)](https://travis-ci.com/acidgenomics/transformer)
[![AppVeyor CI build status](https://ci.appveyor.com/api/projects/status/is5x2avlk98hrhg5/branch/master?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/transformer/branch/master)
[![Anaconda version](https://anaconda.org/bioconda/r-transformer/badges/version.svg) ![Anaconda latest release date](https://anaconda.org/bioconda/r-transformer/badges/latest_release_date.svg) ![Anaconda downloads](https://anaconda.org/bioconda/r-transformer/badges/downloads.svg)](https://anaconda.org/bioconda/r-transformer)

Additional S3 and S4 coercion methods for easy interconversion of [Bioconductor][] data classes.

This package is part of the [basejump][] toolkit.

## Installation

### [R][] method

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
}
Sys.setenv(R_REMOTES_UPGRADE = "always")
# Set `GITHUB_PAT` in `~/.Renviron` if you get a rate limit error.
remotes::install_github("acidgenomics/transformer")
```

### [conda][] method

Configure [conda][] to use the [bioconda][] channels.

```bash
conda install -c bioconda r-transformer
```

[R]: https://www.r-project.org/
[basejump]: https://basejump.acidgenomics.com/
[bioconda]: https://bioconda.github.io/
[conda]: https://conda.io/
