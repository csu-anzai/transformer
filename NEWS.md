## transformer 0.2.6 (2019-09-03)

### Minor changes

- `mutateAll`: Bug fix that improves column creation with `DataFrame`.

## transformer 0.2.5 (2019-08-26)

### Major changes

- Renamed dplyr-like generics into camel case: `innerJoin`, `leftJoin`,
  `rightJoin`, `fullJoin`, `semiJoin`, `antiJoin`; `mutateAll`, `mutateAt`,
  `mutateIf`; `selectIf`.
- Reworked `mutate*` and `select*` verbs to use simple base R code internally,
  and now methods are only defined for `DataFrame` class. This way we don't
  run into any masking issues with dplyr, and we don't need to list it as a
  suggested package.

### Minor changes

- Dropped support for `select_all` and `select_all`. These verbs don't really
  make sense in the context of Bioconductor-related internal code.

## transformer 0.2.4 (2019-08-15)

### New functions

Improved S4 method support for dplyr verbs working on `DataFrame` class.

- Joins: `inner_join`, `left_join` (already defined), `right_join`, `full_join`,
  `semi_join`, and `anti_join`. These methods use `merge` internally but
  ensure that rows are not unexpectedly reordered.
- Scoped variants of `mutate` and `select`: `mutate_all`, `mutate_at`,
  `mutate_if`, `transmute_at`, `transmute_if`; `select_all`, `select_at`,
  `select_if`. These variants keep track of row names. Internally, these
  functions coerce to `tbl_df` and then call the dplyr function. Return is
  coerced back to `DataFrame`, with row names preserved.

### Major changes

- `relevel:` Made S3 methods for `relevel` defunct of favor of recommend S4
  method usage of `droplevels` instead. S4 method support for `droplevels` is
  already provided in S4Vectors (i.e. `List`, `DataFrame)`. Here we are
  improving `droplevels` functionality by adding additional method support
  for `Ranges` (to support `IRanges`, `GRanges`) as well as
  `SummarizedExperiment`, which enables automatic leveling of factors in
  `colData` or `rowData`.

## transformer 0.2.3 (2019-08-13)

### Minor changes

- Fix for automatic decoding of `Rle` columns, which will error otherwise when
  attempting to coerce from `DataFrame` to `data.frame`.

## transformer 0.2.2 (2019-08-08)

### Minor changes

- Reexporting dplyr `left_join` generic. Improved `DataFrame` method for
  `left_join` to keep row names, which correspond to the `x` object.

## transformer 0.2.1 (2019-08-05)

### Minor changes

- Improved documentation consistency, using new acidroxygen package.

## transformer 0.2.0 (2019-07-19)

### New methods

- `relevel`: Added S3 method support for `DataFrame` and `GRanges` objects
  containing factor columns.

### Major changes

- Migraged `atomize`, `decode`, and `encode` from brio here.
- Removed `as.DataFrame` S3 method in favor of simply using S4 `as` coercion.
- Tightened up `as.SummarizedExperiment` S3 methods to support
  `SummarizedExperiment` and `RangedSummarizedExperiment` specifically.

### Minor changes

- Improved S3 method documentation. Use `rdname` tag instead of `method`
  (or legacy `s3method`).

## transformer 0.1.12 (2019-07-12)

### Major changes

- Improved automatic row name handling for `data.table` and `tbl_df` when
  coercing to `DataFrame`.
- Added coercion support from `IRanges` to `data.frame`, `data.table`, and
  `tbl_df`, which works like the default `GRanges` coercion methods.
- Added new `matchRowNameColumn` function, which can detect row name columns
  inside data frame classes that don't support row names assignment, currently
  `data.table` and `tbl_df`.

## transformer 0.1.11 (2019-05-04)

### Minor changes

- Importing `GRanges` class from GenomicRanges package.
- Improved Travis CI and AppVeyor configuration.

## transformer 0.1.10 (2019-04-25)

### Minor changes

- S4 generic reexport documentation fixes.

## transformer 0.1.9 (2019-04-22)

### Minor changes

- Initial `DataFrame` S4 method support for `left_join`.

## transformer 0.1.8 (2019-03-28)

- Updated working examples and unit tests to use acidtest package.

## transformer 0.1.7 (2019-03-22)

### Minor changes

- Migrated code to [Acid Genomics].

## transformer 0.1.6 (2019-02-25)

### Minor changes

- Improve documentation regarding default row name handling for `as_tibble`
  and `as.data.table`.

## transformer 0.1.5 (2019-02-18)

### Major changes

- Added support for coercion to `data.table` class. `DataFrame` and `GRanges`
  class objects are initially supported.

## transformer 0.1.4 (2019-01-23)

### Minor changes

- Fix for `DataFrame` to `tbl_df` coercion that doesn't make names syntactically
  valid during internal `as.data.frame()` call.

## transformer 0.1.3 (2019-01-22)

### Major changes

- Migrated these functions to [brio][] package: `atomize()`, `factorize()`,
  `encode()`, `decode()`.

## transformer 0.1.2 (2019-01-20)

### Major changes

- Migrated `decode()` and `encode()` S4 methods from [basejump][].
- Improved `atomize()` compatibility with [Bioconductor][] 3.7 release. Now
  DataFrame objects containing Rle columns will get decoded consistently without
  applying stringsAsFactors in an unexpected fashion. This works correctly in
  Bioconductor 3.8 simply using `as.data.frame()` internally, but has unwanted
  coercion of `character` to `factor` columns for Rle-encoded data when using
  the Bioconductor 3.7 release.

### Minor changes

- Improved code coverage to 100%.

## transformer 0.1.1 (2019-01-14)

### Major changes

- Renamed package from S4Transformer to simply transformer.
- Migrated `atomize()` and `factorize()` from [basejump][].

### Minor changes

- Ensuring `tibble::as_tibble()` gets reexported.
- Now exporting `as.DataFrame()` S3 coercion methods, for consistency.
- Improved documentation for `as()` coercion methods.
- Improved [Travis CI][] and [AppVeyor][] CI build settings.

## transformer 0.1.0 (2019-01-06)

Initial release. Migrated useful S4 coercion methods from [basejump][] for
easier unit testing in a separate package.

[AppVeyor CI]: https://www.appveyor.com/
[basejump]: https://basejump.acidgenomics.com/
[brio]: https://brio.acidgenomics.com/
[Bioconductor]: https://bioconductor.org/
[Travis CI]: https://travis-ci.com/
