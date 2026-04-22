## R package development

### Key commands

```
# To run code
Rscript -e "devtools::load_all(); code"

# To run all tests
Rscript -e "devtools::test()"

# To run all tests for files starting with {name}
Rscript -e "devtools::test(filter = '^{name}')"

# To run all tests for R/{name}.R
Rscript -e "devtools::test_active_file('R/{name}.R')"

# To run a single test "blah" for R/{name}.R
Rscript -e "devtools::test_active_file('R/{name}.R', desc = 'blah')"

# To redocument the package
Rscript -e "devtools::document()"

# To check pkgdown documentation
Rscript -e "pkgdown::check_pkgdown()"

# To check the package with R CMD check
Rscript -e "devtools::check()"

# To format code
air format .
```

### Coding

- Always run `air format .` after generating code.
- Don't use piping.
- `furniture` supports both data.frames, data.tables, and tibbles; ensure new code works with both (avoid silently coercing between them).
- Keep the dependency footprint light; prefer base R over adding new Imports.

### Testing

- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`.
- All new code should have an accompanying test.
- If there are existing tests, place new tests next to similar existing tests.
- Strive to keep your tests minimal with few comments.
- For user-facing functions like `table1()`, `tableC()`, `tableF()`, and `washer()`, test both data.frame and data.table inputs.

### Documentation

- Every user-facing function should be exported and have roxygen2 documentation.
- Examples in roxygen should use the package's own data.

### `NEWS.md`

- Every user-facing change should be given a bullet in `NEWS.md`. Do not add bullets for small documentation changes or internal refactorings.
- Each bullet should briefly describe the change to the end user and mention the related issue in parentheses.
- A bullet can consist of multiple sentences but should not contain any new lines (i.e. DO NOT line wrap).
- If the change is related to a function, put the name of the function early in the bullet.
- Order bullets alphabetically by function name. Put all bullets that don't mention function names at the beginning.
- Group bullets under a version heading (e.g. `# furniture 1.10.2`) at the top of the file.

### GitHub

- If you use `gh` to retrieve information about an issue, always use `--comments` to read all the comments.
- The repository is `tysonstanley/furniture`.

### Writing

- Use sentence case for headings.
- Use US English.

### Proofreading

If the user asks you to proofread a file, act as an expert proofreader and editor with a deep understanding of clear, engaging, and well-structured writing.
Work paragraph by paragraph, always starting by making a TODO list that includes individual items for each top-level heading.
Fix spelling, grammar, and other minor problems without asking the user. Label any unclear, confusing, or ambiguous sentences with a FIXME comment.
Only report what you have changed.