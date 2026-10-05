# Mini Relational DBMS in OCaml

A small, functional relational database management system implemented in OCaml. It provides core relational algebra operations, functional dependency analysis, and normalization level detection (1NF, 2NF, 3NF).

## Features

- **Table validation** – ensures schema consistency, correct types, and nullability constraints.
- **Insertion** – adds rows to a table with type and length checks.
- **Cartesian product** – combines two tables, rejecting duplicate column names.
- **Projection** – selects a subset of columns in a specified order.
- **Restriction** – filters rows using a predicate function.
- **Functional dependency detection** – computes all functional dependencies (FDs) satisfied by the data.
- **Elementary FDs** – identifies FDs with no redundant attributes on the left-hand side.
- **Normalization level** – determines whether a table is in 1NF, 2NF, or 3NF.
- **Unit tests** – comprehensive test suite covering all operations.

## Tech Stack

- **Language:** OCaml (>= 4.13)
- **Build:** Make
- **Testing:** Custom test runner (`run_tests`)

## Project Structure

```
.
├── bin/           # Compiled executable
├── src/           # Source code
│   └── projet.ml
├── Makefile       # Build instructions
└── README.md      # This file
```

## Installation

1. Ensure OCaml and Make are installed.
2. Clone the repository.
3. Build the project:
   ```bash
   make
   ```

## Usage

```bash
make
make run     # build and run the executable
make clean   # remove build artifacts
```

## API Overview

### Types

| Type | Description |
|------|-------------|
| `dbtype` | Primitive types: `TInt`, `TText` |
| `coltype` | Column type: `(dbtype * bool)` where bool indicates nullability |
| `dbvalue` | Cell values: `VInt of int`, `VText of string`, `VNull` |
| `schema` | List of `(string * coltype)` |
| `row` | `dbvalue list` |
| `table` | `{ cols : schema; rows : row list }` |
| `fd` | Functional dependency: `(string list * string list)` |

### Core Functions

| Function | Type | Description |
|----------|------|-------------|
| `check_table` | `table -> bool` | Validates a table |
| `insert` | `table -> row -> table` | Inserts a row |
| `prod` | `table -> table -> table` | Cartesian product |
| `projection` | `table -> string list -> table` | Projects columns |
| `restrict` | `table -> (row -> bool) -> table` | Filters rows |
| `compute_deps` | `table -> fd list` | All satisfied FDs |
| `compute_elementary_deps` | `table -> fd list` | Elementary FDs |
| `normalization_level` | `table -> int` | Returns 1, 2, or 3 |

### Auxiliary Functions

- `check_row_type`, `common_cols_names`, `combine_rows`
- `get_indices`, `project_row`, `project_schema`
- `check_fd`, `subsets`, `is_elementary`
- `find_candidate_keys`, `complement`, `violates_3nf`

## Testing

The project includes a built-in test runner. After building, execute:
```bash
./bin/exec
```

Expected output:
```bash
./bin/exec
Running tests...

OK check_table: basic table valid
OK check_table: with NULL (nullable) valid
OK check_table: NULL on non-nullable column invalid

OK insert: adds row correctly
OK insert: rejects wrong length

OK prod: cartesian product correct
OK prod: rejects duplicate column names

OK projection: works correctly
OK projection: multiple fields in order
OK projection: rejects non-existent field

OK restrict: filters correctly
OK restrict: returns empty when all filtered

OK compute_deps: finds functional dependencies

OK compute_elementary_deps: returns elementary dependencies

OK normalization_level: partial dependency -> 1NF (got 1)
OK normalization_level: transitive dependency -> 2NF (got 2)
OK normalization_level: normalized -> 3NF (got 3)

All tests completed!
```

## Limitations

- **Performance:** FD generation explores all subsets of attributes, leading to exponential complexity.
- **Types:** Only integers and strings are supported; no booleans, dates, or floats.
- **Operations:** Missing union, difference, and join operations.
- **Interface:** No graphical or command-line interface beyond the test runner.

## Future Work

- Add more primitive types (bool, date, float).
- Implement additional relational operators (union, difference, natural join).
- Optimize FD discovery using pruning or heuristics.
- Develop a simple REPL or GUI for interactive queries.

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.

## Contributing

Contributions are welcome! Please open an issue or submit a pull request.

## Contact

For questions or feedback, please open an issue on GitHub.