# AoC2024

### running

e.g. day1 -> `dune exec day1`

### debugging
using day1 as an example:  
- need to add byte mode to `dune` file like the following stanza:

    ```
    (executable
    (public_name day1)
    (name main)
    (libraries day1 shared)
    (modes byte exe)
    )
    ```

- use `dune build` or whatever the path to the dune file is, or ` dune build --watch --terminal-persistence=clear-on-rebuild` for auto rebuilds
- given we are using a `dune-workspace` the bytecode exeuctable file will be at `_build/default/day4/bin/main.bc`  
- you can then debug this with `ocamldebug` or vs code.  

**for vs code:**  
(this doesnt work atm!)
- follow https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform
- after making a `launch.json` add `"stopOnEntry" : true`
- breakpoints can only be set in the copy of the `.ml` file in `${workspaceRoot}/_build/bin/main.ml` and not the original directory such as `${workspaceRoot}/day4/bin/main.ml` (todo surely this can be fixed)