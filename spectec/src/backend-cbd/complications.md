# complications

1. spectec's control flow model is still based on admin instructions and code rewriting
    - Looking at BR, it seems possible to auto translate into some sort of control stack scheme,
      but would basically be hardcoding
    - BR itself would probably have to actually be hardcoded to avoid the recursive countdown to 0
2. The spectec stack has instructions on it
    - partly because of the admin instruction stuff, but also with constants etc
    - so some instructions like I32_CONST don't even really have rules in spectec
3. Many spectec assignments have some sort of globbing/pattern matching, a direct
   translation would require arrays in CBD, but ideally we would want to translate
   into a loop without intermediate data structures, probably requiring some dataflow
   analysis
4. Spectec rewrite rules give the instructions params, which have to be translated to reads
    - this isn't that bad but might be complicated in the presence of variable length instrs

# idea: leverage the spectec interpreter for analysis

We could maybe use the existing interpreter, but make it abstract, and generate code
instead of directly evaluating. It has constructs like push and pop context, which
seem translatable to blocks etc. However it still has some higher level stuff like
lists, and still uses admin instrs and rewriting.

# idea: make our own ast, translate from AL

I think rewriting AL into our own AST, using some ideas from the interpreter, would
be the best path. But I think it would still have to have some hardcoding around
control flow, e.g. BR specifically would be very hard to rewrite.
