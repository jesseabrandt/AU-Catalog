# Framework — AU-Catalog

**North star:** [north_star.md](north_star.md)

This project has a dual purpose: (a) analyze AU catalog text data using MiniLM
embeddings combined with existing regex-parsed structural metadata, and
(b) stress-test the `modelrunnR` API in real use. Work decisions should serve
both purposes.

## Invariants

1. **Course-year is the unit of analysis.** Cleaning never collapses or dedupes
   rows. Downstream code may aggregate; the pipeline preserves resolution.

2. **Existing regex-parsing outputs are preserved.** The structural metadata
   extracted from course descriptions (dept, course_num, credits, academic_year,
   etc.) is load-bearing — it's how embeddings join back to structure. Refactors
   of the parsing code are welcome; the parsed outputs should not silently
   change. If a refactor changes a field, call it out.

3. **One canonical pipeline.** Each step has one script, one purpose, linear
   flow from raw → cleaned → embedded → analyzed. No parallel half-finished
   versions.

4. **Reproducible runs.** Inputs, model versions, and outputs are traceable.
   `modelrunnR` is used where it fits (not forced).

5. **Readable steps.** A reader should be able to open the repo and trace what
   ran and why. Filenames, comments, and README reflect the pipeline order.

6. **Archive before adding.** New scripts, directories, or dependencies
   require either replacing something or being required by an active spec. No
   sprawl by accretion.

7. **`modelrunnR` friction is logged, not routed around.** This project
   doubles as a real-world test of the `modelrunnR` API. Awkward ergonomics,
   missing features, confusing errors, forced workarounds — each one is a
   finding worth capturing. Route findings to
   `/workspace/r-packages/modelrunnR/notes/AU-Catalog-findings.md`.

## Decision Tree

```
├─ Explicit in active spec/plan?          → EXECUTE
├─ Standing invariant above?              → EXECUTE
├─ Closed decision (spec/conversation)?   → EXECUTE
├─ Bug in call graph, blocking task?      → EXECUTE (note in commit)
├─ Bug outside call graph?                → SURFACE (don't touch)
├─ Design decision with >1 valid answer?  → ASK
├─ Touches shared state (remote/CI)?      → ASK
├─ Would violate an invariant?            → ASK
└─ Opportunistic cleanup, unrelated?      → QUEUE to todo (don't touch)
```

**Test for "closed decision":** Has the user already said what the answer is,
anywhere — spec, conversation, memory? If yes, execute. If no, ask.

## Session Start Ritual

1. Read `north_star.md`
2. Read this framework
3. Read the most recent spec in `docs/superpowers/specs/`
4. `git status` + `git log --oneline -5`
5. **State intent:** current task, files to touch, invariants in play, session
   mode (interactive / autonomous)

## Completion Criteria

- All spec/plan steps committed
- Invariants verified — especially #1 (course-year resolution), #2 (parsing
  outputs unchanged), and #3 (no parallel pipelines)
- `/audit` on changed files
- No stranded references (broken paths to archived or moved files)
- Any `modelrunnR` friction logged to the findings file (invariant #7)
- Gaps documented for next task
