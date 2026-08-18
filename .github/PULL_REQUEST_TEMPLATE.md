<!-- What does this change and why? -->

## Checklist

- [ ] `dune build @all`, `dune runtest`, `dune build @fmt @doc` pass locally
- [ ] `dune build @dogfood-fast` passes
- [ ] New behavior has unit/contract tests; process, Dune, PPX, or cache
      behavior has a fixture
- [ ] Config or JSON surface changes update the schemas and their docs
- [ ] Operator-contract changes (rules, profiles, dedup precedence) update the
      frozen registry table in `test/unit_tests.ml` and the fixture full-ID
      goldens in `test/e2e_tests.ml`, and are called out in the PR description
- [ ] No path writes into an original target workspace
- [ ] `CHANGELOG.md` is updated for user-visible changes
