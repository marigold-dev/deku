# LIGO VSCode Plugin

This plugin is an LSP implementation for the LIGO language family.

Currently, it is highly experimental and may contain bugs. Language Server capabilities on Windows are supported only if running in WSL mode.

## Functionality
Code navigation

- [x] Jump to definition
- [x] Find references
- [x] Folding range
- [x] Selection range
- [x] Jump to type definition (limited)
- [x] Document symbols
- [ ] Workspace symbols

Diagnostics

- [x] Parser diagnostics
- [x] Compiler diagnostics (if LIGO is available in PATH)

Code editing

- [x] Hover suggestions
- [x] Rename symbol
- [x] Code completion for variable names
- [x] Code completion for record fields and constructors (limited)
- [x] Signature help
- [ ] Refactorings

Formatting

- [x] Whole document formatting
- [ ] On-type formatting
- [ ] Document range formatting

## Releasing the plugin

Once the plugin is ready for release, code should be pushed to the `vscode-production` branch.
`vscode-extension-publish` job should be triggered manually from this branch pipeline.

You can read more about manual interaction with the pipeline [here](https://docs.gitlab.com/ee/ci/pipelines/#add-manual-interaction-to-your-pipeline).
