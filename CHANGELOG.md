# Revision history for heist-extra

## 0.5.0.0 (unreleased)

- Add syntax highlighting for code blocks using skylighting (#10)
- Remove prism.js `language-*` class hack (no longer needed with static highlighting)
- Add static math rendering for `$...$` and `$$...$$` via `texmath` (LaTeX → MathML at build time)
- `RenderCtx` gains an `idPrefix` field (default `""`) and a new
  `footnote:id` splice is exposed (#12). The splice emits
  `idPrefix <> show idx` and is intended for the `id="…"` attributes
  on footnote refs and list items. Unprefixed rendering is unchanged;
  callers only need to act when they render multiple documents into a
  single page (e.g. note embedding) and want to namespace footnote IDs.
- Add `pandocSpliceWithoutFootnoteList`: renders a `Pandoc` doc,
  wires footnote refs, but does not emit the trailing `<ol>` of
  footnote definitions. Intended for custom block renderers (e.g.
  callout bodies) that are themselves invoked inside an outer
  `pandocSplice` — the outer already emits the document-level list
  via `gatherFootnotes`, so the nested render must not emit a second
  copy.
- Re-export `renderPandocWith` for callers that want to take over
  footnote handling entirely.

## 0.4.0.0 (2025-08-19)

- **Backwards incompatible** changes
  - Improved Header renderer to included heading ID (#6)
- Handle simple inline raw html such as for `<kbd>` (#8)

## 0.3.0.0 (2023-08-09)

- `treeSplice`: pass children to sortKey function (#2)
- Remove video format hack (#4)
- Require `pandoc-types >= 1.23` (adds `Figure` block support)

## 0.2.0.0 (2022-11-14)

- Drop `heist-emanote`, and depend instead of the updated `heist` package.

## 0.1.0.0 (2022-09-24)

Initial release.
