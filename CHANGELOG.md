# Revision history for heist-extra

## 0.5.0.0 (unreleased)

- Add syntax highlighting for code blocks using skylighting (#10)
- Remove prism.js `language-*` class hack (no longer needed with static highlighting)
- Add static math rendering for `$...$` and `$$...$$` via `texmath` (LaTeX → MathML at build time)
- Wrap raw HTML/inline blobs in a unique `<rawhtml>` element (with `display: contents`) instead of `<div>`/`<span>` ([#13](https://github.com/srid/heist-extra/pull/13)). Avoids xmlhtml's "div cannot contain text looking like its end tag" crash when raw HTML contains a literal `</div>` — most painfully, mermaid SVG with `<foreignObject><div>…</div>` HTML labels. Fixes [srid/emanote#119](https://github.com/srid/emanote/issues/119).
- Pandoc `Table` rendering now applies the AST fields it had been silently dropping ([#15](https://github.com/srid/heist-extra/pull/15)): per-column alignment from `ColSpec` (with cell-level `Alignment` overriding the column default), column widths via a generated `<colgroup>`, cell `RowSpan`/`ColSpan` as `rowspan`/`colspan` attributes, row & cell `Attr` merged into the rendered `<tr>`/`<th>`/`<td>`, and `TableFoot` rows rendered into `<tfoot>`. Captions are still skipped — commonmark-hs doesn't emit them. Pure helpers (`alignmentStyle`, `colSpecsToColgroup`, `cellSpanAttrs`, `cellColumnIndices`, `mergeStyleKVs`) live in the new `Heist.Extra.Splices.Pandoc.Render.Internal` module so consumers get a clean public API. Fixes [srid/emanote#27](https://github.com/srid/emanote/issues/27).
- Group orphan opener/closer raw-HTML blocks around the markdown content between them, so the renderer emits a real DOM element instead of two stranded `<rawhtml>` wrappers. CommonMark "type 6" HTML blocks end at the next blank line, which makes Pandoc split `<details>\n\nbody\n\n</details>` into three blocks: an opener `RawBlock`, a `Para`, and a closer `RawBlock`. Without grouping, each raw blob lives in its own wrapper and the markdown paragraph ends up a sibling of the (empty) details element rather than its child. The new `Heist.Extra.Splices.Pandoc.RawHtmlGroup.groupRawHtmlBlocks` pass walks the AST and rewrites those triplets into a `B.Div` carrying the tag in its `"tag"` attribute, which the renderer already turns into the named element. The `Div` arm of the renderer now also strips that `"tag"` directive from the serialized attributes so it doesn't leak as a literal `tag="…"` on the output element. Fixes [srid/emanote#433](https://github.com/srid/emanote/issues/433).
- `RenderCtx` gains a typed `userData :: Dynamic` slot (default `toDyn ()`), with `getUserData` / `setUserData` accessors. Lets a downstream library attach per-render state — for example, an embed-ancestor stack used by Emanote's cycle detection — without that state having to live on every renderer's signature. **Backwards-incompatible**: the `RenderCtx` data constructor is no longer exported (the data declaration moved to a new `Heist.Extra.Splices.Pandoc.Ctx.Internal` module for in-package use). Callers must now construct via `mkRenderCtx` / `emptyRenderCtx` and cannot pattern-match with `RenderCtx { … }` or use `RecordWildCards` — field-accessor reads and record-update writes (`ctx { blockSplice = … }`) continue to work. Closes [srid/emanote#684](https://github.com/srid/emanote/issues/684).

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
