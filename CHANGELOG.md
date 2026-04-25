# Revision history for heist-extra

## 0.5.0.0 (unreleased)

- Add syntax highlighting for code blocks using skylighting (#10)
- Remove prism.js `language-*` class hack (no longer needed with static highlighting)
- Add static math rendering for `$...$` and `$$...$$` via `texmath` (LaTeX → MathML at build time)
- Wrap raw HTML/inline blobs in a unique `<rawhtml>` element (with `display: contents`) instead of `<div>`/`<span>` ([#13](https://github.com/srid/heist-extra/pull/13)). Avoids xmlhtml's "div cannot contain text looking like its end tag" crash when raw HTML contains a literal `</div>` — most painfully, mermaid SVG with `<foreignObject><div>…</div>` HTML labels. Fixes [srid/emanote#119](https://github.com/srid/emanote/issues/119).

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
