# Revision history for heist-extra

## 0.5.0.0 (2025-12-18)

- Add syntax highlighting for code blocks using skylighting (#10)
- Remove prism.js `language-*` class hack (no longer needed with static highlighting)

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
