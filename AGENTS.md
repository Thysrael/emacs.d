# Repository Instructions

This is a personal Emacs configuration, not an application project. Keep changes compact, modern, consistent, and maintainable.

## Entry Points
- `early-init.el` contains startup-only optimizations; keep it small and avoid package setup there.
- `init.el` decides what loads through `thy/init-files`, then `load-file`s matching `core/init-*.el` files.
- Do not assume a module is active just because it exists; check `thy/init-files` first. The local-machine module list is currently empty.

## Package Conventions
- Package setup uses built-in `package.el`, `package-vc`, and `use-package`; do not introduce straight.el/elpaca without an explicit request.
- Prefer `use-package` with `:ensure`, `:hook`, `:bind`, `:commands`, `:custom`, and minimal `:config`.
- Avoid broad eager loading. Use `:demand t` only when a package must be active immediately.
- Built-in packages should use `:ensure nil` when configured with `use-package`.
- `no-littering` is configured in `core/init-util.el`; generated/cache files should go through its `no-littering-expand-*` helpers when possible.

## Style
- Use `lexical-binding` in Emacs Lisp files.
- Prefer current Emacs APIs and simple built-ins over compatibility shims.
- Prefer native Emacs features over custom scripts unless the built-in path is clearly insufficient.
- Prefix custom functions and macros with `thy/`; do not introduce new `+` or `my/` names.
- Add docstrings to custom functions unless the function is purely local and trivial.
- Keep comments in English and only where they explain non-obvious behavior.
- Use `;` for end-of-line comments and `;;` for standalone comments. Avoid `;;;` section comments except for conventional file headers such as `;;; -*- lexical-binding: t -*-`.
- Keep related settings in the module that owns the feature; avoid duplicating package config across modules.
- In `early-init.el`, keep `setq` forms to one variable per form.

## Verification
- Fast syntax/load check: `emacs --batch --load early-init.el --load init.el`.
- For a touched module: `emacs --batch --load early-init.el --load core/init-package.el --load core/init-util.el --load core/<module>.el`.
- For compile checks: `emacs --batch -Q --eval '(byte-compile-file "core/<module>.el")'` may miss repo initialization, so prefer a load check when package state matters.
- There is no Makefile, CI workflow, lockfile, or formatter config in this repo; do not invent project commands.
