# hare-mode
An emacs mode for editing the hare language in Emacs. It includes:

- Syntax highlighting
- Indentation
- Some keybindings

## Indentation
Hare mode uses SMIE for indentation. It defines a simple grammar, and tries to
indent accordingly.  Until all quirks are fixed in the grammar and indentation,
we offer some indentation bindings, making handling of indentation a little
easier.

## Bindings

| Binding | Name                          |
|---------|-------------------------------|
| tab     | `hare-mode-indent-forward`    |
| backtab | `hare-mode-indent-backward`   |


