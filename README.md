# Introduction

Emacs initialization file with focus on included tools and
usability. The file is largely based on [Prot's
dotemacs](https://protesilaos.com/dotemacs/) and tries to include
[Doom
Emacs'](https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly)
good practices for startup time. Startup time is reduced through
deferred loading as much as possible.

This is an eternal work in progress as is Emacs. 

# Structure

- `init.el` : main config file.
  
- `snippets` : additional mode-specific code snippets. 

- `lisp` : Lisp code, helper functions or useful pieces of code.
  (empty as of now)


## Navigation

I use the excellent `selectrum` and `ctrlf` to navigate and
search. Projects can be set and then explored with `treemacs`.

## LSP-mode

Most of the programming configuration gravitates around `lsp-mode`,
the language server protocol mode. It's particularly helpful to
deal with multiple programming languages in a similar fashion.

## Writing

I often write in Org-mode so I use `flyspell` to check out my writing
mistakes. I also use snippets for code blocks or LaTeX inserts which I
use all the time. For easy and quick referencing, I use the excellent
`org-ref` package. 

There's additional code pertaining to my writing configuration, such
as LaTeX classes and compilation. 

## Snippets

Thanks to `yasnippet`, I have a handy way to insert code blocks or
expressions in any file, be it text or code files.

## Ju(lia)Pyt(hon)R

Works well for Python/R/Julia for now. It supports different
environments in org-mode to obtain files *à la* notebook,
`emacs-jupyter` does the heavy lifting. 

I used `conda.el` to switch environments for Python but `elpy`, my
Python IDE actually relies on `pyvenv` so I'll stick with that
instead.

## Miscellaneous

A lot of good ideas from my configuration come from helpful users
with excellent initialization files, one can look at : 

- [Prot's dotemacs](https://protesilaos.com/dotemacs/)

- [Steve Purcell's init file](https://github.com/purcell/emacs.d)

- [The using Emacs blog](https://cestlaz.github.io/stories/emacs/)

- [Jamie Collinson's init file](https://jamiecollinson.com/blog/my-emacs-config/)




