# Hugo.el

This is an Emacs package that provides an interactive interface for the Hugo
static site generator. It is loosely modeled after the great Magit package and
provides not only an interactive content management facility, but also some
minor mode niceties that make blogging on the platform a little easier.

The underlying code is a nearly wholesale copy of my `octopress.el` package,
which provided nearly identical facilities for the Octopress static site
generator, so if you ever used that, you should feel right at home with this.

## Installation

For now, this package is only available on GitHub and you must clone or download
the script and install it in your load path. You might accomplish this using Git
by running this command in your `user-emacs-directory`:

```sh
$ git clone https://github.com/aaronbieber/hugo.el hugo
```

Then you can add that directory to your load path and load the package by adding
these lines to your `init.el` (or similar):

```cl
(add-to-list 'load-path (expand-file-name "hugo" user-emacs-directory))
(require 'hugo)
```

Feel free to use `use-package` or whatever you're comfortable with.

## Usage

The primary interface into `hugo.el` is via the function `hugo-status`, which
will first prompt you for the path to your site's root directory (unless you run
it from a buffer visiting a file that can be discerned to live within a Hugo
site), and then display the main status pane.

Within the status pane, you will see the absolute path to your site on disk, the
status of the local server, and a list of your drafts and posts.

You can press `?` at any time to open the help pane, and from there you can
easily run any of the provided commands, which should be self-explanatory.

## Configuration

There are configurable options accessible via the `customize` facility in
Emacs. Run `M-x customize-group RET hugo RET` to see them.

## Errata

If you are using `hugo.el` to author multiple Hugo sites, you will undoubtedly
encounter problems. While most of the internals of the package are written to
accommodate multiple sites, the configurable settings are stored centrally and
so it is currently impossible to customize any of the settings for only a single
site.

Further, switching from one Hugo site to another during the same Emacs session
may not always work as expected. Generally, if you run `hugo-status` from within
a buffer visiting a file within a Hugo site, it should figure it out
automatically and you'll be on your way. If ever that does not happen, open an
issue.

These are problems that have existed in `octopress.el` since the beginning, and
I am not likely to solve them until I am juggling a few Hugo sites and have the
need.
