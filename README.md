# Jonathan's Emacs Configuration

As the [warning says](emacs.el) Many years of cruft here.

Some from using different flavors of emacs on assorted platforms over the
decades, attempting to maintain compatibility as the world evolves. Most from
perceived attempts to make things work more easily in the context du jour.

`init.el` is the starting point whose purpose is to orchestrate:

1. loading `config.el`, or it's `config-generic.el` starter

1. Issuing a warning about making a real `config.el`

1. Warn borrowers who might think using this is a good idea, from back in the day
   when these lived on multi-user systems where one learned by looking at
   others' set ups.
1. Set up where the rest can be found
1. Byte-compile the main initialization file, from back when byte-compiling
   made a substantial difference in start up times.

Most configuration happens in the main - `emacs.el` - initialization file.
When packages use `custom.el` I move most customization into `emacs.el` so I
have one place to look.

While there are conditionals referencing other versions of emacs the only time
I address these are when I need to use multiple versions, rare these days so YMMV.

Currently I primarily use the GNU
[EmacsForMacOS](https://emacsformacosx.com/), version 29.1 with native GUI
support version on macOS, occasionally reverting to [Mitsuharu Yamamoto's Emacs Mac
Port](https://github.com/railwaycat/homebrew-emacsmacport)
