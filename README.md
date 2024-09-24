# Jonathan's Emacs Configuration

As the [warning says](emacs.el) Many years of cruft here.

Some of that is from using different flavors of Emacs on assorted platforms
over the decades, attempting to maintain compatibility as the world evolves.
Most from perceived attempts to make things work more easily in the context du
jour.

## Installation

What I do: create a symlink: `ln -s [wherever you have cloned this git repo]
~/.emacs.d` and start Emacs.

## Discussion

`init.el` is the starting point whose purpose is to orchestrate:

1. Loading `config.el`, or making a copy from the `config-generic.el` starter,
   and issuing a warning about personalizing `config.el`

1. Warning borrowers who might think using this is a good idea, originally
   from back in the day when files like these lived on multi-user systems
   where one learned by looking at others' set ups. These stop if you follow
   the instructions.
   
1. Refresh any byte-compiled files resulting from some other version of Emacs,
   which I've discovered is a cause of many mysterious problems resulting from
   switching between Emacs versions.

1. Byte-compile the main initialization file, from back when byte-compiling
   made a substantial difference in start up times. Some habits die hard.

Most of the customization happens in the main - `emacs.el` - initialization file.
When packages use `custom.el` I move most customization into `emacs.el` so I
have one place to look; although I also define a `custom-file` - see comments in
`config-generic.el` - so `init.el` does not get modified.

## Compatibility

While there are conditionals referencing other versions of emacs the only time
I address these are when I need to use multiple versions, rarer than it used
to be.

Most recently I have reverted to using the [Mitsuharu Yamamoto's Emacs Mac
Port](https://github.com/railwaycat/homebrew-emacsmacport), via `brew install
emacs-mac --HEAD` in order to clone its git repo and use the current work
branch to build the formula from source. That resulted in, as of this writing,
Emacs 29.4. For Finder convenience I created a [helper
app](https://github.com/railwaycat/homebrew-emacsmacport/blob/master/docs/emacs-start-helpers.md#user-content-helper-2).

Prior to that I was using the GNU
[EmacsForMacOS](https://emacsformacosx.com/), with native GUI support, version
on macOS. However starting with versions 29.2 I've run into a lot of
beach-balling / crashes / and issues with copy and paste not working that all
resist debugging attempts.

On Debian Linux I use whatever the current standard installation produces,
currently 28.2
