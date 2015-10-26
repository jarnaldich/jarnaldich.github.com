## Setting Up Spacemacs for Python Development
For those of you who do not know yet, [Spacemacs](https://github.com/syl20bnr/spacemacs) is an emacs configuration focused on ergonomics with EVIL mode (VI - like emulation) turned on by default.

Spacemacs comes with [configuration layers](https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org) to enhance its functionality for particular purposes. For example, there are layers for specific languages or frameworks. Of course, there is a python layer included in the installation, but I had to tune some vars to make it work for me. This blog posts explains the tweaking process.

## 1 - Activate the python layer

User configuration is placed in a `~/.spacemacs` file. You can also edit this file directly by pressing `SPC f e d` in spacemacs.

To activate the python layer, you just need to add a `python` entry into the `dotspacemacs-configuration-layers` inside the `dotspacemacs/layers` `defun`.

After doing so, the configuration can be reloaded with `SPC f e R`.



## See also
- Read more about spacemacs tweaking in general [here](http://thume.ca/howto/2015/03/07/configuring-spacemacs-a-tutorial/)
- [This page](https://touk.pl/blog/2015/10/14/getting-started-with-haskell-stack-and-spacemacs/) will help Haskell / Stack users set up spacemacs.