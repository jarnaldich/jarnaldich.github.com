## Setting Up Spacemacs for Python Development
For those of you who do not know yet, [Spacemacs](https://github.com/syl20bnr/spacemacs) is an emacs configuration focused on ergonomics with EVIL mode (VI - like emulation) turned on by default.

Spacemacs comes with [configuration layers](https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org) to enhance its functionality for particular purposes. For example, there are layers for specific languages or frameworks. Of course, there is a python layer included in the installation, but I had to tune some vars to make it work for me. This blog posts explains the tweaking process.

## Activate the Python layer

User configuration is placed in a `~/.spacemacs` file. You can also edit this file directly by pressing `SPC f e d` in spacemacs.

To activate the python layer, you just need to add a `python` entry into the `dotspacemacs-configuration-layers` inside the `dotspacemacs/layers` `defun`.

After doing so, the configuration can be reloaded with `SPC f e R`.

## Tweak projectile

Minor configurations should be placed inside the `dotspacemacs/config` function in `.spacemacs`. Spacemacs comes with [Projectile](http://batsov.com/projectile/), an emacs mode for project-wide configurations. 

### Make sure you have a `setup.py`... or whatever
Projectile detects what kind of project you are working in from the existence of some "marker" file. You can check which projects does projectile recognize by inpecting the `projectile-project-types` hash, or by looking at the contents of the `projectile.el` file. 
If you do the latter, the relevant project types for python programming and its associated attributes look like:

```lisp
(projectile-register-project-type 'django '("manage.py") "python manage.py runserver" "python manage.py test")
(projectile-register-project-type 'python-pip '("requirements.txt") "python setup.by build" "python -m unittest discover")
(projectile-register-project-type 'python-pkg '("setup.py") "python setup.py build" "python -m unittest discover")
(projectile-register-project-type 'python-tox '("tox.ini") nil "tox")
```

So you should try to make sure that Projectile and you agree on the kind of project you have at hand. In particular, if you are developing a vanilla python module, make sure you include the proper `setup.py` file. You can check if the configuration went ok with the command:  `projectile-project-info`

Also notice that the rest of arguments will be the defaults for building a project and making tests, in case you need to tweak something.

### Toggle between test and implementation
One of the things proectile can do is switching between the test and implementation. You may need to include a function to accomodate whatever convention you use for test files and add it to the `dotspacemacs/config` defun. In my case, I use a `test_` prefix in my test files.

```lisp
  (setq projectile-test-prefix-function
        (lambda (x)
          (cond ((eq x 'python-pkg)
                 "test_")
                (t (error "Prefix not defined for this project type")))))
```

After reloading, `SPC p a` should toggle between test and implementation. `C-c p P` will run all your project tests, after prompting for the apropiate command. In my case, something like:

```
python -m unittest discover -p "test_*.py"
```

## See also
- Read more about spacemacs tweaking in general [here](http://thume.ca/howto/2015/03/07/configuring-spacemacs-a-tutorial/)
- [This page](https://touk.pl/blog/2015/10/14/getting-started-with-haskell-stack-and-spacemacs/) will help Haskell / Stack users set up spacemacs.