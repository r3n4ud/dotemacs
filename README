This is my personal GNU/Emacs configuration.

```bash
cd
git clone --recurse-submodules https://github.com/r3n4ud/dotemacs.git
ln -s dotemacs/emacs.d .emacs.d
cd .emacs.d
emacs -Q --batch --eval "(progn
  (require 'ob-tangle)
  (org-babel-tangle-file \"emacs.org\"))"
```
