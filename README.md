Complete keepassxc-cli with [Ivy](https://github.com/abo-abo/swiper/).

Install `counsel-keepassxc`

    (add-to-list 'load-path "<path-of-counsel-keepassxc>")
    (require 'counsel-keepassxc)
    (setq counsel-keepassxc-database-file "path/of/keepassxc-database")

Start search keepassxc-cli with [Ivy](https://github.com/abo-abo/swiper/)

    M-x counsel-keepassxc

Keybindings

   `RET` view entry(default action)


Type `M-o` to call other actions

   `u` copy username

   `p` copy password

   `l` copy url

   `n` copy notes

   `a` add entry

   `c` clone entry

   `e` edit entry

   `d` delete entry

    
`view` `add` `clone` `edit` a entry will open a entry buffer with keybindings

   `C-c C-k` kill entry buffer without commit

   `C-c C-c` commit modified entry and close entry buffer

   `C-c C-e` enter edit mode

   `TAB` move to next field

