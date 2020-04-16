Currently Netica does not ship with pkg-config support.  These files
provide the missing support.


Netica version 6.07 -- Edit the first line of `netica.pc` to reflect
the location for Netica source files on your system.  Then move
`netica.pc` to to someplace that `pkg-config` will find it.  On Ubuntu
Linux 18.04 I use `/usr/lib/pkgconfig`, or `/usr/share/pkgconfig`.ls

Netica version 5.04 -- Use the file `netica504.pc` instead.  Rename
the file to `netica.pc` when you install it (so it will get
overridden when you upgrade to version 6.07).

For Windows computer, pkg-config can be installed using 
(https://chocolatey.org/packages/pkgconfiglite)[chocolatey].
The location of the files should be in the variable `PKG_CONFIG_PATH`,
but I can't figure out what that value is.  

On my computer, I'm using a version of `pkg-config` written in perl
and distributed as part of the (https://strawberryperl.com)[Stawberry
Perl].  It places is config files in `C:\Strawberry\c\lib\pkgconfig`.

The command `pkg-config --variable pc_path pkg-config` should return
the path for `.pc` files, on a properly configured system (it doesn't
seem to work with the Strawberry implementation).  You could always
try searching for `.pc`.
