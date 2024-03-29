# Guix package repository for use at the BIMSB

This repository provides Guix package definitions for use at the BIMSB
in addition to package definitions that come with GNU Guix.  These
package definitions cannot be added to GNU Guix upstream because they
are for proprietary software.


## Non-free packages

This project provides package definitions for applications and
libraries that have been released under non-free licenses or where the
license situation is not clear enough to be sure.

You should make sure that your use case is covered under the license
terms.  We encourage you not to use software that is released under
non-free licenses.

If you are the author of software listed here as non-free, and you
think that your software actually qualifies as free software, please
feel free to contact us via email.


# How to use

See [Specifying Additional
Channels](https://guix.gnu.org/manual/en/guix.html#Specifying-Additional-Channels)
in the Guix manual for instructions on how to add it to your installation
or simply add the following snippet to your `channels.scm`:

(channel
  (name 'guix-bimsb-nonfree)
  (url "https://github.com/BIMSBbioinfo/guix-bimsb-nonfree"))

# On Free Software

The GNU operating system has been developed so that users can have
freedom in their computing.  GNU is "free software", meaning that
users have the
[four essential freedoms](http://www.gnu.org/philosophy/free-sw.html):

0. to run the program
1. to study and change the program in source code form
2. to redistribute exact copies, and
3. to distribute modified versions.

Packages found in the Guix System Distribution provide only software
that conveys these four freedoms and that are permitted by the
[free software distribution guidelines](http://www.gnu.org/distros/free-system-distribution-guidelines.html).

Examples of non-free licenses include the original Artistic License or
open source licenses that forbid commercial usage.

If you consider releasing software, please avoid non-free licenses.  A
list of licenses that permit the above four freedoms and are
compatible with the GNU General Public License can be found on the
[Free Software Foundation's list of licenses](http://www.gnu.org/licenses/license-list.html#GPLCompatibleLicenses).
