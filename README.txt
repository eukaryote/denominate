DESCRIPTION

Denominate is a simple program and an associated library supporting bulk file and directory name normalization. If you’ve ever spent a long time changing filenames to get rid of whitespace or convert periods and underscores to hyphens, you know why it exists.

Denominate makes it very easy to normalize the names using a default scheme, and the associated library exposes enough reusable functionality that you can easily define your own custom renaming schemes. Additionally, it provides some useful generic functions related to file and directory walking and renaming.

INSTALL

If you have _make_ installed, you can use the Makefile to configure, build and install, as follows:

make all
make install

This configures the build to install as a user-package (doesn't require administrative rights) that is installed in the current user's home directory ($HOME/bin, $HOME/lib, etc.). It is equivalent to:

runhaskell Setup.hs configure --user --prefix=$HOME
runhaskell Setup.hs build
runhaskell Setup.hs install

If you wish to install globally or to use non-standard install options, you can run Cabal manually. For example, to do a default global install, you would do:

runhaskell Setup.hs configure
runhaskell Setup.hs build
runhaskell Setup.hs install

The last command will require administrative privileges to install the executable and library somewhere that they are usable by all users.

USAGE

The name of the executable that is built and installed is _denominate_. After installing, invoking the program is as simple as:

denominate <base_dir>

The sole argument is <base_dir>, which is the base directory to start renaming of files in. The base directory itself will not be renamed, but all files and directories in and below the base directory will be renamed using the default renaming scheme (which converts spaces, underscores, and periods to hyphens and eliminates non-alphanumeric characters, as well as converting all uppercase letters to lowercase letters).

EXAMPLE

Initial state before converting:

$HOME/Papers/
$HOME/Papers/index.HTML
$HOME/Papers/to_read/
$HOME/Papers/to_read/_A_MONAD_TUTORIAL__.PDF
$HOME/Papers/to_read/.CONFIG
$HOME/Papers/to_read/haskell-notes.txt
$HOME/Papers/to_read/low\ priority/
$HOME/Papers/to_read/low\ priority/Func-Prog-Notes##1.pdf
$HOME/Papers/to_read/low\ priority/Func-Prog-Notes##2.pdf

Final state after converting:

$HOME/Papers/
$HOME/Papers/index.html
$HOME/Papers/to-read/
$HOME/Papers/to-read/a-monad-tutorial.pdf.PDF
$HOME/Papers/to-read/.config
$HOME/Papers/to-read/haskell-notes.txt
$HOME/Papers/to-read/low-priority/
$HOME/Papers/to-read/low-priority/func-prog-notes-1.pdf
$HOME/Papers/to-read/low-priority/func-prog-notes-2.pdf

Note the following:

 * The name of the root directory was not changed.
 * File extensions are lowercased (HTML -> html) as well as the filename without the extension.
 * Initial and trailing non-alphanumerics are eliminated altogether (_A_MONAD_TUTORIAL__ -> a-monad-tutorial).
 * Files starting with a period keep the period (this is the only exception to the previous line).
 * Spaces are converted to hyphens (low\ priority -> low-priority).
 * Multiple non-alphanumerics are converted to a single hyphen (Func-Prog-Notes##1.pdf -> func-prog-notes-1.pdf) unless they are at the beginning or end of the filename without extension (in which case they are eliminated altogether).

HISTORY

 * Aug. 15, 2008: version 0.5; updated tests to use QuickCheck2 (no longer works with QuickCheck1), added README.txt, uploaded to Hackage.

 * Nov. 07, 2007: version 0.4.1; changed exposed module from Denominate to System.Denominate

 * Nov. 07, 2007: version 0.4; includes support for GHC 6.8 (in addition to 6.6), requires Cabal >= 1.2

 * Oct. 12, 2007: version 0.3; adds quickcheck tests to test corner cases

 * Oct. 11, 2007: version 0.2; fixes a directory renaming problem

 * Sep. 29, 2007: version 0.1; initial release

