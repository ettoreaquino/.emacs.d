# An attempt to dive in deeper waters with Emacs.
Emacs has an initialization file usually called .emacs, .emacs.el or init.el.
Emacs looks for this within your home directory "~/" or at the .emacs.d directory.

I particularly like the idea of using the "init.el", for the simple reason that it
stays within the .emacs.d/ directory, which can be maintained using git with all the
other necessary emacs files and packages. 

## Overall Setup
All packages can be cloned by grabing the dependencies
 `git clone https://github.com/ettoreaquino/.emacs.d.git`

## Java Setup with Eclim
### Eclipse
  In case you don't have Eclipse installed: " https://www.eclipse.org/downloads/ "
  
### Eclim
  Eclim is a protocol for communication with and Eclipse server from some client.
  Get the core protocol from " http://eclim.org/install.html " downloading the ".bin" file.

In case you are running in a linux system:
  1. Place the ".bin" file inside your eclipse/ directory.
  2. `chmod +x eclim_##.bin` where `##` stands for the version number downloaded;
  3. `./eclim_##.bin` in case a permission is denied `sudo ./eclim_##.bin`
  4. `emacs`
     1. `M-x list-packages`
     2. Install `eclim`
     3. Install `ac-emacs-eclim` "auto complete"
  
## Survival guide for the first week of emacs
The following guide was composed from magnars tamplate:
 "https://github.com/magnars/.emacs.d/blob/master/README.md"
and slowly improved using Emacs manual:
 "https://www.gnu.org/software/emacs/manual/"

When you start using emacs for the first time, your habits fight you every inch
of the way. Your fingers long for the good old familiar keybindings. Here's an
overview of the most commonly used shortcuts to get you through this pain:

* `C      ` Shorthand for the ctrl-key
* `M      ` Shorthand for the meta-key (bound to cmd on my mac settings)
* `S      ` Shorthand for the shift-key

### Files

* `C-x C-f` Open a file. Starts in the current directory
* `C-x f  ` Open a recently visited file
* `C-x o  ` Open a file in the current project (based on .git ++)
* `C-x C-s` Save this file
* `C-x C-w` Save as ...
* `C-x C-j` Jump to this files' current directory
* `C-x b  ` Switch to another open file (buffer)
* `C-x C-b` List all open files (buffers)

### Cut copy and paste

* `C-space` Start marking stuff. C-g to cancel.
* `C-w    ` Cut (aka kill)
* `C-k    ` Cut till end of line
* `M-w    ` Copy
* `C-y    ` Paste (aka yank)
* `M-y    ` Cycle last paste through previous kills
* `C-x C-y` Choose what to paste from previous kills
* `C-@    ` Mark stuff quickly. Press multiple times

### General

* `C-g    ` Quit out of whatever mess you've gotten yourself into
* `M-x    ` Run a command by name
* `C-.    ` Autocomplete
* `C-_    ` Undo one entry of the undo records
* `M-_    ` Redo
* `C-x u  ` Show the undo-tree
* `C-x m  ` Open magit. It's a magical git interface for emacs
* `C-x C-c` Kill Emacs (save-buffers-kill-emacs)
* `C-z    ` Suspend Emacs (suspend-emacs). Resume from shell with %emacs

### Navigation

* `C-arrow` Move past words/paragraphs
* `C-a    ` Go to start of line
* `C-e    ` Go to end of line
* `M-g M-g` Go to line number
* `C-x C-i` Go to symbol
* `C-s    ` Search forward. Press `C-s` again to go further.
* `C-r    ` Search backward. Press `C-r` again to go further.

### Buffer Menu (`C-x C-b`)
  https://www.gnu.org/software/emacs/manual/html_node/emacs/Several-Buffers.html
* "." Indicates the buffer is current
* "%" Indicates the buffer is a read-only
* "*" Indicates the buffer is modified
* `d  ` Flag buffer for deletion. Move point down to next line
* `s  ` Flag buffer for saving
* `x  ` Perform all flagged deletions and saves
* `u  ` Remove all flags from the current line. Move down

### Window Management

* `C-x 0  ` Close this window
* `C-x 1  ` Close other windows
* `C-x 2  ` Split window horizontally
* `C-x 3  ` Split window vertically

### Frame Management
* `C-x 5 2` Create a new frame
* `C-x 5 b` Selects a buffer in another frame
* `C-x 5 f` Visit a file and select its buffer in another frame
* `C-x 5 d` Select a Dired buffer for a directory in another frame
* `C-x 5 0` Close the selected frame

### Help

* `F1 t   ` Basic tutorial
* `F1 k   ` Help for a keybinding
* `F1 r   ` Emacs' extensive documentation
