25 July 2005
------------

The files/directories in this directory were moved from
Tina McCoy's (tmmccoy) home directory.

The permissions of the transferred files are such that 
they are now owned by Donna O'Leary and are in the "facadm"
Unix group.  As of this date, the following users are in
this group: appel, arora, aslp, becci, doleary, doug, dpd,
felten, funk, gch, jps, ken, tmmccoy, bwk, and jwiddis.

As these files were originally created with a Unix editor,
they had Unix line endings (linefeed).  For the following
subdirectories only, these text files were converted so
that they have DOS (i.e., Windows) line endings (carriage
return, line feed):

classof05  classof06  classof07

The text files in the above subdirectories were also
renamed to have a ".txt" appended to the original file
name.  Under Windows, this will generally associate the
file with a simple editor application such as Notepad.

Additional technical notes (for posterity):
   * The files/directories are owned by doleary.facadm
   * Directories are setgid so that new files and
     subdirectories are also in the facadm group.
   * "other" access was removed: chmod o-rwx
   * When new files are created, they may or may not
     have the correct "other" permission.  Since the
     files are in restricted directories, this is
     not a problem.  However, a chmod o-rwx from time
     to time is a good idea.
