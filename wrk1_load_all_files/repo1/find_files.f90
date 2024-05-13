module find_files
use windows_h

type  WIN32_FIND_DATAW {
  DWORD, sequence::  dwFileAttributes;
  integer*4, sequence :: ftCreationTime;
  integer*4, sequence :: ftLastAccessTime;
  integer*4, sequence :: ftLastWriteTime;
  integer*4, sequence :: nFileSizeHigh;
  integer*4, sequence :: nFileSizeLow;
  integer*4, sequence :: dwReserved0;
  integer*4, sequence :: dwReserved1;
  integer*4, sequence :: cFileName[MAX_PATH];
  integer*4, sequence :: cAlternateFileName[14];
  integer*4, sequence :: dwFileType; ! Obsolete. Do not use.
  integer*4, sequence :: dwCreatorType; ! Obsolete. Do not use
  integer*4, sequence :: wFinderFlags; ! Obsolete. Do not use
end type WIN32_FIND_DATAW

implicit none
integer, dllimport :: FindFirstFileA


end module find_files